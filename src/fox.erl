-module(fox).
-on_load(init/0).
-export([btl/2, ltb/2, array/1, array/2, cast_array/2, linspace/3, op/2, op/3]).%, op/3, op_nif/4]).


-type int_seq()::    <<_:1, _:_*32>> | [pos_integer(), ...] | pos_integer().  % How a list of ints can be represented.
-type double_seq():: <<_:1, _:_*64>> | [number()     , ...] | number().       % How a list of doubles can be represented.
-type num_seq()::int_seq()|double_seq().

-record(array,{content::double_seq(), shape::int_seq(), stride::int_seq()}).
-type array()::#array{}.

init()->
  Dir = case code:priv_dir(fox) of
              {error, bad_name} ->
                  filename:join(
                    filename:dirname(
                      filename:dirname(
                        code:which(?MODULE))), "priv");
              D -> D
          end,
    SoName = filename:join(Dir, atom_to_list(?MODULE)),
    erlang:load_nif(SoName, 0).



% Handle transfer of list to binaries/binaries to lists.
-type c_number()::i|d.
-spec btl(num_seq(), c_number()) -> [number()].
btl(B,T)->
  case B of
    L when is_list(L)   -> B;
    N when is_number(N) -> [B];
    _ ->
      case T of 
        d -> F = fun F(<<>>)->[]; F(<<H:64/native-float,   Tl/binary>>) -> [H|F(Tl)] end, F(B);
        i -> F = fun F(<<>>)->[]; F(<<H:32/native-integer, Tl/binary>>) -> [H|F(Tl)] end, F(B);
        _ -> throw("Invalid argument: expected i or d")
      end
  end.

-spec ltb(num_seq(), c_number()) -> binary().
ltb(L,T)->
  case L of
    B when is_binary(B) -> B;
    I when is_number(I) -> ltb([I],T);
    _ ->
    case T of 
      d -> lists:foldl(fun(H,Acc)-> <<Acc/binary, H:64/native-float>>   end, <<>>, L);
      i -> lists:foldl(fun(H,Acc)-> <<Acc/binary, H:32/native-integer>> end, <<>>, L);
      _ -> throw("Invalid argument: expected i or d")
    end
  end.


% Creates stride array.
-spec gen_strides(int_seq())-> int_seq().
gen_strides(Shape)->
  case Shape of
    L when is_list(L) -> 
      {Strides,_} = lists:mapfoldr(fun(H,Acc)-> {Acc, H*Acc} end, 1, Shape),
      Strides;
    B when is_binary(B) ->
      % Failed to to it directly with a binary
      ltb(gen_strides(btl(B, i)), i);
    N when is_number(N) ->
      [1];
    _ ->
      io:format("Input ~w ", [Shape]),
      throw("Unknown input")
    end.


% Creates an array that can be read in C.
-spec array(double_seq(), int_seq())-> array().
array(Content, Shape)->
  Content_bin = ltb(Content,d),
  Shape_bin   = ltb(Shape,i),
  Stride_bin  = ltb(gen_strides(Shape),i),

  % Check size compatibility.
  [Shape_left, Stride_left] = lists:map(fun (<<I:32/native-integer, _/binary>>)-> I end, [Shape_bin, Stride_bin]),
  if not (bit_size(Content_bin)/64 == Stride_left*Shape_left) ->
    throw("Content/shape pair of invalid size");
  true->
    {array,Content_bin,Shape_bin,Stride_bin}
  end.

-spec array(double_seq()|array()|number()) -> array().
array(Content) ->
  case Content of
    N when is_number(N)       -> array(N,1);
    B when is_binary(B)       -> array(B, floor(bit_size(Content)/64));
    A when is_record(A,array) -> Content;
    C when is_list(C)         -> %Supposed to be a list.
      E = fun Extract(L, Shapes)  -> 
            case L of
              I when is_number(I) -> lists:reverse(Shapes);
              [H | _ ]            -> Extract(H, [length(L)|Shapes])
            end
      end,
      array(lists:flatten(Content), E(Content, []));

    _ -> throw("Expected either a number, binary, array, or list")
  end.


-type array_rep() :: c|erlang|list.
-spec cast_array(array() | double_seq(), array_rep()) -> array().
cast_array(A, R)->
  case A of
    {array, Content, Shape, Stride} ->
      case R of
        c      -> {array, ltb(Content,d), ltb(Shape,i), ltb(Stride,i)}; %c:      only binaries
        erlang -> {array, ltb(Content,d), btl(Shape,i), btl(Stride,i)}; %erlang: shape and stride are lists
        list   -> {array, btl(Content,d), btl(Shape,i), btl(Stride,i)}; %list:  all are lists.
        _      -> throw("Expected one of c, erlang, list")
      end;
    _ -> cast_array(array(A), R)
  end.


% Produce a binary contaning Num doubles, evenly spaced on the range [Start, Stop[.
linspace(Start,Stop,Num)->
  linspace_nif(float(Start),float(Stop),trunc(Num)).

linspace_nif(_,_,_)->
  nif_not_loaded.

% Perform operation Op.
op(Op, {array, Content, Shape, _})->
  {array, op_nif(Op, ltb(Content,d)), Shape, gen_strides(Shape)}.

op_nif(_,_)->
  throw("Nif not loaded.").

% merge right according to function Merge.
merger(Merge,Lin,Rin) when is_list(Lin), is_list(Rin)->
  [L,R] = lists:map(fun lists:reverse/1, [Lin, Rin]),
  Merger = fun It(Lm, Rm)->
    case {Lm,Rm} of
      {[],Ri} -> Ri;
      {Li,[]} -> Li;
      {[Lh|Lt], [Rh|Rt]} -> [ Merge([Lh, Rh]) | It(Lt, Rt)]
    end
  end,
  lists:reverse(Merger(L,R)).

% Perform binary operation op.
op(Op, Lhs, Rhs)->
  {#array{shape=Lse}, #array{shape=Rse}} = {cast_array(Lhs, erlang), cast_array(Rhs, erlang)},           % Cast Lhs and Rhs to erlang readable
  Res_shape  = merger(fun lists:max/1, Lse, Rse),
  Dest_c     = cast_array({array, <<>>, Res_shape, gen_strides(Res_shape)}, c),
  Dest_cont  = bin_op_nif(Op, Dest_c, cast_array(Lhs, c), cast_array(Rhs, c)),
  Dest_c#array{content=Dest_cont}.

bin_op_nif(_,_,_,_)->
  throw("Nif not loaded").