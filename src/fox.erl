-module(fox).
-on_load(init/0).
-export([array/1, array/2, to_lists/1, op/2, op/3]).

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


array(Content, Shape)->
  ExpectedSize = lists:foldr(fun (X, Prod)->X*Prod end, 1, Shape),
  ActualSize   = lists:flatlength(Content),
  if ActualSize =/= ExpectedSize ->
    erlang:error("Shape mismatch content.");
  true ->
    {Strides,_} = lists:mapfoldr(fun(H,Acc)-> {Acc, H*Acc} end, 1, Shape), 
    {
      lists:foldl(fun(H,Acc)-> <<Acc/binary, H:64/native-float>>   end, <<>>, Content),   % Binary of content
      lists:foldl(fun(H,Acc)-> <<Acc/binary, H:32/native-integer>> end, <<>>,   Shape),   % Binary of Shape
      lists:foldl(fun(H,Acc)-> <<Acc/binary, H:32/native-integer>> end, <<>>, Strides)    % Binary of Strides
    }
  end.

array(Content)   when is_number(Content) -> array([Content], [1]);
array(Content)   when is_list(Content)   ->
  ExtractDim = fun Extract(L, Shapes)  -> 
                   case L of
                     I when is_number(I) -> lists:reverse(Shapes);
                     [H | _ ]            -> Extract(H, [length(L)|Shapes])
                   end
               end,
  Shapes = ExtractDim(Content, []),
  ExpectedSize = lists:foldr(fun (X, Prod)->X*Prod end, 1, Shapes),
  ActualSize = lists:flatlength(Content),
  if 
    ExpectedSize =/= ActualSize->
      erlang:error('Input list is of incorrect dimensions');
    true ->
      array(lists:flatten(Content), Shapes)
  end.



btd(<<H:64/native-float, T/binary>>,   Acc) -> btd(T, [H|Acc]);btd(_, Acc) -> lists:reverse(Acc).
bti(<<H:32/native-integer, T/binary>>, Acc) -> bti(T, [H|Acc]);bti(_, Acc) -> lists:reverse(Acc).

to_lists({Content, Shape, Strides})->
  {btd(Content, []), bti(Shape, []), bti(Strides, [])}.

op(Op, Rhs) when is_tuple(Rhs)-> op_nif(Op, Rhs);
op(Op, Rhs) -> op_nif(Op, array(Rhs)).

op_nif(_,_)->
  nif_not_loaded.

op(Op, Lhs, Rhs)->
  [Lhs_f, Rhs_f] = lists:map(fun(I)-> if is_tuple(I)-> I; true-> array(I) end end, [Lhs, Rhs]),
  op_nif(Op, Lhs_f, Rhs_f).

op_nif(_,_,_)->
  nif_not_loaded.
