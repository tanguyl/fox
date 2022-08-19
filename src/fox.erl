-module(fox).
-on_load(init/0).
-export([array/1, array/2, to_lists/1, op/2, op/3, op_nif/4]).

-record(array,{content,shape,dim}).

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



% Transfer lists from/to binaries/lists
%---------------------------------------------------------------------------------------------------
% Binary to list of double/ints
btl_d(B)->btl_d(B,[]).
btl_d(<<H:64/native-float, T/binary>>,   Acc) -> btl_d(T, [H|Acc]);btl_d(_, Acc) -> lists:reverse(Acc).
btl_i(B)->btl_i(B,[]).
btl_i(<<H:32/native-integer, T/binary>>, Acc) -> btl_i(T, [H|Acc]);btl_i(_, Acc) -> lists:reverse(Acc).

% Lists of numbers to binaries of doubles/ints.
ltb_d(L)->lists:foldl(fun(H,Acc)-> <<Acc/binary, H:64/native-float>>   end, <<>>, L).
ltb_i(L)->lists:foldl(fun(H,Acc)-> <<Acc/binary, H:32/native-integer>> end, <<>>, L).

% Array creation utilities
strides_of(Shape)->
  {Strides,_} = lists:mapfoldr(fun(H,Acc)-> {Acc, H*Acc} end, 1, Shape),
  Strides.

%Create an array.
array(Content, Shape)->
  ExpectedSize = lists:foldr(fun (X, Prod)->X*Prod end, 1, Shape),
  ActualSize   = lists:flatlength(Content),
  if ActualSize =/= ExpectedSize ->
    erlang:error("Shape mismatch content.");
  true ->
    {ltb_d(Content), ltb_i(Shape), ltb_i(strides_of(Shape))}
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


to_lists({Content, Shape, Strides})->
  {btl_d(Content), btl_i(Shape), btl_i(Strides)}.

op(Op, {Rhs,Shape,Stride})-> {op_nif(Op, Rhs), Shape, Stride};
op(Op, Rhs) -> 
  {Content,Shape,Stride} = array(Rhs),
  {op_nif(Op, Content), Shape, Stride}.

op_nif(_,_)->
  nif_not_loaded.


%Throw an error if input lists cannot be broadcasted together.
can_broadcast(_,[])-> true;
can_broadcast([],_)->true;
can_broadcast([L|Lt], [R|Rt]) when L==R; L==1; R==1-> can_broadcast(Lt,Rt).

% Merge multiple arrays into a single one.
% Input lists are padded left with Padding.
% Fct takes as input the list of current heads, return the wished value.
% Returns a list: [MergedList, PaddedList1, ...]
map_n(Fct, Padding, Lists)->
  Max_size     = lists:max(lists:map(fun length/1, Lists)),
  Lists_padded = lists:map(
                            fun Pad(I)-> if length(I) < Max_size -> Pad([Padding|I]); true->I end end,
                            Lists
                          ),

  Work         =  fun F( [[]|_] ) -> [];
                      F(It_lists) -> [Fct(lists:map(fun hd/1, It_lists)) | F(lists:map(fun tl/1, It_lists))]
                  end,

  [Work(Lists_padded) | Lists_padded].

% A bit more to study here!
concat_right(Lhs, Rhs)->
  [Lhs_r, Rhs_r] = lists:map(fun lists:reverse/1, [Lhs, Rhs]),
  Concat         =  
                    fun F([], [], Lacc, Racc, _) -> [[Lacc], [Racc]];
                        F([H|Lt], [H|Rt], Acc, Acc, Dir)->
                          F(Lt, Rt, H*Acc, H*Acc, Dir);
                        F([1|Lt], [Rh|Rt], Lacc, Racc, Dir) when Dir==left;Dir==center->
                          F(Lt, Rt, Lacc, Rh*Racc, left);
                        F([Lh|Lt], [1|Rt], Lacc, Racc, Dir) when Dir==right;Dir==center->
                          F(Lt, Rt, Lh*Lacc, Racc, right);
                        F(L, R, Lacc, Racc, center)->
                            [lists:reverse([hd(L)*Lacc |tl(L)]), lists:reverse( [hd(R)*Racc |tl(R)])];
                        F(L, R, Lacc, Racc, _)->
                            [lists:reverse([Lacc|L]), lists:reverse([Racc|R])]
                    end,
  Concat(Lhs_r, Rhs_r, 1, 1, center).

op(Op, Lhs, Rhs)->
  %TODO: check input compatibility
  [Lhs_a, Rhs_a]                        = lists:map(fun(I)-> if is_tuple(I)-> I; true-> array(I) end end, [Lhs, Rhs]),  % Make sure inputs are arrays
  [Lhs_shape, Rhs_shape]                = lists:map(fun(I)->btl_i(element(2,I))end, [Lhs_a, Rhs_a]),                    % Extract shape into a "easy to read" format
  can_broadcast(lists:reverse(Lhs_shape), lists:reverse(Rhs_shape)),                                                    % Throws an error if incompatible shapes
  [Res_shape, Lhs_padded, Rhs_padded]   = map_n(fun lists:max/1, 1, [Lhs_shape, Rhs_shape]),                              % Calculate output shape, pad inputs shapes
  [Lhs_shape_c, Rhs_shape_c]            = concat_right(Lhs_padded, Rhs_padded),                                           % Concatenate input shapes
  [Res_shape_c,_,_]                     = map_n(fun lists:max/1, 1, [Lhs_shape_c, Rhs_shape_c]),                          % Calculate output corresponding shape

  [Dest_f, Lhs_f, Rhs_f] = [                                                                                            %Make modified array "nif ready"     
                        {<<>>,             ltb_i(Res_shape_c), ltb_i(strides_of(Res_shape_c))},
                        {element(1,Lhs_a), ltb_i(Lhs_shape_c), ltb_i(strides_of(Lhs_shape_c))},
                        {element(1,Rhs_a), ltb_i(Rhs_shape_c), ltb_i(strides_of(Rhs_shape_c))}
                    ],

  {op_nif(Op, Dest_f, Lhs_f, Rhs_f), ltb_i(Res_shape), ltb_i(strides_of(Res_shape))}.


op_nif(_,_,_,_)->
  nif_not_loaded.
