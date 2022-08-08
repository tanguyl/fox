-module(fox).
-on_load(init/0).
-export([array/1, apply_op/3, array/2, array_to_atom/1]).

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

%Input: list of ints/floats.
%Output: binary of contiguous doubles.
array(Content, Shape)->
    Array = [length(Content), length(Shape)] ++ Shape ++ Content,
    build_array(Array).

array(Content) 
  when is_list(Content) -> array(Content, [length(Content)]);
array(Content) when is_number(Content)->
  array([Content], [1]).

build_array(_)->
  erlang:nif_error("Nif library was not loaded.").

%Input: an array.
%Output: a str representation of the array.
array_to_atom(_)->
  erlang:nif_error("Nif library was not loaded.").



%Input: two ndarrays A,B
%Output: ndarray C = A + B
apply_op(Op, Lhs, Rhs) when not is_binary(Lhs) -> apply_op(Op, array(Lhs), Rhs);
apply_op(Op, Lhs, Rhs) when not is_binary(Rhs) -> apply_op(Op, Lhs, array(Rhs));
apply_op(Op, Lhs, Rhs) ->
   apply_nif(Op, Lhs, Rhs).

apply_nif(_,_,_)->
  erlang:nif_error("Nif library was not loaded.").


