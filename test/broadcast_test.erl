-module(broadcast_test).
-include_lib("eunit/include/eunit.hrl").

str_to_list(S)->
    NumsStr = string:lexemes(S, " "),
    lists:map(fun(Str) -> list_to_integer(Str) end, NumsStr).

get_3_arrays(File)->
    Loop = 
        fun
        IterateFile(0, R) -> lists:reverse(R);
        IterateFile(N, R) ->
            case io:get_line(File, "") of
                eof  -> eof;
                Line ->
                    if length(Line) < 4->
                        IterateFile(N, R);
                    true ->
                        [Shape, Content] =string:lexemes(string:trim(Line, trailing, "\n"), "content"),
                        Array = fox:array(str_to_list(Content), str_to_list(Shape)),
                        IterateFile(N-1, [Array|R])
                    end
            end
        end,
    Loop(3, []).

small_vectors_test()->
    C = fox:array(lists:seq(1,3), [3, 1,1]),
    D = fox:array(lists:seq(1,3), [3, 1]),
    E = fox:array([2,3,4,3,4,5,4,5,6], [3,3,1]),
    E = fox:apply_op('+',C,D),

    A = fox:array([1,2]),
    B = fox:array([2,4]),
    B = fox:apply_op('+',A,A).

small_op_test()->
    A = fox:array([1]),
    B = fox:array([2]),

    io:format("\nTesting +\n"), B = fox:apply_op('+', A, A),
    io:format("Testing *\n"), B = fox:apply_op('*', A, B),
    io:format("Testing /\n"), A = fox:apply_op('/', B, B),
    io:format("Testing -\n"), A = fox:apply_op('-', B, A),
    io:format("Should thrown an exception.\n"),
    ?assertError('Failed to read operation.', fox:apply_op('i',A,B)).


huge_vectors_sigssev_test()->
    N = 100000,
    A = fox:array(lists:seq(1,N), [N, 1, 1]),
    B = fox:array(lists:seq(1,N), [N,1]),
    ?assertError('Overflow: output array cannot be represented.', fox:apply_op('+',A,B)).


broadcast_cmp_numpy_test() ->
    PrivDir = case code:priv_dir(my_application) of
                 {error, bad_name} -> "priv";
                 P -> P end,
    {ok, File} = file:open(filename:join(PrivDir, "test_cases.txt"), [read]),

    Loop = 
        fun L(I)->
            case get_3_arrays(File) of
                eof -> 
                    ok;
                [Lhs, Rhs, Result] ->
                    io:format("Testing line : ~b~n", [I]),
                    Result = fox:apply_op('+',Lhs, Rhs),
                    L(I+4);
                _ -> 
                    L(I+1)
            end
        end,            
    Loop(1),
    file:close(File).

