-module(eval_test).
-include_lib("eunit/include/eunit.hrl").


basic_test()->
    [A,B,C] = [fox:array(0), fox:array([-1, -3, -5]), fox:array(3/2)],
    A = fox:eval(['+', 2, '-',1, 3]),
    B = fox:eval(['-', 1, '*', [1,2,3], [2]]),
    C = fox:eval(['+', 1, '*', 2, '/', 1, 4]).

nested_test()->
    R = fox:array([11, 14, 17]),
    R = fox:eval(['+', ['*', 2, 4], ['*', [1,2,3], 3]]). % 2*4 + [1,2,3]*3