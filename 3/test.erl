-module(test).

-export([my_fun3/2]).

my_fun1(X, Y) when hd(X) > 1 ; Y == 0 ->
    X + Y.

my_fun2(X, Y) when hd(X) > 1 orelse Y == 0 ->
    X + Y.

my_fun3(X, _) ->
    X.
