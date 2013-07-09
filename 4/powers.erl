-module(powers).

-export([raise/2, raise_accumulator/2, nth_root/2]).

-define(LIMIT, 1.0e-8).

raise(X, N) ->
    if N == 0 ->
            1;
       N == 1 ->
            X;
       N > 0 ->
            X * raise(X, N - 1);
       true ->
            1.0 / raise(X, -N)
    end.

raise(X, N, Acc) ->
    if N == 0 ->
            Acc;
       true ->
            raise(X, N - 1, X * Acc)
    end.

raise_accumulator(X, N) ->
    if N > 0 ->
            raise(X, N, 1);
       N == 0 ->
            1;
       true ->
            1.0 / raise_accumulator(X, -N)
    end.

nth_root(X, N) ->
    nth_root(X, N, X / 2.0).

nth_root(X, N, A) ->
    F = raise(A, N) - X,
    Fprime = N * raise(A, N - 1),
    Next = A - (F / Fprime),
    Change = if Next > A -> 
                     Next - A;
                true ->
                     A - Next
             end,
    io:format("A=~f, F=~f, Fprime=~f, Change=~f, Limit=~f~n", [Next, F, Fprime, Change, ?LIMIT]),
    if Change < ?LIMIT ->
            Next;
       true ->
            nth_root(X, N, Next)
    end.
