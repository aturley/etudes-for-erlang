-module(calculus).

-export([derivative/2]).

derivative(F, X) ->
    derivative(F, X, 1.0e-10).

derivative(F, X, Delta) ->
    (F(X + Delta) - F(X)) / Delta.
