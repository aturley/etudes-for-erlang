-module(dijkstra).

-export([gcd/2]).

gcd(M, N) ->
    %% http://erlang.org/doc/reference_manual/expressions.html#id77026
    if M == N -> M;
       M > N -> gcd(M - N, N);
       M < N -> gcd(M, N - M)
    end.
