-module(people).

-export([men_over_40/1, men_or_over_40/1]).

men_over_40(People) ->
    [P || {_, G, A} = P <- People, G == $M, A > 40].

men_or_over_40(People) ->
    [P || {_, G, A} = P <- People, (G == $M) orelse (A > 40)].
