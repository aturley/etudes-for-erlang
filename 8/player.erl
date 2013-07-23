-module(player).
-export([init/0, play/1]).

init() ->
    spawn(?MODULE, play, [[]]).

play(Cards) ->
    receive
        {deal, Card} ->
            play([Card | Cards]);
        {play, DealerPid} ->
            [TopCard | RestCards] = Cards,
            DealerPid ! {play, self(), TopCard},
            play(RestCards);
        {tie_break, DealerPid} ->
            {TieBreakers, RestCards} = get_tie_breakers(Cards),
            DealerPid ! {tie_break, self(), TieBreakers},
            play(RestCards);
        {outcome, ReceivedCards} ->
            play(Cards ++ ReceivedCards);
        {card_count, DealerPid} ->
            DealerPid ! {card_count, self(), length(Cards)},
            play(Cards)
    end.

get_tie_breakers(Cards) ->
    get_tie_breakers(Cards, []).

get_tie_breakers(Cards, TieBreakers) when length(TieBreakers) == 3 orelse length(Cards) == 0 ->
    {TieBreakers, Cards};
get_tie_breakers([TopCard | RestCards], TieBreakers) ->
    get_tie_breakers(RestCards, [TopCard | TieBreakers]).
