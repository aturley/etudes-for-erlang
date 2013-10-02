-module(player).
-export([init/0, play/1]).

-include_lib("eunit/include/eunit.hrl").

init() ->
    spawn(?MODULE, play, [[]]).

play(Cards) ->
    receive
        {deal, Card} ->
            play([Card | Cards]);
        {play, DealerPid} ->
            {TopCard, RestCards} = get_play_card(Cards),
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

get_play_card([]) ->
    {none, []};
get_play_card([PlayCard | RestCards]) ->
    {PlayCard, RestCards}.

get_tie_breakers([]) ->
    {none, []};
get_tie_breakers(Cards) when length(Cards) < 3 ->
    {lists:reverse(Cards), []};
get_tie_breakers(Cards) ->
    {TieBreakers, RestCards} = lists:split(3, Cards),
    {lists:reverse(TieBreakers), RestCards}.

%%
%% TESTS
%%

play_card_with_no_cards_test() ->
    {none, []} = get_play_card([]).

play_card_with_1_card_test() ->
    {1, []} = get_play_card([1]).

play_card_with_2_cards_test() ->
    {1, [2]} = get_play_card([1, 2]).

tie_breaker_with_no_cards_test() ->
    {none, []} = get_tie_breakers([]).

tie_breaker_with_1_card_test() ->
    {[1], []} = get_tie_breakers([1]).

tie_breaker_with_2_cards_test() ->
    {[2, 1], []} = get_tie_breakers([1, 2]).

tie_breaker_with_3_cards_test() ->
    {[3, 2, 1], []} = get_tie_breakers([1, 2, 3]).

tie_breaker_with_4_cards_test() ->
    {[3, 2, 1], [4]} = get_tie_breakers([1, 2, 3, 4]).

