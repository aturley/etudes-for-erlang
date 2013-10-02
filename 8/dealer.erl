-module(dealer).
-export([next_activity/1, start_state/2]).

-include_lib("eunit/include/eunit.hrl").

start_state(Players, Deck) ->
    {init, Players, Deck, []}.

next_activity({GameState, _, _, _} = State) ->
    case GameState of
        init ->
            handle_init(State);
        play ->
            handle_play(State);
        tie ->
            handle_tie(State);
        {winner, _} ->
            announce_winner(State)
    end.

deal_to(Players, Deck) ->
    deal_to(Players, [], Deck).

deal_to(_, _, []) ->
    {ok};
deal_to([], Dealt, Cards) ->
    deal_to(Dealt, [], Cards);
deal_to([Player | Players], Dealt, [Card | Deck]) ->
    Player ! {deal, Card},
    deal_to(Players, [Player | Dealt], Deck).

handle_init({_, Players, Deck, _}) ->
    deal_to(Players, Deck),
    {play, Players, Deck, []}.

handle_play({_, Players, Deck, _}) ->
    Plays = play_card(Players),
    case {get_play_winner(Plays), plays_to_pot(Plays)} of
        {tie, Pot} ->
            {tie, Players, Deck, Pot};
        {WinningPlayer, Pot} ->
            WinningPlayer ! {outcome, Pot},
            {play_or_won(Players, Deck), Players, Deck, []}
    end.

handle_tie({_, Players, Deck, Pot}) ->
    Plays = tie_break(Players),
    case {get_tie_winner(Plays), tie_plays_to_pot(Plays)} of
        {tie, TiePot} ->
            {tie, Pot ++ TiePot};
        {WinningPlayer, WinningPot} ->
            WinningPlayer ! {outcome, Pot ++ WinningPot},
            {play_or_won(Players, Deck), Players, Deck, []}
    end.

announce_winner({{winner, WinningPlayer}, _, _, _} = State) ->
    io:format("~p has won!~n", [WinningPlayer]),
    State.

play_or_won(Players, Deck) ->
    lists:foreach(fun (Player) -> Player ! {card_count, self()} end, Players),
    case receive_card_count(Players, Deck, 0, 0) of
        none ->
            play;
        Winner ->
            {winner, Winner}
    end.

receive_card_count(Players, _, ReceivedCounts, Winner) when ReceivedCounts == length(Players) ->
    Winner;
receive_card_count(Players, Deck, ReceivedCounts, Winner) ->
    receive
        {card_count, Player, Count} when Count == length(Deck) ->
            receive_card_count(Players, Deck, ReceivedCounts + 1, Player);
        {card_count, _, _} ->
            receive_card_count(Players, Deck, ReceivedCounts + 1, Winner)
    end.
    
receive_cards(Players) ->
    receive_cards(Players, [], 0).

receive_cards(Players, Received, ReceiveCount) when length(Players) == ReceiveCount ->
    Received;
receive_cards(Players, Received, ReceiveCount) ->
    receive
        {play, _, none} ->
            receive_cards(Players, Received, ReceiveCount + 1);
        {play, Player, Card} ->
            receive_cards(Players, [{Player, Card} | Received], ReceiveCount + 1);
        {tie_break, _, none} ->
            receive_cards(Players, Received, ReceiveCount + 1);
        {tie_break, Player, Cards} ->
            receive_cards(Players, [{Player, Cards} | Received], ReceiveCount + 1)
    end.

play_card(Players) ->
    lists:foreach(fun (Player) -> Player ! {play, self()} end, Players),
    receive_cards(Players).

get_play_winner([Play | RestPlays]) ->
    get_play_winner(RestPlays, Play).

get_play_winner([], {WinningPlayer, _}) ->
    WinningPlayer;
get_play_winner(_, tie) ->
    tie;
get_play_winner([Play | RestPlay], WinningPlay) ->
    get_play_winner(RestPlay, get_highest_play(WinningPlay, Play)).

get_highest_play({_, C1}, {_, C2}) when C1 == C2 ->
    tie;
get_highest_play({P1, C1}, {_, C2}) when C1 > C2 ->
    {P1, C1};
get_highest_play({_, C1}, {P2, C2}) when C1 < C2 ->
    {P2, C2}.

plays_to_pot(Plays) ->
    [play_to_card(Play) || Play <- Plays].

play_to_card({_, Card}) ->
    Card.

tie_break(Players) ->
    lists:foreach(fun (Player) -> Player ! {tie_break, self()} end, Players),
    receive_cards(Players).

get_tie_winner(TiePlays) ->
    Plays = tie_plays_to_plays(TiePlays),
    get_play_winner(Plays).

tie_plays_to_plays(TiePlays) ->
    [tie_play_to_play(TiePlay) || TiePlay <- TiePlays].

tie_play_to_play({Player, [TiePlayCard | _]}) ->
    {Player, TiePlayCard}.

tie_plays_to_pot(Plays) ->
    lists:foldl(fun tie_play_into_pot/2, [], Plays).

tie_play_into_pot({_, Cards}, Pot) ->
    Pot ++ Cards.

%%
%% TESTS
%%

plays_to_pot_test() ->
    [1, 2] = plays_to_pot([{p1, 1}, {p2, 2}]).

get_play_winner_p1_test() ->
    p1 = get_play_winner([{p1, 2}, {p2, 1}]).

get_play_winner_p2_test() ->
    p2 = get_play_winner([{p1, 1}, {p2, 2}]).

get_play_winner_tie_test() ->
    tie = get_play_winner([{p1, 2}, {p2, 2}]).

tie_plays_to_pot_test() ->
    [1, 2, 3, 4, 5, 6] = tie_plays_to_pot([{p1, [1, 2, 3]}, {p2, [4, 5, 6]}]).

tie_plays_to_plays_test() ->
    [{p1, 1}, {p2, 4}] = tie_plays_to_plays([{p1, [1, 2, 3]}, {p2, [4, 5, 6]}]).

get_tie_winner_p1_test() ->
    p1 = get_tie_winner([{p1, [7, 8, 9]}, {p2, [4, 5, 6]}]).

get_tie_winner_p2_test() ->
    p2 = get_tie_winner([{p1, [1, 2, 3]}, {p2, [4, 5, 6]}]).


get_tie_winner_tie_test() ->
    tie = get_tie_winner([{p1, [1, 2, 3]}, {p2, [1, 5, 6]}]).

