-module(dealer).
-export([deal_to/2]).

deal_to(Players, Deck) ->
    deal_to(Players, [], shuffle(Deck)).

deal_to(_, _, []) ->
    {ok};
deal_to([], Dealt, Cards) ->
    deal_to(Dealt, [], Cards);
deal_to([Player | Players], Dealt, [Card | Deck]) ->
    Player ! {deal, Card},
    deal_to(Players, [Player | Dealt], Deck).

shuffle(List) -> shuffle(List, []).
shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, [H | Acc]).


receive_cards(Players, Received) when length(Players) == length(Received) ->
    Received;
receive_cards(Players, Received) ->
    receive
        {play, Player, Card} ->
            receive_cards(Players, [{Player, Card} | Received]);
        {tie_break, Player, Cards} ->
            receive_cards(Players, [{Player, Cards} | Received])
    end.

play_card(Players) ->
    lists:foreach(fun (Player) Player ! {play, self()} end, Players),
    receive_cards(Players, []).

get_play_winner([{Player, Card} = Play | Plays]) ->
    get_play_winner(Plays, Play, [Card]).

get_play_winner([{Player, Card} = Play | Plays], {_, WinningCard} = WinningPlay, Pot) when WinningCard == Card ->
    

tie_break(Players, Pot) ->
    lists:foreach(fun (Player) Player ! {tie_break, self()} end, Players),
    {receive_cards(Players, []), Pot}.
