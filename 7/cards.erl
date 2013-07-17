-module(cards).

-export([make_deck/0, show_deck/1]).

make_deck() ->
    [{R, S} || R <- ranks(), S <- suits()] ++ extras().

suits() ->
    ["Hearts", "Diamonds", "Spades", "Clubs"].

ranks() ->
    ["A", "2", "3", "4", "5", "6", "7", "8", "9", "Jack", "Queen", "King"].

extras() ->
    [{"Red", "Joker"}, {"Black", "Joker"}].

show_deck(Deck) ->
    lists:foreach(fun(Item) -> io:format("~p~n", [Item]) end, Deck).
