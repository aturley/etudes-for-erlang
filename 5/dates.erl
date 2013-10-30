-module(dates).

-export([date_parts/1]).

date_parts(DateStr) ->
    [Int || {Int, _} <- [string:to_integer(Date) || Date <- re:split(DateStr, "-", [{return, list}])]].

