-module(dates).

-export([date_parts/1, julian/1]).

date_parts(Date) ->
    SplitDate = re:split(Date, "-", [{return, list}]),
    [get_year(SplitDate), get_month(SplitDate), get_day(SplitDate)].
    
get_year([Year, _, _]) ->
    {YearInt, _} = string:to_integer(Year),
    YearInt.

get_month([_, Month, _]) ->
    {MonthInt, _} = string:to_integer(Month),
    MonthInt.

get_day([_, _, Day]) ->
    {DayInt, _} = string:to_integer(Day),
    DayInt.

julian(DateString) ->
    [Year, Month, Day] = date_parts(DateString),
    julian(Year, Month, Day, days_per_month(Year), 0).

days_per_month(Year) ->
    [31, days_per_february(Year), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31].

days_per_february(Year) ->
    28 + case is_leap_year(Year) of
        true -> 1;
        false -> 0
    end.

is_leap_year(Year) ->
    (Year rem 4 == 0 andalso Year rem 100 /= 0) orelse (Year rem 400 == 0).

julian(_, Month, Day, DaysPerMonth, Accumulator) when 13 - length(DaysPerMonth) == Month ->
    Day + Accumulator;
julian(Year, Month, Day, [DaysThisMonth|DaysPerMonth], Accumulator) ->
    julian(Year, Month, Day, DaysPerMonth, Accumulator + DaysThisMonth).
