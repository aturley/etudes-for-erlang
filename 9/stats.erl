-module(stats).

-export([minimum/1, maximum/1, range/1, mean/1, stdv/1]).

minimum(NumberList) ->
    try 
        minimum(tl(NumberList), hd(NumberList))
    of
        Result -> Result
    catch
        error:Error ->
            {error, Error}
    end.

minimum([First|Numbers], Min) when Min > First ->
    minimum(Numbers, First);
minimum([_|Numbers], Min) ->
    minimum(Numbers, Min);
minimum([], Min) ->
    Min.

maximum(NumberList) ->
    try
        maximum(tl(NumberList), hd(NumberList))
    of
        Result -> Result
    catch
        error:Error ->
            {error, Error}
    end.

maximum([First|Numbers], Max) when First > Max ->
    maximum(Numbers, First);
maximum([_|Numbers], Max) ->
    maximum(Numbers, Max);
maximum([], Max) ->
    Max.

range(Numbers) when is_list(Numbers) ->
    {minimum(Numbers), maximum(Numbers)}.

mean(Numbers) ->
    try
        lists:foldl(fun(X, Y) -> X + Y end, 0, Numbers) / length(Numbers)
    of
        Result -> Result
    catch
        error:Error ->
            {error, Error}
    end.

stdv(Numbers) ->
    try
        N = length(Numbers),
        {Sum, SumSquares} = lists:foldl(fun(X, {Sum, SumSquares}) -> {X + Sum, X * X + SumSquares} end,
                                        {0, 0}, 
                                        Numbers),
        math:sqrt((N * SumSquares - Sum * Sum) / (N * (N - 1)))
    of
        Result -> Result
    catch
        error:Error ->
            {error, Error}
    end.

