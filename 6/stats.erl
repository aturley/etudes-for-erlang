-module(stats).

-export([minimum/1, maximum/1, range/1]).

minimum([First|Numbers]) ->
    minimum(Numbers, First).

minimum([First|Numbers], Min) when Min > First ->
    minimum(Numbers, First);
minimum([_|Numbers], Min) ->
    minimum(Numbers, Min);
minimum([], Min) ->
    Min.

maximum([First|Numbers]) ->
    maximum(Numbers, First).

maximum([First|Numbers], Max) when First > Max ->
    maximum(Numbers, First);
maximum([_|Numbers], Max) ->
    maximum(Numbers, Max);
maximum([], Max) ->
    Max.

range(Numbers) when is_list(Numbers) ->
    {minimum(Numbers), maximum(Numbers)}.
