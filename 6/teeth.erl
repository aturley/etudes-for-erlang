-module(teeth).

-export([alert/1]).

alert(PocketDepths) ->
    alert(PocketDepths, [], 1).

alert([], Alerts, _) ->
    lists:reverse(Alerts);
alert([[0]|PocketDepths], Alerts, Tooth) ->
    alert(PocketDepths, Alerts, Tooth + 1);
alert([PocketDepth|PocketDepths], Alerts, Tooth) ->
    case should_alert(PocketDepth) of
        true ->
            alert(PocketDepths, [Tooth | Alerts], Tooth + 1);
        false ->
            alert(PocketDepths, Alerts, Tooth + 1)
    end.
    
should_alert([Depth|_]) when Depth >= 4 ->
    true;
should_alert([_|Depths]) ->
    should_alert(Depths);
should_alert([]) ->
    false.
