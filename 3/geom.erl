-module(geom).

-export([area/1, area_helper/3]).

-spec(area({atom(),number(),number()}) ->number()).
area({ShapeType, X, Y}) when X >= 0 andalso Y >= 0 ->
    area_helper(ShapeType, X, Y).


-spec(area_helper(atom(),number(),number()) ->number()).
area_helper(rectangle, X, Y) ->
    X * Y;
area_helper(triangle, X, Y) ->
    X * Y / 2;
area_helper(ellipse, X, Y) ->
    math:pi() * X * Y;
area_helper(Z, X, Y) ->
    math:pi() * X * Y * Z.




