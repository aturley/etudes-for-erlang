-module(geom).

-export([area/3]).

area(Type, X, Y) when X > 0, Y > 0 ->
    case Type of
        triangle ->
            X * Y / 2;
        rectangle ->
            X * Y;
        ellipse ->
            X * Y * math:pi()
    end.
