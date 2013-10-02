%% @author Andrew Turley
%% @doc Functions for calculating areas of geometric shapes.
%% @copyright 2013 Andrew Turley
%% @version 0.1

-module(geom).

-export([area/2]).

%% @doc Find the area of a rectangle given the height and width.

-spec(area(number(),number()) -> number()).

area(Height, Width) ->
    Height * Width.









