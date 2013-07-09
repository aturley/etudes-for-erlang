-module(geom).

-export([area/0]).

area() ->
    Shape = get_shape(),
    Dimensions = get_dimensions(Shape),
    area(Shape, Dimensions).

get_shape() ->
    letter_to_shape(io:fread("R)ectangle, T)riangle, or E)llipse > ", "~s")).

letter_to_shape({ok, [Letter|_]}) ->
    case string:to_lower(string:strip(Letter)) of
        "t" ->
            triangle;
        "r" ->
            rectangle;
        "e" ->
            ellipse;
        Shape ->
            io:fwrite("Unknown shape '~s' ('~s').", [Letter, Shape]),
            error
    end.

get_prompts(rectangle) ->
    {"Enter height: ", "Enter width: "};
get_prompts(triangle) ->
    {"Enter base: ", "Enter height: "};
get_prompts(ellipse) ->
    {"Enter major axis: ", "Enter minor axis: "}.

get_dimensions(error) ->
    error;
get_dimensions(Shape) ->
    {PromptA, PromptB} = get_prompts(Shape),
    check_dimensions({io:fread(PromptA, "~d"), io:fread(PromptB, "~d")}).

check_dimensions({{error, _}, {_, _}}) ->
    io:fwrite("Error in first dimension."),
    error;
check_dimensions({{_, _}, {error, _}}) ->
    io:fwrite("Error in second dimension."),
    error;
check_dimensions({{ok, [A]}, {ok, [B]}}) when A =< 0; B =< 0 ->
    io:fwrite("Both dimensions must be positive."),
    error;
check_dimensions({{ok, [A]}, {ok, [B]}}) when A > 0, B > 0 ->
    {A, B}.

area(rectangle, {Height, Width}) ->
    Height * Width;
area(triangle, {Base, Height}) ->
    Base * Height / 2;
area(ellipse, {MajorAxis, MinorAxis}) ->
    math:pi() * MajorAxis * MinorAxis;
area(_, _) ->
    ok.
