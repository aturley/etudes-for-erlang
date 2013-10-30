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

number_read_result_to_float({error, Value}) ->
    {error, Value};
number_read_result_to_float({_, [Value]}) ->
    string:to_float(Value ++ ".0").

get_dimensions(error) ->
    error;
get_dimensions(Shape) ->
    {PromptA, PromptB} = get_prompts(Shape),
    check_dimensions({number_read_result_to_float(io:fread(PromptA, "~s")), number_read_result_to_float(io:fread(PromptB, "~s"))}).

check_dimensions({{error, _}, {_, _}}) ->
    io:fwrite("Error in first dimension."),
    error;
check_dimensions({{_, _}, {error, _}}) ->
    io:fwrite("Error in second dimension."),
    error;
check_dimensions({{A, _}, {B, _}}) when A =< 0; B =< 0 ->
    io:fwrite("Both dimensions must be positive."),
    error;
check_dimensions({{A, _}, {B, _}}) when A > 0, B > 0 ->
    {A, B}.

area(rectangle, {Height, Width}) ->
    Height * Width;
area(triangle, {Base, Height}) ->
    Base * Height / 2;
area(ellipse, {MajorAxis, MinorAxis}) ->
    math:pi() * MajorAxis * MinorAxis;
area(_, _) ->
    ok.
