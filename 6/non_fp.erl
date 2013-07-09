-module(non_fp).

-export([generate_teeth/2]).

generate_teeth(TeethExist, GoodToothProbability) ->
    generate_teeth(TeethExist, GoodToothProbability, []).

generate_teeth([ToothExists|TeethExist], GoodToothProbability, Accumulator) ->
    case ToothExists of
        $T -> generate_teeth(TeethExist, GoodToothProbability, [generate_tooth(GoodToothProbability)|Accumulator]);
        $F -> generate_teeth(TeethExist, GoodToothProbability, [[0]|Accumulator])
    end;
generate_teeth([], _, Accumulator) ->
    lists:reverse(Accumulator).

generate_tooth(GoodToothProbablity) ->
    random:seed(now()),
    Toothiness = (random:uniform() < GoodToothProbablity),
    case Toothiness of
        true ->
            generate_tooth(2, 6, []);
        false ->
            generate_tooth(3, 6, [])
    end.

generate_tooth(_, Remaining, Accumulator) when Remaining == 0 ->
    lists:reverse(Accumulator);
generate_tooth(BaseDepth, Remaining, Accumulator) ->
    generate_tooth(BaseDepth, Remaining - 1, [random_depth(BaseDepth)|Accumulator]).

random_depth(BaseDepth) ->
    BaseDepth + random:uniform(3) - 2.
