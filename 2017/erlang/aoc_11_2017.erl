-module(aoc_11_2017).
-compile(export_all).

main() ->
    Input = input(),
    EmptyStepMap = #{
        <<"n">> => 0,
        <<"s">> => 0,
        <<"ne">> => 0,
        <<"nw">> => 0,
        <<"sw">> => 0,
        <<"se">> => 0
    },
    StepMap = hex_steps(Input, EmptyStepMap),
    Part1 = get_steps_away(StepMap),
    Part2 = hex_steps(Input, EmptyStepMap, 0),
    {Part1, Part2}.

%% Returns the least number of steps required to reach the path in StepMap
get_steps_away(StepMap) ->
    OptimumStepMap = check_for_shorter_path(StepMap),
    lists:sum(maps:values(OptimumStepMap)).

%% Function reduces the number of steps to form an optimum path 
check_for_shorter_path(StepMap) ->
    StepMap2 = check_for_shorter_path({<<"n">>, <<"sw">>, <<"nw">>}, StepMap),
    StepMap3 = check_for_shorter_path({<<"n">>, <<"se">>, <<"ne">>}, StepMap2),
    StepMap4 = check_for_shorter_path({<<"s">>, <<"nw">>, <<"sw">>}, StepMap3),
    StepMap5 = check_for_shorter_path({<<"s">>, <<"ne">>, <<"se">>}, StepMap4),
    StepMap6 = check_for_shorter_path({<<"nw">>, <<"ne">>, <<"n">>}, StepMap5),
    case check_for_shorter_path({<<"sw">>, <<"se">>, <<"s">>}, StepMap6) of
        StepMap -> StepMap;
        NewStepMap -> check_for_shorter_path(NewStepMap)
    end.

check_for_shorter_path({K1, K2, K3}, StepMap) ->
    #{K1 := V1, K2 := V2, K3 := V3} = StepMap,
    case {V1, V2} of
        {0, _} -> StepMap;
        {_, 0} -> StepMap;
        _ ->
            Steps = min(V1, V2),
            StepMap#{K1 := V1-Steps, K2 := V2-Steps, K3 := V3+Steps}
    end.

hex_steps([], StepMap) -> StepMap;
hex_steps([Step|Rest], StepMap) ->
    #{Step := Count1} = StepMap,
    hex_steps(Rest, StepMap#{Step := Count1+1}).

hex_steps([], _, Max) -> Max;
hex_steps([Step|Rest], StepMap, Max) ->
    #{Step := Count1} = StepMap,
    NewStepMap = StepMap#{Step := Count1+1},
    NewMax = case get_steps_away(NewStepMap) of
        N when N > Max -> N;
        _ -> Max
    end,
    hex_steps(Rest, NewStepMap, NewMax).

input() ->
    {ok, Data} = file:read_file("../inputs/day11_data.txt"),
    binary:split(Data, [<<",">>, <<"\n">>], [global, trim_all]).