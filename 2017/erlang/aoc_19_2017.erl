-module(aoc_19_2017).
-compile(export_all).

main() ->
    GridMap = input(),
    packet_route(GridMap).

packet_route(GridMap) ->
    FirstRow = [ Val || {{1,_}, _} = Val <- maps:to_list(GridMap)],
    [{Start, Value}] = lists:filter(fun({_, X}) -> X =/= ($ ) end, FirstRow),
    packet_route_main({Start, Value}, down, GridMap, 0, []).

packet_route_main({_, ($ )}, _, _, StepCount, Letters) -> {lists:reverse(Letters), StepCount};
packet_route_main({Index, Value}, Direction, GridMap, StepCount, Letters) ->
    NewLetters = case (Value >= $A andalso Value =< $Z) of
        true -> [Value|Letters];
        false -> Letters
    end,

    {NextIndex, NextVal, NewDir} = case get_next_steps(Value, Index, Direction) of
        [{Dir1, Index1}, {Dir2, Index2}] -> 
            case GridMap of
                #{Index1 := ($ ), Index2 := Val} -> {Index2, Val, Dir2};
                #{Index1 := Val, Index2 := ($ )} -> {Index1, Val, Dir1}
            end;
        NewIndex -> {NewIndex, maps:get(NewIndex, GridMap), Direction}
    end,
    packet_route_main({NextIndex, NextVal}, NewDir, GridMap, StepCount+1, NewLetters).

get_next_steps($+, Index, Direction) -> 
    case Direction of
        up -> [{left, get_next_step(Index, left)}, {right, get_next_step(Index, right)}];
        down -> [{left, get_next_step(Index, left)}, {right, get_next_step(Index, right)}];
        left -> [{up, get_next_step(Index, up)}, {down, get_next_step(Index, down)}];
        right -> [{up, get_next_step(Index, up)}, {down, get_next_step(Index, down)}]
    end;
get_next_steps(_, Index, Direction) -> get_next_step(Index, Direction).

get_next_step({X,Y}, down) -> {X+1,Y};
get_next_step({X,Y}, up) -> {X-1,Y};
get_next_step({X,Y}, left) -> {X,Y-1};
get_next_step({X,Y}, right) -> {X,Y+1}.

create_grid(Input) ->
    create_grid(Input, #{}, 1).

create_grid([], Map, _) ->
    Map;
create_grid([H|T], Map, Row) ->
    RowVals = binary_to_list(H),
    Indices = [{Row, Y} || Y <- lists:seq(1, length(RowVals))],
    NewMap = maps:merge(maps:from_list(lists:zip(Indices, RowVals)), Map),
    create_grid(T, NewMap, Row+1).

input() ->
    {ok, Data} = file:read_file("../inputs/day19_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    create_grid(Lines).