-module(aoc_3_2019).
-export([start/0]).

start() ->
    [Input1, Input2] = aoc_helper:parse_input("day3", binary),
    [SplitInput1, SplitInput2] = [binary:split(Input1, <<",">>, [global]), binary:split(Input2, <<",">>, [global])],
    {Part1, Intersections} = part1(SplitInput1, SplitInput2),
    Part2 = part2(SplitInput1, SplitInput2, Intersections),
    io:format("~p~n", [{Part1, Part2}]).

part1(FirstInput, SecondInput) ->
    FirstWireGridMap = mark_cables_on_grid_map(FirstInput, {0,0}, #{}, 1),
    SecondWireGridMap = mark_cables_on_grid_map(SecondInput, {0,0}, FirstWireGridMap, 2),
    Intersections = maps:fold(fun(K, V, Acc) -> case V > 1 of true -> [K|Acc]; false -> Acc end end, [], SecondWireGridMap),
    ManhattanDistResults = [abs(X) + abs(Y)|| {X, Y} <- Intersections],
    MinManhattanDis = lists:min(ManhattanDistResults),
    {MinManhattanDis, Intersections}.


part2(FirstInput, SecondInput, Intersections) ->
    InititalIntersectionMap = lists:foldl(fun(X, Acc) -> Acc#{X => undefined} end, #{}, Intersections),
    FirstResult = part2(FirstInput, {0,0}, 0, InititalIntersectionMap),
    SecondResult = part2(SecondInput, {0,0}, 0, InititalIntersectionMap),
    lists:min([maps:get(Coord, FirstResult) + maps:get(Coord, SecondResult) || Coord <- Intersections]).

part2([], _, _, IntersectionMap) ->
    IntersectionMap;
part2([<<Direction:1/binary, Distance/binary>>|Rest], Coord, Steps, IntersectionMap) ->
    DistanceVal = binary_to_integer(Distance),
    {NewCoord, Path} = path_from_coord(Direction, DistanceVal, Coord),
    part2(Rest, NewCoord, Steps+DistanceVal, part2_func(Path, Steps+1, IntersectionMap)).

part2_func([], _, IntersectionMap) ->
    IntersectionMap;
part2_func([Coord|Rest], Steps, IntersectionMap) ->
    case IntersectionMap of
        #{Coord := undefined} ->
            part2_func(Rest, Steps+1, IntersectionMap#{Coord => Steps});
        _ ->
            part2_func(Rest, Steps+1, IntersectionMap)
    end.

mark_cables_on_grid_map([], _, GridMap, _) ->
    GridMap;
mark_cables_on_grid_map([Move|Rest], CurrentPosition, GridMap, Wire) ->
    {NewPosition, NewGridMap} = move_on_grid_map(Move, CurrentPosition, GridMap, Wire),
    mark_cables_on_grid_map(Rest, NewPosition, NewGridMap, Wire).

move_on_grid_map(<<"U", Distance/binary>>, {CurrentX, CurrentY}, GridMap, Wire) ->
    DistanceVal = binary_to_integer(Distance),
    Path = [{CurrentX, Y} || Y <- lists:seq(CurrentY+1, CurrentY+DistanceVal)],
    {{CurrentX, CurrentY+DistanceVal}, lists:foldl(fun(A, B) -> mark_coord_on_grid_map(A, B, Wire) end, GridMap, Path)};
move_on_grid_map(<<"D", Distance/binary>>, {CurrentX, CurrentY}, GridMap, Wire) ->
    DistanceVal = binary_to_integer(Distance),
    Path = [{CurrentX, Y} || Y <- lists:seq(CurrentY-1, CurrentY-DistanceVal, -1)],
    {{CurrentX, CurrentY-DistanceVal}, lists:foldl(fun(A, B) -> mark_coord_on_grid_map(A, B, Wire) end, GridMap, Path)};
move_on_grid_map(<<"L", Distance/binary>>, {CurrentX, CurrentY}, GridMap, Wire) ->
    DistanceVal = binary_to_integer(Distance),
    Path = [{X, CurrentY} || X <- lists:seq(CurrentX-1, CurrentX-DistanceVal, -1)],
    {{CurrentX-DistanceVal, CurrentY}, lists:foldl(fun(A, B) -> mark_coord_on_grid_map(A, B, Wire) end, GridMap, Path)};
move_on_grid_map(<<"R", Distance/binary>>, {CurrentX, CurrentY}, GridMap, Wire) ->
    DistanceVal = binary_to_integer(Distance),
    Path = [{X, CurrentY} || X <- lists:seq(CurrentX+1, CurrentX+DistanceVal)],
    {{CurrentX+DistanceVal, CurrentY}, lists:foldl(fun(A, B) -> mark_coord_on_grid_map(A, B, Wire) end, GridMap, Path)}.

path_from_coord(<<"U">>, Distance, {CurrentX, CurrentY}) ->
    Path = [{CurrentX, Y} || Y <- lists:seq(CurrentY+1, CurrentY+Distance)],
    {{CurrentX, CurrentY+Distance}, Path};
path_from_coord(<<"D">>, Distance, {CurrentX, CurrentY}) ->
    Path = [{CurrentX, Y} || Y <- lists:seq(CurrentY-1, CurrentY-Distance, -1)],
    {{CurrentX, CurrentY-Distance}, Path};
path_from_coord(<<"L">>, Distance, {CurrentX, CurrentY}) ->
    Path = [{X, CurrentY} || X <- lists:seq(CurrentX-1, CurrentX-Distance, -1)],
    {{CurrentX-Distance, CurrentY}, Path};
path_from_coord(<<"R">>, Distance, {CurrentX, CurrentY}) ->
    Path = [{X, CurrentY} || X <- lists:seq(CurrentX+1, CurrentX+Distance)],
    {{CurrentX+Distance, CurrentY}, Path}.

mark_coord_on_grid_map(Coord, GridMap, Wire) ->
    case maps:is_key(Coord, GridMap) of
        true when Wire == 2 ->
            GridMap#{Coord => 2};
        false when Wire == 1 ->
            GridMap#{Coord => 1};
        _ ->
            GridMap
    end.

