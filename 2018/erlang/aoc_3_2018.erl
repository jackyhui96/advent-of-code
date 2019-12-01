-module(aoc_3_2018).
-compile(export_all).

start() ->
    Input = input(),
    CoordMap = part1(Input),
    part2(Input, CoordMap).

part1(Input) -> 
    Part1Func =
        fun(Values, AccMap) ->
            Coords = produce_coords_from_values(Values),
            FoldFunc = 
                fun(Elem, Acc) -> 
                    case Acc of 
                        #{Elem := Count} -> Acc#{Elem := Count+1};
                        _ -> Acc#{Elem => 1} 
                    end
                end,
            lists:foldl(FoldFunc, AccMap, Coords)
        end,
    CoordMap = lists:foldl(Part1Func, #{}, Input),
    OverlappedCoords = [{Coord, Count} || {Coord, Count} <- maps:to_list(CoordMap), Count > 1],
    io:format("~p~n", [length(OverlappedCoords)]),
    CoordMap.

part2(Input, CoordMap) ->
    Result = [{Claim, [Coord || Coord <- produce_coords_from_values(Elem), maps:get(Coord, CoordMap) > 1]} || Elem = [Claim|_] <- Input],
    {NonOverlappingClaim, _} = lists:keyfind([], 2, Result),
io:format("~s~n", [NonOverlappingClaim]).

produce_coords_from_values(Values) ->
    [_, X, Y, W, H] = [binary_to_integer(V) || V <- Values],
    Rows = lists:seq(X, X+W-1),
    Cols = lists:seq(Y, Y+H-1),
    [{R, C} || R <- Rows, C <- Cols].


input() ->
    {ok, Data} = file:read_file("../inputs/day3_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    ParsedInput = [binary:split(L, [<<" ">>, <<"@">>, <<":">>, <<"x">>, <<"#">>, <<",">>], [global, trim_all]) ||L <- Lines],
    ParsedInput.