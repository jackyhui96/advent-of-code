-module(aoc_3_2020).

-export([
    start/0,
    part1/1,
    part2/1]).

start() ->
    Input = input(),
    part1(Input),
    part2(Input).

part1(Input) ->
    Result = count_trees_in_path({0, 0}, Input, {3, 1}),
    io:format("~p~n", [Result]),
    Result.

part2(Input) ->
    Steps = [
        {1, 1},
        {3, 1},
        {5, 1},
        {7, 1},
        {1, 2}
    ],
    Counts = [count_trees_in_path({0, 0}, Input, Step) || Step <- Steps],
    Result = lists:foldl(fun(A, B) -> A * B end, 1, Counts),
    io:format("~p~n", [Result]),
    Result.

count_trees_in_path(StartingPosition, Grid, Step) ->
    count_trees_in_path(StartingPosition, Grid, Step, 0).

count_trees_in_path(CurrentPosition, Grid, Step, Count) ->
    NewPosition = step_position(CurrentPosition, Step),
    try
        NewCount = 
            case at_grid_area(NewPosition, Grid) of
                tree -> Count + 1;
                open -> Count
            end,
        count_trees_in_path(NewPosition, Grid, Step, NewCount)
    catch _:_ ->
        Count
    end.

translate_square($.) -> open;
translate_square($#) -> tree.

step_position({CurrentX, CurrentY}, {StepX, StepY}) ->
    {CurrentX + StepX, CurrentY + StepY}.

at_grid_area({X, Y}, Grid) ->
    RowArea = at_col_area(Y, Grid),
    Square = at_row_area(X, RowArea),
    translate_square(Square).

at_row_area(N, RowArea) ->
    % binary:at is 0 indexed
    RowLength = erlang:size(RowArea),
    Position = N rem RowLength,
    binary:at(RowArea, Position).

at_col_area(N, Grid) ->
    maps:get(N, Grid).

input() ->
    {ok, Data} = file:read_file("2020/inputs/day3_data.txt"),
    Lines = binary:split(Data, [<<"\n">>], [global, trim_all]),
    ZippedList = lists:zip(lists:seq(0, length(Lines) - 1), Lines),
    maps:from_list(ZippedList).


-include_lib("eunit/include/eunit.hrl").

example_test_() ->
    Input = #{
        0 => <<"..##.......">>,
        1 => <<"#...#...#..">>,
        2 => <<".#....#..#.">>,
        3 => <<"..#.#...#.#">>,
        4 => <<".#...##..#.">>,
        5 => <<"..#.##.....">>,
        6 => <<".#.#.#....#">>,
        7 => <<".#........#">>,
        8 => <<"#.##...#...">>,
        9 => <<"#...##....#">>,
        10 => <<".#..#...#.#">>
    },
    [
        ?_assertEqual(tree, at_grid_area({13, 0}, Input)),
        ?_assertEqual(7, part1(Input)),
        ?_assertEqual(336, part2(Input))
    ].

puzzle_test_() ->
    Input = input(),
    [
        ?_assertEqual(162, part1(Input)),
        ?_assertEqual(3064612320, part2(Input))
    ].
