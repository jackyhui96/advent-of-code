-module(aoc_1_2019).
-export([start/0]).

start() ->
    Input = aoc_helper:parse_input("day1", integer),
    Part1 = lists:sum(lists:map(fun part1/1, Input)),
    Part2 = lists:sum(lists:map(fun part2/1, Input)),
    io:format("~p~n", [{Part1, Part2}]).

part1(Input) ->
    erlang:floor(Input / 3) - 2.

part2(Input) ->
    part2(Input, []).

part2(Input, Acc) ->
    case part1(Input) of
        N when N > 0 ->
            part2(N, [N|Acc]);
        _ ->
            lists:sum(Acc)
    end.