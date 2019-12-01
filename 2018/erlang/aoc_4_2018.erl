-module(aoc_4_2018).
-compile(export_all).

start() ->
    Input = input(),
    SortedInput = lists:sort(Input),
    io:format("~p~n", [SortedInput]),
    part1(Input),
    part2(Input).

part1(Input) ->
    ok.

part2(Input) ->
    ok.

input() ->
    {ok, Data} = file:read_file("../inputs/day4_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Lines.