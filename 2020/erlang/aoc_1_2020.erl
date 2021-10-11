-module(aoc_1_2020).

-export([
    start/0,
    part1/1,
    part2/1]).

start() ->
    Input = input(),
    part1(Input),
    part2(Input).

part1(Input) ->
    [[A, B]] = generate_pairs_with_predicate(Input, fun(X) -> lists:sum(X) == 2020 end),
    Result = A * B,
    io:format("~p~n", [Result]),
    Result.

part2(Input) ->
    [[A, B, C]] = generate_triplet_with_predicate(Input, fun(X) -> lists:sum(X) == 2020 end),
    Result = A * B * C,
    io:format("~p~n", [Result]),
    Result.

generate_pairs_with_predicate(List, Pred) ->
    ZippedList = lists:zip(lists:seq(1, length(List)), List),
    lists:usort([
        lists:sort(Pair) ||
        {IndexA, A} <- ZippedList, {IndexB, B} <- ZippedList,
        begin Pair = [A, B], Pred(Pair) end,
        IndexA =/= IndexB]).

generate_triplet_with_predicate(List, Pred) ->
    ZippedList = lists:zip(lists:seq(1, length(List)), List),
    lists:usort([
        lists:sort(Pair) ||
        {IndexA, A} <- ZippedList, {IndexB, B} <- ZippedList, {IndexC, C} <- ZippedList,
        begin Pair = [A, B, C], Pred(Pair) end,
        IndexA =/= IndexB andalso IndexB =/= IndexC andalso IndexA =/= IndexC]).


input() ->
    {ok, Data} = file:read_file("2020/inputs/day1_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [binary_to_integer(X) || X <- Lines].


-include_lib("eunit/include/eunit.hrl").

example_test_() ->
    Input = [1721, 979, 366, 299, 675, 1456],
    ?_assertEqual(514579, part1(Input)),
    ?_assertEqual(241861950, part2(Input)).

puzzle_test_() ->
    Input = input(),
    ?_assertEqual(73371, part1(Input)),
    ?_assertEqual(127642310, part2(Input)).