-module(aoc_1_2018).
-compile(export_all).

start() ->
    Input = input(),
    part1(Input),
    part2(Input).

part1(Input) ->
    Sum = lists:sum(Input),
    io:format("~p~n", [Sum]).

part2(Input) ->
    DuplicateResult = sum_loop_until_duplicate(Input),
    io:format("~p~n", [DuplicateResult]).

sum_loop_until_duplicate(List) ->
    sum_loop_until_duplicate(List, List, [], 0).

sum_loop_until_duplicate([], OriginalList, Results, CurrentResult) ->
    sum_loop_until_duplicate(OriginalList, OriginalList, Results, CurrentResult);
sum_loop_until_duplicate([H|T], OriginalList, Results, CurrentResult) ->
    NextResult = H + CurrentResult,
    case lists:member(NextResult, Results) of
        true ->
            NextResult;
        false ->
            sum_loop_until_duplicate(T, OriginalList, [NextResult|Results], NextResult)
    end.

input() ->
    {ok, Data} = file:read_file("../inputs/day1_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [binary_to_integer(X) || X <- Lines].