-module(aoc_4_2019).
-export([start/0]).

start() ->
    [MinRange, MaxRange] = aoc_helper:parse_input("day4", integer, <<"-">>),
    PossibleNumbers = [integer_to_list(Number) || Number <- lists:seq(MinRange, MaxRange)],
    Part1 = check_numbers_with_critera(PossibleNumbers, part1),
    Part2 = check_numbers_with_critera(PossibleNumbers, part2),
    io:format("~p~n", [{Part1, Part2}]).

check_numbers_with_critera(Numbers, Type) ->
    check_numbers_with_critera(Numbers, Type, 0).
check_numbers_with_critera([], _, Acc) ->
    Acc;
check_numbers_with_critera([Number|Rest], Type, Acc) ->
    case number_criteria_checker(Number, Type) of
        true ->
            check_numbers_with_critera(Rest, Type, Acc + 1);
        false ->
            check_numbers_with_critera(Rest, Type, Acc)
    end.


number_criteria_checker(Number, Type) ->
     number_criteria_checker(Number, false, Type).

number_criteria_checker([_], Flag, _) ->
    Flag;
number_criteria_checker([Current, Current, Current | Rest], Flag, Type) when Type == part2 ->
    NewList = remove_until_mismatching_adjacent_numbers([Current|Rest]),
    number_criteria_checker(NewList, Flag, Type);
number_criteria_checker([Current, Next | Rest], Flag, Type) when Next >= Current ->
    number_criteria_checker([Next | Rest], Flag orelse Current == Next, Type);
number_criteria_checker(_, _, _) ->
    false.

remove_until_mismatching_adjacent_numbers([A, A | Rest]) ->
    remove_until_mismatching_adjacent_numbers([A | Rest]);
remove_until_mismatching_adjacent_numbers(List) ->
    List.



