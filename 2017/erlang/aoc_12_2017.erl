-module(aoc_12_2017).
-compile(export_all).

main() ->
    Input = input(),
    Part1 = length(find_programs_in_group(Input, <<"0">>, [])),
    Part2 = length(find_all_groups(Input)),
    {Part1, Part2}.


find_all_groups(Map) ->
    Programs = maps:keys(Map),
    find_all_groups(Programs, Map, []).

find_all_groups([], _, Acc) -> Acc;
find_all_groups([Program|Rest], Map, Acc) ->
    Group = find_programs_in_group(Map, Program, []),
    find_all_groups(Rest -- Group, Map, [Group|Acc]).


find_programs_in_group(_Map, [], Acc) -> Acc;
find_programs_in_group(Map, [Program|Rest], Acc) ->
    NewAcc = case lists:member(Program, Acc) of
        false ->
            UnseenPrograms = find_programs_in_group(Map, Program, Acc),
            [UP || UP <- UnseenPrograms, not lists:member(UP, Acc)] ++ Acc;
        true ->
            Acc
    end,
    find_programs_in_group(Map, Rest, NewAcc);
    
find_programs_in_group(Map, Program, Acc) ->
    #{Program := ConnectedPrograms} = Map,
    NewAcc = [Program|Acc],
    UnseenPrograms = [CP || CP <- ConnectedPrograms, not lists:member(CP, NewAcc)],
    case UnseenPrograms of
        [] -> [Program];
        _ ->
            find_programs_in_group(Map, UnseenPrograms, NewAcc)
    end.


input() ->
    {ok, Data} = file:read_file("../inputs/day12_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    maps:from_list([begin 
        [Program|ConnectedPrograms] = binary:split(L, [<<" <-> ">>, <<", ">>], [global, trim_all]), 
        {Program, ConnectedPrograms}
     end|| L <- Lines]).