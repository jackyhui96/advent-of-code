-module(aoc_16_2017).
-compile(export_all).

-define(SIZE, 16).

main() ->
    Input = input(),
    % Input = [<<"s1">>, <<"x3/4">>, <<"pe/b">>],
    Programs = << <<X>> || X <- lists:seq($a, $p)>>,

    Part1 = program_dance(Input, Programs),
    ct:pal("Part1: ~p~n", [Part1]),

    Part2 = program_dance_cycle(Input, Programs),
    ct:pal("Part2: ~p~n", [Part2]).


program_dance_cycle(Input, Programs) ->
    program_dance_cycle(Input, Programs, #{}, 1000000000).

program_dance_cycle(_, Programs, _, 0) ->
    Programs;

program_dance_cycle(Input, Programs, Map, Count) ->
    {NewPrograms, NewMap} = case Map of
        #{Programs := Val} ->
            {Val, Map};
        _ ->
            Result = program_dance(Input, Programs),
            {Result, Map#{Programs => Result}}
    end,
    program_dance_cycle(Input, NewPrograms, NewMap, Count-1).


program_dance([], Programs) -> Programs;

program_dance([<<"s", X/binary>>|Rest], Programs) ->
    NewPrograms = spin(binary_to_integer(X), Programs),
    program_dance(Rest, NewPrograms);

program_dance([<<"x", Bin/binary>>|Rest], Programs) ->
    [A, B] = binary:split(Bin, <<"/">>),
    NewPrograms = exchange({binary_to_integer(A), binary_to_integer(B)}, Programs),
    program_dance(Rest, NewPrograms);

program_dance([<<"p", Bin/binary>>|Rest], Programs) ->
    [A, B] = binary:split(Bin, <<"/">>),
    NewPrograms = partner({A, B}, Programs),
    program_dance(Rest, NewPrograms).

spin(X, Bin) ->
    Length = ?SIZE-X,
    <<Front:Length/binary, End/binary>> = Bin,
    <<End/binary, Front/binary>>.

exchange({A, B}, Bin) ->
    ValA = binary:at(Bin, A),
    ValB = binary:at(Bin, B),
    partner({<<ValA>>, <<ValB>>}, Bin).

partner({A, B}, Bin) ->
    partner(A, B, Bin, <<>>).

partner(_, _, <<>>, Acc) ->
    Acc;
partner(A, B, <<A:1/binary, Rest/binary>>, Acc) ->
    partner(A, B, Rest, <<Acc/binary, B/binary>>);
partner(A, B, <<B:1/binary, Rest/binary>>, Acc) ->
    partner(A, B, Rest, <<Acc/binary, A/binary>>);
partner(A, B, <<H:1/binary, Rest/binary>>, Acc) ->
    partner(A, B, Rest, <<Acc/binary, H/binary>>).
    






input() ->
    {ok, Data} = file:read_file("day16_data.txt"),
    binary:split(Data, [<<",">>, <<"\n">>], [global, trim_all]).