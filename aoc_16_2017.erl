-module(aoc_16_2017).
-compile(export_all).

-define(SIZE, 5).

main() ->
    % Input = input(),
    Input = [<<"s1">>, <<"x3/4">>, <<"pe/b">>],
    Programs = << <<X>> || X <- lists:seq($a, $e)>>,

    Part1 = ok,
    ct:pal("Part1: ~p~n", [Input]).

    % Part2 = ok,
    % ct:pal("Part2: ~p~n", [Part2]).

program_dance([<<"s", X:1>>|Rest], Programs) ->
    NewPrograms = spin(X, Programs),
    program_dance(Rest, NewPrograms);

program_dance([<<"x", A:1, "/", B:1>>|Rest], Programs) ->
    NewPrograms = exchange({A, B}, Programs),
    program_dance(Rest, NewPrograms);

program_dance([<<"p", A:1, "/", B:1>>|Rest], Programs) ->
    NewPrograms = partner({A, B}, Programs),
    program_dance(Rest, NewPrograms).


spin(X, Bin) ->
    Length = ?SIZE-X,
    <<Front:Length, End:X>> = Bin,
    <<End, Bin>>.

exchange({A, B}, Bin) ->
    ValA = binary:at(Bin, A),
    ValB = binary:at(Bin, B),
    partner({ValA, ValB}, Bin).

partner({A, B}, Bin) ->
    partner(A, B, Bin, <<>>).

partner(A, B, Bin, Acc) ->
    Acc;
partner(A, B, <<A:1, Rest>>, Acc) ->
    partner(A, B, Rest, <<Acc/binary, B>>);
partner(A, B, <<B:1, Rest>>, Acc) ->
    partner(A, B, Rest, <<Acc/binary, A>>);
partner(A, B, <<H:1, Rest>>, Acc) ->
    partner(A, B, Rest, <<Acc/binary, H>>).
    






input() ->
    {ok, Data} = file:read_file("day16_data.txt"),
    binary:split(Data, [<<",">>, <<"\n">>], [global, trim_all]).