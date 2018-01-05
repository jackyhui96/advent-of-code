-module(aoc_16_2017).
-compile(export_all).

-define(SIZE, 16).

main() ->
    Input = input(),
    Programs = << <<X>> || X <- lists:seq($a, $p)>>,

    Part1 = program_dance(Input, Programs),
    ct:pal("Part1: ~p~n", [Part1]),

    Part2 = program_dance_cycle(Input, Programs),
    ct:pal("Part2: ~p~n", [Part2]).


program_dance_cycle(Input, Programs) ->
    program_dance_cycle(Input, Programs, #{}, 1000000000, 0).

program_dance_cycle(_, Programs, _, 0, _) ->
    Programs;

program_dance_cycle(Input, Programs, Prev, Count, CurrCycleCount) ->
    case Prev of
        #{Programs := {PrevCycleCount, _}} ->
            %% If we find a previous state, the answer is in the list of previous states
            %% Calculate the remainder of the cycle length on count, this is the index
            %% If index -1 then use the answer found in previous
            AnswerIndex = case Count rem (CurrCycleCount-PrevCycleCount)-1 of
                -1 -> PrevCycleCount;
                N -> N
            end,
            [Result] = [ Answer || {Index, Answer} <- maps:values(Prev), Index =:= AnswerIndex],
            Result;
        _ ->
            NewPrograms = program_dance(Input, Programs),
            NewPrev = Prev#{Programs => {CurrCycleCount, NewPrograms}},
            program_dance_cycle(Input, NewPrograms, NewPrev, Count-1, CurrCycleCount+1)
    end.

program_dance([], Programs) -> Programs;

program_dance([<<"s", X/binary>>|Rest], Programs) ->
    NewPrograms = spin(binary_to_integer(X), Programs),
    program_dance(Rest, NewPrograms);

program_dance([<<"x", Bin/binary>>|Rest], Programs) ->
    {A, B} = split_by_slash(Bin),
    NewPrograms = exchange({binary_to_integer(A), binary_to_integer(B)}, Programs),
    program_dance(Rest, NewPrograms);

program_dance([<<"p", Bin/binary>>|Rest], Programs) ->
    {A, B} = split_by_slash(Bin),
    NewPrograms = partner({A, B}, Programs),
    program_dance(Rest, NewPrograms).

spin(X, Bin) ->
    Length = ?SIZE-X,
    <<Front:Length/binary, End/binary>> = Bin,
    <<End/binary, Front/binary>>.

%% Messy duplication, but I prefer this way so you can see where the A and B values are matched
exchange({A, B}, Bin) ->
    case A < B of
        true -> 
            OffsetA = A,
            OffsetB = B-A-1,
            <<Front:OffsetA/binary, ValA:1/binary, Mid:OffsetB/binary, ValB:1/binary, End/binary>> = Bin,
            <<Front/binary, ValB/binary, Mid/binary, ValA/binary, End/binary>>;
        false ->
            OffsetB = B,
            OffsetA = A-B-1,
            <<Front:OffsetB/binary, ValB:1/binary, Mid:OffsetA/binary, ValA:1/binary, End/binary>> = Bin,
            <<Front/binary, ValA/binary, Mid/binary, ValB/binary, End/binary>>
    end.

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

split_by_slash(<<A:1/binary, "/", B:1/binary>>) -> {A, B};
split_by_slash(<<A:1/binary, "/", B:2/binary>>) -> {A, B};
split_by_slash(<<A:2/binary, "/", B:1/binary>>) -> {A, B};
split_by_slash(<<A:2/binary, "/", B:2/binary>>) -> {A, B}.
    
input() ->
    {ok, Data} = file:read_file("day16_data.txt"),
    binary:split(Data, [<<",">>, <<"\n">>], [global, trim_all]).