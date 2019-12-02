-module(aoc_2_2019).
-export([start/0]).

start() ->
    Input = aoc_helper:parse_input("day2", integer, <<",">>),
    ZippedInputList = lists:zip(lists:seq(0, length(Input)-1), Input),
    InputMap = maps:from_list(ZippedInputList),
    Part1 = part1(set_initial_program_parameters(12, 2, InputMap)),
    Part2 = part2(InputMap),
    io:format("~p~n", [{get_result_in_position_zero(Part1), Part2}]).

part1(InputMap) -> 
    part1(0, InputMap).

part1(Index, InputMap) -> 
    [A, B, C, D] = lists:seq(Index, Index + 3),
    OpCode = maps:get(A, InputMap, 0),
    ValA = maps:get(B, InputMap, 0),
    ValB = maps:get(C, InputMap, 0),
    ValC = maps:get(D, InputMap, 0),
    case opcode(OpCode, ValA, ValB, ValC, InputMap) of
        stop ->
            InputMap;
        ResultMap ->
            part1(Index+4, ResultMap)
    end.

part2(InputMap) ->
    PossibleVals = lists:seq(0, 99),
    try
        [part2_func(A, B, InputMap) || A <- PossibleVals, B <- PossibleVals]
    catch
        throw:Thrown ->
           {result, Result} = Thrown,
           Result
    end.

part2_func(A, B, InputMap) ->
    UpdatedInputMap = set_initial_program_parameters(A, B, InputMap),
    case get_result_in_position_zero(part1(UpdatedInputMap)) of
        19690720 ->
            Result = 100 * A + B,
            throw({result, Result});
        _ ->
            ok
    end.

get_result_in_position_zero(#{0 := Result}) ->
    Result.

set_initial_program_parameters(A, B, InputMap) ->
    InputMap#{1 => A, 2 => B}.

opcode(1, A, B, C, Map) ->
    #{A := NumA, B := NumB} = Map,
    Map#{C => NumA + NumB};
opcode(2, A, B, C, Map) ->
    #{A := NumA, B := NumB} = Map,
    Map#{C => NumA * NumB};
opcode(99, _, _, _, _) ->
    stop.
