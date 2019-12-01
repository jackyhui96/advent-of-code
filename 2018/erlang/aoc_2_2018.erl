-module(aoc_2_2018).
-compile(export_all).

start() ->
    Input = input(),
    part1(Input),
    part2(Input).

part1(Input) ->
    ResultList = [check_string_for_exactly_2_and_3_chars(X) || X <- Input],
    {Exact2Count, Exact3Count} = count_exactly_2_and_3_chars_occurences(ResultList),
    Checksum = Exact2Count * Exact3Count,
    io:format("~b~n", [Checksum]).

part2(Input) ->
    [{A, B}|_] = lists:flatten([[{X,Y} || Y <- Input, check_string_differs_by_1_char(X,Y)] || X <- Input]),
    LengthOfCommonLetters = binary:longest_common_prefix([A,B]),
    <<PartA:LengthOfCommonLetters/binary, _:1/binary, PartB/binary>> = A,
    io:format("~s~n", [<<PartA/binary, PartB/binary>>]).

check_string_differs_by_1_char(StringA, StringB) ->
    check_string_differs_by_1_char(StringA, StringB, unchecked).

check_string_differs_by_1_char(<<>>, <<>>, differ_by_one) ->
    true;
check_string_differs_by_1_char(<<>>, <<>>, unchecked) ->
    false;
check_string_differs_by_1_char(<<SameChar:1/binary, Xs/binary>>, <<SameChar:1/binary, Ys/binary>>, DifferingType) ->
    check_string_differs_by_1_char(Xs, Ys, DifferingType);
check_string_differs_by_1_char(<<X:1/binary, Xs/binary>>, <<Y:1/binary, Ys/binary>>, unchecked) ->
    check_string_differs_by_1_char(Xs, Ys, differ_by_one);
check_string_differs_by_1_char(<<X:1/binary, _/binary>>, <<Y:1/binary, _/binary>>, differ_by_one) ->
    false.


count_exactly_2_and_3_chars_occurences(ResultList) ->
    count_exactly_2_and_3_chars_occurences(ResultList, 0, 0).

count_exactly_2_and_3_chars_occurences([], Exact2Count, Exact3Count) ->
    {Exact2Count, Exact3Count};
count_exactly_2_and_3_chars_occurences([H|T], Exact2Count, Exact3Count) ->
    {NextExact2Count, NextExact3Count} = 
        case H of
            {true, false} ->
                {Exact2Count+1, Exact3Count};
            {false, true} ->
                {Exact2Count, Exact3Count+1};
            {true, true} ->
                {Exact2Count+1, Exact3Count+1};
            {false, false} ->
                {Exact2Count, Exact3Count}
        end,
    count_exactly_2_and_3_chars_occurences(T, NextExact2Count, NextExact3Count).

check_string_for_exactly_2_and_3_chars(BinString) ->
    CharCountMap = convert_string_to_char_count_map(BinString),
    maps:fold(fun check_string_for_exactly_2_and_3_chars_map_func/3, {false, false}, CharCountMap).

check_string_for_exactly_2_and_3_chars_map_func(_, _, {true, true}) ->
    {true, true};
check_string_for_exactly_2_and_3_chars_map_func(_, V, {Exact2, Exact3}) ->
    case V of
        2 ->
            {true, Exact3};
        3 ->
            {Exact2, true};
        _ ->
            {Exact2, Exact3}
    end.


convert_string_to_char_count_map(BinString) ->
    convert_string_to_char_count_map(BinString, #{}).

convert_string_to_char_count_map(<<>>, CharCountMap) ->
    CharCountMap;
convert_string_to_char_count_map(<<H:1/binary, T/binary>>, CharCountMap) ->
    UpdatedCharCountMap = case CharCountMap of
        #{H := Count} ->
            CharCountMap#{H := Count+1};
        _ ->
            CharCountMap#{H => 1}
    end,
    convert_string_to_char_count_map(T, UpdatedCharCountMap).

input() ->
    {ok, Data} = file:read_file("../inputs/day2_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Lines.