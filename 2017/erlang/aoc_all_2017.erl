-module(aoc_all_2017).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

run_test(Day, F, Answer) ->
    
    
    {Time, Result} = timer:tc(F, []),
    io:format("Day ~2.. B: ~p, Time:~10.. B~n", [Day, ?assertEqual(Answer, Result), Time]).

main() ->
    Days = [
        {1, fun aoc_1_2017:main/0, {1031, 1080}},
        {2, fun aoc_2_2017:main/0, {51139, 272}},
        {3, fun aoc_3_2017:main/0, {438, 266330}},
        {4, fun aoc_4_2017:main/0, {325, 119}},
        {5, fun aoc_5_2017:main/0, {387096, 28040648}},
        {6, fun aoc_6_2017:main/0, {6681, 2392}},
        {7, fun aoc_7_2017:main/0, {<<"dtacyn">>, 521}},
        {8, fun aoc_8_2017:main/0, {4902, 7037}},
        {9, fun aoc_9_2017:main/0, {20530, 9978}},
        {10, fun aoc_10_2017:main/0, {62238, "2b0c9cc0449507a0db3babd57ad9e8d8"}},
        {11, fun aoc_11_2017:main/0, {824, 1548}},
        {12, fun aoc_12_2017:main/0, {169, 179}},
        {13, fun aoc_13_2017:main/0, {1612, 3907994}},
        {14, fun aoc_14_2017:main/0, {8106, 1164}},
        {15, fun aoc_15_2017:main/0, {638, 343}},
        {16, fun aoc_16_2017:main/0, {<<"gkmndaholjbfcepi">>, <<"abihnfkojcmegldp">>}},
        {17, fun aoc_17_2017:main/0, {1670, 2316253}},
        {18, fun aoc_18_2017:main/0, {2951, 7366}},
        {19, fun aoc_19_2017:main/0, {"LIWQYKMRP", 16764}},
        {20, fun aoc_20_2017:main/0, {157, 499}},
        {21, fun aoc_21_2017:main/0, {133, 2221990}},
        {22, fun aoc_22_2017:main/0, {5223, 2511456}},
        {23, fun aoc_23_2017:main/0, {5929, 907}},
        {24, fun aoc_24_2017:main/0, {1940, 1928}},
        {25, fun aoc_25_2017:main/0, 2526}
    ],
    lists:foreach(fun({Day, _, _}) -> compile:file("aoc_" ++ integer_to_list(Day) ++ "_2017") end, Days),
    lists:foreach(fun({Day, Func, Ans}) -> run_test(Day, Func, Ans) end, Days).