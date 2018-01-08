-module(aoc_1_2017).
-compile(export_all).

main() ->
    Input = input(),
    Length = length(Input),
    
    Part1 = calc_captcha(Input, 1),
    Part2 = calc_captcha(Input, round(Length/2)),
    {Part1, Part2}.

calc_captcha(List, Offset) ->
    Length = length(List),
    calc_captcha(List, 1, Offset, Length, 0).

calc_captcha(_, Index, _, Length, Acc) when Index > Length -> Acc;
calc_captcha(List, Index, Offset, Length, Acc) ->
    Val = lists:nth(Index, List), 
    TargetIndex = (Index - 1 + Offset) rem Length + 1,
    NewAcc = case lists:nth(TargetIndex, List) of
        Val -> 
            Acc + Val;
        _ -> 
            Acc
    end,
    calc_captcha(List, Index+1, Offset, Length, NewAcc).

input() ->
    {ok, Data} = file:read_file("../inputs/day1_data.txt"),
    [ X - $0 || <<X>> <= Data, X =/= $\n].