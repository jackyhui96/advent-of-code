-module(aoc_1_2017).
-compile(export_all).

main() ->
    Input = input(),
    Size = byte_size(Input),
    
    Part1 = calc_captcha(Input, Size, 1),
    Part2 = calc_captcha(Input, Size, round(Size/2)),
    {Part1, Part2}.

calc_captcha(Bin, Size, Offset) ->
    calc_captcha(Bin, 0, Offset, Size, 0).

calc_captcha(_, Index, _, Size, Sum) when Index >= Size -> Sum;
calc_captcha(Bin, Index, Offset, Size, Sum) ->
    Val = binary:at(Bin, Index), 
    TargetIndex = (Index + Offset) rem Size,
    NewAcc = case binary:at(Bin, TargetIndex) of
        Val -> Sum + Val;
        _ -> Sum
    end,
    calc_captcha(Bin, Index + 1, Offset, Size, NewAcc).

input() ->
    {ok, Data} = file:read_file("../inputs/day1_data.txt"),
    << <<(X - $0)>> || <<X>> <= Data, X =/= $\n>>.