-module(aoc_1_2017).
-compile(export_all).

main() ->
    Input = input() ++ [(hd(input()))],
    Input2 = input(),
    Part1 = calc_captcha(Input, 0),
    ct:pal("Part1: ~p~n", [Part1]),
    Part2 = calc_captcha2(Input2, 1, length(Input2), 0),
    ct:pal("Part2: ~p~n", [Part2]).

calc_captcha([], Acc) ->
    Acc;
calc_captcha([X,X|T], Acc) ->
    calc_captcha([X|T], Acc+X);
calc_captcha([_|T], Acc) ->
    calc_captcha(T, Acc).

calc_captcha2(_, Index, Length, Acc) when Index > Length ->
    Acc;
calc_captcha2(Input, Index, Length, Acc) ->
    Val = lists:nth(Index, Input), 
    TargetIndex = (Index - 1 + round(Length/2)) rem Length + 1,
    NewAcc = case Val =:= lists:nth(TargetIndex, Input) of
        true ->
            Acc + Val;
        false ->
            Acc
    end,
    calc_captcha2(Input, Index+1, Length, NewAcc).

input() ->
    {ok, Data} = file:read_file("day1_data.txt"),
    [ X - $0 || <<X>> <= Data, X =/= $\n].