-module(aoc_17_2017).
-compile(export_all).

main() ->
    Input = input(),

    [_,Part1|_] = spinlock(Input, 2018),
    ct:pal("Part1: ~p~n", [Part1]),

    Part2 = spinlock2(Input, 50000000),
    ct:pal("Part2: ~p~n", [Part2]).


spinlock(Step, Size) ->
    spinlock(Step, [0], 1, Size).

spinlock(_, Buffer, Size, Size) ->
    Buffer;
spinlock(Step, Buffer, Length, Size) ->
    NewBuffer = [Length|rotate(Step + 1, Buffer)],
    spinlock(Step, NewBuffer, Length + 1, Size).


%% Only need to track what's after the 0 which is always inserted
%% when current position is 1
spinlock2(Step, Size) ->
    spinlock2(Step, 0, [], 1, Size).

spinlock2(_, _, Buffer, Size, Size) ->
    Buffer;
spinlock2(Step, CurrentPos, Buffer, Length, Size) ->
    NewPos = (CurrentPos + Step) rem Length + 1,
    NewBuffer = case NewPos of
        1 -> Length;
        _ -> Buffer
    end,
    spinlock2(Step, NewPos, NewBuffer, Length + 1, Size).


rotate(_, [X]) -> [X];
rotate(Step, List) -> 
    rotate(Step, List, []).

rotate(0, List, Acc) ->
    List ++ lists:reverse(Acc);
rotate(Step, [], Acc) ->
    rotate(Step, lists:reverse(Acc), []);
rotate(Step, [H|T], Acc) ->
    rotate(Step-1, T, [H|Acc]).

input() -> 328.

