-module(aoc_5_2017).
-compile(export_all).

% main() ->
%     Input = input(),
%     NumberedInput = lists:zip(lists:seq(1, length(Input)), Input),
%     Map = maps:from_list(NumberedInput),
    
%     Part1 = map_jump(1, 0, Map),
%     Part2 = map_jump2(1, 0, Map),
%     {Part1, Part2}.

% map_jump(Index, Count, Jumps) ->
%     case Jumps of
%         #{Index := Jump} ->
%             NewJumps = Jumps#{Index := Jump+1},
%             map_jump(Index+Jump, Count+1, NewJumps);
%         _ -> 
%             Count
%     end.

% map_jump2(Index, Count, Jumps) ->
%     case Jumps of
%         #{Index := Jump} ->
%             NewJumps = case Jump >= 3 of
%                 true ->
%                     Jumps#{Index := Jump-1};
%                 false ->
%                     Jumps#{Index := Jump+1}
%             end,
%             map_jump2(Index+Jump, Count+1, NewJumps);
%         _ -> 
%             Count
%     end.

% input() ->
%     {ok, Data} = file:read_file("../inputs/day5_data.txt"),
%     Bins = binary:split(Data, <<"\n">>, [global, trim_all]),
%     [binary_to_integer(B) || B <- Bins].

main() ->
    Offsets = input(),
    Part1 = zip([], Offsets, 0),
    Part2 = zip2([], Offsets, 0, fun part2_incr_func/1),
    {Part1, Part2}.

input() ->
    {ok, Data} = file:read_file("../inputs/day5_data.txt"),
    [binary_to_integer(N) || N <- binary:split(Data, <<"\n">>, [global, trim_all])].
 
%% no instructions, we're done
zip(_, [], Count) -> Count;
zip(Prev, [N|Next], Count) when N >= 0 ->
    {Skipped, NewNext} = take(N, {Prev, [N+1|Next]}),
    zip(Skipped, NewNext, Count+1);
zip(Prev, [N|Next], Count) when N < 0 ->
    {NewNext, Skipped} = take(abs(N), {[N+1|Next], Prev}),
    zip(Skipped, NewNext, Count+1).
 
take(0, Acc) -> Acc;
take(_, {_, []}=Acc) -> Acc;
take(N, {Prev, [H|T]}) -> take(N-1, {[H|Prev],T}).
 
zip2(_, [], Count, _) -> Count;
zip2(Prev, [N|Next], Count, IncrFunc) when N >= 0 ->
    {Skipped, NewNext} = take(N, {Prev, [IncrFunc(N)|Next]}),
    zip2(Skipped, NewNext, Count+1, IncrFunc);
zip2(Prev, [N|Next], Count, IncrFunc) when N < 0 ->
    {NewNext, Skipped} = take(abs(N), {[IncrFunc(N)|Next], Prev}),
    zip2(Skipped, NewNext, Count+1, IncrFunc).
 
part2_incr_func(N) when N >= 3 -> N-1;
part2_incr_func(N) -> N+1.