-module(aoc_5_2017).
-compile(export_all).

main() ->
    Input = input(),
    NumberedInput = lists:zip(lists:seq(1, length(Input)), Input),
    Map = maps:from_list(NumberedInput),
    Part1 = map_jump(1, 0, Map),
    ct:pal("Part1: ~p~n", [Part1]),
    Part2 = map_jump2(1, 0, Map),
    ct:pal("Part2: ~p~n", [Part2]).

map_jump(Index, Count, Jumps) ->
    case Jumps of
        #{Index := Jump} ->
            NewJumps = Jumps#{Index := Jump+1},
            map_jump(Index+Jump, Count+1, NewJumps);
        _ -> 
            Count
    end.

map_jump2(Index, Count, Jumps) ->
    case Jumps of
        #{Index := Jump} ->
            NewJumps = case Jump >= 3 of
                true ->
                    Jumps#{Index := Jump-1};
                false ->
                    Jumps#{Index := Jump+1}
            end,
            map_jump2(Index+Jump, Count+1, NewJumps);
        _ -> 
            Count
    end.

input() ->
    {ok, Data} = file:read_file("day5_data.txt"),
    Bins = binary:split(Data, <<"\n">>, [global, trim_all]),
    [binary_to_integer(B) || B <- Bins].