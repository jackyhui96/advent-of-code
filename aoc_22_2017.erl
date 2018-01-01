-module(aoc_22_2017).
-compile(export_all).

main() ->
    Part1 = ok,
    ct:pal("Part1: ~p~n", [Part1]),
    Part2 = ok,
    ct:pal("Part2: ~p~n", [Part2]).

input() ->
    {ok, Data} = file:read_file("day22_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    RowSize = byte_size(hd(Lines)),
    ColSize = length(Lines),
    Map = create_grid_map(Lines, 0, RowSize, #{}),
    ct:pal("DEBUG: ~p~n", [Data]),
    {RowSize, ColSize}.

create_grid_map([Line|Rest], LineIndex, Size, Acc) ->
    Indices = [ {LineIndex, X} || X <- lists:seq(0, Size-1)],
    NewIndexedLine = [ {} || <<B>> <= Line]
    NewAcc = Acc#{{X, Y}, }