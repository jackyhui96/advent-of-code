-module(aoc_9_2017).
-compile(export_all).

main() ->
    Input = input(),
    % process_stream(Input).

process_stream(Data) ->
    

input() ->
    {ok, Data} = file:read_file("day9_data.txt"),
    Data.