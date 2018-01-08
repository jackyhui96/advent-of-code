-module(aoc_9_2017).
-compile(export_all).

main() ->
    Input = input(),
    process_stream(Input, 0, 0, group, 0).

process_stream(<<>>, _Level, Score, _Type, GarbageSize) -> 
    {Score, GarbageSize};

process_stream(<<"!", _:1/binary, Data/binary>>, Level, Score, Type, GarbageSize) -> 
    process_stream(Data, Level, Score, Type, GarbageSize);

process_stream(<<">", Data/binary>>, Level, Score, garbage, GarbageSize) -> 
    process_stream(Data, Level, Score, group, GarbageSize);

process_stream(<<_:1/binary, Data/binary>>, Level, Score, garbage, GarbageSize) -> 
    process_stream(Data, Level, Score, garbage, GarbageSize+1);

process_stream(<<"<", Data/binary>>, Level, Score, _Type, GarbageSize) -> 
    process_stream(Data, Level, Score, garbage, GarbageSize);

process_stream(<<"{", Data/binary>>, Level, Score, Type, GarbageSize) when Type =/= garbage -> 
    process_stream(Data, Level+1, Score, group, GarbageSize);

process_stream(<<"}", Data/binary>>, Level, Score, group, GarbageSize) -> 
    process_stream(Data, Level-1, Score+Level, group, GarbageSize);

process_stream(<<_:1/binary, Data/binary>>, Level, Score, Type, GarbageSize) -> 
    process_stream(Data, Level, Score, Type, GarbageSize).

input() ->
    {ok, Data} = file:read_file("../inputs/day9_data.txt"),
    Data.