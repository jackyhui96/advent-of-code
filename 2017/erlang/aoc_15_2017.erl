-module(aoc_15_2017).
-compile(export_all).

main() ->
    {GenAStart, GenBStart} = input(),
    InitialA = generate_a(GenAStart),
    InitialB = generate_b(GenBStart),

    Part1 = dueling_generators(InitialA, InitialB, 0, 40000000),
    InitialA2 = generate_a2(GenAStart),
    InitialB2 = generate_b2(GenBStart),
    Part2 = dueling_generators2(InitialA2, InitialB2, 0, 5000000),

    {Part1, Part2}.
    

dueling_generators(_, _, Count, 0) -> Count;
dueling_generators(GeneratorA, GeneratorB, Count, Times) ->
    NewCount = case judge(<<GeneratorA:32>>, <<GeneratorB:32>>) of
        true -> Count+1;
        false -> Count
    end,

    dueling_generators(generate_a(GeneratorA), generate_b(GeneratorB), NewCount, Times-1).


dueling_generators2(_, _, Count, 0) -> Count;
dueling_generators2(GeneratorA, GeneratorB, Count, Times) ->
    NewCount = case judge(<<GeneratorA:32>>, <<GeneratorB:32>>) of
        true -> Count+1;
        false -> Count
    end,

    dueling_generators2(generate_a2(GeneratorA), generate_b2(GeneratorB), NewCount, Times-1).


judge(<<_:16, X:16>>, <<_:16, X:16>>) -> true;
judge(_, _) -> false.

generate_a(Prev) -> generator(Prev, 16807).
generate_b(Prev) -> generator(Prev, 48271).

generate_a2(Prev) -> 
    case generate_a(Prev) of
        N when N rem 4 =:= 0 -> N;
        N -> generate_a2(N)
    end.

generate_b2(Prev) -> 
    case generate_b(Prev) of
        N when N rem 8 =:= 0 -> N;
        N -> generate_b2(N)
    end.

generator(Prev, Factor) ->
    (Prev * Factor) rem 2147483647.

input() ->
    {ok, Data} = file:read_file("../inputs/day15_data.txt"),
    [LineA, LineB] = binary:split(Data, <<"\n">>),
    <<"Generator A starts with ", A/binary>> = LineA,
    <<"Generator B starts with ", B/binary>> = LineB,
    {binary_to_integer(A), binary_to_integer(B)}.