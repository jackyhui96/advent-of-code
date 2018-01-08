-module(aoc_4_2017).
-compile(export_all).

main() ->
    Input = input(),
    Part1 = length([1 || X <- Input, length(X) =:= length(lists:usort(X))]),
    Part2 = length([1 || X <- Input, length(X) =:= length(lists:usort([ lists:sort(Y) || Y <- X]))]),
    {Part1, Part2}.

input() ->
    {ok, Data} = file:read_file("../inputs/day4_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [begin 
        Words = binary:split(L, <<" ">>, [global, trim_all]),
        [ binary_to_list(W) || W <- Words]
    end || L <- Lines].


