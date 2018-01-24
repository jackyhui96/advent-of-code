-module(aoc_2_2017).
-compile(export_all).

main() ->
    Input = input(),
    checksum(Input, 0, 0).

checksum([], Sum1, Sum2) ->
    {Sum1, trunc(Sum2)};
checksum([Row|Rest], Sum1, Sum2) ->
    Range1 = lists:max(Row) - lists:min(Row),
    Range2 = even_divisible_values(Row),
    checksum(Rest, Sum1+Range1, Sum2+Range2).

even_divisible_values([H|T]) -> even_divisible_values(H, T, T).

even_divisible_values(_, [], List) -> even_divisible_values(List);
even_divisible_values(X, [Y|_], _) when X rem Y =:= 0 -> X / Y;
even_divisible_values(X, [Y|_], _) when Y rem X =:= 0 -> Y / X;
even_divisible_values(X, [_|T], List) -> even_divisible_values(X, T, List).

input() ->
    {ok, Data} = file:read_file("../inputs/day2_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [begin 
        Bin = binary:split(L, <<"\t">>, [global, trim_all]),
        [binary_to_integer(B) || B <- Bin]
    end || L <- Lines].