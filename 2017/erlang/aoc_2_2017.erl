-module(aoc_2_2017).
-compile(export_all).

main() ->
    Input = input(),
    checksum(Input, 0, 0).

checksum([], Sum1, Sum2) ->
    {trunc(Sum1), trunc(Sum2)};
checksum([H|T], Sum1, Sum2) ->
    Range1 = lists:max(H) - lists:min(H),
    Range2 = divisible_values(H, H),
    checksum(T, Sum1+Range1, Sum2+Range2).

divisible_values(Input) ->
    divisible_values(Input,Input).

divisible_values([], _Input) ->
    error;

divisible_values([H|T],Input) ->
    Result = [ {X rem H, X / H} || X <- Input--[H]],
    case lists:keyfind(0, 1, Result) of
        {0, Val} -> 
            Val;
        false ->
            divisible_values(T, Input)
    end.

input() ->
    {ok, Data} = file:read_file("../inputs/day2_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    [begin 
        Bin = binary:split(L, <<"\t">>, [global, trim_all]),
        [ binary_to_integer(B) || B <- Bin]
    end || L <- Lines].