-module(aoc_24_2017).
-compile(export_all).

main() ->
    Input = input(),
    Part1 = part1(0, Input, 0),
    {_Len, Part2} = part2(0, Input, {0,0}),
    {Part1, Part2}.

part1(X, Pairs, Value) ->
    Combinations = get_bridge_combinations(X, Pairs),
    case Combinations of
        [] -> Value;
        _ ->
        lists:max([begin
            [NextX] = Pair -- [X],
            part1(NextX, Next, Value+lists:sum(Pair))
        end || {Pair, Next} <- Combinations])
    end.
 
part2(X, Pairs, Value={Len,Strength}) ->
    Combinations = get_bridge_combinations(X, Pairs),
    case Combinations of
        [] -> Value;
        _ ->
            lists:max([begin
                [NextX] = Pair -- [X],
                part2(NextX, Next, {Len+1, Strength+lists:sum(Pair)})
            end || {Pair, Next} <- Combinations])
    end.

get_bridge_combinations(X, Pairs) ->
    {Match, NoMatch} = lists:partition(fun([A,B]) -> A == X orelse B == X end, Pairs),
    [{Pair, (Match--[Pair])++NoMatch} || Pair <- Match].

input() ->
    {ok, Data} = file:read_file("../inputs/day24_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    F = fun 
        (<<A:1/binary, "/", B/binary>>) ->
            [binary_to_integer(A), binary_to_integer(B)];
        (<<A:2/binary, "/", B/binary>>) ->
            [binary_to_integer(A), binary_to_integer(B)]
    end,
    [F(L) || L <- Lines].

