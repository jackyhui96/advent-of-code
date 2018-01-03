-module(aoc_24_2017).
-compile(export_all).

main() ->
    Input = input(),

    %% Slow implementation, not sure how to improve
    Bridges = find_bridges(Input),
    {Part1, _} = lists:max(Bridges),
    ct:pal("Part1: ~p~n", [Part1]),

    F = fun({Strength, Length}, {MaxStrength, MaxLength}) ->
        case Length of
            MaxLength ->
                {max(Strength, MaxStrength), MaxLength};
            L when L > MaxLength ->
                {Strength, Length};
            _ ->
                {MaxStrength, MaxLength}
        end
    end,
    {Part2, _} = lists:foldl(F, {0,0}, Bridges),
    ct:pal("Part2: ~p~n", [Part2]).

find_bridges(List) ->
    StartList = find_matching_parts(0, List),
    find_bridges(StartList, List, []).

find_bridges([], _, Acc) -> Acc;

find_bridges([Bridge|Rest], List, Acc) when not is_list(Bridge) ->
    NewList = List -- [Bridge],
    Parts = find_matching_parts(Bridge, NewList),
    NewBridges = [ [P|[Bridge]] || P <- Parts],
    BridgeStrength = bridge_strength([Bridge]),
    find_bridges(Rest++NewBridges, List, [{BridgeStrength, 1}|Acc]);

find_bridges([Bridge|Rest], List, Acc) ->
    NewList = List -- Bridge,
    Parts = find_matching_parts(Bridge, NewList),
    NewBridges = [ [P|Bridge] || P <- Parts],
    BridgeStrength = bridge_strength(Bridge),
    find_bridges(Rest++NewBridges, List, [{BridgeStrength, length(Bridge)}|Acc]).

bridge_strength(Bridge) ->
    lists:sum([X+Y || {X, Y} <- Bridge]).

find_matching_parts(0, List) ->
    [{0, X} || {0, X} <- List];
find_matching_parts({0, A}, List) ->
    F = fun(Y) ->
        case Y of
            {A, _} -> true;
            {_, A} -> true;
            _ -> false
        end
    end,
    [ X || X <- List, F(X)];
find_matching_parts([{A, B},{_, B}|_], List) ->
    F = fun(Y) ->
        case Y of
            {A, _} -> true;
            {_, A} -> true;
            _ -> false
        end
    end,
    [X || X <- List, F(X)];
find_matching_parts([{A, B},{B, _}|_], List) ->
    F = fun(Y) ->
        case Y of
            {A, _} -> true;
            {_, A} -> true;
            _ -> false
        end
    end,
    [X || X <- List, F(X)];
find_matching_parts([{A, B},{A, _}|_], List) ->
    F = fun(Y) ->
        case Y of
            {B, _} -> true;
            {_, B} -> true;
            _ -> false
        end
    end,
    [X || X <- List, F(X)];
find_matching_parts([{A, B},{_, A}|_], List) ->
    F = fun(Y) ->
        case Y of
            {B, _} -> true;
            {_, B} -> true;
            _ -> false
        end
    end,
    [X || X <- List, F(X)].

input() ->
    {ok, Data} = file:read_file("day24_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    F = fun 
        F(<<A:1/binary, "/", B/binary>>) ->
            {binary_to_integer(A), binary_to_integer(B)};
        F(<<A:2/binary, "/", B/binary>>) ->
            {binary_to_integer(A), binary_to_integer(B)}
    end,
    [F(L) || L <- Lines].