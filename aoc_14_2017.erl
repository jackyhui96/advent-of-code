-module(aoc_14_2017).
-compile(export_all).

-define(GRID_SIZE, 128).

main() ->
    Input = input(),
    BitHashes = lists:flatten([convert_hex_to_bits(knot_hash(I)) || I <- Input]),
    
    Part1 = lists:sum(BitHashes),
    ct:pal("Part1: ~p~n", [Part1]),

    Part2 = length(find_all_regions(BitHashes)),
    ct:pal("Part2: ~p~n", [Part2]).

%% Uses pattern matching to pull out the 4 bits for each Hexdigit
convert_hex_to_bits(Hex) ->
    [begin 
        Int = list_to_integer([Digit], 16),
        <<0:4, A:1, B:1, C:1, D:1>> = <<Int:8>>,
        [A,B,C,D]
    end || Digit <- Hex].

%% Uses previous code which can find all groups of which a program is connected to
%% Can therefore find regions where bits of 1 are connected
find_all_regions(BitHashes) ->
    Indexes = [{A, B}|| A <- lists:seq(1, ?GRID_SIZE), B <- lists:seq(1,?GRID_SIZE)],
    GridWithUsedBits = [{Index, 1} || {Index, 1} <- lists:zip(Indexes, BitHashes)],
    GridMap = maps:from_list(GridWithUsedBits),
    Map = maps:from_list([{Index, find_neighbours(Index, GridMap)}  || {Index, _} <- GridWithUsedBits]),
    aoc_12_2017:find_all_groups(Map).

%% Return the adjacent neighbours of an index where the bit value is 1
find_neighbours({A,B}, GridMap) ->
    F = fun(X, Map) -> 
        case 
            Map of #{X := _} -> true;
            _ -> false 
        end 
    end, 
    Neighbours = [{A+1, B}, {A-1, B}, {A, B+1}, {A, B-1}],
    [Index || {X, Y} = Index <- Neighbours, (X >= 1 andalso X =< ?GRID_SIZE), 
                                            (Y >= 1 andalso Y =< ?GRID_SIZE), 
                                            F(Index, GridMap)].

input() ->
    Input = "oundnydw",
    [Input ++ [$-|erlang:integer_to_list(N)]|| N <- lists:seq(0, ?GRID_SIZE-1)].
    
knot_hash(Input) ->
    SparseHash = aoc_10_2017:knot_hash_64(Input),
    DenseHash = aoc_10_2017:sparse_to_dense_hash(SparseHash),
    aoc_10_2017:convert_list_to_hex(DenseHash).