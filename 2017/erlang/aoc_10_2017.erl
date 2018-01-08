-module(aoc_10_2017).
-compile(export_all).

-define(SIZE, 256).

main() ->
    Input = input(),
    Part1List = knot_hash(Input),
    Part1 = lists:nth(1, Part1List) * lists:nth(2, Part1List),

    SparseHash = knot_hash_64(input2()),
    DenseHash = sparse_to_dense_hash(SparseHash),
    Part2 = convert_list_to_hex(DenseHash),

    {Part1, Part2}.

knot_hash_64(Input) ->
    SeqLengths = Input ++ [17, 31, 73, 47, 23],
    knot_hash_64_func(SeqLengths).

knot_hash_64_func(SeqLengths) ->
    SeqLengths64 = lists:append(lists:duplicate(64, SeqLengths)),
    knot_hash(SeqLengths64).

knot_hash(SeqLengths) ->
    List = lists:zip(lists:seq(0,?SIZE-1),lists:seq(0,?SIZE-1)),
    Map = maps:from_list(List),
    knot_hash(Map, 0, 0, SeqLengths).

knot_hash(Map, _, _, []) ->
    [begin  #{Index := Element} = Map, Element end || Index <- lists:seq(0, ?SIZE-1)];
knot_hash(Map, CurrentPos, SkipSize, [Length|Rest]) ->
    NewMap = rev_length_elements(CurrentPos, Length, Map),
    knot_hash(NewMap, get_index(CurrentPos+Length+SkipSize), SkipSize+1, Rest).
    
rev_length_elements(_, Length, _) when Length > ?SIZE -> 
    error;
rev_length_elements(CurrentPos, Length, Map) ->
    Indexes = get_indexes(CurrentPos, Length),
    IndexToNewIndex = lists:zip(Indexes, lists:reverse(Indexes)),
    NewKeyVals = [begin  #{Index := Element} = Map, {NewIndex, Element}  end || {Index, NewIndex} <- IndexToNewIndex],
    lists:foldl(fun({Index, Element}, Acc) -> Acc#{Index => Element} end, Map, NewKeyVals).

get_indexes(CurrentPos, Length) ->
    [get_index(Index) || Index <- lists:seq(CurrentPos, CurrentPos+Length-1)].

get_index(Index) ->
    Index rem ?SIZE.

sparse_to_dense_hash(Hash) ->
    sparse_to_dense_hash(Hash, []).

%% For each block of 16, xor all the elements in the block
sparse_to_dense_hash([], Acc) -> lists:reverse(Acc);
sparse_to_dense_hash(Hash, Acc) ->
    {[H|T], Rest} = lists:split(16, Hash),
    NewHash = lists:foldl(fun(X, Y) -> X bxor Y end, H, T),
    sparse_to_dense_hash(Rest, [NewHash|Acc]).
    
convert_list_to_hex(List) ->
    Hex = [ integer_to_list(Int, 16) || Int <- List],
    %% This is to pad zeros if the hex is a single digit
    PaddedHex = [ case C of [_,_] -> C; [X] -> [$0|[X]] end || C <- Hex],
    string:to_lower(lists:append(PaddedHex)).

input() ->
    {ok, Data} = file:read_file("../inputs/day10_data.txt"),
    Bin = binary:split(Data, [<<",">>, <<"\n">>], [global, trim_all]),
    [ binary_to_integer(X) || X <- Bin].

input2() ->
    {ok, Data} = file:read_file("../inputs/day10_data.txt"),
    [C|| <<C>> <= Data, C =/= $\n].
    