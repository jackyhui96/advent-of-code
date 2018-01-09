-module(aoc_21_2017).
-compile(export_all).

main() ->
    %% Being lazy and should clean-up into a function, expand each rule for each flip and rotation
    Rules = maps:from_list(lists:flatten([ lists:zip(get_rotations(K), lists:duplicate(8, V)) || {K, V} <- maps:to_list(input())])),
    Start = ".#...####",

    Art1 = generate_art(Start, Rules, 5),
    Part1 = length([ 1 || $# <- Art1]),
    Art2 = generate_art(Start, Rules, 18),
    Part2 = length([ 1 || $# <- Art2]),

    {Part1, Part2}.

generate_art(String, _, 0) -> String;
generate_art(String, Rules, N) ->
    StringSquares = convert_string_to_grid(String),
    Result = combine_squares(process_squares_with_rule(StringSquares, Rules, [])),
    generate_art(Result, Rules, N-1).

combine_squares(Squares) ->
    BlockSize = case hd(Squares) of
        [_,_,_,_,_,_,_,_,_] -> 3;
        [_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_] -> 4
    end,
    combine_squares(Squares, length(Squares), BlockSize, []).


combine_squares([], _, _, Acc) -> lists:flatten(lists:reverse(Acc));
combine_squares(Squares, NumOfSquares, SubSize, Acc) ->
    RowSize = round(math:sqrt(NumOfSquares)),
    {Row, Rest} = lists:split(RowSize, Squares),
    RowString = join_squares(Row, SubSize, []),
    combine_squares(Rest, NumOfSquares, SubSize, [RowString|Acc]).

join_squares([[]|_], _, Acc) -> lists:reverse(Acc);
join_squares(Row, SubSize, Acc) ->
    F = fun(X, {CAcc, RAcc}) ->
        {Chars, Rest} = lists:split(SubSize, X),
        {[Chars|CAcc], [Rest|RAcc]}
    end,

    {Line, Rest} = lists:foldl(F, {[],[]}, Row),
    join_squares(lists:reverse(Rest), SubSize, [lists:reverse(Line)|Acc]).

process_squares_with_rule([], _, Acc) -> lists:reverse(Acc);
process_squares_with_rule([Square|Rest], Rules, Acc) ->
    NewString = match_patterns_to_rule(Square, Rules),
    process_squares_with_rule(Rest, Rules, [NewString|Acc]).


match_patterns_to_rule(P, Rules) ->
    #{P := Val} = Rules,
    Val.

convert_string_to_grid(String) ->
    Length = length(String),
    Size = round(math:sqrt(Length)),
    case (Size rem 2 =:= 0) of
        true -> 
            break_grid(String, Length div 4, 2, []);
        false -> 
            break_grid(String, Length div 9, 3, [])
    end.

break_grid([], _, _, Acc) -> lists:append(lists:reverse(Acc));
break_grid(String, NumOfSquares, SubSize, Acc) when SubSize =:= 2 ->

    Bin = list_to_binary(String),

    LineSize = round(math:sqrt(NumOfSquares))*SubSize,
    Squares = [
        begin
            A = binary:part(Bin, {X, SubSize}),
            B = binary:part(Bin, {X+LineSize, SubSize}),
            binary_to_list(<<A/binary, B/binary>>)
        end || X <- lists:seq(0, LineSize-1, SubSize)],
    break_grid(my_sublist(String, LineSize*SubSize+1, tail), NumOfSquares, SubSize, [Squares|Acc]);
    
break_grid(String, NumOfSquares, SubSize, Acc) when SubSize =:= 3 ->

    Bin = list_to_binary(String),

    LineSize = round(math:sqrt(NumOfSquares))*SubSize,
    Squares = [
        begin
            A = binary:part(Bin, {X, SubSize}),
            B = binary:part(Bin, {X+LineSize, SubSize}),
            C = binary:part(Bin, {X+(LineSize*2), SubSize}),
            binary_to_list(<<A/binary, B/binary, C/binary>>)
        end || X <- lists:seq(0, LineSize-1, SubSize)],
    break_grid(my_sublist(String, LineSize*SubSize+1, tail), NumOfSquares, SubSize, [Squares|Acc]).

my_sublist([], _, _) -> [];
my_sublist([A,B,C|_], 1, 3) -> [A,B,C];
my_sublist([A,B|_], 1, 2) -> [A,B];
my_sublist(String, 1, tail) -> String;
my_sublist([_|T], Index, Length) ->
    my_sublist(T, Index-1, Length).

symmetric([A,B,C,D]) ->
    [D,B,C,A];
symmetric([A,B,C,D,E,F,G,H,I]) ->
    [I,F,C,H,E,B,G,D,A].
    
flip([A,B,C,D]) ->
    [C,D,A,B];
flip([A,B,C,D,E,F,G,H,I]) ->
    [G,H,I,D,E,F,A,B,C].

get_rotations(Grid) ->
    G1 = symmetric(Grid),
    G2 = flip(G1),
    G3 = symmetric(G2),
    G4 = flip(G3),
    G5 = symmetric(G4),
    G6 = flip(G5),
    G7 = symmetric(G6),
    [Grid, G1, G2, G3, G4, G5, G6, G7].
    

input() ->
    {ok, Data} = file:read_file("../inputs/day21_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    maps:from_list([parse_input(L)|| L <- Lines]).

parse_input(<<In1:2/binary, "/", In2:2/binary, " => ", Out1:3/binary, "/", Out2:3/binary, "/", Out3:3/binary>>) ->
     {binary_to_list(<<In1/binary,In2/binary>>), binary_to_list(<<Out1/binary,Out2/binary,Out3/binary>>)};
parse_input(<<In1:3/binary, "/", In2:3/binary, "/", In3:3/binary, " => ", Out1:4/binary, "/", Out2:4/binary, "/", Out3:4/binary, "/", Out4:4/binary>>) ->
    {binary_to_list(<<In1/binary,In2/binary,In3/binary>>), binary_to_list(<<Out1/binary,Out2/binary,Out3/binary,Out4/binary>>)}.



%% ====================================================================
%% Unit tests
%% ====================================================================
%% Needed Eunits to help with debugging

-include_lib("eunit/include/eunit.hrl").

iteration1_test() ->
    Rules = maps:from_list(lists:flatten([ lists:zip(get_rotations(K), lists:duplicate(8, V)) || {K, V} <- maps:to_list(input())])),
    Start = ".#...####",
    ?assertEqual(".#.##.....#.#.##", generate_art(Start, Rules, 1)).

iteration2_test() ->
    Rules = maps:from_list(lists:flatten([ lists:zip(get_rotations(K), lists:duplicate(8, V)) || {K, V} <- maps:to_list(input())])),
    Start = ".#...####",
    ?assertEqual("#...###..#...#.###.##.#.#..#..####..", generate_art(Start, Rules, 2)).

iteration3_test() ->
    Rules = maps:from_list(lists:flatten([ lists:zip(get_rotations(K), lists:duplicate(8, V)) || {K, V} <- maps:to_list(input())])),
    Start = ".#...####",
    ?assertEqual("..#.##..##.##..#.##..####....##...#.#.##..#..#...#.#...#..#..###..#..#..#..#....#", generate_art(Start, Rules, 3)).

iteration4_test() ->
    Rules = maps:from_list(lists:flatten([ lists:zip(get_rotations(K), lists:duplicate(8, V)) || {K, V} <- maps:to_list(input())])),
    Start = ".#...####",
    ?assertEqual("##.##..###.#.##.###..##.#.......#....#####.#.#####.###..##...##.#########.....#...#..###...#...###..##..#.#.########..#...#...#.###....#...####.", generate_art(Start, Rules, 4)).

% iteration5_test() ->
%     Rules = maps:from_list(lists:flatten([ lists:zip(get_rotations(K), lists:duplicate(8, V)) || {K, V} <- maps:to_list(input())])),
%     Start = ".#...####",
%     ?assertEqual(".#.#...#.#...#.#..#..#..#..#..#..#..#...#.#...#.#...#.#....#..#.###....##..#.##.##..#..#.#.#.#..#..###.#.#...#.#...##..#.##..##..#.....#.#...#.##...#..###...###..#....#.###...###..#..#.##..#..#..#...#.#....#.#...#.#..##..#.##..#.##..#...#.#...#.##..#.#.###...###..####...###...###...##..##..#..#..#.....#.#..#.#...#.#..###..", generate_art(Start, Rules, 5)).
