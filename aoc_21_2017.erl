-module(aoc_21_2017).
-compile(export_all).

main() ->
    Rules = input(),
    Start = ".#...####",
    Part1 = generate_art(Start, Rules),
    ct:pal("Part1: ~p~n", [Part1]),

    Part2 = ok,
    ct:pal("Part2: ~p~n", [Part2]).

generate_art(String, Rules) ->
    StringSquares = convert_string_to_grid(String),
    process_squares_with_rule(StringSquares, Rules, []).
    
process_squares_with_rule([], _, Acc) -> Acc;
process_squares_with_rule([String|Rest], Rules, Acc) ->
    Patterns = get_rotations(String),
    NewString = match_patterns_to_rule(Patterns, Rules),
    process_squares_with_rule(Rest, Rules, [NewString|Acc]).


match_patterns_to_rule([Pattern|Rest], Rules) ->
    ct:pal("Debug: ~p~n", [Pattern]),
    case Rules of
        #{Pattern := Val} ->
            Val;
        _ ->
            match_patterns_to_rule(Rest, Rules)
    end.


convert_string_to_grid(String) ->
    Length = length(String),
    case Length rem 2 of
        0 -> break_grid(String, Length div 4, 2);
        _ -> break_grid(String, Length div 9, 3)
    end.

break_grid(String, NumOfSquares, SubSize) ->
    MaxIndex = (round(math:sqrt(NumOfSquares))-1) * SubSize,
    Values = lists:seq(0, MaxIndex, SubSize),
    StartPositions = [{X,Y} || X <- Values, Y <- Values],
    SquareIndices = [ [{X,Y}|| X <- lists:seq(Col,Col+SubSize-1), Y <- lists:seq(Row,Row+SubSize-1)] || {Col, Row} <- StartPositions],
    F = fun(X, Y) ->
        Index = X*round(math:sqrt(NumOfSquares))*SubSize+Y+1,
        {{X,Y}, lists:nth(Index, String)}
    end,
    [
        [ F(X, Y) || {X,Y} <- Indices]
    || Indices <- SquareIndices].

symmetric([_,_,_,_] = Grid) ->
    [{{2-1-Y, 2-1-X}, V}|| {{X,Y}, V} <- Grid];
symmetric([_,_,_,_,_,_,_,_,_] = Grid) ->
    [{{3-1-Y, 3-1-X}, V}|| {{X,Y}, V} <- Grid].
    
flip([_,_,_,_] = Grid) ->
    [{{2-1-X, Y}, V}|| {{X,Y}, V} <- Grid];
flip([_,_,_,_,_,_,_,_,_] = Grid) ->
    [{{3-1-X, Y}, V}|| {{X,Y}, V} <- Grid].

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
    {ok, Data} = file:read_file("day21_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    F = fun
        F(<<In1:2/binary, "/", In2:2/binary, " => ", Out1:3/binary, "/", Out2:3/binary, "/", Out3:3/binary>>) ->
            {binary_to_list(<<In1/binary,In2/binary>>), binary_to_list(<<Out1/binary,Out2/binary,Out3/binary>>)};
        F(<<In1:3/binary, "/", In2:3/binary, "/", In3:3/binary, " => ", Out1:4/binary, "/", Out2:4/binary, "/", Out3:4/binary, "/", Out4:4/binary>>) ->    
            {binary_to_list(<<In1/binary,In2/binary,In3/binary>>), binary_to_list(<<Out1/binary,Out2/binary,Out3/binary,Out4/binary>>)}
    end,
    maps:from_list([F(L)|| L <- Lines]).