-module(aoc_22_2017).
-compile(export_all).

main() ->
    Input = input(),
    StartIndex = get_middle_index(Input),

    Part1Acc = sporifica_virus(StartIndex, up, [], Input, 10000),
    %% Count the number of times the virus infected a node
    ct:pal("Part1: ~p~n", [length([1 || infect <- Part1Acc])]),

    Part2Acc = evolved_sporifica_virus(StartIndex, up, [] ,Input, 10000000),
    ct:pal("Part2: ~p~n", [length([1 || infect <- Part2Acc])]).

    
sporifica_virus(_, _, Acc, _, 0) -> Acc;
sporifica_virus(Index, Direction, Acc, Map, Bursts) ->
    CurrentNode = case Map of
        #{Index := V} -> V;
        _ -> $.
    end,
    NewDirection = case CurrentNode of
        $# -> turn(Direction, right);
        _ -> turn(Direction, left)
    end,
    {NewMap, NewAcc} = case CurrentNode of
        $. -> {Map#{Index => $#}, [infect|Acc]};
        _ -> {Map#{Index => $.}, [clean|Acc]}
    end,
    NewIndex = move_forward(Index, NewDirection),
    sporifica_virus(NewIndex, NewDirection, NewAcc, NewMap, Bursts-1).

evolved_sporifica_virus(_, _, Acc, _, 0) -> Acc;
evolved_sporifica_virus(Index, Direction, Acc, Map, Bursts) ->
    CurrentNode = case Map of
        #{Index := V} -> V;
        _ -> $.
    end,
    NewDirection = case CurrentNode of
        $. -> turn(Direction, left);
        $W -> Direction;
        $# -> turn(Direction, right);
        $F -> turn(Direction, reverse)
    end,
    {NewMap, NewAcc} = case CurrentNode of
        $. -> {Map#{Index => $W}, [weaken|Acc]};
        $W -> {Map#{Index => $#}, [infect|Acc]};
        $# -> {Map#{Index => $F}, [flag|Acc]};
        $F -> {Map#{Index => $.}, [clean|Acc]}
    end,
    NewIndex = move_forward(Index, NewDirection),
    evolved_sporifica_virus(NewIndex, NewDirection, NewAcc, NewMap, Bursts-1).

get_middle_index(Map) ->
    MiddleIndex = round(math:sqrt(map_size(Map))) div 2,
    {MiddleIndex, MiddleIndex}.

move_forward({Y, X}, up) -> {Y-1, X};
move_forward({Y, X}, right) -> {Y, X+1};
move_forward({Y, X}, down) -> {Y+1, X};
move_forward({Y, X}, left) -> {Y, X-1}.

turn(up, right) -> right;
turn(right, right) -> down;
turn(down, right) -> left;
turn(left, right) -> up;
turn(up, left) -> left;
turn(left, left) -> down;
turn(down, left) -> right;
turn(right, left) -> up;
turn(up, reverse) -> down;
turn(left, reverse) -> right;
turn(down, reverse) -> up;
turn(right, reverse) -> left.

input() ->
    {ok, Data} = file:read_file("day22_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    Size = length(Lines),
    create_grid_map(Lines, 0, Size, #{}).

create_grid_map([], _, _, Acc) -> Acc;
create_grid_map([Line|Rest], LineIndex, Size, Acc) ->
    Indices = [{LineIndex, X} || X <- lists:seq(0, Size-1)],
    LineMap = maps:from_list(lists:zip(Indices, binary_to_list(Line))),
    NewAcc = maps:merge(Acc, LineMap),
    create_grid_map(Rest, LineIndex+1, Size, NewAcc).