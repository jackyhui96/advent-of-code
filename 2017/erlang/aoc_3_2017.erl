-module(aoc_3_2017).
-compile(export_all).

main() ->
    Input = input(),
    Part1 = spiral_memory(lists:seq(2,Input), #{{0,0} => 1}, {0,0}, right),
    Part2 = spiral_memory2(1, #{{0,0} => 1}, {0,0}, right, Input),
    {Part1, Part2}.
    
spiral_memory([], _, {X, Y}, _) ->
    abs(X) + abs(Y);

spiral_memory([H|T], Map, {X,Y}, right) ->
    Target = {X+1, Y+1},
    NewCoord = {X+1, Y},
    Direction = case Map of
        #{Target := _} -> right;
        _ -> up
    end,
    spiral_memory(T, Map#{NewCoord => H}, NewCoord, Direction);

spiral_memory([H|T], Map, {X,Y}, up) ->
    Target = {X-1, Y+1},
    NewCoord = {X, Y+1},
    Direction = case Map of
        #{Target := _} -> up;
        _ -> left
    end,
    spiral_memory(T, Map#{NewCoord => H}, NewCoord, Direction);

spiral_memory([H|T], Map, {X,Y}, left) ->
    Target = {X-1, Y-1},
    NewCoord = {X-1, Y},
    Direction = case Map of
        #{Target := _} -> left;
        _ -> down
    end,
    spiral_memory(T, Map#{NewCoord => H}, NewCoord, Direction);

spiral_memory([H|T], Map, {X,Y}, down) ->
    Target = {X+1, Y-1},
    NewCoord = {X, Y-1},
    Direction = case Map of
        #{Target := _} -> down;
        _ -> right
    end,
    spiral_memory(T, Map#{NewCoord => H}, NewCoord, Direction).


spiral_memory2(Current, _Dict, _Coord, _, Max) when Current > Max ->
    Current;

spiral_memory2(_, Map, {X,Y}, right, Max) ->
    Target = {X+1, Y+1},
    NewCoord = {X+1, Y},
    Neighbours = get_valid_neighbours(NewCoord, Map),
    Value = lists:sum(Neighbours),
    Direction = case Map of
        #{Target := _} -> right;
        _ -> up
    end,
    spiral_memory2(Value, Map#{NewCoord => Value}, NewCoord, Direction, Max);

spiral_memory2(_, Map, {X,Y}, up, Max) ->
    Target = {X-1, Y+1},
    NewCoord = {X, Y+1},
    Neighbours = get_valid_neighbours(NewCoord, Map),
    Value = lists:sum(Neighbours),
    Direction = case Map of
        #{Target := _} -> up;
        _ -> left
    end,
    spiral_memory2(Value, Map#{NewCoord => Value}, NewCoord, Direction, Max);
    
spiral_memory2(_, Map, {X,Y}, left, Max) ->
    Target = {X-1, Y-1},
    NewCoord = {X-1, Y},
    Neighbours = get_valid_neighbours(NewCoord, Map),
    Value = lists:sum(Neighbours),
    Direction = case Map of
        #{Target := _} -> left;
        _ -> down
    end,
    spiral_memory2(Value, Map#{NewCoord => Value}, NewCoord, Direction, Max);

spiral_memory2(_, Map, {X,Y}, down, Max) ->
    Target = {X+1, Y-1},
    NewCoord = {X, Y-1},
    Neighbours = get_valid_neighbours(NewCoord, Map),
    Value = lists:sum(Neighbours),
    Direction = case Map of
        #{Target := _} -> down;
        _ -> right
    end,
    spiral_memory2(Value, Map#{NewCoord => Value}, NewCoord, Direction, Max).

input() ->
    {ok, Data} = file:read_file("../inputs/day3_data.txt"),
    binary_to_integer(Data).

get_valid_neighbours(Coord, Map) ->
    [begin 
        case Map of
            #{N := V} -> V;
            _ -> 0
        end
    end || N <- neighbours(Coord)].

neighbours({X, Y}) ->
    [{X+1, Y}, {X+1, Y+1}, {X, Y+1}, {X-1, Y+1}, {X-1, Y}, {X-1, Y-1}, {X, Y-1}, {X+1, Y-1}].