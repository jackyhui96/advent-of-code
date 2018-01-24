-module(aoc_3_2017).
-compile(export_all).

main() ->
    Input = input(),
    Part1 = spiral_memory(lists:seq(2,Input), #{{0,0} => 1}, {0,0}, right),
    Part2 = spiral_memory2(1, #{{0,0} => 1}, {0,0}, right, Input),
    {Part1, Part2}.
    
spiral_memory([], _, {X, Y}, _) ->
    abs(X) + abs(Y);
spiral_memory([H|T], Map, Pos, Direction) ->
    {Target, NewCoord} = move(Pos, Direction),
    NewDirection = rotate(Direction, Target, Map),
    spiral_memory(T, Map#{NewCoord => H}, NewCoord, NewDirection).

spiral_memory2(Current, _Dict, _Coord, _, Max) when Current > Max -> Current;
spiral_memory2(_, Map, Pos, Direction, Max) ->
    {Target, NewCoord} = move(Pos, Direction),
    Neighbours = get_valid_neighbours(NewCoord, Map),
    Value = lists:sum(Neighbours),
    NewDirection = rotate(Direction, Target, Map),
    spiral_memory2(Value, Map#{NewCoord => Value}, NewCoord, NewDirection, Max).

move({X, Y}, right) -> {{X+1, Y+1}, {X+1, Y}};
move({X, Y}, up) -> {{X-1, Y+1}, {X, Y+1}};
move({X, Y}, left) -> {{X-1, Y-1}, {X-1, Y}};
move({X, Y}, down) -> {{X+1, Y-1}, {X, Y-1}}.

rotate(down, Target, Map) ->
    case Map of
        #{Target := _} -> down;
        _ -> right
    end;

rotate(left, Target, Map) ->
    case Map of
        #{Target := _} -> left;
        _ -> down
    end;

rotate(up, Target, Map) ->
    case Map of
        #{Target := _} -> up;
        _ -> left
    end;

rotate(right, Target, Map) ->
    case Map of
        #{Target := _} -> right;
        _ -> up
    end.

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