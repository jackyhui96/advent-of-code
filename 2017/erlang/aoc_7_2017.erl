-module(aoc_7_2017).
-compile(export_all).

main() ->
    Input = input(),
    
    Root = find_root(maps:to_list(Input)),
    {Part2Node, Weight} = find_wrong_weight(Root, Input),
    [Part2List] = [ Children || {_, Children} <- maps:values(Input), lists:member(Part2Node, Children)],
    Part2 = hd([calculate_weight(X, Input) || X <- Part2List, X =/= Part2Node]) - calculate_weight(Part2Node, Input) + Weight,
    {Root, Part2}.

input() ->
    {ok, Bin} = file:read_file("../inputs/day7_data.txt"),
    BinLines = binary:split(Bin, <<"\n">>, [global, trim_all]),
    format_bin_lines(BinLines).

find_wrong_weight(Root, Nodes) when not is_list(Root) ->
    #{Root := {W, Children}} = Nodes,
    NodeWeights = [{C, calculate_weight(C, Nodes)} || C <- Children],
    BadNodes = [Name || {Name, Weight} = X <- NodeWeights, lists:keymember(Weight, 2, (NodeWeights--[X])) =:= false],
    case BadNodes of
        [] -> {Root, W};
        [Node] -> 
            find_wrong_weight(Node, Nodes);
        _ ->
            find_wrong_weight([Children], Nodes)
            
    end;

find_wrong_weight([], _Nodes) -> false;
find_wrong_weight([H|T], Nodes) ->
    case find_wrong_weight(H, Nodes) of
        false ->
            find_wrong_weight(T, Nodes);
        Node ->
            Node
    end.

check_weight(_Name, []) -> true;
check_weight(Name, Nodes) ->
    #{Name := {_Weight, Children}} = Nodes, 
    case lists:usort([calculate_weight(C, Nodes) || C <- Children]) of
        [_] -> true;
        _ -> false
    end.

calculate_weight(Name, Nodes) ->
    #{Name := {Weight, Children}} = Nodes, 
    case Children of
        [] ->
            Weight;
        _ ->
            Weight + lists:sum([calculate_weight(C, Nodes)|| C <- Children])
    end.

find_root(Nodes) ->
    find_root(Nodes, Nodes).

find_root([], _Nodes) ->
    error;
find_root([{Name1,_} = Node|Rest], Nodes) ->
    case lists:all(
            fun({_, {_, Children}}) ->
                not lists:member(Name1, Children)
            end, Nodes -- [Node]) of
        false ->
            find_root(Rest, Nodes);
        true ->
            Name1
    end.

format_bin_lines(BinLines) ->
    lists:foldl(fun format_bin/2, #{}, BinLines).

format_bin(<<Bin/binary>>, Acc) ->
    {BracketStartPos, _} = binary:match(Bin, <<"(">>),
    <<Name/binary>> = binary:part(Bin, {0, BracketStartPos-1}),
    {BracketEndPos, _} = binary:match(Bin, <<")">>),
    <<Num/binary>> = binary:part(Bin, {BracketStartPos+1, BracketEndPos-BracketStartPos-1}),

    case binary:match(Bin, <<"-> ">>) of
        {BindingStartPos, Length} ->
            BinStringChildren = binary:part(Bin, {BindingStartPos+Length, byte_size(Bin)-BindingStartPos-Length}),
            Children = binary:split(BinStringChildren, <<", ">>, [global, trim_all]),
            Acc#{Name => {erlang:binary_to_integer(Num), Children}};
        nomatch ->
            Acc#{Name => {erlang:binary_to_integer(Num), []}}
    end.