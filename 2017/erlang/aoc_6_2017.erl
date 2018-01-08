-module(aoc_6_2017).
-compile(export_all).

main() ->
    Input = input(),
    NumberedInput = lists:zip(Input, lists:seq(1, length(Input))),
    
    {Part1, State} = reallocation(NumberedInput, [], length(NumberedInput)),
    Part2 = reallocation2(State, State, length(NumberedInput), 0),
    
    {Part1, Part2}.

reallocation2(State, TargetState, Length, Count) -> 
    {Blocks, IndexOfMaxBlocks} = hd(sort_highest_first(State)),
    NewState = redistribute_blocks(IndexOfMaxBlocks, Blocks, State, Length),
    
    case NewState of
        TargetState ->
            Count+1;
        _ ->
            reallocation2(NewState, TargetState, Length, Count+1)
    end.

reallocation(State, Prevs, Length) -> 
    {Blocks, IndexOfMaxBlocks} = hd(sort_highest_first(State)),
    NewState = redistribute_blocks(IndexOfMaxBlocks, Blocks, State, Length),
    
    case lists:member(NewState, Prevs) of
        true ->
            {length(Prevs)+1, NewState};
        false ->
            reallocation(NewState, [NewState|Prevs], Length)
    end.

redistribute_blocks(IndexOfMaxBlocks, Blocks, State, Length) ->
    %% initialise the highest block to 0
    InitState = lists:keyreplace(IndexOfMaxBlocks, 2, State, {0, IndexOfMaxBlocks}),
    redistribute_blocks_func(IndexOfMaxBlocks+1, Blocks, InitState, Length).

redistribute_blocks_func(_, 0, State, _) ->
    State;
redistribute_blocks_func(Index, Blocks, State, Length) when Index > Length ->
    %% Wrap around and redistribute on the first index
    redistribute_blocks_func(1, Blocks, State, Length);
redistribute_blocks_func(Index, Blocks, State, Length) ->
    {NumBlocks, _} = lists:nth(Index, State),
    NewState = lists:keyreplace(Index, 2, State, {NumBlocks+1, Index}),
    redistribute_blocks_func(Index+1, Blocks-1, NewState, Length).

sort_highest_first(List) ->
    F = fun({BlkA, BnkA}, {BlkB, BnkB}) ->
        case BlkA =:= BlkB of
            true ->
                BnkA =< BnkB;
            false ->
                BlkA > BlkB
        end
    end,
    lists:sort(F, List).

input() ->
    {ok, Data} = file:read_file("../inputs/day6_data.txt"),
    Bins = binary:split(Data, [<<"\t">>, <<"\n">>], [global, trim_all]),
    [binary_to_integer(B) || B <- Bins].