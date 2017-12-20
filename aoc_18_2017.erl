-module(aoc_18_2017).
-compile(export_all).

main() ->
    Input = input(),

    Part1 = duet(Input),
    ct:pal("Part1: ~p~n", [Part1]),

    Part2 = ok,
    ct:pal("Part2: ~p~n", [Part2]).

duet(Input) ->
    ZipList = list_to_zlist(Input),
    RegisterMap = maps:from_list([{<<C>>,0}|| {_,<<C>>,_} <- Input, C >= $a, C =< $z]),
    duet(ZipList, RegisterMap, []).

duet({_, []}, RegMap, Sounds) -> 
    {Sounds, RegMap};
duet(ZList, RegMap, Sounds) ->
    Instruction = current(ZList),
    {NewInstruction, NewZList2} = case Instruction of
        {<<"jgz">>, R, X} ->
            case RegMap of
                #{R := Y} when Y > 0 ->
                    NewZList = jump(binary_to_integer(X), ZList),
                    {current(NewZList), NewZList};
                M when is_map(M) ->
                    {Instruction, ZList};
                _ ->
                    case binary_to_integer(R) > 0 of
                        true ->  
                            NewZList = jump(binary_to_integer(X), ZList),
                            {current(NewZList), NewZList};
                        false ->
                            {Instruction, ZList}
                    end
            end;
        _ ->
            {Instruction, ZList}
    end,
    try execute_instruction(NewInstruction, RegMap, Sounds, NewZList2) of
        {NewRegMap, NewSounds} -> duet(next(ZList), NewRegMap, NewSounds)
    catch
        Throw -> {throw, Throw} 
    end.



execute_instruction(Instruction, RegMap, Sounds, ZList) ->
    case Instruction of
        {<<"set">>, R, X} ->
            case RegMap of
                #{X := Y} -> 
                    {RegMap#{R := Y}, Sounds};
                _ -> 
                    {RegMap#{R := binary_to_integer(X)}, Sounds}
            end;

        {<<"add">>, R, X} ->
            case RegMap of
                #{X := Y} -> 
                    {RegMap#{R := maps:get(R, RegMap) + Y}, Sounds};
                _ -> 
                    {RegMap#{R := maps:get(R, RegMap) + binary_to_integer(X)}, Sounds}
            end;

        {<<"mul">>, R, X} ->
            case RegMap of
                #{X := Y} -> 
                    {RegMap#{R := maps:get(R, RegMap) * Y}, Sounds};
                _ -> 
                    {RegMap#{R := maps:get(R, RegMap) * binary_to_integer(X)}, Sounds}
            end;

        {<<"mod">>, R, X} ->
            case RegMap of
                #{X := Y} -> 
                    {RegMap#{R := maps:get(R, RegMap) rem Y}, Sounds};
                _ -> 
                    {RegMap#{R := maps:get(R, RegMap) rem binary_to_integer(X)}, Sounds}
            end;

        {<<"snd">>, R, <<>>} ->
            {RegMap, [maps:get(R, RegMap)|Sounds]};

        {<<"rcv">>, R} ->
            case maps:get(R, RegMap) of
                0 -> {RegMap, Sounds};
                _ ->
                    throw(hd(Sounds))
            end;
        _ -> {RegMap, Sounds}
    end.

jump(N, ZList) when N > 0 ->
    lists:foldl(fun(_, Acc) -> next(Acc) end, ZList, lists:seq(1,N));

jump(N, ZList) when N < 0 ->
    lists:foldl(fun(_, Acc) -> prev(Acc) end, ZList, lists:seq(1,abs(N))).


input() ->
    {ok, Data} = file:read_file("day18_data.txt"),
    Lines = binary:split(Data, [<<"\n">>], [global, trim_all]),
    [{Op, R, trim_binary(Rest)}|| <<Op:3/binary, " ", R:1/binary, Rest/binary>> <- Lines].

trim_binary(<<" ", Rest/binary>>) -> Rest;
trim_binary(Bin) -> Bin.

list_to_zlist(L) when is_list(L) -> {[], L}.
zlist_to_list({Pre, Post}) -> lists:reverse(Pre) ++ Post.
prev({[H|T], Post}) -> {T, [H|Post]}.
next({Pre, [H|T]}) -> {[H|Pre], T}.
current({_, [Current|_]}) -> Current.
