-module(aoc_18_2017).
-compile(export_all).

main() ->
    Input = input(),

    {throw, [Part1|_]} = run_program(Input),
    Part2 = duet(Input),
    {Part1, Part2}.


duet(Input) ->
    {ZipList, RegisterMap} = setup(Input),
    Self = self(),
    PidA = spawn(fun() -> run_duet_program(ZipList, RegisterMap#{<<"p">> := 1}, [], Self, undefined) end),
    PidB = spawn(fun() -> run_duet_program(ZipList, RegisterMap#{<<"p">> := 0}, [], Self, undefined) end),
    PidA ! {duet, PidB},
    PidB ! {duet, PidA},
    receive
        {_, PidA, TimesSent} ->
            TimesSent
    end.

setup(Input) ->
    ZipList = list_to_zlist(Input),
    RegisterMap = maps:from_list([{<<C>>,0}|| {_,<<C>>,_} <- Input, C >= $a, C =< $z]),
    {ZipList, RegisterMap}.

run_program(Input) ->
    {ZipList, RegisterMap} = setup(Input),
    run_program(ZipList, RegisterMap, []).

run_program({_, []}, RegMap, Sounds) -> 
    {Sounds, RegMap};
run_program(ZList, RegMap, Sounds) ->
    Instruction = current(ZList),
    {NewInstruction, NewZList} = handle_jumps(RegMap, ZList, Instruction),
    try execute_instruction(NewInstruction, RegMap, Sounds) of
        {NewRegMap, NewSounds} -> run_program(next(NewZList), NewRegMap, NewSounds)
    catch
        Throw -> {throw, Throw} 
    end.

%% Get the Pid of the program we want to talk to from the Parent
run_duet_program(ZList, RegMap, Sounds, Parent, undefined) -> 
    receive {duet, Pid} ->
        run_duet_program(ZList, RegMap, Sounds, Parent, Pid)
    end;
run_duet_program(ZList, RegMap, Sounds, Parent, Pid) ->
    Instruction = current(ZList),
    {NewInstruction, NewZList} = handle_jumps(RegMap, ZList, Instruction),
    try execute_instruction_duet(NewInstruction, RegMap, Sounds, Pid) of
        {NewRegMap, NewSounds} -> run_duet_program(next(NewZList), NewRegMap, NewSounds, Parent, Pid)
    catch
        Throw -> Parent ! Throw
    end.

%% If instruction is jump, then jump by the value of R
%% Can handle multiple jumps, i.e. jumping to another jump instruction
handle_jumps(RegMap, ZList, {<<"jgz">>, R, X} = Instruction) ->
    {NewInstruction, NewZList} = handle_jump(RegMap, ZList, R, X, Instruction),
    case NewZList of
        ZList ->
            {NewInstruction, NewZList};
        _ ->
            handle_jumps(RegMap, NewZList, NewInstruction)
    end;
handle_jumps(_, ZList, Instruction) ->
    {Instruction, ZList}.

handle_jump(RegMap, ZList, R, X, Instruction) ->
    case RegMap of
        #{R := Y, X := Z} when Y > 0 ->
            NewZList = jump(Z, ZList),
            {current(NewZList), NewZList};
        #{R := Y} when Y > 0 ->
            NewZList = jump(binary_to_integer(X), ZList),
            {current(NewZList), NewZList};
        #{R := Y} when Y =< 0 -> 
            {Instruction, ZList};
        _ ->
            case binary_to_integer(R) > 0 of
                true ->  
                    NewZList = jump(binary_to_integer(X), ZList),
                    {current(NewZList), NewZList};
                false -> 
                    {Instruction, ZList}
            end
    end.

%% Jump N times in a zipper list
jump(N, ZList) when N > 0 ->
    lists:foldl(fun(_, Acc) -> next(Acc) end, ZList, lists:seq(1,N));
jump(N, ZList) when N < 0 ->
    lists:foldl(fun(_, Acc) -> prev(Acc) end, ZList, lists:seq(1,abs(N))).

execute_instruction(Instruction, RegMap, Sounds) ->
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

        {<<"rcv">>, R, <<>>} ->
            case maps:get(R, RegMap) of
                0 -> {RegMap, Sounds};
                _ ->
                    throw(Sounds)
            end;
        _ -> {RegMap, Sounds}
    end.

%% Different rules for snd and rcv when running as a duet
execute_instruction_duet(Instruction, RegMap, Sounds, Pid) ->
    case Instruction of
        {<<"snd">>, R, <<>>} ->
            Val = maps:get(R, RegMap),
            Pid ! integer_to_binary(Val),
            {RegMap, [Val|Sounds]};

        {<<"rcv">>, R, <<>>} ->
            Result = receive
                X ->
                    execute_instruction({<<"set">>, R, X}, RegMap, Sounds)
            after 10 ->
                throw({deadlock, self(), length(Sounds)})
            end,
            Result;
        _ -> 
            execute_instruction(Instruction, RegMap, Sounds)
    end.


input() ->
    {ok, Data} = file:read_file("../inputs/day18_data.txt"),
    Lines = binary:split(Data, [<<"\n">>], [global, trim_all]),
    [{Op, R, trim_binary(Rest)}|| <<Op:3/binary, " ", R:1/binary, Rest/binary>> <- Lines].

trim_binary(<<" ", Rest/binary>>) -> Rest;
trim_binary(Bin) -> Bin.

%% Functions to use a zipper list
list_to_zlist(L) when is_list(L) -> {[], L}.
zlist_to_list({Pre, Post}) -> lists:reverse(Pre) ++ Post.
prev({[H|T], Post}) -> {T, [H|Post]}.
next({Pre, [H|T]}) -> {[H|Pre], T}.
current({_, [Current|_]}) -> Current.
