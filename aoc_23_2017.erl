-module(aoc_23_2017).
-compile(export_all).

main() ->
    Input = input(),

    %% Currently logs the number of multiply operations until program crashes
    %% Need to refactor code in future to avoid crashing
    run_program(Input).

%% Part2 is at the bottom of this code

execute_instruction(Instruction, RegMap, Acc) ->
    case Instruction of
        {<<"set">>, R, X} ->
            case RegMap of
                #{X := Y} -> {RegMap#{R := Y}, [set|Acc]};
                _ -> {RegMap#{R := binary_to_integer(X)}, [set|Acc]} 
            end;

        {<<"sub">>, R, X} ->
            case RegMap of
                #{X := Y} -> {RegMap#{R := maps:get(R, RegMap) - Y}, [sub|Acc]};
                _ -> {RegMap#{R := maps:get(R, RegMap) - binary_to_integer(X)}, [sub|Acc]} 
            end;

        {<<"mul">>, R, X} ->
            case RegMap of
                #{X := Y} -> {RegMap#{R := maps:get(R, RegMap) * Y}, [mul|Acc]};
                _ -> {RegMap#{R := maps:get(R, RegMap) * binary_to_integer(X)}, [mul|Acc]}
            end;
        _ -> 
            {RegMap, Acc} 
    end.

run_program(Input) ->
    {ZipList, RegisterMap} = aoc_18_2017:setup(Input),
    run_program(ZipList, RegisterMap, []).

run_program({_, []}, RegMap, Acc) -> 
    {RegMap, Acc};
run_program(ZList, RegMap, Acc) ->
    Instruction = current(ZList),
    {NewInstruction, NewZList} = handle_jumps(RegMap, ZList, Instruction),
    ct:pal("Debug: ~p~n", [length([mul || mul <- Acc])]),
    {NewRegMap, NewAcc} = execute_instruction(NewInstruction, RegMap, Acc),
    run_program(next(NewZList), NewRegMap, NewAcc).

handle_jumps(RegMap, ZList, {<<"jnz">>, R, X} = Instruction) ->
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
        #{R := Y, X := Z} when Y =/= 0 ->
            NewZList = aoc_18_2017:jump(Z, ZList),
            {current(NewZList), NewZList};
        #{R := Y} when Y =/= 0 ->
            NewZList = aoc_18_2017:jump(binary_to_integer(X), ZList),
            {current(NewZList), NewZList};
        #{R := 0} -> 
            {Instruction, ZList};
        _ ->
            case binary_to_integer(R) =/= 0 of
                true ->  
                    NewZList = aoc_18_2017:jump(binary_to_integer(X), ZList),
                    {current(NewZList), NewZList};
                false -> 
                    {Instruction, ZList}
            end
    end.

input() ->
    {ok, Data} = file:read_file("day23_data.txt"),
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


% DFreiberg (Reddit) 
% The key to understanding what this code does is starting from the end and working backwards:

%     If the program has exited, g had a value of 0 at line 29.
%     g == 0 at line 29 when b == c.
%     If g != 0 at line 29, b increments by 17.
%     b increments no other times on the program.
%     Thus, lines 25 through 31 will run 1000 times, on values of b increasing by 17, before the program finishes.

% So, given that there is no jnz statement between lines 25 and 28 that could affect things:

%     If f == 0 at line 25, h will increment by 1.
%     This can happen once and only once for any given value of b.
%     f == 0 if g == 0 at line 15.
%     g == 0 at line 15 if d*e == b.
%     Since both d and e increment by 1 each in a loop, this will check every possible value of d and e less than b.
%     Therefore, if b has any prime factors other than itself, f will be set to 1 at line 25.

% Looking at this, then h is the number of composite numbers between the lower limit and the upper limit, counting by 17.

optimise_program() ->
    LowerLimit = 107900,
    UpperLimit = 124900,
    InputNumbers = lists:seq(LowerLimit, UpperLimit, 17),
    length([ 1 || X <- InputNumbers, composite_check(X)]).

composite_check(N) ->
    composite_check(N, lists:seq(2, N-1)).

composite_check(N, []) -> false;
composite_check(N, [H|T]) ->
    case N rem H of
        0 -> true;
        _ -> composite_check(N, T)
    end.