-module(aoc_25_2017).
-compile(export_all).

main() ->
    ct:pal("Start: ~n"),
    {StartState, N, MapRules} = input(),
    
    {Pre, Post} = run_turing_machine(StartState, N, MapRules, list_to_zlist([<<"0">>])),
    Part1 = length([1 || <<"1">> <- Pre]) + length([1 || <<"1">> <- Post]),
    ct:pal("Part1: ~p~n", [Part1]).

run_turing_machine(_, 0, _, Tape) -> Tape;
run_turing_machine(State, N, MapRules, Tape) ->
    {NewState, NewTape} = case read_tape(Tape) of
        <<"0">> ->
            #{State := {{_, NextV, Step, NextState}, _}} = MapRules,
            {NextState, write_to_tape(NextV, Tape)};
        <<"1">> ->
            #{State := {_, {_, NextV, Step, NextState}}} = MapRules,
            {NextState, write_to_tape(NextV, Tape)}
    end,
    run_turing_machine(NewState, N-1, MapRules, move_tape(Step, NewTape)).

%% ----------------------------------------
%% Functions for Turing Tape
%% ----------------------------------------
tape_left({[], Post}) -> {[], [<<"0">>|Post]};
tape_left(ZList) -> prev(ZList).

tape_right({Pre, []}) -> {Pre, [<<"0">>]};
tape_right(ZList) -> next(ZList).

write_to_tape(X, {Pre, []}) -> {Pre, [X]};
write_to_tape(X, {Pre, [_|Rest]}) -> {Pre, [X|Rest]}.

move_tape(<<"left">>, Tape) -> tape_left(Tape);
move_tape(<<"right">>, Tape) -> tape_right(Tape).

read_tape({_, []}) -> <<"0">>;
read_tape(ZList) -> current(ZList).

%% Functions to use a zipper list
list_to_zlist(L) when is_list(L) -> {[], L}.
zlist_to_list({Pre, Post}) -> lists:reverse(Pre) ++ Post.
prev({[H|T], Post}) -> {T, [H|Post]}.
next({Pre, [H|T]}) -> {[H|Pre], T}.
current({_, [Current|_]}) -> Current.

%% ----------------------------------------
%% Input Functions
%% ----------------------------------------

%% Decided to parse the input... in a messy binary pattern match way
input() ->
    {ok, Data} = file:read_file("day25_data.txt"),
    [Start|Groups] = binary:split(Data, <<"\n\n">>, [global, trim_all]),
    <<"Begin in state ", StartState:1/binary, ".\nPerform a diagnostic checksum after ", Rest/binary>> = Start,
    [N, _] = binary:split(Rest, <<" ">>),
    MapRules = get_state_rules(Groups, #{}),
    {StartState, binary_to_integer(N), MapRules}.

get_state_rules([], Map) -> Map; 
get_state_rules([Group|Rest], Map) -> 
    [<<"In state ", State:1/binary, ":">>|Rules] = binary:split(Group, <<"\n">>, [global, trim_all]),
    {FirstGroup, SecondGroup} = lists:split(4, Rules),
    [  
        <<"  If the current value is ", A/binary>>,
        <<"    - Write the value ", B/binary>>,
        <<"    - Move one slot to the ", C/binary>>,
        <<"    - Continue with state ", D/binary>>
    ] = FirstGroup,
    [  
        <<"  If the current value is ", W/binary>>,
        <<"    - Write the value ", X/binary>>,
        <<"    - Move one slot to the ", Y/binary>>,
        <<"    - Continue with state ", Z/binary>>
    ] = SecondGroup,
    FirstRule = {trim_binary(A), trim_binary(B), trim_binary(C), trim_binary(D)},
    SecondRule = {trim_binary(W), trim_binary(X), trim_binary(Y), trim_binary(Z)},
    get_state_rules(Rest, Map#{State => {FirstRule, SecondRule}}).

trim_binary(<<" ", Rest/binary>>) -> binary:part(Rest, {0, byte_size(Rest)-1});
trim_binary(<<Rest/binary>>) -> binary:part(Rest, {0, byte_size(Rest)-1}).
    