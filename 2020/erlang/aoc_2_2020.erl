-module(aoc_2_2020).

-export([
    start/0,
    part1/1,
    part2/1]).

start() ->
    Input = input(),
    part1(Input),
    part2(Input).

part1(Input) ->
    Result = length(lists:filter(fun check_password_with_policy_method_1/1, Input)),
    io:format("~p~n", [Result]),
    Result.

part2(Input) ->
    Result = length(lists:filter(fun check_password_with_policy_method_2/1, Input)),
    io:format("~p~n", [Result]),
    Result.

check_password_with_policy_method_1({_PasswordPolicy = {Min, Max, Char}, Password}) ->
    Matches = [C || <<C>> <= Password, <<C>> == Char],
    NumMatches = length(Matches),
    NumMatches >= Min andalso NumMatches =< Max.

check_password_with_policy_method_2({_PasswordPolicy = {Pos1, Pos2, Char}, Password}) ->
    ASize = Pos1 - 1,
    BSize = Pos2 - Pos1 - 1,
    <<_:ASize/binary, CharA:1/binary, _:BSize/binary, CharB:1/binary, _/binary>> = Password,
    case {CharA, CharB} of
        {CharA = Char, CharB = Char} -> false;
        {CharA = Char, _}            -> true;
        {_, CharB = Char}            -> true;
        _                            -> false
    end.

input() ->
    {ok, Data} = file:read_file("2020/inputs/day2_data.txt"),
    Separators = [<<"-">>, <<" ">>, <<":">>],
    Lines = binary:split(Data, [<<"\n">>], [global, trim_all]),
    [begin
        [Min, Max, Char, Password] = binary:split(L, Separators, [global, trim_all]),
        {
            {
                erlang:binary_to_integer(Min),
                erlang:binary_to_integer(Max),
                Char
            },
            Password
        }
     end
    || L <- Lines].


-include_lib("eunit/include/eunit.hrl").

example_test_() ->
    Input = [
        {{1, 3, <<"a">>}, <<"abcde">>},
        {{1, 3, <<"b">>}, <<"cdefg">>},
        {{2, 9, <<"c">>}, <<"ccccccccc">>}
    ],
    [
        ?_assertEqual(2, part1(Input)),
        ?_assertEqual(1, part2(Input))
    ].

puzzle_test_() ->
    Input = input(),
    [
        ?_assertEqual(536, part1(Input)),
        ?_assertEqual(558, part2(Input))
    ].
