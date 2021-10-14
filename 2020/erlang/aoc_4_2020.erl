-module(aoc_4_2020).

-export([
    start/0,
    part1/1,
    part2/1]).

start() ->
    Input = input(),
    part1(Input),
    part2(Input).

part1(Input) ->
    Result = length([I || I <- Input, is_valid_passport_v1(I)]),
    io:format("~p~n", [Result]),
    Result.

part2(Input) ->
    Result = length([I || I <- Input, is_valid_passport_v1(I) andalso is_valid_passport_v2(I)]),
    io:format("~p~n", [Result]),
    Result.

is_valid_passport_v1(Passport) ->
    case Passport of
        #{
            <<"byr">> := _,
            <<"iyr">> := _,
            <<"eyr">> := _,
            <<"hgt">> := _,
            <<"hcl">> := _,
            <<"ecl">> := _,
            <<"pid">> := _
        } -> true;
        _ -> false
    end.

is_valid_passport_v2(Passport) ->
    maps:fold(fun is_key_value_valid_fold_fun/3, true, Passport).

is_key_value_valid_fold_fun(Key, Value, PreviousCheck) ->
    CurrentCheck =
        try
            case Key of
                <<"byr">> -> 
                    BirthYear = erlang:binary_to_integer(Value),
                    check_number_in_range(BirthYear, 1920, 2002);
                <<"iyr">> -> 
                    IssueYear = erlang:binary_to_integer(Value),
                    check_number_in_range(IssueYear, 2010, 2020);
                <<"eyr">> -> 
                    ExpiryYear = erlang:binary_to_integer(Value),
                    check_number_in_range(ExpiryYear, 2020, 2030);
                <<"hgt">> ->
                    NumberSize = erlang:size(Value) - 2,
                    <<BinValue:NumberSize/binary, Suffix:2/binary>> = Value,
                    HeightNumber = erlang:binary_to_integer(BinValue),
                    true = (Suffix == <<"cm">> andalso check_number_in_range(HeightNumber, 150, 193))
                        orelse (Suffix == <<"in">> andalso check_number_in_range(HeightNumber, 59, 76));
                <<"hcl">> ->
                    <<"#", HairColourCode/binary>> = Value,
                    <<_:6/binary>> =
                        <<
                            <<C>> || <<C>> <= HairColourCode,
                            check_number_in_range(C, $a, $f) orelse check_number_in_range(C, $0, $9)
                        >>,
                    true;
                <<"ecl">> ->
                    EyeColours = [<<"amb">>, <<"blu">>, <<"brn">>, <<"gry">>, <<"grn">>, <<"hzl">>, <<"oth">>],
                    lists:member(Value, EyeColours);
                <<"pid">> ->
                        <<_:9/binary>> =
                            <<
                                <<C>> || <<C>> <= Value,
                                check_number_in_range(C, $0, $9)
                            >>,
                        true;
                <<"cid">> ->
                    true
            end
        catch _:_ ->
            false
        end,
    PreviousCheck andalso CurrentCheck.

check_number_in_range(Number, Min, Max) ->
    Number >= Min andalso Number =< Max.

input() ->
    {ok, Data} = file:read_file("2020/inputs/day4_data.txt"),
    Lines = binary:split(Data, [<<"\n\n">>], [global, trim_all]),
    [parse_passport_from_line(L) || L <- Lines].

parse_passport_from_line(Line) ->
    Separators = [<<" ">>, <<"\n">>],
    KeyValues = binary:split(Line, Separators, [global, trim_all]),
    maps:from_list([begin [K,V] = binary:split(KV, <<":">>), {K, V} end || KV <- KeyValues]).

-include_lib("eunit/include/eunit.hrl").

example_test_() ->
    Input = [
        #{
            <<"ecl">> => <<"gry">>,
            <<"pid">> => <<"860033327">>,
            <<"eyr">> => <<"2020">>,
            <<"hcl">> => <<"#fffffd">>,
            <<"byr">> => <<"1937">>,
            <<"iyr">> => <<"2017">>,
            <<"cid">> => <<"147">>,
            <<"hgt">> => <<"183cm">>
        },
        #{
            <<"iyr">> => <<"2013">>,
            <<"ecl">> => <<"amb">>,
            <<"cid">> => <<"350">>,
            <<"eyr">> => <<"2023">>,
            <<"pid">> => <<"028048884">>,
            <<"hcl">> => <<"#cfa07d">>,
            <<"byr">> => <<"1929">>
        },
        #{
            <<"hcl">> => <<"#ae17e1">>,
            <<"iyr">> => <<"2013">>,
            <<"eyr">> => <<"2024">>,
            <<"ecl">> => <<"brn">> ,
            <<"pid">> => <<"760753108">>,
            <<"byr">> => <<"1931">>,
            <<"hgt">> => <<"179cm">>
        }
        #{
            <<"hcl">> => <<"#cfa07d">>,
            <<"eyr">> => <<"2025">>,
            <<"pid">> => <<"166559648">>,
            <<"iyr">> => <<"2011">>,
            <<"ecl">> => <<"brn">>,
            <<"hgt">> => <<"59in">>
        }
    ],
    [
        ?_assertEqual(2, part1(Input)),
        ?_assertEqual(2, part2(Input))
    ].

puzzle_test_() ->
    Input = input(),
    [
        ?_assertEqual(192, part1(Input)),
        ?_assertEqual(101, part2(Input))
    ].
