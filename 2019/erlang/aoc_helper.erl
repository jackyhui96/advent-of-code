-module(aoc_helper).
-export([parse_input/2]).


parse_input(Day, Type) ->
    parse_input(Day, Type, <<"\n">>).
    
parse_input(Day, Type, SplitBy) ->
    {ok, Data} = file:read_file("../inputs/" ++ Day ++ "_data.txt"),
    SplitResult = binary:split(Data, SplitBy, [global, trim_all]),
    case Type of
        integer ->
            [binary_to_integer(X) || X <- SplitResult];
        binary ->
            SplitResult
    end.