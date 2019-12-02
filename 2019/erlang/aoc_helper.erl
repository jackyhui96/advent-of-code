-module(aoc_helper).
-compile(export_all).


parse_input(Day, Type) ->
    {ok, Data} = file:read_file("../inputs/" ++ Day ++ "_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    case Type of
        integer ->
            [binary_to_integer(X) || X <- Lines];
        binary ->
            Lines
    end.