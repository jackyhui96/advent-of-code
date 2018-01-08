-module(aoc_13_2017).
-compile(export_all).

main() ->
    Input = input(),
    Part1 = get_severity_of_packet_journey(Input),
    Part2 = get_delay_of_undetected_packet_journey(Input),
    {Part1, Part2}.

get_severity_of_packet_journey(Layers) ->
    get_severity_of_packet_journey(Layers, []).

get_severity_of_packet_journey([], []) -> 0;
get_severity_of_packet_journey([], Scores) -> lists:sum(Scores);
get_severity_of_packet_journey(Layers, Scores) ->
    {NewLayers, NewScores} = run_packet_through_firewall(Layers, Scores, 0),
    get_severity_of_packet_journey(NewLayers, NewScores).


get_delay_of_undetected_packet_journey(Layers) ->
    get_delay_of_undetected_packet_journey(Layers, 0).

get_delay_of_undetected_packet_journey(Layers, Delay) ->
    case is_packet_detected_in_firewall(Layers, Delay) of
        caught -> 
            get_delay_of_undetected_packet_journey(Layers, Delay+1);
        passed -> Delay
    end.


is_packet_detected_in_firewall(Layers, Delay) ->
    is_packet_detected_in_firewall(Layers, [], Delay).

%% Packet is caught if severity scores list is non-empty
is_packet_detected_in_firewall(_, [_], _) -> caught;
%% Packet passes as there is no more layers in the firewall list
is_packet_detected_in_firewall([], _, _) -> passed;
is_packet_detected_in_firewall(Layers, Scores, Delay) ->
    {NewLayers, NewScores} = run_packet_through_firewall(Layers, Scores, Delay),
    is_packet_detected_in_firewall(NewLayers, NewScores, Delay).

%% Packet is only caught if the scanner is in the starting position of that layer
%% Offset is used to shift the layer so that we can delay the packet
run_packet_through_firewall([{Layer, Depth}|Rest], Scores, Offset) ->
    NewScores = case (Layer + Offset) rem ((Depth-1)*2) of
        0 -> [Layer * Depth|Scores];
        _ -> Scores
    end,
    {Rest, NewScores}.


input() ->
    {ok, Data} = file:read_file("../inputs/day13_data.txt"),
    Lines = binary:split(Data, [<<"\n">>, <<": ">>], [global, trim_all]),
    F = fun F([], Acc) -> 
            lists:reverse(Acc);
        F([X,Y|T], Acc) ->
            F(T, [{binary_to_integer(X), binary_to_integer(Y)}|Acc])
    end,
    F(Lines, []).
    