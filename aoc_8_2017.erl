-module(aoc_8_2017).
-compile(export_all).

main() ->
    Input = input(),
    Data = [get_registry_update(I) || I <- Input],
    Instructions = [{Uvm, get_conditional(D)} || {Uvm, D} <- Data],
    Registries = lists:usort(lists:append([[Var1, Var2] || {#{var := Var1}, #{var := Var2}} <- Instructions])),
    RegistryMap = lists:foldl(fun(X, Acc) -> Acc#{X => 0} end, #{}, Registries),
    {NewRegistryMap, Max} = run_instructions(Instructions, RegistryMap, 0),
    MaxVal = maps:fold(fun(K, V, Acc) -> 
        case V > Acc of 
            true -> V; 
            false -> Acc 
        end 
    end, 0, NewRegistryMap),
    ct:pal("Part1: ~p~n", [MaxVal]),
    ct:pal("Part2: ~p~n", [Max]).

run_instructions([], RegistryMap, Max) -> {RegistryMap, Max};
run_instructions([{UvMap, CMap}|Instructions], RegistryMap, Max) ->
    #{var := Var1, op := Op1, num := Num1} = UvMap,
    #{var := Var2, op := Op2, num := Num2} = CMap,
    #{Var1 := CurrentVar1, Var2 := CurrentVar2} = RegistryMap,
    
    {UpdatedRegistryMap, NewMax} = case operation(Op2, CurrentVar2, Num2) of
        true ->
            NewValue = operation(Op1, CurrentVar1, Num1),
            case NewValue > Max of
                true ->
                    {RegistryMap#{Var1 := NewValue}, NewValue};
                false ->
                    {RegistryMap#{Var1 := NewValue}, Max}
            end;
        false ->
            {RegistryMap, Max}
    end,
    run_instructions(Instructions, UpdatedRegistryMap, NewMax).

operation(<<"inc">>, Var, Num) ->
    Var + Num;
operation(<<"dec">>, Var, Num) ->
    Var - Num;
operation(<<"<">>, Var, Num) ->
    Var < Num;
operation(<<">">>, Var, Num) ->
    Var > Num;
operation(<<"==">>, Var, Num) ->
    Var == Num;
operation(<<"!=">>, Var, Num) ->
    Var =/= Num;
operation(<<">=">>, Var, Num) ->
    Var >= Num;
operation(<<"<=">>, Var, Num) ->
    Var =< Num.   

input() ->
    {ok, Data} = file:read_file("day8_data.txt"),
    binary:split(Data, <<"\r\n">>, [global, trim_all]).

get_registry_update(Line) ->
    [Var, Line2] = binary:split(Line, <<" ">>),
    [Op, Line3] = binary:split(Line2, <<" ">>),
    [Num, Line4] = binary:split(Line3, <<" ">>),
    UpdateVarMap = #{var => Var, op => Op, num => binary_to_integer(Num)},
    {UpdateVarMap, Line4}.

get_conditional(<<"if ", Line/binary>>) ->
    [Var, Line2] = binary:split(Line, <<" ">>),
    [Op, Num] = binary:split(Line2, <<" ">>),
    ConditionalMap = #{var => Var, op => Op, num => binary_to_integer(Num)},
    ConditionalMap.