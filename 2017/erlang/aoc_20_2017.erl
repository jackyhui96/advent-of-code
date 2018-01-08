-module(aoc_20_2017).
-compile(export_all).

main() ->
    Input = input(),
    {_, Part1} = find_closest_to_origin(Input),
    %% No more collisions after 39 iterations
    Part2 = length(simulate_particles(Input, 39)),
    {Part1, Part2}.


simulate_particles(ParticleList, 0) -> ParticleList;
simulate_particles(ParticleList, N) ->
    NewParticleList = update_particles(ParticleList),
    CollidedParticles = find_collided(NewParticleList),
    simulate_particles(NewParticleList--CollidedParticles, N-1).

update_particles(Particles) ->
    [update_particle(P) || P <- Particles].

update_particle(Particle) ->
    [{p, {Px, Py, Pz}}, {v, {Vx, Vy, Vz}}, {a, {Ax, Ay, Az}}] = Particle,
    NewVx = Vx + Ax,
    NewVy = Vy + Ay,
    NewVz = Vz + Az,

    NewPx = Px + NewVx,
    NewPy = Py + NewVy,
    NewPz = Pz + NewVz,
    [{p, {NewPx, NewPy, NewPz}}, {v, {NewVx, NewVy, NewVz}}, {a, {Ax, Ay, Az}}].

find_collided(ParticleList) ->
    find_collided(ParticleList, #{}, []).

find_collided([], _, Acc) -> Acc;
find_collided([Particle|Rest], Map, Acc) ->
    [{p, Pos}, _, _] = Particle,
    {NewMap, NewAcc} = case Map of
        #{Pos := OtherParticle} ->
            {Map, [Particle,OtherParticle|Acc]};
        _ ->
            {Map#{Pos => Particle}, Acc}
    end,
    find_collided(Rest, NewMap, NewAcc).

absolute_acceleration(Particle) ->
    [_, _, {a, {X, Y, Z}}] = Particle,
    abs(X) + abs(Y) + abs(Z).

find_closest_to_origin(Particles) ->
    F = fun(Particle) -> absolute_acceleration(Particle) end,
    %% Get the the absolute acceleration of each particle and their index
    Result = lists:zip([F(P) || P <- Particles], lists:seq(0, length(Particles)-1)),
    lists:min(Result).

create_particle_list(Input) ->
    create_particle_list(Input, []).
    
create_particle_list([], Acc) -> lists:reverse(Acc);
create_particle_list([H|T], Acc) ->
    Particle = [ begin 
        <<_, Rest/binary>> = P,
        [X, Y, Z] = binary:split(Rest, [<<",">>, <<"=<">>, <<">">>], [global, trim_all]),
        {binary_to_integer(X), binary_to_integer(Y), binary_to_integer(Z)}
      end || P <- H],
    Result = lists:zip([p, v, a], Particle),
    create_particle_list(T, [Result|Acc]).

input() ->
    {ok, Data} = file:read_file("../inputs/day20_data.txt"),
    Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
    SplitLines = [binary:split(L, <<", ">>, [global, trim_all]) || L <- Lines],
    create_particle_list(SplitLines).