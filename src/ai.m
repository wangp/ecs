%-----------------------------------------------------------------------------%

:- module ai.
:- interface.

:- import_module ecs.
:- import_module game_event.
:- import_module prim.
:- import_module world.

:- pred run_ais(game_time::in, world::in, ecs::in, ecs::out,
    game_events::in, game_events::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module list.
:- import_module math.
:- import_module map.

:- import_module ecs.ai.
:- import_module ecs.loc.
:- import_module ecs.control.
:- import_module vec2.

run_ais(Time, World, !Ecs, !Events) :-
    get_all(!.Ecs, Enemies),
    map.foldl2(update(Time, World), Enemies, !Ecs, !Events).

:- pred update(game_time::in, world::in, entity::in, ai::in,
    ecs::in, ecs::out, game_events::in, game_events::out) is det.

update(Time, World, Entity, AI0, !Ecs, !Events) :-
    AI0 = ufo(FireTime0),
    (
        get_pos(!.Ecs, Entity, Pos),
        in_bounds(World ^ bounds, Pos)
    ->
        ( Time >= FireTime0 ->
            fire(Time, Entity, !Ecs, !Events),
            NextFireTime = FireTime0 + 1.0,
            set(Entity, ufo(NextFireTime), !Ecs)
        ;
            true
        )
    ;
        push(Time, kill_enemy(Entity), !Events)
    ).

:- pred fire(game_time::in, entity::in, ecs::in, ecs::out,
    game_events::in, game_events::out) is det.

fire(Time, Enemy, !Ecs, !Events) :-
    (
        get_ships(!.Ecs, [Ship | _]),
        get(!.Ecs, Ship, ShipLoc),
        get_pos(!.Ecs, Enemy, EnemyPos)
    ->
        O = EnemyPos,
        W = 0.3, % bullet speed
        P = ShipLoc ^ pos,
        V = ShipLoc ^ vel,
        ( lead_target(O, W, P, V, T) ->
            Dir = P + T*V - O,
            Vel = W * norm(Dir),
            Angle = atan2(Vel),
            push(Time, spawn_enemy_bullet(O, Vel, Angle), !Events)
        ;
            true
        )
    ;
        true
    ).

:- pred lead_target(vec2::in, float::in, vec2::in, vec2::in, float::out)
    is semidet.

lead_target(O, W, P, V, T) :-
    A = dot(V, V) - W*W,
    B = dot(P-O, V) * 2.0,
    C = dot(P-O, P-O),
    ( A = 0.0 ->
        fail
    ;
        Roots = math.solve_quadratic(A, B, C),
        (
            Roots = one_root(T1),
            T = abs(T1)
        ;
            Roots = two_roots(T1, T2),
            T = min(abs(T1), abs(T2))
        )
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
