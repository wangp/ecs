%-----------------------------------------------------------------------------%

:- module control.
:- interface.

:- import_module ecs.
:- import_module game_event.
:- import_module prim.

:- pred run_controls(game_time::in, ecs::in, ecs::out,
    game_events::in, game_events::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module map.
:- import_module math.

:- import_module ecs.loc.
:- import_module ecs.prop.
:- import_module ecs.control.
:- import_module game_event.
:- import_module vec2.

run_controls(Time, !Ecs, !Events) :-
    get_all(!.Ecs, Controls),
    map.foldl2(run_control(Time), Controls, !Ecs, !Events).

:- pred run_control(game_time::in, entity::in, control::in,
    ecs::in, ecs::out, game_events::in, game_events::out) is det.

run_control(Time, Entity, Control0, !Ecs, !Events) :-
    Control0 = ship_control(Thrust, Left, Right, Fire, FireTime0),
    (
        Thrust = yes,
        thrust(Entity, 0.01, !Ecs)
    ;
        Thrust = no
    ),
    TurnSpeed = math.pi / 30.0,
    (
        Left = yes,
        turn(Entity, TurnSpeed, !Ecs)
    ;
        Left = no
    ),
    (
        Right = yes,
        turn(Entity, -TurnSpeed, !Ecs)
    ;
        Right = no
    ),
    (
        Fire = yes,
        ( Time >= FireTime0 ->
            fire(Time, Entity, !Ecs, !Events),
            Control = Control0 ^ fire_time := Time + 0.25,
            set(Entity, Control, !Ecs)
        ;
            true
        )
    ;
        Fire = no
    ).

:- pred thrust(entity::in, float::in, ecs::in, ecs::out) is det.

thrust(Entity, Amount, !Ecs) :-
    ( get(!.Ecs, Entity, Loc0) ->
        Vel1 = Loc0 ^ vel + Amount * cos_sin(Loc0 ^ angle),
        Loc = Loc0 ^ vel := cap(0.5, Vel1),
        set(Entity, Loc, !Ecs)
    ;
        true
    ).

:- pred turn(entity::in, float::in, ecs::in, ecs::out) is det.

turn(Entity, Delta, !Ecs) :-
    ( get(!.Ecs, Entity, Loc0) ->
        Angle0 = Loc0 ^ angle,
        Angle = Angle0 + Delta,
        Loc = Loc0 ^ angle := Angle,
        set(Entity, Loc, !Ecs)
    ;
        true
    ).

:- pred fire(game_time::in, entity::in, ecs::in, ecs::out,
    game_events::in, game_events::out) is det.

fire(Time, Ship, !Ecs, !Events) :-
    (
        get(!.Ecs, Ship, ShipLoc),
        get_player(!.Ecs, Ship, Player)
    ->
        ShipPos = ShipLoc ^ pos,
        ShipVel = ShipLoc ^ vel,
        Angle = ShipLoc ^ angle,
        push(Time, spawn_bullet(ShipPos, ShipVel, Angle, Player), !Events)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
