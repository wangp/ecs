%-----------------------------------------------------------------------------%

:- module main.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module int.

:- import_module ai.
:- import_module collision.
:- import_module control.
:- import_module ecs.
:- import_module ecs.timer.
:- import_module enemy_spawner.
:- import_module game_event.
:- import_module input_event.
:- import_module input_poll.
:- import_module new_entity.
:- import_module physics.
:- import_module prim.
:- import_module rand.
:- import_module render.
:- import_module sdl.
:- import_module star.
:- import_module timer.
:- import_module vec2.
:- import_module world.

main(!IO) :-
    sdl.init(!IO),
    Xres = 800,
    Yres = 800,
    sdl.create_window_and_renderer(Xres, Yres, Window, Renderer, !IO),
    Bounds = bounds(-30.0, 30.0, -30.0, 30.0),
    Screen = screen(Window, Renderer, make_transform(Xres, Yres, Bounds)),

    Time = 0.0,
    init(4, RS0),
    setup(Bounds, Time, World0, Ecs0, Events0, RS0, RS1),
    loop(Screen, Time, World0, _World, Ecs0, _Ecs, Events0, _Events, RS1, _RS,
        !IO),
    sdl.quit(!IO).

:- pred setup(bounds::in, game_time::in, world::out, ecs::out,
    game_events::out, rs::in, rs::out) is det.

setup(Bounds, Time, !:World, !:Ecs, !:Events, !RS) :-
    !:World = world(playing, Bounds, no, WinLose, Player, 0),
    init(!:Ecs),
    init(!:Events),
    new_entity(WinLose, !Ecs),
    new_player(Player, !Ecs),
    set_timeout(Player ^ player_entity, Time + 0.25,
        spawn_ship(Player, 0.0, no), !Ecs),
    new_entity(EnemySpawner, !Ecs),
    rand_enemy_spawn_time(Time, SpawnTime, !RS),
    set_timeout(EnemySpawner, SpawnTime,
        spawn_enemy(EnemySpawner), !Ecs),
    make_starfield(Bounds, 100, !Ecs, !RS),
    seed_asteroid(Bounds, Time, !Events, !RS),
    seed_asteroid(Bounds, Time, !Events, !RS),
    seed_asteroid(Bounds, Time, !Events, !RS),
    run_events(!World, !Ecs, !Events, !RS).

:- pred seed_asteroid(bounds::in, game_time::in,
    game_events::in, game_events::out, rs::in, rs::out) is det.

seed_asteroid(Bounds, Time, !Events, !RS) :-
    Bounds = bounds(MinX, MaxX, MinY, MaxY),
    Margin = 5.0,
    rand_range(MinX + Margin, MaxX - Margin, X, !RS),
    rand_range(MinY + Margin, MaxY - Margin, Y, !RS),
    % Avoid the origin.
    (
        -Margin =< X, X =< Margin,
        -Margin =< Y, Y =< Margin
    ->
        seed_asteroid(Bounds, Time, !Events, !RS)
    ;
        Life = 3,
        push(Time, spawn_asteroid(vec2(X, Y), Life), !Events)
    ).

:- pred loop(screen::in, game_time::in, world::in, world::out,
    ecs::in, ecs::out, game_events::in, game_events::out, rs::in, rs::out,
    io::di, io::uo) is det.

loop(Screen, Time, !World, !Ecs, !Events, !RS, !IO) :-
    step(Screen, Time, !World, !Ecs, !Events, !RS, !IO),

    % XXX hacky timing
    Delay = 1000/60,
    sdl.delay(Delay, !IO),
    DeltaTime = float(Delay)/1000.0,
    Time1 = Time + DeltaTime,

    ( !.World ^ game_state = quit ->
        true
    ; is_paused(!.World) ->
        loop(Screen, Time, !World, !Ecs, !Events, !RS, !IO)
    ;
        loop(Screen, Time1, !World, !Ecs, !Events, !RS, !IO)
    ).

:- pred step(screen::in, game_time::in, world::in, world::out,
    ecs::in, ecs::out, game_events::in, game_events::out, rs::in, rs::out,
    io::di, io::uo) is det.

step(Screen, Time, !World, !Ecs, !Events, !RS, !IO) :-
    get_input_events(InputEvents, !IO),
    apply_input_events(InputEvents, !World, !Ecs),
    ( is_paused(!.World) ->
        true
    ;
        run_controls(Time, !Ecs, !Events),
        run_ais(Time, !.World, !Ecs, !Events)
    ),
    run_timers(Time, !Ecs, !Events),
    run_collisions(Time, !.Ecs, !Events),
    run_events(!World, !Ecs, !Events, !RS),
    ( is_paused(!.World) ->
        true
    ;
        run_physics(!.World, !Ecs)
    ),
    sweep_dead(!Ecs),
    render(Screen, Time, !.World, !.Ecs, !IO).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
