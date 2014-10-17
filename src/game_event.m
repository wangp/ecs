%-----------------------------------------------------------------------------%

:- module game_event.
:- interface.

:- import_module bool.
:- import_module ecs.
:- import_module prim.
:- import_module rand.
:- import_module vec2.
:- import_module world.

:- type game_events.

:- type game_event
    --->    spawn_ship(player, float, bool)
    ;       kill_ship(entity)
    ;       make_vulnerable(entity)
    ;       spawn_bullet(vec2, vec2, float, player)
    ;       kill_bullet(entity)
    ;       spawn_asteroid(vec2, life)
    ;       kill_asteroid(entity)
    ;       spawn_enemy(entity)
    ;       kill_enemy(entity)
    ;       spawn_enemy_bullet(vec2, vec2, float)
    ;       kill_enemy_bullet(entity)
    ;       spawn_explosion(vec2)
    ;       kill_explosion(entity)
    ;       hit(hit_type, entity, entity)
    ;       check_win_lose.

:- type hit_type
    --->    bullet_hits_asteroid
    ;       bullet_hits_enemy
    ;       asteroid_hits_ship
    ;       enemy_hits_ship
    ;       enemy_bullet_hits_ship.

:- pred init(game_events::out) is det.

:- pred push(game_time::in, game_event::in, game_events::in, game_events::out)
    is det.

:- pred run_events(world::in, world::out, ecs::in, ecs::out,
    game_events::in, game_events::out, rs::in, rs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module cord.
:- import_module float.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.

:- import_module ecs.ai.
:- import_module ecs.collider.
:- import_module ecs.loc.
:- import_module ecs.prop.
:- import_module ecs.timer.
:- import_module ecs.vis.
:- import_module new_entity.
:- import_module enemy_spawner.

:- type game_events == cord(pair(game_time, game_event)).

%-----------------------------------------------------------------------------%

init(init).

push(Time, Event, Events0, Events) :-
    Events = snoc(Events0, Time - Event).

run_events(!World, !Ecs, !Events, !RS) :-
    ( head_tail(!.Events, Time - Event, !:Events) ->
        run_event(Time, Event, !World, !Ecs, !Events, !RS),
        run_events(!World, !Ecs, !Events, !RS)
    ;
        true
    ).

%-----------------------------------------------------------------------------%

:- pred run_event(game_time::in, game_event::in, world::in, world::out,
    ecs::in, ecs::out, game_events::in, game_events::out, rs::in, rs::out)
    is det.

run_event(Time, Event, !World, !Ecs, !Events, !RS) :-
    (
        Event = spawn_ship(Player, Angle, Invuln),
        new_ship(Time, Player, Angle, Invuln, !Ecs)
    ;
        Event = kill_ship(Ship),
        mark_dead(Ship, !Ecs),
        (
            get(!.Ecs, Ship, Loc),
            get_player(!.Ecs, Ship, Player),
            PlayerEntity = Player ^ player_entity,
            get_life(!.Ecs, PlayerEntity, Life0)
        ->
            ShipPos = Loc ^ pos,
            push(Time, spawn_explosion(ShipPos), !Events),

            Life = Life0 - 1,
            set_life(PlayerEntity, Life, !Ecs),
            ( Life = 0 ->
                potential_win_lose(Time, !.World, !Ecs)
            ;
                Angle = Loc ^ angle,
                set_timeout(PlayerEntity, Time + 0.25,
                    spawn_ship(Player, Angle, yes), !Ecs)
            )
        ;
            true
        )
    ;
        Event = make_vulnerable(Ship),
        set_blink(Ship, fg, no, !Ecs),
        set_collider_enabled(Ship, enabled, !Ecs)
    ;
        Event = spawn_bullet(Pos, Vel, Angle, Player),
        new_bullet(Time, Pos, Vel, Angle, Player, !Ecs, !RS)
    ;
        Event = kill_bullet(Entity),
        mark_dead(Entity, !Ecs)
    ;
        Event = spawn_asteroid(Pos, Life),
        new_asteroid(Pos, Life, !Ecs, !RS),
        change_num_asteroids(+1, !World)
    ;
        Event = kill_asteroid(Asteroid),
        change_num_asteroids(-1, !World),
        mark_dead(Asteroid, !Ecs),
        (
            get_life(!.Ecs, Asteroid, Life0),
            get_pos(!.Ecs, Asteroid, Pos0)
        ->
            push(Time, spawn_explosion(Pos0), !Events),
            Life = Life0 - 1,
            ( Life = 0 ->
                potential_win_lose(Time, !.World, !Ecs)
            ;
                push(Time, spawn_asteroid(Pos0, Life), !Events),
                push(Time, spawn_asteroid(Pos0, Life), !Events),
                push(Time, spawn_asteroid(Pos0, Life), !Events)
            )
        ;
            true
        )
    ;
        Event = spawn_enemy(Owner),
        ( is_game_over(!.World) ->
            true
        ;
            !.World ^ bounds = bounds(_MinX, MaxX, _MinY, MaxY),
            new_ufo(Time, vec2(MaxX, MaxY * 0.6), Owner, !Ecs)
        )
    ;
        Event = kill_enemy(Entity),
        mark_dead(Entity, !Ecs),
        potential_win_lose(Time, !.World, !Ecs),
        ( get_owner(!.Ecs, Entity, Owner) ->
            rand_enemy_spawn_time(Time, SpawnTime, !RS),
            set_timeout(Owner, SpawnTime, spawn_enemy(Owner), !Ecs)
        ;
            true
        )
    ;
        Event = spawn_enemy_bullet(Pos, Vel, Angle),
        new_enemy_bullet(Time, Pos, Vel, Angle, !Ecs)
    ;
        Event = kill_enemy_bullet(Entity),
        mark_dead(Entity, !Ecs)
    ;
        Event = spawn_explosion(Pos),
        new_explosion(Time, Pos, !Ecs)
    ;
        Event = kill_explosion(Entity),
        mark_dead(Entity, !Ecs)
    ;
        Event = hit(bullet_hits_asteroid, Bullet, Asteroid),
        player_scores(Bullet, 1, !Ecs),
        push(Time, kill_bullet(Bullet), !Events),
        push(Time, kill_asteroid(Asteroid), !Events)
    ;
        Event = hit(bullet_hits_enemy, Bullet, Enemy),
        player_scores(Bullet, 10, !Ecs),
        push(Time, kill_bullet(Bullet), !Events),
        push(Time, kill_enemy(Enemy), !Events)
    ;
        Event = hit(asteroid_hits_ship, Asteroid, Ship),
        push(Time, kill_asteroid(Asteroid), !Events),
        push(Time, kill_ship(Ship), !Events)
    ;
        Event = hit(enemy_hits_ship, Enemy, Ship),
        push(Time, kill_enemy(Enemy), !Events),
        push(Time, kill_ship(Ship), !Events)
    ;
        Event = hit(enemy_bullet_hits_ship, Bullet, Ship),
        push(Time, kill_enemy_bullet(Bullet), !Events),
        push(Time, kill_ship(Ship), !Events)
    ;
        Event = check_win_lose,
        check_win_lose(!.Ecs, !World)
    ).

:- pred player_scores(entity::in, int::in, ecs::in, ecs::out) is det.

player_scores(Bullet, Points, !Ecs) :-
    (
        get_player(!.Ecs, Bullet, Player),
        PlayerEntity = Player ^ player_entity,
        get_score(!.Ecs, PlayerEntity, Score0)
    ->
        set_score(PlayerEntity, Score0 + Points, !Ecs)
    ;
        true
    ).

:- pred potential_win_lose(game_time::in, world::in, ecs::in, ecs::out) is det.

potential_win_lose(Time, World, !Ecs) :-
    set_timeout(World ^ win_lose, Time + 1.0, check_win_lose, !Ecs).

:- pred check_win_lose(ecs::in, world::in, world::out) is det.

check_win_lose(Ecs, !World) :-
    ( !.World ^ game_state = playing ->
        ( lose_condition(Ecs, !.World) ->
            !World ^ game_state := lose
        ; win_condition(Ecs, !.World) ->
            !World ^ game_state := win
        ;
            true
        )
    ;
        true
    ).

:- pred win_condition(ecs::in, world::in) is semidet.

win_condition(Ecs, World) :-
    World ^ num_asteroids = 0,
    ai.get_all(Ecs, AI),
    count(AI, 0).

:- pred lose_condition(ecs::in, world::in) is semidet.

lose_condition(Ecs, World) :-
    get_life(Ecs, World ^ player ^ player_entity, 0).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
