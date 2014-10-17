%-----------------------------------------------------------------------------%

:- module new_entity.
:- interface.

:- import_module bool.

:- import_module ecs.
:- import_module prim.
:- import_module rand.
:- import_module vec2.

:- pred new_player(player::out, ecs::in, ecs::out) is det.

:- pred new_ship(game_time::in, player::in, float::in, bool::in,
    ecs::in, ecs::out) is det.

:- pred new_bullet(game_time::in, vec2::in, vec2::in, float::in, player::in,
    ecs::in, ecs::out, rs::in, rs::out) is det.

:- pred new_asteroid(vec2::in, life::in, ecs::in, ecs::out, rs::in, rs::out)
    is det.

:- pred new_ufo(game_time::in, vec2::in, entity::in, ecs::in, ecs::out) is det.

:- pred new_enemy_bullet(game_time::in, vec2::in, vec2::in, float::in,
    ecs::in, ecs::out) is det.

:- pred new_explosion(game_time::in, vec2::in, ecs::in, ecs::out) is det.

:- pred new_star(vec2::in, vec2::in, ecs::in, ecs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module math.
:- import_module maybe.

:- import_module ecs.ai.
:- import_module ecs.collider.
:- import_module ecs.control.
:- import_module ecs.loc.
:- import_module ecs.prop.
:- import_module ecs.timer.
:- import_module ecs.vis.
:- import_module game_event.

new_player(player(Entity), !Ecs) :-
    new_entity(Entity, !Ecs),
    set_life(Entity, 3, !Ecs),
    set_score(Entity, 0, !Ecs).

new_asteroid(Pos, Life, !Ecs, !RS) :-
    ( asteroid_params(Life, Scale, Speed) ->
        rand_range(-pi, pi, Angle, !RS),
        rand_range(-0.1, 0.1, AngleVel, !RS),
        Vel = Speed * cos_sin(Angle),
        Loc = loc(Pos, Vel, Angle, AngleVel, yes),
        new_entity(Entity, !Ecs),
        set(Entity, Loc, !Ecs),
        set(Entity, collider(asteroid_tag, circle(Scale), enabled), !Ecs),
        set(Entity, fg, vis(asteroid_sprite, Scale, none, no), !Ecs),
        set_life(Entity, Life, !Ecs)
    ;
        true % unexpected
    ).

:- pred asteroid_params(life::in, float::out, float::out) is semidet.

asteroid_params(1, 0.6, 0.20).
asteroid_params(2, 1.6, 0.15).
asteroid_params(3, 3.0, 0.10).

new_ship(Time, Player, Angle, Invuln, !Ecs) :-
    new_entity(Ship, !Ecs),
    set_player(Ship, Player, !Ecs),
    set(Ship, ship_control(no, no, no, no, Time), !Ecs),
    set(Ship, loc(zero, zero, Angle, 0.0, yes), !Ecs),
    (
        Invuln = yes,
        CanCollide = disabled,
        MaybeBlink = yes(blink(Time, 0.05)),
        set_timeout(Ship, Time + 2.0, make_vulnerable(Ship), !Ecs)
    ;
        Invuln = no,
        CanCollide = enabled,
        MaybeBlink = no
    ),
    Scale = 1.0,
    set(Ship, collider(ship_tag, circle(Scale), CanCollide), !Ecs),
    set(Ship, fg, vis(ship_sprite, Scale, none, MaybeBlink), !Ecs).

new_bullet(Time, Pos, BaseVel, Angle, Player, !Ecs, !RS) :-
    new_entity(Entity, !Ecs),
    set_player(Entity, Player, !Ecs),
    Vel = BaseVel + 0.5 * cos_sin(Angle),
    set(Entity, loc(Pos, Vel, Angle, 0.0, yes), !Ecs),
    Scale = 0.2,
    set(Entity, collider(bullet_tag, circle(Scale), enabled), !Ecs),
    set(Entity, fg, vis(bullet_sprite, Scale, none, no), !Ecs),
    set_timeout(Entity, Time + 1.0, kill_bullet(Entity), !Ecs).

new_ufo(Time, Pos, Owner, !Ecs) :-
    new_entity(Entity, !Ecs),
    FireTime = Time + 0.5,
    set(Entity, ufo(FireTime), !Ecs),
    set(Entity, loc(Pos, vec2(-0.15, 0.0), 0.0, 0.0, no), !Ecs),
    Scale = 1.0,
    set(Entity, collider(enemy_tag, circle(Scale), enabled), !Ecs),
    set(Entity, fg, vis(ufo_sprite, Scale, none, no), !Ecs),
    set_owner(Entity, Owner, !Ecs).

new_enemy_bullet(Time, Pos, Vel, Angle, !Ecs) :-
    new_entity(Entity, !Ecs),
    set(Entity, loc(Pos, Vel, Angle, 0.0, yes), !Ecs),
    Scale = 0.2,
    set(Entity, collider(enemy_bullet_tag, circle(Scale), enabled), !Ecs),
    set(Entity, fg, vis(enemy_bullet_sprite, Scale, none, no), !Ecs),
    set_timeout(Entity, Time + 3.0, kill_enemy_bullet(Entity), !Ecs).

new_explosion(Time, Pos, !Ecs) :-
    new_entity(Entity, !Ecs),
    set(Entity, loc(Pos, zero, 0.0, 0.0, no), !Ecs),
    EndTime = Time + 0.25,
    Anim = anim(cubic_ease_out, Time, EndTime, 0.0, 1.0),
    set(Entity, fg/*overlay*/, vis(explosion_sprite, 1.0, Anim, no), !Ecs),
    set_timeout(Entity, EndTime, kill_explosion(Entity), !Ecs).

new_star(Pos, Vel, !Ecs) :-
    new_entity(Entity, !Ecs),
    set(Entity, loc(Pos, Vel, 0.0, 0.0, yes), !Ecs),
    set(Entity, bg, vis(star_sprite, 0.05, none, no), !Ecs).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
