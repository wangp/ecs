%-----------------------------------------------------------------------------%

:- module collision.
:- interface.

:- import_module ecs.
:- import_module game_event.
:- import_module prim.

:- pred run_collisions(game_time::in, ecs::in,
    game_events::in, game_events::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module float.
:- import_module list.
:- import_module map.
:- import_module pair.

:- import_module ecs.collider.
:- import_module ecs.loc.
:- import_module vec2.

run_collisions(Time, Ecs, !Events) :-
    get_all(Ecs, Colliders0),
    map.to_assoc_list(Colliders0, Colliders),
    check_colliders(Time, Ecs, Colliders, !Events).

:- pred check_colliders(game_time::in, ecs::in,
    assoc_list(entity, collider)::in, game_events::in, game_events::out)
    is det.

check_colliders(_Time, _Ecs, [], !Events).
check_colliders(Time, Ecs, [X | Ys], !Events) :-
    ( X = _ - collider(_, _, enabled) ->
        check_colliders_2(Time, Ecs, X, Ys, !Events)
    ;
        true
    ),
    check_colliders(Time, Ecs, Ys, !Events).

:- pred check_colliders_2(game_time::in, ecs::in, pair(entity, collider)::in,
    assoc_list(entity, collider)::in, game_events::in, game_events::out)
    is det.

check_colliders_2(_Time, _Ecs, _X, [], !Events).
check_colliders_2(Time, Ecs, X, [Y | Ys], !Events) :-
    check_collider_pair(Time, Ecs, X, Y, !Events),
    check_colliders_2(Time, Ecs, X, Ys, !Events).

:- pred check_collider_pair(game_time::in, ecs::in, pair(entity, collider)::in,
    pair(entity, collider)::in, game_events::in, game_events::out) is det.

check_collider_pair(Time, Ecs, EntityA - ColliderA, EntityB - ColliderB,
        !Events) :-
    % Already checked EnabledA.
    ColliderA = collider(TagA, ShapeA, _EnabledA),
    ColliderB = collider(TagB, ShapeB, EnabledB),
    ( EnabledB = disabled ->
        true
    ; tags_hit(TagA, TagB, Type) ->
        check_collider_pair_2(Time, Ecs, EntityA, ShapeA, EntityB, ShapeB,
            Type, !Events)
    ; tags_hit(TagB, TagA, Type) ->
        check_collider_pair_2(Time, Ecs, EntityB, ShapeB, EntityA, ShapeA,
            Type, !Events)
    ;
        true
    ).

:- pred check_collider_pair_2(game_time::in, ecs::in, entity::in, shape::in,
    entity::in, shape::in, hit_type::in, game_events::in, game_events::out)
    is det.

check_collider_pair_2(Time, Ecs, EntityA, ShapeA, EntityB, ShapeB, Type,
        !Events) :-
    % XXX should wrap across bounds
    (
        get_pos(Ecs, EntityA, PosA),
        get_pos(Ecs, EntityB, PosB),
        shapes_hit(ShapeA, PosA, ShapeB, PosB)
    ->
        push(Time, hit(Type, EntityA, EntityB), !Events)
    ;
        true
    ).

:- pred tags_hit(tag::in, tag::in, hit_type::out) is semidet.

tags_hit(bullet_tag, asteroid_tag, bullet_hits_asteroid).
tags_hit(bullet_tag, enemy_tag, bullet_hits_enemy).
tags_hit(asteroid_tag, ship_tag, asteroid_hits_ship).
tags_hit(enemy_tag, ship_tag, enemy_hits_ship).
tags_hit(enemy_bullet_tag, ship_tag, enemy_bullet_hits_ship).

:- pred shapes_hit(shape::in, vec2::in, shape::in, vec2::in) is semidet.

shapes_hit(circle(RA), PosA, circle(RB), PosB) :-
    length(PosA - PosB) < RA + RB.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
