%-----------------------------------------------------------------------------%

:- module ecs.collider.
:- interface.

:- type collider_components == map(entity, collider).

:- type collider
    --->    collider(tag, shape, enabled).

:- type tag
    --->    asteroid_tag
    ;       ship_tag
    ;       bullet_tag
    ;       enemy_tag
    ;       enemy_bullet_tag.

:- type shape
    --->    circle(radius :: float).

:- type enabled
    --->    disabled
    ;       enabled.

:- pred get_all(ecs::in, collider_components::out) is det.

:- pred get(ecs::in, entity::in, collider::out) is semidet.

:- pred set(entity::in, collider::in, ecs::in, ecs::out) is det.

:- pred set_collider_enabled(entity::in, enabled::in, ecs::in, ecs::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

get_all(Ecs, Ecs ^ all_collider).

get(Ecs, Entity, Component) :-
    map.search(Ecs ^ all_collider, Entity, Component).

set(Entity, Component, !Ecs) :-
    !.Ecs ^ all_collider = Components0,
    map.set(Entity, Component, Components0, Components),
    !Ecs ^ all_collider := Components.

set_collider_enabled(Entity, Enabled, !Ecs) :-
    ( get(!.Ecs, Entity, Component0) ->
        Component0 = collider(Tag, Shape, _Enabled0),
        Component = collider(Tag, Shape, Enabled),
        set(Entity, Component, !Ecs)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
