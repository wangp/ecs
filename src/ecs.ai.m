%-----------------------------------------------------------------------------%

:- module ecs.ai.
:- interface.

:- type ai_components == map(entity, ai).

:- type ai
    --->    ufo(fire_time :: float).

:- pred get_all(ecs::in, ai_components::out) is det.

:- pred set(entity::in, ai::in, ecs::in, ecs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

get_all(Ecs, Ecs ^ all_ai).

set(Entity, Component, !Ecs) :-
    !.Ecs ^ all_ai = Components0,
    map.set(Entity, Component, Components0, Components),
    !Ecs ^ all_ai := Components.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
