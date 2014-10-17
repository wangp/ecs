%-----------------------------------------------------------------------------%

:- module ecs.timer.
:- interface.

:- import_module game_event.
:- import_module prim.

:- type timer_components == map(entity, timer).

:- type timer
    --->    timeout(
                end :: game_time,
                push_event :: game_event
            ).

:- pred get_all(ecs::in, timer_components::out) is det.

:- pred set_timeout(entity::in, game_time::in, game_event::in,
    ecs::in, ecs::out) is det.

:- pred delete(entity::in, ecs::in, ecs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

get_all(Ecs, Ecs ^ all_timer).

set_timeout(Entity, Time, Event, !Ecs) :-
    !.Ecs ^ all_timer = Components0,
    map.set(Entity, timeout(Time, Event), Components0, Components),
    !Ecs ^ all_timer := Components.

delete(Entity, !Ecs) :-
    !.Ecs ^ all_timer = Components0,
    map.delete(Entity, Components0, Components),
    !Ecs ^ all_timer := Components.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
