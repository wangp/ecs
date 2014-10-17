%-----------------------------------------------------------------------------%

:- module timer.
:- interface.

:- import_module ecs.
:- import_module game_event.
:- import_module prim.

:- pred run_timers(game_time::in, ecs::in, ecs::out,
    game_events::in, game_events::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module map.

:- import_module ecs.timer.

run_timers(Now, !Ecs, !Events) :-
    get_all(!.Ecs, Components0),
    map.foldl2(update_component(Now), Components0, !Ecs, !Events).

:- pred update_component(game_time::in, entity::in, timer::in,
    ecs::in, ecs::out, game_events::in, game_events::out) is det.

update_component(Now, Entity, timeout(End, Event), !Ecs, !Events) :-
    ( Now >= End ->
        timer.delete(Entity, !Ecs),
        push(Now, Event, !Events)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
