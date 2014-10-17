%-----------------------------------------------------------------------------%

:- module ecs.control.
:- interface.

:- import_module bool.

:- import_module prim.

:- type control_components == map(entity, control).

:- type control
    --->    ship_control(
                thrust      :: bool,
                left        :: bool,
                right       :: bool,
                fire        :: bool,
                fire_time   :: game_time
            ).

:- pred get_all(ecs::in, control_components::out) is det.

:- pred set_all(control_components::in, ecs::in, ecs::out) is det.

:- pred set(entity::in, control::in, ecs::in, ecs::out) is det.

:- pred get_ships(ecs::in, list(entity)::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

get_all(Ecs, Ecs ^ all_control).

set_all(Components, !Ecs) :-
    !Ecs ^ all_control := Components.

set(Entity, Pos, !Ecs) :-
    !.Ecs ^ all_control = Components0,
    map.set(Entity, Pos, Components0, Components),
    !Ecs ^ all_control := Components.

get_ships(Ecs, Entities) :-
    % True so far.
    map.keys(Ecs ^ all_control, Entities).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
