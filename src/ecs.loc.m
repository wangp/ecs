%-----------------------------------------------------------------------------%

:- module ecs.loc.
:- interface.

:- import_module bool.
:- import_module vec2.

:- type loc_components == map(entity, loc).

:- type loc
    --->    loc(
                pos         :: vec2,
                vel         :: vec2,
                angle       :: float,
                angle_vel   :: float,
                wrap        :: bool
            ).

:- pred get_all(ecs::in, loc_components::out) is det.

:- pred set_all(loc_components::in, ecs::in, ecs::out) is det.

:- pred get(ecs::in, entity::in, loc::out) is semidet.

:- pred get_pos(ecs::in, entity::in, vec2::out) is semidet.

:- pred set(entity::in, loc::in, ecs::in, ecs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

get_all(Ecs, Ecs ^ all_loc).

set_all(Components, !Ecs) :-
    !Ecs ^ all_loc := Components.

get(Ecs, Entity, Loc) :-
    map.search(Ecs ^ all_loc, Entity, Loc).

get_pos(Ecs, Entity, Loc ^ pos) :-
    get(Ecs, Entity, Loc).

set(Entity, Loc, !Ecs) :-
    !.Ecs ^ all_loc = Components0,
    map.set(Entity, Loc, Components0, Components),
    !Ecs ^ all_loc := Components.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
