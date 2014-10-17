%-----------------------------------------------------------------------------%

:- module ecs.
:- interface.

:- include_module ecs.ai.
:- include_module ecs.collider.
:- include_module ecs.loc.
:- include_module ecs.prop.
:- include_module ecs.control.
:- include_module ecs.timer.
:- include_module ecs.vis.

:- type ecs.

:- type entity
    --->    entity(int).

:- type player
    --->    player(player_entity :: entity).

:- pred init(ecs::out) is det.

:- pred new_entity(entity::out, ecs::in, ecs::out) is det.

:- pred mark_dead(entity::in, ecs::in, ecs::out) is det.

:- pred sweep_dead(ecs::in, ecs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module map.
:- import_module set_tree234.

:- import_module ecs.ai.
:- import_module ecs.collider.
:- import_module ecs.control.
:- import_module ecs.loc.
:- import_module ecs.prop.
:- import_module ecs.timer.
:- import_module ecs.vis.

    % Entity/components.
    %
:- type ecs
    --->    ecs(
                next_entity     :: int,
                dead            :: set_tree234(entity),
                all_loc         :: loc_components,
                all_collider    :: collider_components,
                all_timer       :: timer_components,
                all_vis         :: vis_components,
                all_control     :: control_components,
                all_ai          :: ai_components,
                all_prop        :: prop_components
            ).

init(Ecs) :-
    Ecs = ecs(0, init, init, init, init, layers(init, init), init, init, init).

new_entity(entity(Id), !Ecs) :-
    Id = !.Ecs ^ next_entity,
    !Ecs ^ next_entity := Id + 1.

mark_dead(Entity, !Ecs) :-
    !.Ecs ^ dead = Dead0,
    insert(Entity, Dead0, Dead),
    !Ecs ^ dead := Dead.

sweep_dead(Ecs0, Ecs) :-
    Ecs0 = ecs(NextEntity, Dead0, A0, B0, C0, layers(D0, E0), F0, G0, H0),
    ( empty(Dead0) ->
        Ecs = Ecs0
    ;
        to_sorted_list(Dead0, DeadList),
        sweep(DeadList, A0, A),
        sweep(DeadList, B0, B),
        sweep(DeadList, C0, C),
        sweep(DeadList, D0, D),
        sweep(DeadList, E0, E),
        sweep(DeadList, F0, F),
        sweep(DeadList, G0, G),
        sweep(DeadList, H0, H),
        Ecs = ecs(NextEntity, init, A, B, C, layers(D, E), F, G, H)
    ).

:- pred sweep(list(entity)::in, map(entity, T)::in, map(entity, T)::out)
    is det.

sweep(Dead, !Map) :-
    map.delete_list(Dead, !Map).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
