%-----------------------------------------------------------------------------%

:- module input_event.
:- interface.

:- import_module list.

:- import_module ecs.
:- import_module world.

:- type input_event
    --->    quit
    ;       pause
    ;       thrust_on
    ;       thrust_off
    ;       left_on
    ;       left_off
    ;       right_on
    ;       right_off
    ;       fire_on
    ;       fire_off.

:- pred apply_input_events(list(input_event)::in, world::in, world::out,
    ecs::in, ecs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module map.

:- import_module ecs.control.

apply_input_events(InputEvents, !World, !Ecs) :-
    (
        InputEvents = []
    ;
        InputEvents = [_ | _],
        foldl(update_game_state, InputEvents, !World),

        get_all(!.Ecs, Controls0),
        map.map_values_only(foldl(update_control, InputEvents),
            Controls0, Controls),
        set_all(Controls, !Ecs)
    ).

:- pred update_game_state(input_event::in, world::in, world::out) is det.

update_game_state(InputEvent, !World) :-
    (
        InputEvent = quit,
        !World ^ game_state := quit
    ;
        InputEvent = pause,
        !World ^ paused := not(!.World ^ paused)
    ;
        ( InputEvent = thrust_on
        ; InputEvent = thrust_off
        ; InputEvent = left_on
        ; InputEvent = left_off
        ; InputEvent = right_on
        ; InputEvent = right_off
        ; InputEvent = fire_on
        ; InputEvent = fire_off
        )
    ).

:- pred update_control(input_event::in, control::in, control::out) is det.

update_control(InputEvent, !Control) :-
    !.Control = ship_control(_, _, _, _, _),
    (
        InputEvent = quit
    ;
        InputEvent = pause
    ;
        InputEvent = thrust_on,
        !Control ^ thrust := yes
    ;
        InputEvent = thrust_off,
        !Control ^ thrust := no
    ;
        InputEvent = left_on,
        !Control ^ left := yes
    ;
        InputEvent = left_off,
        !Control ^ left := no
    ;
        InputEvent = right_on,
        !Control ^ right := yes
    ;
        InputEvent = right_off,
        !Control ^ right := no
    ;
        InputEvent = fire_on,
        !Control ^ fire := yes
    ;
        InputEvent = fire_off,
        !Control ^ fire := no
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
