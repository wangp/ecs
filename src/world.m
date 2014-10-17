%-----------------------------------------------------------------------------%

:- module world.
:- interface.

:- import_module bool.

:- import_module ecs.
:- import_module vec2.

:- type world
    --->    world(
                game_state :: game_state,
                bounds :: bounds,
                paused :: bool,
                win_lose :: entity,
                player :: player, % could have multiple
                num_asteroids :: int
            ).

:- type game_state
    --->    playing
    ;       win
    ;       lose
    ;       quit.

:- type bounds
    --->    bounds(
                minx :: float,
                maxx :: float,
                miny :: float,
                maxy :: float
            ).

:- pred is_game_over(world::in) is semidet.

:- pred is_paused(world::in) is semidet.

:- pred in_bounds(bounds::in, vec2::in) is semidet.

:- pred change_num_asteroids(int::in, world::in, world::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module float.

is_game_over(World) :-
    GameState = World ^ game_state,
    ( GameState = win
    ; GameState = lose
    ).

is_paused(World) :-
    World ^ paused = yes.

in_bounds(bounds(MinX, MaxX, MinY, MaxY), vec2(X, Y)) :-
    MinX =< X, X =< MaxX,
    MinY =< Y, Y =< MaxY.

change_num_asteroids(Delta, !World) :-
    !World ^ num_asteroids := !.World ^ num_asteroids + Delta.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
