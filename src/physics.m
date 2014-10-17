%-----------------------------------------------------------------------------%

:- module physics.
:- interface.

:- import_module ecs.
:- import_module world.

:- pred run_physics(world::in, ecs::in, ecs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module float.
:- import_module map.

:- import_module ecs.loc.
:- import_module vec2.

run_physics(World, !Ecs) :-
    get_all(!.Ecs, Locs0),
    map.map_values_only(update_component(World ^ bounds), Locs0, Locs),
    set_all(Locs, !Ecs).

:- pred update_component(bounds::in, loc::in, loc::out) is det.

update_component(Bounds, Loc0, Loc) :-
    Loc0 = loc(Pos0, Vel, Angle0, AngleVel, Wrap),
    Pos1 = Pos0 + Vel,
    (
        Wrap = yes,
        Pos = wrap(Bounds, Pos1)
    ;
        Wrap = no,
        Pos = Pos1
    ),
    Loc = loc(Pos, Vel, Angle0 + AngleVel, AngleVel, Wrap).

:- func wrap(bounds, vec2) = vec2.

wrap(bounds(MinX, MaxX, MinY, MaxY), vec2(X1, Y1)) = vec2(X2, Y2) :-
    X2 = wrap1(MinX, MaxX, X1),
    Y2 = wrap1(MinY, MaxY, Y1).

:- func wrap1(float, float, float) = float.

wrap1(Min, Max, X) =
    ( X < Min -> X + W
    ; X > Max -> X - W
    ; X
    ) :-
    W = Max - Min.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
