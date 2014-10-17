%-----------------------------------------------------------------------------%

:- module star.
:- interface.

:- import_module ecs.
:- import_module rand.
:- import_module world.

:- pred make_starfield(bounds::in, int::in, ecs::in, ecs::out, rs::in, rs::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- import_module new_entity.
:- import_module vec2.

make_starfield(Bounds, Number, !Ecs, !RS) :-
    int.fold_up2(make_star(Bounds), 1, Number, !Ecs, !RS).

:- pred make_star(bounds::in, int::in, ecs::in, ecs::out,
    rs::in, rs::out) is det.

make_star(Bounds, _, !Ecs, !RS) :-
    Bounds = bounds(MinX, MaxX, MinY, MaxY),
    rand_range(MinX, MaxX, X, !RS),
    rand_range(MinY, MaxY, Y, !RS),
    rand_range(-0.03, -0.01, VX, !RS),
    new_star(vec2(X, Y), vec2(VX, 0.0), !Ecs).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
