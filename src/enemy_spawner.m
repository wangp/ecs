%-----------------------------------------------------------------------------%

:- module enemy_spawner.
:- interface.

:- import_module prim.
:- import_module rand.

:- pred rand_enemy_spawn_time(game_time::in, game_time::out, rs::in, rs::out)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.

rand_enemy_spawn_time(Time, SpawnTime, !RS) :-
    rand_range(Time + 10.0, Time + 20.0, SpawnTime, !RS).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
