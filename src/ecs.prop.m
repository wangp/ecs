%-----------------------------------------------------------------------------%

:- module ecs.prop.
:- interface.

:- import_module map.

:- import_module prim.

:- type prop_components == map(entity, map(prop_key, prop_value)).

:- type prop_key.

:- type prop_value.

:- pred get_life(ecs::in, entity::in, life::out) is semidet.

:- pred set_life(entity::in, life::in, ecs::in, ecs::out) is det.

:- pred get_owner(ecs::in, entity::in, entity::out) is semidet.

:- pred set_owner(entity::in, entity::in, ecs::in, ecs::out) is det.

:- pred get_player(ecs::in, entity::in, player::out) is semidet.

:- pred set_player(entity::in, player::in, ecs::in, ecs::out) is det.

:- pred get_score(ecs::in, entity::in, int::out) is semidet.

:- pred set_score(entity::in, int::in, ecs::in, ecs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- type prop_key
    --->    life
    ;       owner
    ;       player
    ;       score.

:- type prop_value
    --->    i(int)
    ;       e(entity)
    ;       p(player).

:- pred get_generic(ecs::in, entity::in, prop_key::in, prop_value::out)
    is semidet.

get_generic(Ecs, Entity, Key, Value) :-
    map.search(Ecs ^ all_prop, Entity, Props),
    map.search(Props, Key, Value).

:- pred set_generic(entity::in, prop_key::in, prop_value::in,
    ecs::in, ecs::out) is det.

set_generic(Entity, Key, Value, !Ecs) :-
    !.Ecs ^ all_prop = Propss0,
    ( map.search(Propss0, Entity, Props0) ->
        map.set(Key, Value, Props0, Props)
    ;
        Props = map.singleton(Key, Value)
    ),
    map.set(Entity, Props, Propss0, Propss),
    !Ecs ^ all_prop := Propss.

%-----------------------------------------------------------------------------%

get_life(Ecs, Entity, Value) :-
    get_generic(Ecs, Entity, life, i(Value)).

set_life(Entity, Value, !Ecs) :-
    set_generic(Entity, life, i(Value), !Ecs).

%-----------------------------------------------------------------------------%

get_owner(Ecs, Entity, Value) :-
    get_generic(Ecs, Entity, player, e(Value)).

set_owner(Entity, Value, !Ecs) :-
    set_generic(Entity, player, e(Value), !Ecs).

%-----------------------------------------------------------------------------%

get_player(Ecs, Entity, Value) :-
    get_generic(Ecs, Entity, player, p(Value)).

set_player(Entity, Value, !Ecs) :-
    set_generic(Entity, player, p(Value), !Ecs).

%-----------------------------------------------------------------------------%

get_score(Ecs, Entity, Value) :-
    get_generic(Ecs, Entity, score, i(Value)).

set_score(Entity, Value, !Ecs) :-
    set_generic(Entity, score, i(Value), !Ecs).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
