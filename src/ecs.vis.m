%-----------------------------------------------------------------------------%

:- module ecs.vis.
:- interface.

:- import_module maybe.

:- import_module prim.

:- type vis_components
    --->    layers(
                bg      :: layer,
                fg      :: layer
            ).

:- type layer_id
    --->    bg
    ;       fg.

:- type layer == map(entity, vis).

:- type vis
    --->    vis(
                sprite  :: sprite,
                scale   :: anim,
                blink   :: maybe(blink)
            ).

:- type sprite
    --->    asteroid_sprite
    ;       ship_sprite
    ;       bullet_sprite
    ;       ufo_sprite
    ;       enemy_bullet_sprite
    ;       explosion_sprite
    ;       star_sprite.

:- type anim
    --->    constant(float)
    ;       anim(
                fn      :: easing_function,
                t0      :: game_time,
                t1      :: game_time,
                v0      :: float,
                v1      :: float
            ).

:- type easing_function
    --->    cubic_ease_out.

:- type blink
    --->    blink(
                start   :: game_time,
                rate    :: game_time
            ).

:- pred get_all(ecs::in, vis_components::out) is det.

:- pred get(ecs::in, entity::in, layer_id::in, vis::out) is semidet.

:- pred set(entity::in, layer_id::in, vis::in, ecs::in, ecs::out) is det.

:- pred set_blink(entity::in, layer_id::in, maybe(blink)::in,
    ecs::in, ecs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

get_all(Ecs, Ecs ^ all_vis).

get(Ecs, Entity, Layer, Component) :-
    (
        Layer = bg,
        Ecs ^ all_vis = layers(Map, _)
    ;
        Layer = fg,
        Ecs ^ all_vis = layers(Map, _)
    ),
    map.search(Map, Entity, Component).

set(Entity, Layer, Component, !Ecs) :-
    !.Ecs ^ all_vis = Layers0,
    (
        Layer = bg,
        Layers0 = layers(Bg0, Fg),
        map.set(Entity, Component, Bg0, Bg),
        Layers = layers(Bg, Fg)
    ;
        Layer = fg,
        Layers0 = layers(Bg, Fg0),
        map.set(Entity, Component, Fg0, Fg),
        Layers = layers(Bg, Fg)
    ),
    !Ecs ^ all_vis := Layers.

set_blink(Entity, Layer, Blink, !Ecs) :-
    !.Ecs ^ all_vis = Layers0,
    (
        Layer = bg,
        Layers0 = layers(Bg0, Fg),
        set_blink_2(Entity, Blink, Bg0, Bg),
        Layers = layers(Bg, Fg)
    ;
        Layer = fg,
        Layers0 = layers(Bg, Fg0),
        set_blink_2(Entity, Blink, Fg0, Fg),
        Layers = layers(Bg, Fg)
    ),
    !Ecs ^ all_vis := Layers.

:- pred set_blink_2(entity::in, maybe(blink)::in, layer::in, layer::out)
    is det.

set_blink_2(Entity, Blink, !Layer) :-
    ( map.search(!.Layer, Entity, Component0) ->
        Component = Component0 ^ blink := Blink,
        map.det_update(Entity, Component, !Layer)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
