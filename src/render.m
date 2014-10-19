%-----------------------------------------------------------------------------%

:- module render.
:- interface.

:- import_module io.

:- import_module ecs.
:- import_module prim.
:- import_module sdl.
:- import_module vec2.
:- import_module world.

:- type screen
    --->    screen(
                window :: window,
                renderer :: renderer,
                transform :: transform
            ).

:- type transform
    --->    transform(
                translate :: vec2,
                scale :: vec2
            ).

:- func make_transform(int, int, bounds) = transform.

:- pred render(screen::in, game_time::in, world::in,  ecs::in, io::di, io::uo)
    is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module float.
:- import_module map.
:- import_module maybe.

:- import_module ecs.loc.
:- import_module ecs.prop.
:- import_module ecs.vis.

%-----------------------------------------------------------------------------%

make_transform(Xres, Yres, bounds(MinX, MaxX, MinY, MaxY)) = M :-
    X = float(Xres),
    Y = float(Yres),
    M = transform(
            vec2(X/2.0, Y/2.0),
            vec2(X/(MaxX - MinX), -Y/(MaxY - MinY))).

%-----------------------------------------------------------------------------%

render(Screen, Time, World, Ecs, !IO) :-
    Screen = screen(_Window, Renderer, _Trans),
    sdl.set_render_draw_color(Renderer, 0, 0, 0, 255, !IO),
    sdl.render_clear(Renderer, !IO),

    Bounds = World ^ bounds,
    get_all(Ecs, layers(Fg, Overlay)),
    map.foldl(render_entity(Screen, Bounds, Time, Ecs), Fg, !IO),
    map.foldl(render_entity(Screen, Bounds, Time, Ecs), Overlay, !IO),

    render_hud(Screen, World, Ecs, !IO),

    sdl.render_present(Renderer, !IO).

%-----------------------------------------------------------------------------%

:- pred render_entity(screen::in, bounds::in, game_time::in, ecs::in,
    entity::in, vis::in, io::di, io::uo) is det.

render_entity(Screen, Bounds, Time, Ecs, Entity, Vis, !IO) :-
    (
        get(Ecs, Entity, Loc),
        Vis = vis(Sprite, ScaleAnim, MaybeBlink),
        visible(Time, MaybeBlink, yes)
    ->
        Loc = loc(Pos, _Vel, Angle, _AngleVel, Wrap),
        animate(ScaleAnim, Time, Scale),
        (
            Wrap = yes,
            draw_wrap(Screen, Bounds, Sprite, Pos, Angle, Scale, !IO)
        ;
            Wrap = no,
            draw_sprite(Screen, Sprite, Pos, Angle, Scale, !IO)
        )
    ;
        true
    ).

:- pred visible(game_time::in, maybe(blink)::in, bool::out) is det.

visible(_Time, no, yes).
visible(Time, yes(blink(Start, Rate)), Visible) :-
    Period = floor_to_int((Time - Start) / Rate),
    Visible = pred_to_bool(even(Period)).

:- pred animate(anim::in, game_time::in, float::out) is det.

animate(constant(V), _T, V).
animate(anim(cubic_ease_out, T0, T1, V0, V1), T, V) :-
    P = min(1.0, (T-T0)/(T1-T0)),
    F = P - 1.0,
    K = F*F*F + 1.0,
    V = K*V1 + (1.0-K)*V0.

:- pred draw_wrap(screen::in, bounds::in, sprite::in, vec2::in, float::in,
    float::in, io::di, io::uo) is det.

draw_wrap(Screen, Bounds, Sprite, Pos, Angle, Scale, !IO) :-
    Bounds = bounds(MinX, MaxX, MinY, MaxY),
    H = vec2(MaxX - MinX, 0.0),
    V = vec2(0.0, MaxY - MinY),
    draw_sprite(Screen, Sprite, Pos, Angle, Scale, !IO),
    draw_sprite(Screen, Sprite, Pos - H, Angle, Scale, !IO),
    draw_sprite(Screen, Sprite, Pos + H, Angle, Scale, !IO),
    draw_sprite(Screen, Sprite, Pos - V, Angle, Scale, !IO),
    draw_sprite(Screen, Sprite, Pos + V, Angle, Scale, !IO).

:- pred draw_sprite(screen::in, sprite::in, vec2::in, float::in, float::in,
    io::di, io::uo) is det.

draw_sprite(screen(_Window, Renderer, M), Sprite, Pos, Angle, Scale, !IO) :-
    transform(M, Pos, P),
    transform(M, Pos + Scale * cos_sin(Angle), Q),
    Rad = length(P - Q),
    sprite_color(Sprite, Color),

    P = vec2(PX, PY),
    Q = vec2(QX, QY),
    % Only works for square aspect ratios.
    circle_color(Renderer, round_to_int(PX), round_to_int(PY),
        round_to_int(Rad), Color, !IO),
    ( draw_orientation(Sprite) ->
        line_color(Renderer, round_to_int(PX), round_to_int(PY),
            round_to_int(QX), round_to_int(QY), Color, !IO)
    ;
        true
    ).

:- pred transform(transform::in, vec2::in, vec2::out) is det.

transform(transform(Translate, Scale), vec2(X0, Y0), vec2(X, Y)) :-
    Translate = vec2(TX, TY),
    Scale = vec2(SX, SY),
    X = (SX * X0) + TX,
    Y = (SY * Y0) + TY.

:- pred sprite_color(sprite::in, color::out) is det.

sprite_color(asteroid_sprite, make_color(128, 128, 128, 255)).
sprite_color(ship_sprite, make_color(255, 255, 255, 255)).
sprite_color(bullet_sprite, make_color(0, 255, 255, 255)).
sprite_color(ufo_sprite, make_color(255, 0, 255, 255)).
sprite_color(enemy_bullet_sprite, make_color(255, 0, 0, 255)).
sprite_color(explosion_sprite, make_color(255, 255, 0, 255)).
sprite_color(star_sprite, make_color(128, 128, 255, 255)).

:- pred draw_orientation(sprite::in) is semidet.

draw_orientation(asteroid_sprite).
draw_orientation(ship_sprite).
% draw_orientation(ufo_sprite).

%-----------------------------------------------------------------------------%

:- pred render_hud(screen::in, world::in, ecs::in, io::di, io::uo) is det.

render_hud(_Screen, World, Ecs, !IO) :-
    World ^ game_state = GameState,
    (
        GameState = playing,
        Player = World ^ player,
        (
            get_life(Ecs, Player ^ player_entity, Life),
            get_score(Ecs, Player ^ player_entity, Score)
        ->
            io.write_string("Life: ", !IO),
            io.write_int(Life, !IO),
            io.write_string("; Score: ", !IO),
            io.write_int(Score, !IO),
            io.nl(!IO)
        ;
            true
        )
    ;
        GameState = win,
        io.write_string("YOU'RE WINNER!\n", !IO)
    ;
        GameState = lose,
        io.write_string("you lose\n", !IO)
    ;
        GameState = quit,
        io.write_string("quit\n", !IO)
    ).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
