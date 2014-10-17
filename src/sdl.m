%-----------------------------------------------------------------------------%

:- module sdl.
:- interface.

:- import_module io.

:- type window.

:- type renderer.

:- type key
    --->    sdlk_return
    ;       sdlk_escape
    ;       sdlk_space
    ;       sdlk_a
    ;       sdlk_d
    ;       sdlk_p
    ;       sdlk_s
    ;       sdlk_w
    ;       sdlk_up
    ;       sdlk_down
    ;       sdlk_right
    ;       sdlk_left.

:- pred init(io::di, io::uo) is det.

:- pred create_window_and_renderer(int::in, int::in,
    window::out, renderer::out, io::di, io::uo) is det.

:- pred quit(io::di, io::uo) is det.

:- pred delay(int::in, io::di, io::uo) is det.

:- pred set_render_draw_color(renderer::in,
    int::in, int::in, int::in, int::in, io::di, io::uo) is det.

:- pred render_clear(renderer::in, io::di, io::uo) is det.

:- pred render_present(renderer::in, io::di, io::uo) is det.

% SDL gfx

:- type color.

:- func make_color(int, int, int, int) = color.

:- pred line_color(renderer::in, int::in, int::in, int::in, int::in, color::in,
    io::di, io::uo) is det.

:- pred circle_color(renderer::in, int::in, int::in, int::in, color::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

:- pragma foreign_decl("C", "
    #include <SDL2/SDL.h>
    #include <SDL2/SDL2_gfxPrimitives.h>
").

:- pragma foreign_type("C", window, "SDL_Window *").
:- pragma foreign_type("C", renderer, "SDL_Renderer *").

:- pragma foreign_enum("C", key/0, [
    sdlk_return - "SDLK_RETURN",
    sdlk_escape - "SDLK_ESCAPE",
    sdlk_space - "SDLK_SPACE",
    sdlk_a - "SDLK_a",
    sdlk_d - "SDLK_d",
    sdlk_p - "SDLK_p",
    sdlk_s - "SDLK_s",
    sdlk_w - "SDLK_w",
    sdlk_up - "SDLK_UP",
    sdlk_down - "SDLK_DOWN",
    sdlk_right - "SDLK_RIGHT",
    sdlk_left - "SDLK_LEFT"
]).

:- type color == int.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    init(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    SDL_Init(SDL_INIT_VIDEO);
    /* XXX check error */
").

:- pragma foreign_proc("C",
    create_window_and_renderer(Xres::in, Yres::in, Window::out, Renderer::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    SDL_CreateWindowAndRenderer(Xres, Yres, SDL_WINDOW_OPENGL,
        &Window, &Renderer);
    /* XXX check error */
").

:- pragma foreign_proc("C",
    quit(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    SDL_Quit();
").

:- pragma foreign_proc("C",
    delay(Msec::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    SDL_Delay(Msec);
").

:- pragma foreign_proc("C",
    set_render_draw_color(Renderer::in, R::in, G::in, B::in, A::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    SDL_SetRenderDrawColor(Renderer, R, G, B, A);
").

:- pragma foreign_proc("C",
    render_clear(Renderer::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    SDL_RenderClear(Renderer);
").

:- pragma foreign_proc("C",
    render_present(Renderer::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    SDL_RenderPresent(Renderer);
").

%-----------------------------------------------------------------------------%

make_color(R, G, B, A) = A<<24 \/ B<<16 \/ G<<8 \/ R.

:- pragma foreign_proc("C",
    line_color(Renderer::in, X1::in, Y1::in, X2::in, Y2::in, Color::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    lineColor(Renderer, X1, Y1, X2, Y2, Color);
").

:- pragma foreign_proc("C",
    circle_color(Renderer::in, X::in, Y::in, Rad::in, Color::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    circleColor(Renderer, X, Y, Rad, Color);
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
