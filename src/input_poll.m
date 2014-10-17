%-----------------------------------------------------------------------------%

:- module input_poll.
:- interface.

:- import_module io.
:- import_module list.

:- import_module input_event.

:- pred get_input_events(list(input_event)::out, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module maybe.

:- import_module sdl.

:- pred map_input(sdl.key::in, bool::in, input_event::out) is semidet.

map_input(sdlk_up, yes, thrust_on).
map_input(sdlk_up, no, thrust_off).
map_input(sdlk_left, yes, left_on).
map_input(sdlk_left, no, left_off).
map_input(sdlk_right, yes, right_on).
map_input(sdlk_right, no, right_off).
map_input(sdlk_w, yes, thrust_on).
map_input(sdlk_w, no, thrust_off).
map_input(sdlk_a, yes, left_on).
map_input(sdlk_a, no, left_off).
map_input(sdlk_d, yes, right_on).
map_input(sdlk_d, no, right_off).
map_input(sdlk_space, yes, fire_on).
map_input(sdlk_space, no, fire_off).
map_input(sdlk_escape, yes, quit).
map_input(sdlk_p, yes, pause).

get_input_events(InputEvents, !IO) :-
    get_input_event(Maybe, !IO),
    (
        Maybe = no,
        InputEvents = []
    ;
        Maybe = yes(InputEvent),
        get_input_events(InputEvents0, !IO),
        InputEvents = [InputEvent | InputEvents0]
    ).

:- pred get_input_event(maybe(input_event)::out, io::di, io::uo) is det.

get_input_event(Maybe, !IO) :-
    poll_keyboard(HaveKey, Key, IsDown, !IO),
    (
        HaveKey = yes,
        ( map_input(Key, IsDown, InputEvent) ->
            Maybe = yes(InputEvent)
        ;
            get_input_event(Maybe, !IO)
        )
    ;
        HaveKey = no,
        Maybe = no
    ).

:- pred poll_keyboard(bool::out, sdl.key::out, bool::out, io::di, io::uo)
    is det.

:- pragma foreign_proc("C",
    poll_keyboard(HaveKey::out, Key::out, IsDown::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe, tabled_for_io],
"
    SDL_Event ev;

    HaveKey = MR_NO;
    Key = 0;
    IsDown = MR_NO;

    while (SDL_PollEvent(&ev)) {
        if (ev.type == SDL_KEYDOWN) {
            HaveKey = MR_YES;
            Key = ev.key.keysym.sym;
            IsDown = MR_YES;
            break;
        }
        if (ev.type == SDL_KEYUP) {
            HaveKey = MR_YES;
            Key = ev.key.keysym.sym;
            IsDown = MR_NO;
            break;
        }
    }
").

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
