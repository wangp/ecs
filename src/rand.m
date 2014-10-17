%-----------------------------------------------------------------------------%

:- module rand.
:- interface.

:- type rs.

    % Seed must be non-zero.
    %
:- pred init(int::in, rs::out) is det.

:- pred rand(int::out, rs::in, rs::out) is det.

:- pred rand_float(float::out, rs::in, rs::out) is det.

:- pred rand_range(float::in, float::in, float::out, rs::in, rs::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.

:- pragma foreign_decl("C", "#include <stdint.h>").

:- pragma foreign_type("C", rs, "uint32_t").

:- pragma foreign_decl("C", "
uint32_t xorshift32(uint32_t x);
").

:- pragma foreign_code("C", "
uint32_t xorshift32(uint32_t x)
{
    x ^= x << 13;
    x ^= x >> 17;
    x ^= x << 5;
    return x;
}
").

:- pragma foreign_proc("C",
    init(X0::in, X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = X0;
").

:- pragma foreign_proc("C",
    rand(R::out, X0::in, X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = xorshift32(X0);
    R = X;
").

:- pragma foreign_proc("C",
    rand_float(R::out, X0::in, X::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    X = xorshift32(X0);
    R = (MR_Float) X / (MR_Float) UINT32_MAX;
").

rand_range(Lo, Hi, R, !X) :-
    rand_float(R0, !X),
    R = Lo + (Hi - Lo) * R0.

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
