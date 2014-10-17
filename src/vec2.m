%-----------------------------------------------------------------------------%

:- module vec2.
:- interface.

:- type vec2
    --->    vec2(float, float).

:- func zero = vec2.

:- func cos_sin(float) = vec2.

:- func float * vec2 = vec2.

:- func vec2 + vec2 = vec2.

:- func vec2 - vec2 = vec2.

:- func dot(vec2, vec2) = float.

:- func length(vec2) = float.

:- func norm(vec2) = vec2.

:- func cap(float, vec2) = vec2.

:- func atan2(vec2) = float.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module float.
:- import_module math.

zero = vec2(0.0, 0.0).

cos_sin(X) = vec2(cos(X), sin(X)).

K * vec2(X, Y) = vec2(K*X, K*Y).

vec2(X1, Y1) + vec2(X2, Y2) = vec2(X1 + X2, Y1 + Y2).

vec2(X1, Y1) - vec2(X2, Y2) = vec2(X1 - X2, Y1 - Y2).

dot(vec2(X1, Y1), vec2(X2, Y2)) = X1*X2 + Y1*Y2.

length(V) = sqrt(dot(V, V)).

norm(V) = 1.0/length(V) * V.

cap(Max, V) =
    ( length(V) =< Max ->
        V
    ;
        Max * norm(V)
    ).

atan2(vec2(X, Y)) = atan2(Y, X).

%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sts=4 sw=4 et
