:- use_module(dcgs/dcgs_utils).
:- use_module(library(debug)).

tile(X-Y), "\n" -->
    number(X), ",", number(Y), eol.

tiles([]) --> call(eos).
tiles([X|Xs]) -->
    tile(X), "\n", tiles(Xs).

file("09.txt").

distance(Tx-Ty, X-Y, D) :-
    D #= (abs(X-Tx) + 1) * (abs(Y-Ty) + 1).

max_area([], _, S, S).
max_area([Tx-Ty|Tiles], X-Y, S0, S) :-
    distance(Tx-Ty, X-Y, D),
    ( D #> S0, S1 #= D
    ; D #=< S0, S1 #= S0
    ),
    max_area(Tiles, X-Y, S1, S).

main1 :-
    file(F),
    phrase_from_file(tiles(Tiles), F),
    foldl(max_area(Tiles), Tiles, 0, Sol),
    write(Sol).

horiz(X-Y) -->
    ..., [X1-Y, X2-Y], { X in X1..X2; X in X2..X1 }, ... .

vert(X-Y) -->
    ..., [X-Y1, X-Y2], { Y in Y1..Y2; Y in Y2..Y1 }, ... .


distance_(Tx-Ty, X-Y, D-(Tx-Ty)-(X-Y)) :-
    distance(Tx-Ty, X-Y, D).

push(D-(Tx-Ty)-(X-Y), A0, A) :-
    get_assoc(D, A0, L),
    put_assoc(D, A0, [(Tx-Ty)-(X-Y)|L], A)
    ; put_assoc(D, A0, [(Tx-Ty)-(X-Y)], A).

run([], A, A).
run([Tx-Ty|Tiles], Dists0, Res) :-
    maplist(distance_(Tx-Ty), Tiles, Dists1),
    foldl(push, Dists1, Dists0, Dists2),
    run(Tiles, Dists2, Res).

% making __all__ the assumptions, we saw the entry and found the 2 corners on the
% right of the corridor.
find_max(_, [], 0).
find_max(Tiles, [Dist-(X1-Y1-(X2-Y2))|Dists], Sol) :-
    (select(94645-50248, [X1-Y1, X2-Y2], [X3-Y3]),
     X3 #\= 94645, Y3 #> 50248,
     \+ phrase(inside(94645-50248, X3-Y3), Tiles)
    ;
    select(94645-48530, [X1-Y1, X2-Y2], [X3-Y3]),
    X3 #\= 94645, Y3 #< 48530,
    \+ phrase(inside(X3-Y3, 94645-48530), Tiles)
    ),
    Sol #= Dist
    ; find_max(Tiles, Dists, Sol).

inside(X1-Y1, X2-Y2) -->
    { sort([X1,X2], [Xmin, Xmax]),
      sort([Y1,Y2], [Ymin, Ymax]),
      Xminp #= Xmin + 1,
      Xmaxm #= Xmax - 1,
      Yminp #= Ymin + 1,
      Ymaxm #= Ymax - 1,
      X3 in Xminp..Xmaxm,
      Y3 in Yminp..Ymaxm
    },
    ... , [X3-Y3], ... .

-(A, B, A-B).

flatten([], []).
flatten([D-L|Dists0], Dists) :-
    maplist(-(D), L, DL),
    flatten(Dists0, Dists1),
    append(DL, Dists1, Dists).

main2 :-
    file(F),
    phrase_from_file(tiles(Tiles), F),
    run(Tiles, t, Dists),
    assoc_to_list(Dists, Vals),
    reverse(Vals, Vals1),
    flatten(Vals1, Vals2),
    Tiles = [X|_],
    append(Tiles, [X], Tiles1),
    find_max(Tiles1, Vals2, Sol),
    write(Sol).

run :-
    format("Part 1: ", []), time((main1, nl)),
    format("Part 2: ", []), time((main2, nl)),
    halt.

:- initialization(run).
