:- use_module(dcgs/dcgs_utils).

horiz(F, X-Y, S0, S) -->
    call(F, X-Y, S0, S1),
    { X1 #= X + 1 },
    horiz(F, X1-Y, S1, S).
horiz(_, _, S, S), "\n" -->
    "\n"; call(eos).

vert(_, _, S, S) -->
    "\n"; call(eos).
vert(F, Y, S0, S) -->
    horiz(F, 0-Y, S0, S1),
    "\n",
    { Y1 #= Y + 1 },
    vert(F, Y1, S1, S).

matrix(F, S0, S) -->
    vert(F, 0, S0, S).

cell(X-Y, Papers0, Papers) -->
    "@",
    { put_assoc(X-Y, Papers0, '@', Papers) }.
cell(_, Papers, Papers) --> ".".

surrounding(X-Y, Z) :-
    Xp #= X + 1,
    Xm #= X - 1,
    Yp #= Y + 1,
    Ym #= Y - 1,
    Z = [Xp-Y, Xp-Yp, X-Yp, Xm-Yp, Xm-Y, Xm-Ym, X-Ym, Xp-Ym].

is_paper(Papers, X-Y, 1) :-
    get_assoc(X-Y, Papers, _).
is_paper(Papers, X-Y, 0) :-
    \+ get_assoc(X-Y, Papers, _).

tunblocked_paper(Papers, X-Y-_, T) :-
    surrounding(X-Y, Surrounding),
    maplist(is_paper(Papers), Surrounding, SurroundingPapers),
    sum_list(SurroundingPapers, Count),
    ( Count #>= 4, T = false
    ; Count #< 4, T = true
    ).

part1 :-
    file(F),
    phrase_from_file(matrix(cell, t, Papers), F),
    assoc_to_list(Papers, List),
    tfilter(tunblocked_paper(Papers), List, BlockedPapers),
    length(BlockedPapers, Sol),
    write(Sol).

del_assoc_(X-Y-_, Papers0, Papers1) :-
    del_assoc(X-Y, Papers0, _, Papers1).

step(Papers, Papers2) :-
    assoc_to_list(Papers, List),
    tfilter(tunblocked_paper(Papers), List, Removed),
    ( Removed = [],
      Papers2 = Papers
    ;
      foldl(del_assoc_, Removed, Papers, Papers1),
      step(Papers1, Papers2)
    ).

part2 :-
    file(F),
    phrase_from_file(matrix(cell, t, Papers), F),
    step(Papers, Papers1),
    assoc_to_list(Papers, L1),
    length(L1, LL1),
    assoc_to_list(Papers1, L2),
    length(L2, LL2),
    Sol #= LL1 - LL2,
    write(Sol).

file("04.txt").

run :-
    format("Part 1: ", []), time((part1, nl)),
    format("Part 2: ", []), time((part2, nl)),
    halt.

:- initialization(run).
