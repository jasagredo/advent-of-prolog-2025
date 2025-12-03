:- use_module(dcgs/dcgs_utils).

digit(X) -->
    [C],
    { char_type(C, numeric),
      number_chars(X, [C])
    }.

list(F, [X|Xs]) -->
    call(F, X),
    list(F, Xs).
list(_, []), "\n" -->
    "\n"; call(eos).

lists([]) -->
    "\n"; call(eos).
lists([X|Xs]) -->
    list(digit, X),
    "\n",
    lists(Xs).

digits_to_number(X, Jolt0, Jolt) :-
    Jolt #= Jolt0 * 10 + X.

del_if_smaller_than_next([X,Y], [X]) :-
    X #>= Y.
del_if_smaller_than_next([X,Y], [Y]) :-
    Y #> X.
del_if_smaller_than_next([X,Y|Rest], [Y|Rest]) :-
    X #< Y.
del_if_smaller_than_next([X,Y|Rest], [X|Res]) :-
    del_if_smaller_than_next([Y|Rest], Res).

joltage(L, X, Jolt) :-
    reverse(X, XR),
    length(Jolt0, L),
    % take last L elements as list of digits
    append(Jolt0, Cells, XR),
    reverse(Jolt0, Jolt1),

    joltage_(Jolt1, Cells, BestMatch),
    foldl(digits_to_number, BestMatch, 0,Jolt).
joltage_(Best, [], Best).
joltage_([Head|JoltT], [Cell|Cells], Best) :-
    del_if_smaller_than_next([Cell,Head|JoltT], Jolt1),
    joltage_(Jolt1, Cells, Best).

file("03.txt").

main(L) :-
    file(F),
    phrase_from_file(lists(Xs), F),
    maplist(joltage(L), Xs, Ys),
    sum_list(Ys, Sol),
    write(Sol).

run :-
    format("Part 1: ", []), time((main(2), nl)),
    format("Part 2: ", []), time((main(12), nl)),
    halt.

:- initialization(run).
