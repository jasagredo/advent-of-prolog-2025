:- use_module(dcgs/dcgs_utils).

spaces --> " ", spaces.
spaces --> "".

numbers([X|Xs]) -->
    spaces,
    number(X),
    numbers(Xs).
numbers([]), "\n" -->
    spaces,
    eol.

operands([X|Xs]) -->
    numbers(X),
    eol,
    operands(Xs).
operands([]) --> "".

ops([C|Ops]) -->
    [C],
    { char_type(C, graphic) },
    spaces,
    ops(Ops).
ops([]) --> eol.

apply(Op, A, B, Sol):-
    Z =.. [Op, A, B],
    Sol is Z.

foldl1(Op, [X|Xs], Sol) :-
    foldl(apply(Op), Xs, X, Sol).

\=(A, B, T) :-
    A = B, T = false
    ; A \= B, T = true.

num_from_chars(Num, Chars) :-
    tfilter(\=(' '), Chars, Chars1),
    Chars1 = [_|_],
    number_chars(Num, Chars1).

columns([], [[]]).
columns([X|Xs], [[Num|Y] | Ys]) :-
    num_from_chars(Num, X),
    columns(Xs, [Y|Ys]).
columns([_|Xs], [[]|Ys]) :-
    columns(Xs, Ys).

file("06.txt").

main1 :-
    file(F),
    phrase_from_file((operands(Xs),ops(Ops)), F),
    transpose(Xs, XsT),
    maplist(foldl1, Ops, XsT, Sols),
    sum_list(Sols, Sol),
    write(Sol).

main2 :-
    file(F),
    phrase_from_file(lines(Ls), F),
    reverse(Ls, [Ops0|Lss]),

    reverse(Lss, XsC),
    transpose(XsC, XsCT),
    columns(XsCT, Cols),
    tfilter(\=(' '), Ops0, Ops1),

    maplist(foldl1, Ops1, Cols, Sols),
    sum_list(Sols, Sol),
    write(Sol).

run :-
    format("Part 1: ", []), time((main1, nl)),
    format("Part 2: ", []), time((main2, nl)),
    halt.

:- initialization(run).
