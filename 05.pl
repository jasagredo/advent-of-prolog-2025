:- use_module(dcgs/dcgs_utils).

range(X..Y) -->
    number(X), "-", number(Y), "\n".
ranges(X, Y) -->
    range(Z),
    ranges(X \/ Z, Y).
ranges(X,X) -->
    "\n".

newline_or_end, "\n" -->
    "\n"; call(eos).

numbers(_, Sol, Sol) --> call(eos).
numbers(Range, Cont, Sol) -->
    number(X),
    newline_or_end,
    { X in Range, Cont1 #= Cont + 1
    ; \+ X in Range, Cont1 #= Cont
    },
    "\n",
    numbers(Range, Cont1, Sol).

part1(Sol) :-
    file(F),
    phrase_from_file((ranges(1..0, Range),
                      numbers(Range, 0, Sol)), F).

part2(Sol) :-
    file(F),
    phrase_from_file((ranges(1..0, Range),
                      seq(_)), F),
    X in Range,
    fd_size(X, Sol).

file("05.txt").

run :-
    format("Part 1: ", []), time((part1(Sol1), write(Sol1), nl)),
    format("Part 2: ", []), time((part2(Sol2), write(Sol2), nl)),
    halt.

:- initialization(run).
