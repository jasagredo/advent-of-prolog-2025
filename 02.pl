:- use_module(dcgs/dcgs_utils).

range(X-Y) -->
    numberC(X),
    "-",
    numberC(Y).
ranges([X]) -->
    range(X),
    ( "\n"; ""),
    call(eos).
ranges([X|Xs]) -->
    range(X),
    ",",
    ranges(Xs).

solve(NumReps_1, X-Y, Sol) :-
    call(NumReps_1, NumReps),
    maplist(p_solve(X-Y), NumReps, InvalidIDs0),
    append(InvalidIDs0, InvalidIDs1),
    list_to_set(InvalidIDs1, InvalidIDs),
    sum_list(InvalidIDs, Sol).

lower_bound(First, [], First).
lower_bound(First, [Chunk|_], Res) :-
    Chunk #> First, Res #= First + 1.
lower_bound(First, [Chunk|_], First) :-
    Chunk #< First.
lower_bound(First, [First|Chunks], Res) :-
    lower_bound(First, Chunks, Res).

upper_bound(First, [], First).
upper_bound(First, [Chunk|_], First) :-
    Chunk #> First.
upper_bound(First, [Chunk|_], Res) :-
    Chunk #< First, Res #= First - 1.
upper_bound(First, [First|Chunks], Res) :-
    upper_bound(First, Chunks, Res).

p_solve(_-Y, P, []) :-
    length(Y, LenY),
    P #> LenY.
p_solve(X-Y, P, Result) :-
    length(X, LenX),
    LenXChunk #= LenX // P,
    ( LenX mod P #= 0,
      length(XChunks0, P),
      maplist(length_is(LenXChunk), XChunks0),
      append(XChunks0, X),
      maplist(number_chars, [XFirst|XChunks], XChunks0),
      lower_bound(XFirst, XChunks, LowerBound)
    ;
      LenX mod P #\= 0,
      length(Zeroes, LenXChunk),
      maplist(=('0'),Zeroes),
      number_chars(LowerBound, ['1'|Zeroes])
    ),

    length(Y, LenY),
    LenYChunk #= LenY // P,
    ( LenY mod P #= 0,
      length(YChunks0, P),
      maplist(length_is(LenYChunk), YChunks0),
      append(YChunks0, Y),
      maplist(number_chars, [YFirst|YChunks], YChunks0),
      upper_bound(YFirst, YChunks, UpperBound)
    ;
      LenY mod P #\= 0,
      length(Nines, LenYChunk),
      maplist(=('9'),Nines),
      number_chars(UpperBound, Nines)
    ),

    (
      LowerBound #=< UpperBound,
      numlist(LowerBound, UpperBound, Matches)
    ;
      LowerBound #> UpperBound,
      Matches = []
    ),

    maplist(p_plicate(P), Matches, Result).

p_plicate(P, X, Y) :-
    number_chars(X, Xs),
    length(Xxs, P),
    maplist(=(Xs), Xxs),
    append(Xxs, X1),
    number_chars(Y, X1).

length_is(L, X) :- length(X, L).
flip(F, X, Y) :- call(F, Y, X).

primes([2,3,5,7,11,13,17]).
only_2([2]).
file("02.txt").

main(F) :-
    file(File),
    phrase_from_file(ranges(X), File),
    maplist(solve(F), X, Y),
    sum_list(Y,Sol),
    write(Sol).

run :-
    format("Part 1: ", []), time((main(only_2), nl)),
    format("Part 1: ", []), time((part1_faster, nl)),
    format("Part 2: ", []), time((main(primes), nl)),
    halt.

:- initialization(run).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_fast_2(X-Y, Result) :-
    length(X, LenX),
    ( LenX mod 2 #= 0,
      LenXChunk #= LenX // 2,
      length(XFirst, LenXChunk),
      append(XFirst, XLast, X),
      number_chars(XFirstN, XFirst),
      number_chars(XLastN, XLast),
      ( XLastN #> XFirstN, LowerBound #= XFirstN + 1
      ; XLastN #=< XFirstN, LowerBound #= XFirstN
      )
    ;
      LenX mod 2 #= 1,
      LenXChunk #= LenX // 2,
      length(Zeroes, LenXChunk),
      maplist(=('0'),Zeroes),
      number_chars(LowerBound, ['1'|Zeroes])
    ),

    length(Y, LenY),
    ( LenY mod 2 #= 0,
      LenYChunk #= LenY // 2,
      length(YFirst, LenYChunk),
      append(YFirst, YLast, Y),
      number_chars(YFirstN, YFirst),
      number_chars(YLastN, YLast),
      ( YLastN #>= YFirstN, UpperBound #= YFirstN
      ; YLastN #< YFirstN, UpperBound #= YFirstN - 1
      )

    ;
      LenY mod 2 #= 1,
      LenYChunk #= LenY // 2,
      length(Nines, LenYChunk),
      maplist(=('9'),Nines),
      number_chars(UpperBound, Nines)
    ),

    ( LowerBound #=< UpperBound,
      numlist(LowerBound, UpperBound, Matches)
    ;
      LowerBound #> UpperBound,
      Matches = []
    ),

    maplist(duplicate, Matches, Matches1),
    sum_list(Matches1, Result).

duplicate(X, Y) :-
    number_chars(X, Xs),
    append(Xs, Xs, X1),
    number_chars(Y, X1).

part1_faster :-
    file(F),
    phrase_from_file(ranges(X), F),
    maplist(solve_fast_2, X, Y),
    sum_list(Y,Sol),
    write(Sol).
