:- use_module(dcgs/dcgs_utils).
:- use_module(library(tabling)).

manifold(Max, S, A) -->
  vert(0, Max-S, t, A).

vert(Y, _-MaxY-_, A, A) -->
  call(eos), { MaxY #= Y - 1 }.
vert(Y, MS, A0, A) -->
  horiz(0-Y, MS, A0, A1),
  "\n",
  { Y1 #= Y + 1 },
  vert(Y1, MS, A1, A).

horiz(X-_, MaxX-_-_, A, A), "\n" -->
  ( "\n" ; call(eos) ), { MaxX #= X - 1 }.
horiz(X-Y, M-S, A0, A) -->
  pos(X-Y, S, A0, A1),
  { X1 #= X + 1 },
  horiz(X1-Y, M-S, A1, A).

pos(X-Y, Y-X, A, A) -->
  "S".
pos(_, _, A, A) -->
  ".".
pos(X-Y, _, A0, A) -->
  "^",
  { put_assoc(X-Y, A0, "^", A)}.

in_range(MaxX-MaxY, X-Y, T) :-
    X in 0..MaxX,
    Y in 0..MaxY,
    T = true.
in_range(MaxX-MaxY, X-Y, T) :-
    (
        MaxXX #= MaxX+1,
        X in (inf..(-1) \/ MaxXX..sup)
    ; MaxYY #= MaxY+1,
      Y in (inf..(-1) \/ MaxYY..sup)
    ),
    T = false.

push_if_in_range(Max, Y-X, A0, A1) :-
  in_range(Max, X-Y, true),
  put_assoc(Y-X, A0, beam, A1).
push_if_in_range(Max, Y-X, A, A) :-
  in_range(Max, X-Y, false).

tmember_assoc(X-Y, A, T) :-
    get_assoc(X-Y, A, _),
    T = true
    ;
    \+ get_assoc(X-Y, A, _),
    T = false.

run(_, _, t-Sol, Sol).
run(Max, Splits, Beams0-S0, S) :-
  del_min_assoc(Beams0, By-Bx, _, Beams1),
  By1 #= By + 1,
  if_(tmember_assoc(Bx-By1, Splits),
     (Bxm #= Bx - 1,
      Bxp #= Bx + 1,
      S1 #= S0 + 1,
      NB = [By1-Bxm, By1-Bxp]
     ),
     (S1 #= S0,
      NB = [By1-Bx]
     )
  ),
  foldl(push_if_in_range(Max), NB, Beams1, Beams2),
  run(Max, Splits, Beams2-S1, S).

:- table run2/4.

%% Hmmm:
%%
%%    Part 2: ....
%% % CPU time: 944.192s, 6_374_310_372 inferences
%%

run2(Max, Splits, X-Y, Sol) :-
    Y1 #= Y + 1,
    Max = _-MaxY,
    if_(Y1 = MaxY,
        Sol #= 1,
        if_(tmember_assoc(X-Y1, Splits),
            ( Xm #= X - 1,
              Xp #= X + 1,
              Bs = [Xm-Y1, Xp-Y1],
              tfilter(in_range(Max), Bs, Bs1),
              maplist(run2(Max, Splits), Bs1, Sol1),
              sum_list(Sol1, Sol)
            ),
            (B = X-Y1,
            run2(Max, Splits, B, Sol))
           )
        ).

file("07.txt").

main1 :-
  file(F),
  phrase_from_file(manifold(Max, Y-X, Splits), F),
  list_to_assoc([Y-X-beam], Beams),
  run(Max, Splits, Beams-0, Sol),
  write(Sol).

main2 :-
    file(F),
    phrase_from_file(manifold(Max, Y-X, Splits), F),
    run2(Max, Splits, X-Y, Sol),
    write(Sol).

run :-
    format("Part 1: ", []), time((main1, nl)),
    format("Part 2: ", []), time((main2, nl)),
    halt.

:- initialization(run).
