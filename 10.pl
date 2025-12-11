:- use_module(dcgs/dcgs_utils).

machine(m(Target, Buttons, Joltages)) -->
    target(Target),
    " ",
    { length(Target, LT)},
    buttons(LT, Buttons),
    " ",
    joltage(Joltages).
machines([M|Ms]) -->
    machine(M), "\n",
    machines(Ms).
machines([]) --> ("\n"; ""), call(eos).

to_light('.', 0).
to_light('#', 1).

target(Target) --> "[", seq(X), "]", { maplist(to_light, X, Target) }.

not_comma(',', false).
not_comma(_, true).

number_digit(X, Y) :-
    number_chars(X, [Y]).

button(LT, B) -->
    "(", seq(X), ")",
    {
      tfilter(not_comma, X, X1),
      length(X1, LX),
      LTX #= LT - LX,
      length(Offs, LTX),
      LX #>= 1,
      length(B, LT),
      LT1 #= LT - 1,
      numlist(0, LT1, NumL),
      maplist(number_digit, X2, X1),
      tpartition(\Z^memberd_t(Z,X2), NumL, X2, Offs),
      maplist(B+\Z^nth0(Z, B, 1), X2),
      maplist(B+\Z^nth0(Z, B, 0), Offs)
    }.

buttons(LT, [B|Bs]) -->
    button(LT, B), " ",
    buttons(LT, Bs).
buttons(_, []), " " --> "".

comma_separated([X|Xs]) -->
    number(X), ",",
    comma_separated(Xs).
comma_separated([X]) --> number(X).

joltage(X) --> "{", comma_separated(X), "}".

end --> "".

*(A, B, A*B).
+(A, B, A+B).
#(A, B, A#B).

recurse([], _, [], 1).
recurse([L1|Ls], Vars, [T|Ts], (Expr =:= T) * E) :-
    maplist(*, L1, Vars, Facts),
    foldl1(#, Facts, Expr),
    recurse(Ls, Vars, Ts, E).

var_t(X, T) :-
  catch((integer_si(X), T = true), _, T = false).

run_machine(m(T, B, _), N) :-
    length(B, L),
    length(Vars, L),
    transpose(B, BT),
    recurse(BT, Vars, T, E),
    sat(E),
    ( tpartition(var_t, Vars, _, YY),
      YY = [_|_],
      length(YY, LU),
      length(Weights, LU),
      maplist(=(-1), Weights),
      weighted_maximum(Weights, YY, _),
      sum_list(Vars, N)
    ; sum_list(Vars, N)
    ).

main1 :-
    file(F),
    phrase_from_file(machines(Ms), F),
    maplist(run_machine, Ms, N),
    sum_list(N, Sol),
    write(Sol).

foldl1(F, [X|Xs], S) :-
    foldl(F, Xs, X, S).

post_constraints([C|Cs], Vars, [T|Targets]) -->
    { maplist(*, C, Vars, Vs) },
    constraint(Vs = T),
    post_constraints(Cs, Vars, Targets).
post_constraints([], _, []) --> end.

post_positive([V|Vars]) -->
    constraint([V] >= 0),
    constraint(integral(V)),
    post_positive(Vars).
post_positive([]) --> end.

n_pos_int_vars(N, Vars) -->
    { numlist(N, Idxs),
      maplist(\I^V^(
                  number_chars(I, C),
                  atom_chars(V, ['x'|C])
              ), Idxs, Vars)
    },
    post_positive(Vars).

run_machine2(m(_, B, T), Sol) :-
    length(B, L),
    gen_state(S0),
    n_pos_int_vars(L, Vars, S0, S1),
    transpose(B, BT),
    post_constraints(BT, Vars, T, S1, S2),
    minimize(Vars, S2, S),
    objective(S, Sol).

main2 :-
    file(F),
    phrase_from_file(machines(Ms), F),
    maplist(run_machine2, Ms, N),
    sum_list(N, Sol),
    write(Sol).

file("10.txt").

run :-
    format("Part 1: ~n", []), time((main1, nl)),
    format("Part 2: ", []), time((main2, nl)),
    halt.

:- initialization(run).
