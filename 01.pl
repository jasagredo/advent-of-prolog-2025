:- use_module(dcgs/dcgs_utils).

part1(_, Cnt) --> call(eos), {write(Cnt)}.
part1(Pos, Cnt) -->
    "L",
    number(N),
    { Pos1 #= (Pos - N) mod 100,
      (Pos1 #= 0, Cnt1 #= Cnt + 1 ;
       Pos1 #\= 0, Cnt1 #= Cnt
      )
    },
    eol,
    part1(Pos1, Cnt1).
part1(Pos, Cnt) -->
    "R",
    number(N),
    { Pos1 #= (Pos + N) mod 100,
      (Pos1 #= 0, Cnt1 #= Cnt + 1 ;
       Pos1 #\= 0, Cnt1 #= Cnt
      )
    },
    eol,
    part1(Pos1, Cnt1).

part2(_, Cnt) --> call(eos), {write(Cnt)}.
part2(Pos, Cnt) -->
    "L",
    number(N),
    { Cents #= N // 100,
      Decs #= N mod 100,
      Cnt1 #= Cnt + Cents,
      Pos1 #= (Pos - Decs) mod 100,
      (
          (Pos #\= 0, (Pos1 #> Pos ; Pos1 #= 0)), Cnt2 #= Cnt1 + 1
      ;
          (Pos #= 0; Pos1 #< Pos), Cnt2 #= Cnt1
      )
    },
    eol,
    part2(Pos1, Cnt2).
part2(Pos, Cnt) -->
    "R",
    number(N),
    { Cents #= N // 100,
      Decs #= N mod 100,
      Cnt1 #= Cnt + Cents,
      Pos1 #= (Pos + Decs) mod 100,
      (
          (Pos #\= 0, (Pos1 #< Pos ; Pos1 #= 0)), Cnt2 #= Cnt1 + 1
      ;
          (Pos #= 0; Pos1 #> Pos), Cnt2 #= Cnt1
      )
    },
    eol,
    part2(Pos1, Cnt2).

main(F) :- file(File), phrase_from_file(call(F, 50, 0), File).

file("01.txt").

run :-
    format("Part 1: ", []), time((main(part1), nl)),
    format("Part 2: ", []), time((main(part2), nl)),
    halt.

:- initialization(run).
