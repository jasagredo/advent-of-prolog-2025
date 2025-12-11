:- use_module(dcgs/dcgs_utils).

boxes([X-Y-Z|Xs]) -->
    number(X),
    ",",
    number(Y),
    ",",
    number(Z),
    ("\n"; call(eos)),
    boxes(Xs).
boxes([]) -->
	"\n"; call(eos).

-(X, Y, X-Y).

take(0, _, []).
take(N, [X|Xs], [X|Ys]) :-
    N1 #= N - 1,
    take(N1, Xs, Ys).
size1(X, X-1).

% distance with itself is ignored
distance(ID1-_, ID1-_, A0, A0).
distance(ID1-(X1-Y1-Z1), ID2-(X2-Y2-Z2), A0, A1) :-
    Xs #= X1-X2,
    Ys #= Y1-Y2,
    Zs #= Z1-Z2,
    Dist #= Xs * Xs + Ys * Ys + Zs * Zs,
    put_assoc(Dist, A0, ID1-X1-ID2-X2, A1).

push_distances(Boxes, B, A0, A) :-
    foldl(distance(B), Boxes, A0, A), !.

replace(Throw, _, Throw-_-Throw-_, S, S).
replace(_, Keep, Keep-_-Keep-_, S, S).
replace(Throw, Keep, Throw-X-A-Y, S, [Keep-X-A-Y|S]).
replace(Throw, Keep, A-X-Throw-Y, S, [A-X-Keep-Y|S]).
replace(_, _, X, S, [X|S]).

run(AssocSizes, _, [], Sol) :-
    assoc_to_values(AssocSizes, Vals),
    sort(Vals, Vals1),
    reverse(Vals1, [X,Y,Z|_]),
    Sol #= X * Y * Z.
run(AssocSizes, Pre, [P-_-P-_|Post], Res) :-
    run(AssocSizes, Pre, Post, Res).
run(AssocSizes0, Pre, [P1-X-P2-Y|Post], Res) :-
    sort([P1,P2], [PKeep, PThrow]),
    foldl(replace(PThrow, PKeep), Pre, [], Pre1),
    foldl(replace(PThrow, PKeep), Post, [], Post1),
    reverse(Post1, Post2),
    reverse(Pre1, Pre2),
    get_assoc(PKeep, AssocSizes0, SKeep),
    del_assoc(PThrow, AssocSizes0, SThrow, AssocSizes1),
    SKeep1 #= SKeep + SThrow,
    put_assoc(PKeep, AssocSizes1, SKeep1, AssocSizes),
    run(AssocSizes, [PKeep-X-PKeep-Y|Pre2], Post2, Res).

main1 :-
    file(F),
    phrase_from_file(boxes(Boxes), F),
    length(Boxes, Size),
    numlist(1, Size, IDs),
    maplist(-, IDs, Boxes, TaggedBoxes),
    % TaggedBoxes = [1-(X1-Y1-Z1), 2-(X2-Y2-Z2), ...]
    foldl(push_distances(TaggedBoxes), TaggedBoxes, t, Distances),
    % assoc_to_list(Distances, [1-[id1-X1-id2-X2], 2-[id3-X3-id4-X4], ...])
    assoc_to_values(Distances, Links),
    % LinksFlat = [id1-id2, id3-id4, ...]
    take(1000, Links, Links1000),
    maplist(size1, IDs, Sizes),
    % Sizes = [id1-1, id2-1, id3-1, ...]
    list_to_assoc(Sizes, AssocSizes),
    run(AssocSizes, [], Links1000, FinalLinks),
    write(FinalLinks).

run2(Pre, [P-_-P-_|Post], Res) :-
    run2(Pre, Post, Res).
run2(Pre, [P1-X-P2-Y|Post], Sol) :-
    sort([P1,P2], [PKeep, PThrow]),
    foldl(replace(PThrow, PKeep), Pre, [], Pre1),
    foldl(replace(PThrow, PKeep), Post, [], Post1),
    reverse(Post1, Post2),
    reverse(Pre1, Pre2),
    ( maplist(maplist(PKeep+\(P-_-P-_)^(P = PKeep)), [Pre2,Post2]),
      Sol #= X * Y
    ;
    run2([PKeep-X-PKeep-Y|Pre2], Post2, Sol)).

main2 :-
    file(F),
    phrase_from_file(boxes(Boxes), F),
    length(Boxes, Size),
    numlist(1, Size, IDs),
    maplist(-, IDs, Boxes, TaggedBoxes),
    foldl(push_distances(TaggedBoxes), TaggedBoxes, t, Distances),
    assoc_to_values(Distances, Links),
    time(run2([], Links, Sol)),
    write(Sol).

file("08.txt").

run :-
    format("Part 1: ", []), time((main1, nl)),
    format("Part 2: ", []), time((main2, nl)),
    halt.

:- initialization(run).
