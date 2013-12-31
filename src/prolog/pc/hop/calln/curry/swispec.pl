% Machine generated file

append(_G197, append(_G197)).
evalNatural(_G197, evalNatural(_G197)).


:-@[A, B] :-
	apply(A, [B]).
:-@[A, B, C, D] :-
	:-@[A, B, E],
	:-@[E, C, D].
:-@[A, B, C, D, E] :-
	:-@[A, B, F],
	:-@[F, C, G],
	:-@[G, D, E].
:-@[A, B, C, D, E, F] :-
	:-@[A, B, G],
	:-@[G, C, H],
	:-@[H, D, I],
	:-@[I, E, F].
:-@[A, B, C, D, E, F, G] :-
	:-@[A, B, H],
	:-@[H, C, I],
	:-@[I, D, J],
	:-@[J, E, K],
	:-@[K, F, G].
:-@[A, B, C] :-
	A=..[D|E],
	append([D|E], [B, C], F),
	G=..F,
	(   clause(G, H)
	->  G
	;   :-@^(E, I, J, K),
	    K=..[D|I],
	    L=..[D|J],
	    asserta(L),
	    assert(pcCurryCode(L)),
	    G
	).

% - EOF -
