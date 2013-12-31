%------------------------------------------------------------------------------
%
%                                   Tuple
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module: Tuple
Text:   V knihovn� konstruktor� jsou n-tice reprezentov�ny posloupnost�
        polo�ek z�et�zen�ch oper�torem >/2 jako vhn�zd�n� dvojice. Soubor
        obsahuje predik�ty pro pr�ci s n-ticemi.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     *>*(+FunN, +FunR, +Input, -NewTuple)
Text:   Konstruktor n-tice vhodn� pro pou�it� v predik�tech vy���ho ��du,
        jako je mapList/3.
Example:
        ?- mapList(fstTuple *>* (const(a) *>* sndTuple),[1>2],R).
        R= [2> (a>1)]
        Yes
*/

*>*(FunN,FunR,I,(N>R)):-
        :-@[FunN,I,N],
        :-@[FunR,I,R].

%------------------------------------------------------------------------------
/**     fstTuple(+Tuple, -N)
Text:   Selektor vyd�vaj�c� prvn�ho polo�ku n-tice.
*/

fstTuple(N>_,N).

%------------------------------------------------------------------------------
/**     sndTuple(+Tuple, -R)
Text:   Selektor vyd�vaj�c� 't�lo' n-tice.
Example:
        ?- sndTuple(a>(b>c),R).
        R= (b>c)
        Yes
*/

sndTuple(_>R,R).

%------------------------------------------------------------------------------
/**     appendTuple(+Tuple1, +Tuple2, -Tuple)
Text:   Z�et�zen� n-tic Tuple1 a Tuple2.
*/

appendTuple(A,B,T):-
        A=(AH>S) -> appendTuple(S,B,SB),T=(AH>SB)
                 ;  T=(A>B).

%------------------------------------------------------------------------------
/**     memberTuple(?Member, ?Tuple)
Text:   Usp�je, pokud je Member v n-tici.
Example:
        ?- memberTuple(X,3>(2>1)).
        X= 3;
        X= 2;
        X= 1;
        No
*/

memberTuple(X,T):-
        T=(F>S) -> (F=X ; memberTuple(X,S))
                ;  T=X.
%------------------------------------------------------------------------------
/**     nthGetTuple(+N, +Tuple, -Item)
Text:   Vyd� N-tou polo�ku n-tice.
*/

nthGetTuple(N,_>S,V):-
        N>1, dec(N,NN), nthGetTuple(NN,S,V).
nthGetTuple(1,T,V):-
        T=(H>_) -> V=H ; V=T.

%------------------------------------------------------------------------------
/**     nthSetTuple(+N, +Item, +Tuple, -TupleI)
Text:   Nastav� N-tou polo�ku v n-tici.
*/

nthSetTuple(N,I,H>S,H>V):-
        N>1, dec(N,NN), nthSetTuple(NN,I,S,V).
nthSetTuple(1,I,T,V):-
        T=(_>S) -> V=(I>S) ; V=I.

%------------------------------------------------------------------------------
/**     tuple2List(?ResultTuple, ?List)
Example:
        ?- tuple2List((a>b),[a|b]).
        Yes
*/

tuple2List((R1>R2),[R1|R2]).
    
%------------------------------------------------------------------------------
/**     fstTupleEmpty(+Tuple)
Text:   Usp�je, pokud je prvn� polo�kou v n-tici pr�zdn� seznam.
*/

fstTupleEmpty([]>_).

%------------------------------------------------------------------------------
/**     sndTupleEmpty(+Tuple)
Text:   Usp�je, pokud je druhou polo�kou v n-tici pr�zdn� seznam.
*/

sndTupleEmpty(_>[]).

%- EOF ------------------------------------------------------------------------
