%------------------------------------------------------------------------------
%
%                                   Tuple
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module: Tuple
Text:   V knihovnì konstruktorù jsou n-tice reprezentovány posloupností
        polo¾ek zøetìzených operátorem >/2 jako vhnízdìné dvojice. Soubor
        obsahuje predikáty pro práci s n-ticemi.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     *>*(+FunN, +FunR, +Input, -NewTuple)
Text:   Konstruktor n-tice vhodný pro pou¾ití v predikátech vy¹¹ího øádu,
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
Text:   Selektor vydávající prvního polo¾ku n-tice.
*/

fstTuple(N>_,N).

%------------------------------------------------------------------------------
/**     sndTuple(+Tuple, -R)
Text:   Selektor vydávající 'tìlo' n-tice.
Example:
        ?- sndTuple(a>(b>c),R).
        R= (b>c)
        Yes
*/

sndTuple(_>R,R).

%------------------------------------------------------------------------------
/**     appendTuple(+Tuple1, +Tuple2, -Tuple)
Text:   Zøetìzení n-tic Tuple1 a Tuple2.
*/

appendTuple(A,B,T):-
        A=(AH>S) -> appendTuple(S,B,SB),T=(AH>SB)
                 ;  T=(A>B).

%------------------------------------------------------------------------------
/**     memberTuple(?Member, ?Tuple)
Text:   Uspìje, pokud je Member v n-tici.
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
Text:   Vydá N-tou polo¾ku n-tice.
*/

nthGetTuple(N,_>S,V):-
        N>1, dec(N,NN), nthGetTuple(NN,S,V).
nthGetTuple(1,T,V):-
        T=(H>_) -> V=H ; V=T.

%------------------------------------------------------------------------------
/**     nthSetTuple(+N, +Item, +Tuple, -TupleI)
Text:   Nastaví N-tou polo¾ku v n-tici.
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
Text:   Uspìje, pokud je první polo¾kou v n-tici prázdný seznam.
*/

fstTupleEmpty([]>_).

%------------------------------------------------------------------------------
/**     sndTupleEmpty(+Tuple)
Text:   Uspìje, pokud je druhou polo¾kou v n-tici prázdný seznam.
*/

sndTupleEmpty(_>[]).

%- EOF ------------------------------------------------------------------------
