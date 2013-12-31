%------------------------------------------------------------------------------
%
%                       Seznamove a mnozinove operace
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Seznamové a mno¾inové operace.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                             Mnozinove operace
%------------------------------------------------------------------------------
%       Mnoziny jsou reprezentovany jako usporadane prologovske seznamy.
%------------------------------------------------------------------------------
/**     checkSet(+Set)
Text:   Ovìøí, zda je Set mno¾inou. V tomto pøípadì uspìje, v pøípadì opaèném
        predikát sel¾e.
*/

checkSet([H|T]):-
        member(H,T) -> fail ; isSet(T).
checkSet([]).

%------------------------------------------------------------------------------
/**     list2Set(+List, -Set)
Text:   Konvertuje mno¾inu na seznam.
*/

list2Set(L,S):-
        sort(L,S).

%------------------------------------------------------------------------------
/**     subSet(+Subset, +Set)
Text:   Predikát uspìje, pokud je SubSet podmno¾inou mno¾iny Set.
*/

subSet([H|T],S):-
        memberCk(H,S) -> subSet(T,S).
subSet([],_).

%------------------------------------------------------------------------------
/**     disjuctSets(+Set1, +Set2)
Text:   Predikát uspìje, pokud jsou mno¾iny Set1 a Set2 disjunktní.
*/

disjunctSets([H|T],S):-
        memberCk(H,S) -> fail ; disjunctSets(T,S).
disjunctSets([],_).

%------------------------------------------------------------------------------
/**     unionSets(+Set1, +Set2, -Set12)
Text:   Sjednocení mno¾in.
*/

unionSets([H|T],S2,S):-
        unionSets(T,S2,Ss),
        (memberCk(H,S2) -> S=Ss ; S=[H|Ss]).
unionSets([],S2,S2).

%------------------------------------------------------------------------------
/**     intersectionSets(+Set1, +Set2, -Set12)
Text:   Prùnik mno¾in.
*/

intersectionSets([H|T],S2,S):-
        intersectionSets(T,S2,Ss),
        (memberCk(H,S2) -> S=[H|Ss] ; S=Ss).
intersectionSets([],_,[]).

%------------------------------------------------------------------------------
/**     substractSets(+Set1, +Set2, -Set12)
Text:   Od mno¾iny Set1 je odeètena mno¾ina Set2.
*/

substractSets(S1,[H|T],S):-
        deleteList(S1,H,S1D),
        substractSets(S1D,T,S).
substractSets(R,[],R).

%------------------------------------------------------------------------------
%                              Seznamove operace
%------------------------------------------------------------------------------
% length(?List, ?Int)
%       Uspeje, pokud Int je pocet prvnku v seznamu List. Predikat muze byt
% pouzit k vytvoreni seznamu, ktery obsahuje pouze promenne.
%------------------------------------------------------------------------------
/**     quickSort(+Input, -Sorted)
Text:   Tøídìní seznamu termù Input. Pro porovnávání termù jsou pou¾ívány
        operátory standardního uspoøádání termù (jako @</2).
*/
quickSort(Input,Sorted):-
    quickSortD(Input,Sorted-[]).		 

quickSortD([],T-T).    
quickSortD([Pivot|Rest],LessSorted-BS):-
    split(Pivot,Rest,Less,Bigger),
    quickSortD(Less,LessSorted-[Pivot|LS]), 
    quickSortD(Bigger,LS-BS),!.

% split(Pivot,Rest,Less,Bigger)
split(_,[],[],[]).
split(Pivot,[X|Rest],[X|Less],Bigger):-
    X@<Pivot  -> split(Pivot,Rest,Less,Bigger).
split(Pivot,[X|Rest],Less,[X|Bigger]):-
    X@>=Pivot -> split(Pivot,Rest,Less,Bigger).

%------------------------------------------------------------------------------
/**     quickSortG(+Fun, +Input, -Sorted)
Text:   Tøídìní seznamu libovolných termù Input.
Arg: Fun
        Predikát pou¾ívaný k porovnávání prvkù. Pokud Fun(a,b) uspìje
        je tento výsledek pou¾it jako a<b.
*/

% General quicksort
% P is predicate name used for matching items F(a,b) === a<b  yes/no
% *WARNING* : items of the same value are reversed
quickSortG(F,Input,Sorted):-
    quickSortDG(F,Input,Sorted-[]).		 

quickSortDG(_,[],T-T).    
quickSortDG(F,[Pivot|Rest],LessSorted-BS):-
    splitG(F,Pivot,Rest,Less,Bigger),
    quickSortDG(F,Less,LessSorted-[Pivot|LS]), 
    quickSortDG(F,Bigger,LS-BS),!.

% split(Pivot,Rest,Less,Bigger)
splitG(_,_,[],[],[]).
splitG(F,Pivot,[X|Rest],OLess,OBigger):-
    splitG(F,Pivot,Rest,Less,Bigger),
    (
     :-@([F,X,Pivot])
      ->
     OLess=[X|Less],OBigger=Bigger
      ;
     OLess=Less,OBigger=[X|Bigger]
    ).

%------------------------------------------------------------------------------
/**     memberCk(?Elem, +List)
Text:   Ekvivalent member/2, který nezanechává choice pointy.
*/

memberCk(E,L):-
        pcProlog(swi) -> memberchk(E,L)
                      ;  member(E,L),!.

%------------------------------------------------------------------------------
/**     reverseList(+List, -ReverseList)
Text:   Obrácení poøadí prvkù v seznamu List.
*/

reverseList(L,RL):-
        pcProlog(swi) -> reverse(L,RL)
                      ;  reverseList(L,[],RL).
reverseList_([],RL,RL).
reverseList_([H|T],U,R):-
        reverseList_(T,[H|U],R).

%------------------------------------------------------------------------------
/**     deleteList(+List, ?Elem, ?DelList)
Text:   Odstranìní prvkù Elem ze seznamu List.
*/

deleteList(L,E,DL):-
        pcProlog(swi) -> delete(L,E,DL)
                      ;  deleteList_(L,E,DL).
deleteList_([H|T],E,DT):-
        H=E -> deleteList(T,H,DTs),DT=DTs ; deleteList(T,H,DTs),DT=[E|DTs].
deleteList_([],_,[]).

%------------------------------------------------------------------------------
/**     nthMember(+N, +List, ?Elem)
Text:   Ovìøí resp. vydá N-tou polo¾ku seznamu.
*/

nthMember(N,[_|T],E):-
        N>1 -> dec(N,NN), nthMember(NN,T,E).
nthMember(1,[E|_],E).

%------------------------------------------------------------------------------
/**     flattenList(+List, -FlattenList)
Text:   Narovná prologovský seznam obecných prvkù.
*/

flattenList(I,O):-
        (I=[H|T] ->
         (isList(H) -> flattenList(H,OX),
                       flattenList(T,OT),
                       append(OX,OT,O)
                    ;  flattenList(T,Tt),
                       O=[H|Tt]))
         ;
        (I=[H] ->           % jeden element
         (isList(H) -> O=I
                    ;  flattenList(H,O)))
         ;
        I=[] -> O=[].

%- EOF ------------------------------------------------------------------------
