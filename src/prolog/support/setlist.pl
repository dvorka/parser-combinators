%------------------------------------------------------------------------------
%
%                       Seznamove a mnozinove operace
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Seznamov� a mno�inov� operace.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                             Mnozinove operace
%------------------------------------------------------------------------------
%       Mnoziny jsou reprezentovany jako usporadane prologovske seznamy.
%------------------------------------------------------------------------------
/**     checkSet(+Set)
Text:   Ov���, zda je Set mno�inou. V tomto p��pad� usp�je, v p��pad� opa�n�m
        predik�t sel�e.
*/

checkSet([H|T]):-
        member(H,T) -> fail ; isSet(T).
checkSet([]).

%------------------------------------------------------------------------------
/**     list2Set(+List, -Set)
Text:   Konvertuje mno�inu na seznam.
*/

list2Set(L,S):-
        sort(L,S).

%------------------------------------------------------------------------------
/**     subSet(+Subset, +Set)
Text:   Predik�t usp�je, pokud je SubSet podmno�inou mno�iny Set.
*/

subSet([H|T],S):-
        memberCk(H,S) -> subSet(T,S).
subSet([],_).

%------------------------------------------------------------------------------
/**     disjuctSets(+Set1, +Set2)
Text:   Predik�t usp�je, pokud jsou mno�iny Set1 a Set2 disjunktn�.
*/

disjunctSets([H|T],S):-
        memberCk(H,S) -> fail ; disjunctSets(T,S).
disjunctSets([],_).

%------------------------------------------------------------------------------
/**     unionSets(+Set1, +Set2, -Set12)
Text:   Sjednocen� mno�in.
*/

unionSets([H|T],S2,S):-
        unionSets(T,S2,Ss),
        (memberCk(H,S2) -> S=Ss ; S=[H|Ss]).
unionSets([],S2,S2).

%------------------------------------------------------------------------------
/**     intersectionSets(+Set1, +Set2, -Set12)
Text:   Pr�nik mno�in.
*/

intersectionSets([H|T],S2,S):-
        intersectionSets(T,S2,Ss),
        (memberCk(H,S2) -> S=[H|Ss] ; S=Ss).
intersectionSets([],_,[]).

%------------------------------------------------------------------------------
/**     substractSets(+Set1, +Set2, -Set12)
Text:   Od mno�iny Set1 je ode�tena mno�ina Set2.
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
Text:   T��d�n� seznamu term� Input. Pro porovn�v�n� term� jsou pou��v�ny
        oper�tory standardn�ho uspo��d�n� term� (jako @</2).
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
Text:   T��d�n� seznamu libovoln�ch term� Input.
Arg: Fun
        Predik�t pou��van� k porovn�v�n� prvk�. Pokud Fun(a,b) usp�je
        je tento v�sledek pou�it jako a<b.
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
Text:   Ekvivalent member/2, kter� nezanech�v� choice pointy.
*/

memberCk(E,L):-
        pcProlog(swi) -> memberchk(E,L)
                      ;  member(E,L),!.

%------------------------------------------------------------------------------
/**     reverseList(+List, -ReverseList)
Text:   Obr�cen� po�ad� prvk� v seznamu List.
*/

reverseList(L,RL):-
        pcProlog(swi) -> reverse(L,RL)
                      ;  reverseList(L,[],RL).
reverseList_([],RL,RL).
reverseList_([H|T],U,R):-
        reverseList_(T,[H|U],R).

%------------------------------------------------------------------------------
/**     deleteList(+List, ?Elem, ?DelList)
Text:   Odstran�n� prvk� Elem ze seznamu List.
*/

deleteList(L,E,DL):-
        pcProlog(swi) -> delete(L,E,DL)
                      ;  deleteList_(L,E,DL).
deleteList_([H|T],E,DT):-
        H=E -> deleteList(T,H,DTs),DT=DTs ; deleteList(T,H,DTs),DT=[E|DTs].
deleteList_([],_,[]).

%------------------------------------------------------------------------------
/**     nthMember(+N, +List, ?Elem)
Text:   Ov��� resp. vyd� N-tou polo�ku seznamu.
*/

nthMember(N,[_|T],E):-
        N>1 -> dec(N,NN), nthMember(NN,T,E).
nthMember(1,[E|_],E).

%------------------------------------------------------------------------------
/**     flattenList(+List, -FlattenList)
Text:   Narovn� prologovsk� seznam obecn�ch prvk�.
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
