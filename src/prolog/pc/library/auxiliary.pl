%------------------------------------------------------------------------------
%
%                             Pomocne predikaty
%
%				Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/** Module:
Text:   Diagnostick� a pomocn� predik�ty, bin�rn� vyhled�vac� stromy.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                               System
%------------------------------------------------------------------------------
/**     pcStatistic
Text:   Stav interpretu a knihovny.
*/

pcStatistic:-
        (pcProlog(swi) -> statistics
         ;
        pcProlog(bp) -> statistics
         ;
        true),
        showPcDeBugNames,
        showPcModeOptions.

%------------------------------------------------------------------------------
/**     pcGC
Text:   Synchronn� spu�t�n� garbage collectoru.
*/

pcGC:-
        pcProlog(swi) -> garbage_collect
         ;
        pcProlog(bp) -> garbage_collect
         ;
        true.

%------------------------------------------------------------------------------
%                            Mode support
%------------------------------------------------------------------------------
/**     pcTrue
Text:   Podm�nka pou��van� v m�dech pro v�po�et mno�iny FIRST a FOLLOW.
*/
pcTrue.
pcTrue(_).
pcTrue(_,_).

/**     pcFalse
Text:   Podm�nka pou��van� v m�dech pro v�po�et mno�iny FIRST a FOLLOW.
*/
pcFalse:-fail.
pcFalse(_):-fail.
pcFalse(_,_):-fail.

/**     pcNot(+Predicate)
Text:   Podm�nka pou��van� v m�dech pro v�po�et mno�iny FIRST a FOLLOW.
*/

pcNot(C):-
        C -> fail ; true.
pcNot(C,A):-
        :-@ [C,A] -> fail ; true.
pcNot(C,A,B):-
        :-@ [C,A,B] -> fail ; true.

%------------------------------------------------------------------------------
/**     pcOR(+A, +B, -Result)
Text:   Logick� OR pou��van� v m�dech pro v�po�et mno�iny FIRST a FOLLOW.
Arg:    A,B,Result
        Jsou v�z�ny na atomy 'true' nebo 'false'.
*/

pcOR(A,B,C):-
        (A=true ; B=true) -> C=true ; C=false.

/**     pcAND(+A, +B, -Result)
Text:   Logick� AND pou��van� v m�dech pro v�po�et mno�iny FIRST a FOLLOW.
Arg:    A,B,Result
        Jsou v�z�ny na atomy 'true' nebo 'false'.
*/

pcAND(A,B,C):-
        (A=true , B=true) -> C=true ; C=false.

/**     pcXOR(+A, +B, -Result)
Text:   ExCLUSIVE OR.
Arg:    A,B,Result
        Jsou v�z�ny na atomy 'true' nebo 'false'.
*/
                                       
pcXOR(A,B,C):-
        (A=true,B=true ; A=false,B=false) -> C=false ; C=true.

%------------------------------------------------------------------------------
/**     pcIsReserved(+Atom)
Text:   Usp�je, pokud atom je rezervovanou konstantou v knihovn� konstruktor�.
*/

pcIsReserved(A):-
        member(A,
               [pcEof,          % priznak konce souboru (vyhled)
                pcFollow,       % priznak vypoctu fce follow (<::>)
                pcNil,          % stromy & seznamy
                pcEmpty,        % prazdne parametry
                pcEpsilon       % priznak prijimani epsilon v seznamu cond ll1
               ]).

%------------------------------------------------------------------------------
%                          Binary search trees
%------------------------------------------------------------------------------
%       Tato stromova struktura je pouzivana v deterministickych modech
% parseru pro reprezentaci mnozin FIRST a FOLLOW. Ve stromech se pouziva
% usporadani dle slozitosti termu tj. @< etc.
%       Vnitrni uzly jsou ukladany do struktury bst/3, listy tvori atom nil.
% Hodnoty v listech maji funkci terminatoru operaci nad stromem
% (bstMember(nil) samozrejme selze).
%------------------------------------------------------------------------------
/** list2Bst(+List, -BinarySearchTree)
Text:   Vytvo�� z prologovsk�ho seznamu bin�rn� vyhled�vac� strom.
*/

list2Bst(L,Bst):-
        length(L,Length),
        quickSort(L,SL),
        list2Bst_(Length,SL,Bst,_).

% list2Bst_(+LengthOfList, +List, -BinarySearchTree, -Rest)
list2Bst_(0,L,nil,L).
list2Bst_(_,[],nil,[]).
list2Bst_(Lng,LL,bst(BstL,Val,BstR),Rest):-
        % levy podstrom
        LngL is (Lng-1) // 2,
        list2Bst_(LngL,LL,BstL,[Val|LR]),
        % pravy podstrom
        LngR is (Lng-1)-LngL,
        list2Bst_(LngR,LR,BstR,Rest).

%------------------------------------------------------------------------------
/** bst2List(+Options, +BinarySearchTree, -List)
Text:   P�evede bin�rn� vyhled�vac� strom na prologovsk� seznam.
Arg:    Options
        Pr�chod do hloubky 'prefix', 'infix', 'postfix' nebo pr�chod do ���ky
        'breadth'.
*/

bst2List(Bst,L):-
        bst2List_(infix,Bst,L-[]).

bst2List(Option,Bst,L):-
        bst2List_(Option,Bst,L-[]).

% prefix
bst2List_(prefix,nil,D-D).
bst2List_(prefix,bst(L,Val,R),[Val|LL]-DR):-
        bst2List_(prefix,L,LL-DL),
        bst2List_(prefix,R,DL-DR).

% infix
bst2List_(infix,nil,D-D).
bst2List_(infix,bst(L,Val,R),LL-DR):-
        bst2List_(infix,L,LL-[Val|LR]),
        bst2List_(infix,R,LR-DR).

% postfix
bst2List_(postfix,nil,D-D).
bst2List_(postfix,bst(L,Val,R),LL-DR):-
        bst2List_(postfix,L,LL-DL),
        bst2List_(postfix,R,DL-[Val|DR]).

% breadth
bst2List_(breadth,Bst,L):-
        bst2List_([Bst|D]-D,L).

bst2List_([nil|Q]-D,L):-
        (Q\==D -> bst2List_(Q-D,L) ; L=DL-DL).
bst2List_([bst(L,Val,R)|Q]-[L,R|D],[Val|LDL]-DL):-
        (Q\==D -> bst2List_(Q-D,LDL-DL) ; LDL=DL).

%------------------------------------------------------------------------------
/** bstMember(+Member, +BinarySearchTree)
Text:   Predik�t usp�je, pokud existuje vrchol jeho� ohodnocen� se
        unifikuje s argumentem Member.
*/

bstMember(V,bst(_,V,_)).
bstMember(V,bst(L,Val,R)):-
        V@<Val -> bstMember(V,L) ; bstMember(V,R).

% Poznamka:
%       Vzhledem k hlave klauzule neni prvek nil akceptovan, pokud neni
% vnitrnim vrcholem stromu.

%------------------------------------------------------------------------------
/** bstInsert(+Member, +InBst, -OutBst)
Text:   Tento predik�t za�ad� do InBst hodnotu nov�ho prvku Member, pokud
        se v n�m dosud nevyskytuje a v�sledek vyd� v OutBst. V p��pad�, �e
        Member se ve strom� vyskytuje, predik�t usp�je. Strom nen� vyva�ov�n.
*/

bstInsert(V,nil,bst(nil,V,nil)).
bstInsert(V,bst(L,V,R),bst(L,V,R)).
bstInsert(V,bst(L,Val,R),bst(LL,Val,RR)):-
        V@<Val -> bstInsert(V,L,LL),R=RR ; bstInsert(V,R,RR),L=LL.

%------------------------------------------------------------------------------
/** bstDelete(+Member, +InBst, -OutBst)
Text:   Tento predik�t odstran� z InBst hodnotu prvku Member a v�sledek vyd�
        v OutBst. Predik�t neselh�v�. Strom nen� vyva�ov�n.
*/

bstDelete(V,bst(L,V,nil),L).                    % nema syny v up levy syn
bstDelete(V,bst(nil,V,R),R).                    % up pravy syn
bstDelete(V,bst(L,V,R),bst(LL,M,R)):-           % oba synove -> nahrad L max
        bstDelMax(M,L,LL).
bstDelete(V,bst(L,Val,R),bst(LL,Val,RR)):-      % down
        V@<Val -> bstDelete(V,L,LL),R=RR ; bstDelete(V,R,RR),L=LL.

bstDelete(_,nil,nil).

%------------------------------------------------------------------------------
/** bstShow(+BinarySearchTree)
Text:   Zobrazen� bin�rn�ho vyhled�vac�cho stromu "le��c�ho na lev�m boku".
*/

bstShow(Bst):-
        Width=80, Tab=3,
        bstShowRuler(Width,Tab),                                % pravitko
        bstShow_(Bst,0,Tab),
        bstShowRuler(Width,Tab).                                % pravitko

bstShow_(nil,Off,_):-
        tab(Off),write(nil),nl.
bstShow_(bst(L,Val,R),Off,Ti):-
        Offset is Off+Ti,
        bstShow_(R,Offset,Ti),
        tab(Off),write(Val),nl,
        bstShow_(L,Offset,Ti).

bstShowRuler(Total,D):-
        DD is D-1,
        bstShow__(Total,DD).
bstShow__(Total,D):-
        (Total>D
          -> write('I'), tab(D),
             T is Total-D-1,
             bstShow__(T,D)
          ;  nl).

%------------------------------------------------------------------------------
% bstDelMax(-Max, +Bst, -DelBst)
% - odstrani z binarniho vyhledavaciho stromu maximalni hodnotu (nejpravejsi 
%   vrchol ve stromu a vyda ji v Max

bstDelMax(nil,nil,nil).
bstDelMax(V,bst(nil,V,nil),nil).
bstDelMax(V,bst(L,V,nil),L).
bstDelMax(V,bst(L,Val,R),bst(L,Val,RR)):-
        bstDelMax(V,R,RR).

%------------------------------------------------------------------------------
%                         Zastarale a vyrazene predikaty
%------------------------------------------------------------------------------

optFactorizeByFstSets(_,_):-
        deBugAssert(fail,['Obsolete optFactorizeByFstSets/2',nl,nl]).
optFactorizeByFstSets(_,_,_):-
        deBugAssert(fail,['Obsolete optFactorizeByFstSets/3',nl,nl]).
optFactorizeByFstSets(_,_,_,_):-
        deBugAssert(fail,['Obsolete optFactorizeByFstSets/4',nl,nl]).
optAlts2FstAlts(_,_,_):-
        deBugAssert(fail,['Obsolete optAlts2FstAlts/3',nl]).

firstSetIsElement(_,_):-
        deBugAssert(fail,['Obsolete firstSetIsElement/2',nl]).
firstBstFindAll(_,_,_):-
        deBugAssert(fail,['Obsolete firstBstFindAll/3',nl]).
firstBstFind(_,_,_):-
        deBugAssert(fail,['Obsolete firstBstFind/3',nl]).
firstSetCond2AsciiList(_,_):-
        deBugAssert(fail,['Obsolete firstSetCond2AsciiList/2',nl]).
firstSetCond2Convert(_,_,_):-
        deBugAssert(fail,['Obsolete firstSetCond2Convert/3',nl]).
firstChooseParsers(_,_,_):-
        deBugAssert(fail,['Obsolete firstChooseParsers/3',nl]).
firstChooseParser(_,_,_):-
        deBugAssert(fail,['Obsolete firstChooseParser/3',nl]).

followSetCreate(A,A):-
        deBugAssert(fail,['Obsolete followSetCreate/2',nl]).
followSetCreate(_,A,A):-
        deBugAssert(fail,['Obsolete followSetCreate/3',nl]).
followSetIsElement(_,_):-
        deBugAssert(fail,['Obsolete followSetIsElement/2',nl]).

%- EOF ------------------------------------------------------------------------
