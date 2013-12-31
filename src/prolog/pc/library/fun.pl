%------------------------------------------------------------------------------
%
%             		     Functional goodies
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Aplikace programov�n� vy���ho ��du ze sv�ta funkcion�ln�ho
        programov�n�.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     mapList(+Function, +InputList, -OutputList)
Text:   �asto pot�ebujeme prov�st se v�emi prvky seznamu stejnou operaci
        a z takto z�skan�ch v�sledk� vytvo�it op�t seznam. Takov� zpracov�n�
        zajist� predik�t vy���ho ��du nazvan� mapList.
Example:
        ?- mapList(+(10),[1,5,7],R).
        R = [10,15,17]
        Yes
*/

mapList(Fun,[IH|IT],[OH|OT]):-
        :-@ [Fun,IH,OH],
    	mapList(Fun,IT,OT).
mapList(_,[],[]).

%------------------------------------------------------------------------------
/**     mapListDL(+Function, -InputList, -OutputDifList)
Text:   Predik�t mapListDL/3 je variantou mapList/3. Li�� se t�m, �e vyd�v�
        v�sledek ve form� rozd�lov�ho seznamu.
Example:
        ?- mapListDL(square,[1,5,7],R).
        R = [1,25,49,X]-X
        Yes
*/

mapListDL(_,[],D-D).
mapListDL(Fun,[IH|IT],[OH|OT]-D):-
        :-@ [Fun,IH,OH],
    	mapListDL(Fun,IT,OT-D).

%------------------------------------------------------------------------------
/**     foldL(+Action, +InitVal, +InputList, -OutputList)
Text:   Predik�t vy���ho ��du, kter� aplikuje danou bin�rn� operaci Action
        s levou asociativitou a jej� v�sledky akumuluje.
Example:
        % foldL(f, 3, [a,b,c], R)
        %  =>
        % f(f(f(3,a),b),c)
*/

foldL(F,InVal,[I|IT],Out):-
        :-@ [F,InVal,I,OutVal],
        foldL(F,OutVal,IT,Out).
foldL(_,Out,[],Out).

%------------------------------------------------------------------------------
/**     foldR(+Action, +InitVal, +InputList, -OutputList)
Text:   Predik�t vy���ho ��du, kter� aplikuje danou bin�rn� operaci Action
        s pravou asociativitou a jej� v�sledky akumuluje.
Example:
        % foldR(f, 3, [a,b,c], R)
        %  =>
        % f(a,f(b,f(c,3)))
*/

foldR(F,InVal,[I|IT],Out):-
        foldR(F,InVal,IT,Out_),
        :-@ [F,I,Out_,Out].             % count value when returning
foldR(_,InVal,[],InVal).

%------------------------------------------------------------------------------
/**     mAppend(+InputList, -OutputList)
Text:   Predik�t "multi append" propoj� v�echny podseznamy dan�ho seznamu.
Arg:    InputList
        Seznam seznam�.
Example:
        ?- mAppend([[],[1,2],[3]],L).
        L = [1,2,3]
        Yes
*/

mAppend([],[]).
mAppend(In,Out):-
        foldL( append, [], In, Out ).

%------------------------------------------------------------------------------
/**     mapFilter(+Function, +InputList, -OutputList)
Text:   Varianta predik�tu mapList/3, kter� ignoruje selh�n� predik�tu
        Function.
Arg:    Function
        Operace aplikovan� na polo�ky seznamu InputList.
Example:
        ?- mapFilter(inc(1),[1,a,v,c,5],L).
        L = [2,6]
        Yes
*/

mapFilter(_,[],[]).
mapFilter(Fun,[IH|IT],Output):-
	(
         :-@ [Fun,IH,OH], Output=[OH|OT]
    	  ;
         Output=OT
        ),
        mapFilter(Fun,IT,OT).

%------------------------------------------------------------------------------
/**     nonMapFilter(+Function, +InputList, -OutputList)
Text:   Varianta predik�tu mapList/3, kter� ulo�� do seznamu OutputList pouze
        ty prvky InputList na kter�ch predik�t Function selhal.
Arg:    Function
        Operace aplikovan� na polo�ky seznamu InputList.
Example:
        ?- nonMapFilter(dec,[1,a,v,c,5],L).  % dec/2 check its args
        L = [a,v,c]
        Yes
*/

nonMapFilter(_,[],[]).
nonMapFilter(Fun,[IH|IT],Output):-
	(
         :-@ [Fun,IH,_], Output=OT
    	  ;
         Output=[IH|OT]
        ),
        nonMapFilter(Fun,IT,OT).

%------------------------------------------------------------------------------
/**     filter(+Condition, +InputList, -FilteredList)
Text:   Ze seznamu InputList jsou odfiltrov�ny polo�ky nespl�uj�c� podm�nku
        Condition.
*/

filter(Cond,[IH|IT],Filtered):-
        (:-@ [Cond,IH]
          -> Filtered=[IH|OT]
    	  ;  Filtered=OT),
    	filter(Cond,IT,OT).
filter(_,[],[]).

%------------------------------------------------------------------------------
/**     nonFilter(+Condition, +InputList, -FilteredList)
Text:   Do seznamu FilteredList jsou ulo�eny polo�ky InputList, kter�
        nespl�uj� podm�nku Condition.
*/

nonFilter(Cond,[IH|IT],Filtered):-
        (
         :-@ [ Cond, IH ] -> Filtered=OT
    	                  ;  Filtered=[IH|OT]   
        ),
    	nonFilter(Cond,IT,OT).
nonFilter(_,[],[]).    	

%------------------------------------------------------------------------------
/**     zip(+Pred, +List1, +List2, +OutList)
Text:   Predik�t zip spoj� dva stejn� dlouh� seznamy do jedin�ho tak, �e
        se na stejnolehl� prvky aplikuje zadan� funkce a jej� hodnota se
        um�s�uje na p��slu�n� m�sto ve v�sledn�m seznamu. Jestli�e maj�
        seznamy r�znou d�lku, m� v�sledek velikost krat��ho z nich.
Example:
        ?- zip(+,[1,2,3],[4,5,6],L).
        L = [5,7,9]
        Yes
*/

zip(Pred,[IH1|IT1],[IH2|IT2],[OH|OT]):-
         :-@ [Pred,IH1,IH2,OH],
         zip(Pred,IT1,IT2,OT).
zip(_,[],_,[]).
zip(_,_,[],[]).

%------------------------------------------------------------------------------
/**     exists(+Cond, +List)
Text:   Usp�je, pokud n�kter� z polo�ek v seznamu spl�uje danou podm�nku
        Cond.
Example:
        ?- exists(isOdd,[2,3]).
        Yes
*/

exists(_,[]):-fail.    	
exists(Cond,[IH|IT]):-
         :-@[Cond,IH] -> true ; exists(Cond,IT).
    			  
%------------------------------------------------------------------------------
/**     alls(+Cond, +List)
Text:   Usp�je, pokud v�echny polo�ky v seznamu spl�uj� danou podm�nku Cond.
Example:
        ?- alls(isOdd,[1,3]).
        Yes
*/

alls(_,[]).    	
alls(Cond,[IH|IT]):-
         :-@[Cond,IH] -> alls(Cond,IT).

%------------------------------------------------------------------------------
/**     =->(+Predicate1, +Predicate2, -Result)
Text:   Z�et�zen� aplikace predik�t�. Nejd��ve je aplikov�n Predicate2
        a potom Predicate1. Pou�it� nach�z� p�edev��m v predik�tech vy���ho
        ��du jako fold* �i mapList/3.
Example:
        ?- :-@ [mapList(+) =-> foldR(append,[]),[[0,1],[2]],R].
        R= [+(0), +(1), +(2)]
        Yes
*/

=->(P1,P2,I,O):-
    :-@ [P2,I,Ot],
    :-@ [P1,Ot,O].

%- EOF ------------------------------------------------------------------------
