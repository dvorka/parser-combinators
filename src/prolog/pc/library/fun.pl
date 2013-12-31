%------------------------------------------------------------------------------
%
%             		     Functional goodies
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Aplikace programování vy¹¹ího øádu ze svìta funkcionálního
        programování.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     mapList(+Function, +InputList, -OutputList)
Text:   Èasto potøebujeme provést se v¹emi prvky seznamu stejnou operaci
        a z takto získaných výsledkù vytvoøit opìt seznam. Takové zpracování
        zajistí predikát vy¹¹ího øádu nazvaný mapList.
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
Text:   Predikát mapListDL/3 je variantou mapList/3. Li¹í se tím, ¾e vydává
        výsledek ve formì rozdílového seznamu.
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
Text:   Predikát vy¹¹ího øádu, který aplikuje danou binární operaci Action
        s levou asociativitou a její výsledky akumuluje.
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
Text:   Predikát vy¹¹ího øádu, který aplikuje danou binární operaci Action
        s pravou asociativitou a její výsledky akumuluje.
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
Text:   Predikát "multi append" propojí v¹echny podseznamy daného seznamu.
Arg:    InputList
        Seznam seznamù.
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
Text:   Varianta predikátu mapList/3, která ignoruje selhání predikátu
        Function.
Arg:    Function
        Operace aplikovaná na polo¾ky seznamu InputList.
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
Text:   Varianta predikátu mapList/3, která ulo¾í do seznamu OutputList pouze
        ty prvky InputList na kterých predikát Function selhal.
Arg:    Function
        Operace aplikovaná na polo¾ky seznamu InputList.
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
Text:   Ze seznamu InputList jsou odfiltrovány polo¾ky nesplòující podmínku
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
Text:   Do seznamu FilteredList jsou ulo¾eny polo¾ky InputList, které
        nesplòují podmínku Condition.
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
Text:   Predikát zip spojí dva stejnì dlouhé seznamy do jediného tak, ¾e
        se na stejnolehlé prvky aplikuje zadaná funkce a její hodnota se
        umís»uje na pøíslu¹né místo ve výsledném seznamu. Jestli¾e mají
        seznamy rùznou délku, má výsledek velikost krat¹ího z nich.
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
Text:   Uspìje, pokud nìkterá z polo¾ek v seznamu splòuje danou podmínku
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
Text:   Uspìje, pokud v¹echny polo¾ky v seznamu splòují danou podmínku Cond.
Example:
        ?- alls(isOdd,[1,3]).
        Yes
*/

alls(_,[]).    	
alls(Cond,[IH|IT]):-
         :-@[Cond,IH] -> alls(Cond,IT).

%------------------------------------------------------------------------------
/**     =->(+Predicate1, +Predicate2, -Result)
Text:   Zøetìzená aplikace predikátù. Nejdøíve je aplikován Predicate2
        a potom Predicate1. Pou¾ití nachází pøedev¹ím v predikátech vy¹¹ího
        øádu jako fold* èi mapList/3.
Example:
        ?- :-@ [mapList(+) =-> foldR(append,[]),[[0,1],[2]],R].
        R= [+(0), +(1), +(2)]
        Yes
*/

=->(P1,P2,I,O):-
    :-@ [P2,I,Ot],
    :-@ [P1,Ot,O].

%- EOF ------------------------------------------------------------------------
