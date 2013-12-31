%------------------------------------------------------------------------------
%
%       Implementation of high-order predicate call/n using std. predicates
%                               (LPA Prolog)
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Podpora programování vy¹¹ího øádu.
*/
%------------------------------------------------------------------------------
/**     :-@ [+Term | +ListOfAdditionalArguments]
Text:   Implementace predikátu vy¹¹ího øádu call/n pomocí standardních
        prostøedkù jazyka Prolog.
Arg: Term
        Základ termu.
Arg: ListOfAdditionalArguments
        Dal¹í argumenty, které jsou pøipojeny k zárodku. Je vytvoøen cíl,
        který je zavolán.
Example:
        ?- :-@ [append("Robert "), "Kovalski", Name].
        Name = "Robert Kovalski"
        Yes
*/

:-@([First|RestL]):-
        First =.. FirstL,
        append(FirstL,RestL,GoalL),
        Goal =.. GoalL,
        Goal.

%------------------------------------------------------------------------------
/**     :->(?Wrapper, +Parser)
Text:   Varianta predikátu :-@/1, která je ¹ita na míru knihovním 
        predikátùm PC. 
Example:
        ?- "abc"+LOS :-> symbol("a").
        LOS = ["bc" > a]
        Yes
*/

:->(Wrapper,Parser):-
	 Parser=..ParserList,
         append(ParserList,[Wrapper],GoalList),
	 Goal=..GoalList,
         Goal.

%- EOF ------------------------------------------------------------------------
