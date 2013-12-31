%------------------------------------------------------------------------------
%
%       Implementation of high-order predicate call/n using std. predicates
%                               (LPA Prolog)
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Podpora programov�n� vy���ho ��du.
*/
%------------------------------------------------------------------------------
/**     :-@ [+Term | +ListOfAdditionalArguments]
Text:   Implementace predik�tu vy���ho ��du call/n pomoc� standardn�ch
        prost�edk� jazyka Prolog.
Arg: Term
        Z�klad termu.
Arg: ListOfAdditionalArguments
        Dal�� argumenty, kter� jsou p�ipojeny k z�rodku. Je vytvo�en c�l,
        kter� je zavol�n.
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
Text:   Varianta predik�tu :-@/1, kter� je �ita na m�ru knihovn�m 
        predik�t�m PC. 
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
