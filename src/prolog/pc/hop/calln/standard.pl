%------------------------------------------------------------------------------
%
%         Implementation of :-@ using standard predicates (LPA Prolog)
%
%                               Martin Dvorak
%                                   1999
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

:-@ [First|RestL]:-
        First =.. FirstL,
        append(FirstL,RestL,GoalL),
        Goal =.. GoalL,
        Goal.

%- EOF ------------------------------------------------------------------------
