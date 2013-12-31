%------------------------------------------------------------------------------
%
%         Implementation of :-@ using standard predicates (LPA Prolog)
%
%                               Martin Dvorak
%                                   1999
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

:-@ [First|RestL]:-
        First =.. FirstL,
        append(FirstL,RestL,GoalL),
        Goal =.. GoalL,
        Goal.

%- EOF ------------------------------------------------------------------------
