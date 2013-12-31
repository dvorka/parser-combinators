%------------------------------------------------------------------------------
%
%                     Optimized HOP versions for SWI Prolog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Podpora programov�n� vy���ho ��du - SWI Prolog.
*/
%------------------------------------------------------------------------------
/**     :-@ [+Term | +ListOfAdditionalArguments]
Text:   Implementace predik�tu vy���ho ��du call/n optimalizovan� pro
        SWI Prolog.
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
        apply(First,RestL).

%------------------------------------------------------------------------------
/**     :->(?Wrapper, +Parser)
Text:   Varianta predik�tu :-@/1, kter� je �ita na m�ru knihovn�m 
        predik�t�m PC (SWI Prolog).
Example:
        ?- "abc"+LOS :-> symbol("a").
        LOS = ["bc" > a]
        Yes
*/

Wrapper :-> Parser:-          
        apply(Parser,[Wrapper]).

%- EOF ------------------------------------------------------------------------
