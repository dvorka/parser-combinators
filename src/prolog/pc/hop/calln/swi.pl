%------------------------------------------------------------------------------
%
%                    Optimized :-@ version for SWI Prolog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Podpora programování vy¹¹ího øádu - SWI Prolog.
*/
%------------------------------------------------------------------------------
/**     :-@ [+Term | +ListOfAdditionalArguments]
Text:   Implementace predikátu vy¹¹ího øádu call/n pro SWI Prolog.
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
        apply(First,RestL).

%- EOF ------------------------------------------------------------------------
