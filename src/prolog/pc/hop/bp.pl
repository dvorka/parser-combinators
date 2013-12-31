%------------------------------------------------------------------------------
%
%                     Optimized HOP versions for BinProlog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Podpora programov�n� vy���ho ��du - BinProlog.
*/
%------------------------------------------------------------------------------

/**     :-@ [+Term | +ListOfAdditionalArguments]
Text:   Implementace predik�tu vy���ho ��du call/n optimalizovan� pro
        BinProlog.
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

:-@ CallInput:-
        Goal =.. [call|CallInput],
        Goal.

% Unfortunately =../2 seriously wastes heap

% Another possible implementation is:
%  :-@ [Struct|[A]]        :- call(Struct,A).
%  :-@ [Struct|[A,B]]      :- call(Struct,A,B).
%  :-@ [Struct|[A,B,C]]    :- call(Struct,A,B,C).
%  :-@ [Struct|[A,B,C,D]]  :- call(Struct,A,B,C,D).
%  :-@ [Struct|[A,B,C,D,E]]:- call(Struct,A,B,C,D,E).
% This ad hoc implementation is better than =../2
%------------------------------------------------------------------------------
/**     :->(?Wrapper, +Parser)
Text:   Varianta predik�tu :-@/1, kter� je �ita na m�ru knihovn�m 
        predik�t�m PC (BinProlog).
Example:
        ?- "abc"+LOS :-> symbol("a").
        LOS = ["bc" > a]
        Yes
*/

Wrapper :-> Parser:-
        call(Parser,Wrapper).

%- EOF ------------------------------------------------------------------------
