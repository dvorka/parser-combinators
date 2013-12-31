%------------------------------------------------------------------------------
%
%                     Optimized HOP versions for BinProlog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Podpora programování vy¹¹ího øádu - BinProlog.
*/
%------------------------------------------------------------------------------

/**     :-@ [+Term | +ListOfAdditionalArguments]
Text:   Implementace predikátu vy¹¹ího øádu call/n optimalizovaná pro
        BinProlog.
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
Text:   Varianta predikátu :-@/1, která je ¹ita na míru knihovním 
        predikátùm PC (BinProlog).
Example:
        ?- "abc"+LOS :-> symbol("a").
        LOS = ["bc" > a]
        Yes
*/

Wrapper :-> Parser:-
        call(Parser,Wrapper).

%- EOF ------------------------------------------------------------------------
