%------------------------------------------------------------------------------
%
%                     Optimized :-> version for BinProlog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** :->(?Wrapper, +Parser)
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
