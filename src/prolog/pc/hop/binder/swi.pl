%------------------------------------------------------------------------------
%
%                     Optimized :-> version for SWI Prolog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Predik�t :->/2 podpory programov�n� vy���ho ��du optimalizovan� pro
        SWI Prolog.
*/
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
