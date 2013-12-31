%------------------------------------------------------------------------------
%
%                     Optimized :-> version for BinProlog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** :->(?Wrapper, +Parser)
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
