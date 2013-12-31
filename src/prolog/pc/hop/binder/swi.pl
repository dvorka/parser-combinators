%------------------------------------------------------------------------------
%
%                     Optimized :-> version for SWI Prolog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Predikát :->/2 podpory programování vy¹¹ího øádu optimalizovanı pro
        SWI Prolog.
*/
%------------------------------------------------------------------------------
/**     :->(?Wrapper, +Parser)
Text:   Varianta predikátu :-@/1, která je ¹ita na míru knihovním 
        predikátùm PC (SWI Prolog).
Example:
        ?- "abc"+LOS :-> symbol("a").
        LOS = ["bc" > a]
        Yes
*/

Wrapper :-> Parser:-          
        apply(Parser,[Wrapper]).

%- EOF ------------------------------------------------------------------------
