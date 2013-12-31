%------------------------------------------------------------------------------
%
%           :-> implementation using standard predicates (LPA Prolog)
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** :->(?Wrapper, +Parser)
Text:   Varianta predikátu :-@/1, která je ¹ita na míru knihovním 
        predikátùm PC. 
Example:
        ?- "abc"+LOS :-> symbol("a").
        LOS = ["bc" > a]
        Yes
*/

Wrapper :-> Parser:-
	 Parser=..ParserList,
         append(ParserList,[Wrapper],GoalList),
	 Goal=..GoalList,
         Goal.

%- EOF ------------------------------------------------------------------------
