%------------------------------------------------------------------------------
%
%           :-> implementation using standard predicates (LPA Prolog)
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** :->(?Wrapper, +Parser)
Text:   Varianta predik�tu :-@/1, kter� je �ita na m�ru knihovn�m 
        predik�t�m PC. 
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
