%------------------------------------------------------------------------------
%
%                                Identifiers
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Identifikátory.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     poorIdf(?Wrapper)
Text:   Identifikátor slo¾ený výhradnì z obyèejných znakù.
*/
% - is string of characters e.g. poorIdentifiers
% - digits and '_' ... forbidden

poorIdf(W):-
 W :->
        fulfil(isChar)<+> <@ string2Atom.

%------------------------------------------------------------------------------
/**     stdIdf(?Wrapper)
Text:   "C/Java-like" identifikátor - nesmí zaèínat èíslicí, ale mù¾e
        ji obsahovat, stejnì tak znak '_'.
*/
% - standart identifier (C-like)
% - cann't start with digit, can start with '_' or capital
% - contains both digits and '_'     

stdIdf(W):-
 W :->
        (fulfil(isChar)
          <&>
         fulfil(isDigitChar_)<*>) <@ string2Atom.

%------------------------------------------------------------------------------
/**     prologIdf(?Wrapper)
Text:   Prologovský identifikátor - nesmí zaèínat velkým písmenem
        ani znakem '_'.
*/
% - standart idf but cann't start with '_' or capital

prologIdf(W):-
 W :->
        ( fulfil(isLwrChar) <&> fulfil(isDigitChar_)<*>
           <:
          fulfil(isSpecChar)<+>)  <@ string2Atom.
    
%- EOF ------------------------------------------------------------------------
