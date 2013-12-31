%------------------------------------------------------------------------------
%
%                                Identifiers
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Identifik�tory.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     poorIdf(?Wrapper)
Text:   Identifik�tor slo�en� v�hradn� z oby�ejn�ch znak�.
*/
% - is string of characters e.g. poorIdentifiers
% - digits and '_' ... forbidden

poorIdf(W):-
 W :->
        fulfil(isChar)<+> <@ string2Atom.

%------------------------------------------------------------------------------
/**     stdIdf(?Wrapper)
Text:   "C/Java-like" identifik�tor - nesm� za��nat ��slic�, ale m��e
        ji obsahovat, stejn� tak znak '_'.
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
Text:   Prologovsk� identifik�tor - nesm� za��nat velk�m p�smenem
        ani znakem '_'.
*/
% - standart idf but cann't start with '_' or capital

prologIdf(W):-
 W :->
        ( fulfil(isLwrChar) <&> fulfil(isDigitChar_)<*>
           <:
          fulfil(isSpecChar)<+>)  <@ string2Atom.
    
%- EOF ------------------------------------------------------------------------
