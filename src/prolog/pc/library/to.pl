%------------------------------------------------------------------------------
%
%                 		    To
%			        converting
%
%			       Martin Dvorak
%                                  1999
%------------------------------------------------------------------------------
/** Module:
Text:   Konverze termù.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     ascii(?Term, ?ASCIICode)
Example:
        ?- ascii(a,ASCII).
        ASCII = 97 
        Yes
*/

ascii(A,ASCII):-
    A=[ASCII|_] ; name(A,[ASCII|_]).

%------------------------------------------------------------------------------
/**     ascii2Atom(?ASCIICode, ?Atom)
Example:
        ?- ascii(a,ASCII).
        ASCII = 97 
        Yes
*/

ascii2Atom(ASCII,A):-
    ASCII==[] -> A=[] ; name(A,[ASCII]).

%------------------------------------------------------------------------------
/**     string2Atom(?String, ?Atom)
Example:
        ?- string2Atom("Aphex Twin",Atom).
        Atom = 'Aphex Twin' 
        Yes
*/

string2Atom(S,A):-
    name(A,S).

%------------------------------------------------------------------------------
/**     atom2String(?Atom, ?String)
Example:
        ?- atom2String('Warp',String).
        String = "Warp" 
        Yes
*/
% - it's strange to define in Prolog atom2String when we have string2Atom
%   but this predicate is often used together with <@/2 so the use of
%   variable modes is impossible

atom2String(A,S):-
    name(A,S).

%------------------------------------------------------------------------------
/**     atomic2Atom(?Atomic, ?Atom)
Example:
        ?- atomic2Atom(a,97).
        Yes
*/

atomic2Atom(A,S):-
    name(A,[S]).

%------------------------------------------------------------------------------
/**     upr2Lwr(+String, -?LowerString)
Text:   Konverze velkých písmen øetìzci na malá.
*/
% - *WARNING* ... this function works only with ASCII ... *WARNING*
% - converts all characters inside the string to lower case
% - rem: ascii('A') == 65, ascii('Z') == 90
%        ascii('a') == 97, ascii('z') == 122   ...   diference is 32

upr2Lwr([I|Is],[O|Os]):-
    (( I >= 65 , I =< 90 ) -> O is I+32 ; O=I ),
    upr2Lwr(Is,Os).

upr2Lwr([],[]).    
     
%------------------------------------------------------------------------------
/**     lwr2Upr(+String, ?UpperString)
Text:   Konverze malých písmen v øetìci na velká.
*/
% - *WARNING* ... this function works only with ASCII ... *WARNING*
% - converts all characters inside the string to upper case
% - rem: ascii('A') == 65, ascii('Z') == 90
%        ascii('a') == 97, ascii('z') == 122   ...   diference is 32

lwr2Upr([I|Is],[O|Os]):-
    (( I >= 97 , I =< 122 ) -> O is I-32 ; O=I ),
    lwr2Upr(Is,Os).

lwr2Upr([],[]).

%------------------------------------------------------------------------------
/**     toList(?Term,?TermInList)
Example:
        ?- toList(a,[a]).
        Yes
*/

toList(X,[X]).

%------------------------------------------------------------------------------
/**     univ2List(?Struct, ?List)
Example:
        ?- univ2List(a(b,c),[a,b,c]).
        Yes
*/
% - v kolonach lze pouzivat primo: P <@ =..

univ2List(S,L):-
        S=..L.

%------------------------------------------------------------------------------
/**     univ2Struct(?List, ?Struct)
Example:
        ?- univ2Struct([a,b,c],a(b,c)).
        Yes
*/

univ2Struct(L,S):-
        S=..L.

%- EOF ------------------------------------------------------------------------
