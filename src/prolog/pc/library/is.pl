%------------------------------------------------------------------------------
%
%                 	            Is
%
%				Martin Dvorak
%                                 1998-1999
%------------------------------------------------------------------------------
/** Module:
Text:   Klasifikace term�.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     isWhiteSpace(?String)
Text:   Usp�je pokud String je "pr�zdn� znak".
*/

isWhiteSpace([Char]):-
        member(Char,[32,13,10,9]).

%------------------------------------------------------------------------------
/**     isLwrChar(+String)
Example:
        ?- isLwrChar("a").
        Yes
*/
% - order was choosen by the frequency of each character in English:
%        member(Char,"etraonishdlfcmugpywbvkjxqz").

isLwrChar([Char]):-
        integer(Char), Char>=97, Char=<122.
                       
%------------------------------------------------------------------------------
/**     isUprChar(+String)
Example:
        ?- isUprChar("A").
        Yes
*/
% - order was choosen by the frequency of each character in English:
%        member(Char,"ETRAONISHDLFCMUGPYWBVKJXQZ").

isUprChar([Char]):-
        integer(Char), Char>=65, Char=<90.

%------------------------------------------------------------------------------
/**     isChar(+String)
Text:   isUprChar/1 nebo isLwrChar/1.
*/
%        member(Char,"etraonishdlfcmugpywbvkjxqzETRAONISHDLFCMUGPYWBVKJXQZ").

isChar(Char):-
        isUprChar(Char)
        ;
        isLwrChar(Char).

%------------------------------------------------------------------------------
/** isDigit(+String)
Example:
        ?- isDigit("1").
        Yes
*/
%        member(Digit,"0123456789").

isDigit([Digit]):-
        integer(Digit), Digit>=48, Digit=<57.

%------------------------------------------------------------------------------
/**     isChar_(+String)
Text:   Usp�je, pokud String je p�smeno nebo znak '_'.
*/

isChar_(Char):-
        isChar(Char)
        ;
        Char=="_".

%------------------------------------------------------------------------------
/**     isDigitChar(+String)
Text:   Usp�je pokud String je ��slice nebo p�smeno.
*/

isDigitChar(Char):-
        isChar(Char)
        ;    
        isDigit(Char).

%------------------------------------------------------------------------------
/**     isDigitChar_(+String)
Text:   Usp�je pokud String je ��slice, p�smeno nebo znak '_'.
*/

isDigitChar_(Char):-
        isChar_(Char)
        ;    
        isDigit(Char).

%------------------------------------------------------------------------------
/**     isVowel(+String)
Example:
        ?- isVowel("a").
        Yes
*/
% - order was choosen by the frequency of each character in English.

isVowel([Char]):-
        member(Char,"eaoiuyEAOIUY").

%------------------------------------------------------------------------------
/** isConsonant(+String)
Example:
        ?- isConsonant("x").
        Yes
*/
% - order was choosen by the frequency of each character in English.

isConsonant([Char]):-
        member(Char,"trnshdlfcmgpwbvkjxqzTRNSHDLFCMGPWBVKJXQZ").
                     
%------------------------------------------------------------------------------
/**     isSpec(+String)
Text:   Usp�je pokud String je speci�ln� znak.
*/

isSpec([Char]):-
        member(Char,".,;|?()=!'*/-+\@#$^&:~[]<>").

%------------------------------------------------------------------------------
/**     isSpecChar(+String)
Text:   Usp�je pokud String je speci�ln� znak pou�iteln� v prologovsk�m
        identifik�toru.
*/

isSpecChar([Char]):-
        member(Char,[92|"?=*+-/@$&#:~^<>"]). % ascii(\,92)

%------------------------------------------------------------------------------
/** isOdd(+Atom)
Example:
        ?- isOdd(17).
        Yes
*/

isOdd(Number):-
        integer(Number), 1 is Number mod 2.

%------------------------------------------------------------------------------
/** isEven(+Atom)
Example:
        ?- isEven(20).
        Yes
*/

isEven(Number):-
        integer(Number), 0 is Number mod 2.

%------------------------------------------------------------------------------
/**     isBetween(+Low, +High, +Value)
Text:   Low, High a Value jsou ��seln� atomy. Predik�t usp�je pokud pro
        Value plat� Low=<Value=<High.
Example:
        ?- isBetween(100,104,102).
        Yes
*/

isBetween(L,H,V):-
        integer(L),integer(H), integer(V),
        L=<V, V=<H.

%------------------------------------------------------------------------------
/**     sIsBetween(+Low, +High, +Value)
Text:   Low, High a Value jsou jednoznakov� �et�zce. Predik�t usp�je pokud pro
        Value plat� Low=<Value=<High.
Example:
        ?- isBetween("d","h","f").
        Yes
*/

sIsBetween([L],[H],[V]):-
        integer(L),integer(H), integer(V),
        L=<V, V=<H.
        

%------------------------------------------------------------------------------
/**     isNotBetween(+Low, +High, +Value)
Example:
        ?- isNotBetween(100,107,101).
        No
*/

isNotBetween(L,H,V):-
        isBetween(L,H,V) -> false; true.

%------------------------------------------------------------------------------
/**     sIsNotBetween(+Low, +High, +Value)
Example:
        ?- sIsNotBetween("d","h","f").
        No
*/

sIsNotBetween(L,H,V):-
        sIsBetween(L,H,V) -> false; true.

%------------------------------------------------------------------------------
/**     isAscii(+Atom)
Text:   Ov���, zda Atom m��e b�t ASCII k�dem.
*/

isAscii(Atom):-
        integer(Atom) -> isBetween([0],[255],[Atom]).

%------------------------------------------------------------------------------
/**     isList(+List)
Text:   Ov���, zda List je prologovsk� seznam.
*/

isList([]).
isList([_|_]).

%------------------------------------------------------------------------------
/**     isSet(+Set)
Text:   Ov���, zda je Set mno�inou.
*/

isSet([H|T]):-
        member(H,T) -> fail ; isSet(T).
isSet([]).

%-EOF -------------------------------------------------------------------------
