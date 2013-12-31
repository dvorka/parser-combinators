%------------------------------------------------------------------------------
%
%                           More parser mutators I
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Mut�tory parser� I.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/** whole(+Parser, ?Wrapper)
Text:   Mut�tor whole zajist�, �e do seznamu �sp�n�ch rozklad� budou ulo�eny
        pouze ty derivace, kter� zpracovaly cel� vstup.
*/
% - yields mutant which guarantees processing of the whole input

whole(P,I+L):-
 I+Lt :->
        P,
 filter(wholeSem,Lt,L).

wholeSem(Descr>_):-
        pcGetInput(Descr,Input)
         ->
        (Input=[];Descr=file(_);Descr=quicFile(_);Descr=file(_)).

%------------------------------------------------------------------------------
/** #>(+Parser,?Wrapper)
Text:   Prefixn� mut�tor ignoruj�c� pr�zdn� znaky p�edch�zej�c� sentenci
        p�ij�man� parserem Parser.
*/

#>(P,W):-
 W :->
        (whiteSpace<*@>return(whiteSpace)-sndTuple) &> P.

%------------------------------------------------------------------------------
/** <#(+Parser,?Wrapper)
Text:   Postfixn� mut�tor ignoruj�c� pr�zdn� znaky n�sleduj�c� za sentenc�
        p�ij�manou parserem Parser.
*/

<#(P,W):-
 W :->
        P <& (whiteSpace<*@>return(whiteSpace)-sndTuple).

%------------------------------------------------------------------------------
/** ignore(+JunkParser,+Parser,?Wrapper)
Text:   Na vstup je opakovan� aplikov�n Parser, jeho v�sledky jsou v�ak
        ignorov�ny a jako v�sledn� hodnota je vyd�n atom 'junk'.
*/
% - generalization of whitespace mutators
% - Junk parser is repeatedly aplicated and it's results are ignored

ignore(J,P,W):-
 W :->
        (J<*@>return(junk)-sndTuple) &> P.

%------------------------------------------------------------------------------
/** det(+Parser, ?Wrapper)
Text:   Mut�tor zajist�, �e seznam �sp�n�ch rozklad� bude obsahovat nejv��e
        jednu derivaci, kter� nav�c zpracovala cel� vstup.
*/

det(P,W):-
 W :->
        whole(P)<> .

%------------------------------------------------------------------------------
/** kill(+Parser, ?Wrapper)
Text:   Mut�tor kill bez ohledu na v�stup parseru vyprazd�uje seznam �sp�n�ch
        rozklad� - za ka�d�ch okolnost� tedy signalizuje ne�sp�n� rozklad
        (debug a m�d filter/1).
*/
% Mutator kill makes list of successes empty -> termination (debug purposes)

kill(P,I+[]):-
 I+_ :->
        P.
        
%- EOF ------------------------------------------------------------------------
