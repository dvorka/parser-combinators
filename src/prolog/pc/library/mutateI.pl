%------------------------------------------------------------------------------
%
%                           More parser mutators I
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Mutátory parserù I.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/** whole(+Parser, ?Wrapper)
Text:   Mutátor whole zajistí, ¾e do seznamu úspì¹ných rozkladù budou ulo¾eny
        pouze ty derivace, které zpracovaly celý vstup.
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
Text:   Prefixní mutátor ignorující prázdné znaky pøedcházející sentenci
        pøijímané parserem Parser.
*/

#>(P,W):-
 W :->
        (whiteSpace<*@>return(whiteSpace)-sndTuple) &> P.

%------------------------------------------------------------------------------
/** <#(+Parser,?Wrapper)
Text:   Postfixní mutátor ignorující prázdné znaky následující za sentencí
        pøijímanou parserem Parser.
*/

<#(P,W):-
 W :->
        P <& (whiteSpace<*@>return(whiteSpace)-sndTuple).

%------------------------------------------------------------------------------
/** ignore(+JunkParser,+Parser,?Wrapper)
Text:   Na vstup je opakovanì aplikován Parser, jeho výsledky jsou v¹ak
        ignorovány a jako výsledná hodnota je vydán atom 'junk'.
*/
% - generalization of whitespace mutators
% - Junk parser is repeatedly aplicated and it's results are ignored

ignore(J,P,W):-
 W :->
        (J<*@>return(junk)-sndTuple) &> P.

%------------------------------------------------------------------------------
/** det(+Parser, ?Wrapper)
Text:   Mutátor zajistí, ¾e seznam úspì¹ných rozkladù bude obsahovat nejvý¹e
        jednu derivaci, která navíc zpracovala celý vstup.
*/

det(P,W):-
 W :->
        whole(P)<> .

%------------------------------------------------------------------------------
/** kill(+Parser, ?Wrapper)
Text:   Mutátor kill bez ohledu na výstup parseru vyprazdòuje seznam úspì¹ných
        rozkladù - za ka¾dých okolností tedy signalizuje neúspì¹ný rozklad
        (debug a mód filter/1).
*/
% Mutator kill makes list of successes empty -> termination (debug purposes)

kill(P,I+[]):-
 I+_ :->
        P.
        
%- EOF ------------------------------------------------------------------------
