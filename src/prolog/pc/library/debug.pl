%------------------------------------------------------------------------------
%
%                  	       PC debug system
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Predik�ty j�dra lad�c�ho syst�mu knihovny.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     deBugName(+DebugFlag)
Text:   deBugName/1 je struktura, kter� obsahuje v�echna
        definovan� lad�c� jm�na. Vol�n� predik�tu tedy usp�je,
        pokud DebugFlag je aktivn� lad�c� jm�no tzn. lad�c� akce
        pod t�mto jm�nem se maj� prov�d�t. 
Arg: DebugFlag
        Atom, kter� je lad�c�m jm�nem.
Example:
        ?- deBugName(profile).
        Yes
*/

%pcDeBugName(_).                % all

pcDeBugName(verbose).           % verbose
pcDeBugName(warn).              % warnings
pcDeBugName(ll1).               % runtime ll1
%pcDeBugName(pseudoll1).         % runtime pseudoll1
pcDeBugName(pseudoll1Warn).     % mod ll1 (kolize ve vyhledu)
%pcDeBugName(eFirst).            % mod vypoctu epsilon a mny FIRST
pcDeBugName(modewarn).          % warnings
%pcDeBugName(debug).             % ladeni
pcDeBugName(optimize).          % optimalizace (kolize pri vytvareni 'tabulek')
%pcDeBugName(profile).           % profiling
pcDeBugName(shallow).           % ll1 <::>
pcDeBugName(follow).            % vypocet follow & testy nalezeni
pcDeBugName(expr).              % vyrazy

pcDeBugName(noname).            % E klauzule

%------------------------------------------------------------------------------
/**     showPcModeOptions
Text:   V�pis aktivn�ch lad�c�ch jmen, viz tak� pcStatistic/0.
*/

showPcDeBugNames:-
        printf([nl,'Active pcDeBugNames: ',nl]),
        showPcDeBugNames_,nl.

showPcDeBugNames_:-
        pcDeBugName(X), (X\=noname,printf([X,' ']) ; true),
        fail.
showPcDeBugNames_.

%------------------------------------------------------------------------------
/**     deBug(+DebugFlag, +Message)
Text:   Pokud je DebugName aktivn� lad�c� jm�no, vyp�e do proudu
        definovanovan�ho ve struktu�e pcStdErr/1, kter�
        je pova�ov�n za standardn� chybov� v�stup, hl�ku Message.
        Predik�t v�dy usp�je. Lad�c� jm�na jsou definov�na ve struktu�e
        pcDeBugName/1.
Arg: DebugName
        Atom, kter� je lad�c�m jm�nem.
Example:
        ?- deBug(showStatus,['Running...',nl]).
        Running...

        Yes
*/

deBug(Flag,Masage):-
    pcDeBugName(Flag) -> pcError(Masage) ; true.        % vypis na StdErr

%------------------------------------------------------------------------------
/**     deBugAssert(+Condition, +Message)
Text:   Pokud nen� spln�na podm�nka Condition, je pozastaveno vykon�v�n�
        programu, vyps�na hl�ka Message a spu�t�n intern� debugger
        interpretu.
Example:
        ?- deBugAssert(fail,[' some error.',nl]).
        Assert:
         some error.
        ?_
*/

deBugAssert(Cond,Masage):-
        Cond ; pcError([nl,'Assert:',nl,' '|Masage]),trace.

%------------------------------------------------------------------------------
/**     printf(+MessageString)
Text:   Vyp�e MessageString do aktu�ln�ho v�stupn�ho proudu.
Arg: MessageString
        �et�zec m��e obsahovat krom� prom�nn�ch a atom�, kter� maj� b�t
        vyps�ny, rovn� atomy se speci�ln�m v�znamem (nl a tab).
Example:
        ?- X is 1, printf(['Result is:',nl,tab,X]).
        Result is:
        .       1
        X=1
        Yes
*/

printf([X|Xs]):-
    (X==nl -> nl ; ( X==tab -> tab(10) ; write(X) ))
    ->
    printf(Xs).
printf([]).
        
%------------------------------------------------------------------------------
/**     printf(+Options,+MessageString)
Text:   Vyp�e MessageString dle Options.
Arg: Options
        Volby ur�uj� zp�sob v�pisu. Volba 'display' vypisuje termy pomoc�
        prologovsk�ho predik�tu display/1 tj. bez pou�it� oper�torov�
        notace, volba 'write' stejn� jako printf/1.
Arg: MessageString
        �et�zec m��e obsahovat krom� prom�nn�ch a atom�, kter� maj� b�t
        vyps�ny, rovn� atomy se speci�ln�m v�znamem ('nl' a 'tab').
Example:
        ?- X is 1+2, printf(display,['Result is: ',X,nl]).
        Result is: +(1, 2)
        Yes
*/

printf(display,[X|Xs]):-
    (X==nl -> nl ; ( X==tab -> tab(10) ; display(X) )),
    printf(display,Xs).
printf(write,M):-
    printf(M).
printf(_,[]).

%------------------------------------------------------------------------------
/**     fprintf(+MessageString)
Text:   Vyp�e MessageString do standardn�ho v�stupn�ho proudu, kter�
        je d�n strukturou pcStdOut/1. Predik�t je nezbytn� p�i pr�ci
        se soubory a pro pr�ci knihovn�ho debuggeru.
Arg: MessageString
        �et�zec m��e obsahovat krom� prom�nn�ch a atom�, kter� maj� b�t
        vyps�ny, rovn� atomy se speci�ln�m v�znamem ('nl' a 'tab').
*/

fprintf(Masage):-                       % presmeruj na stdout
        pcStdOut(StdOut),
        telling(Old),tell(StdOut),
         printf(Masage),
        told,tell(Old).

%------------------------------------------------------------------------------
/**     fprintf(+Stream,+MessageString)
Text:   Vyp�e MessageString do v�stupn�ho proudu Stream.
Arg: MessageString
        �et�zec m��e obsahovat krom� prom�nn�ch a atom�, kter� maj� b�t
        vyps�ny, rovn� atomy se speci�ln�m v�znamem ('nl' a 'tab').
Example:
        ?- X is 1+2, fprintf(user,['Result is: ',X,nl]).
        Result is: +(1, 2)
        Yes
*/

fprintf(Stream,Masage):-
        telling(Old),tell(Stream),
         printf(Masage),
        told,tell(Old).

%------------------------------------------------------------------------------
% fprintf(+Stream, +Options, +Masage)
% - Options:    (display,write)

fprintf(Stream,Options,Masage):-
        telling(Old),tell(Stream),
         printf(Options,Masage),
        told,tell(Old).

%- EOF ------------------------------------------------------------------------
