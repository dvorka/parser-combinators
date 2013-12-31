%------------------------------------------------------------------------------
%
%                  	       PC debug system
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Predikáty jádra ladícího systému knihovny.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     deBugName(+DebugFlag)
Text:   deBugName/1 je struktura, která obsahuje v¹echna
        definovaná ladící jména. Volání predikátu tedy uspìje,
        pokud DebugFlag je aktivní ladící jméno tzn. ladící akce
        pod tímto jménem se mají provádìt. 
Arg: DebugFlag
        Atom, který je ladícím jménem.
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
Text:   Výpis aktivních ladících jmen, viz také pcStatistic/0.
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
Text:   Pokud je DebugName aktivní ladící jméno, vypí¹e do proudu
        definovanovaného ve struktuøe pcStdErr/1, který
        je pova¾ován za standardní chybový výstup, hlá¹ku Message.
        Predikát v¾dy uspìje. Ladící jména jsou definována ve struktuøe
        pcDeBugName/1.
Arg: DebugName
        Atom, který je ladícím jménem.
Example:
        ?- deBug(showStatus,['Running...',nl]).
        Running...

        Yes
*/

deBug(Flag,Masage):-
    pcDeBugName(Flag) -> pcError(Masage) ; true.        % vypis na StdErr

%------------------------------------------------------------------------------
/**     deBugAssert(+Condition, +Message)
Text:   Pokud není splnìna podmínka Condition, je pozastaveno vykonávání
        programu, vypsána hlá¹ka Message a spu¹tìn interní debugger
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
Text:   Vypí¹e MessageString do aktuálního výstupního proudu.
Arg: MessageString
        Øetìzec mù¾e obsahovat kromì promìnných a atomù, které mají být
        vypsány, rovnì¾ atomy se speciálním významem (nl a tab).
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
Text:   Vypí¹e MessageString dle Options.
Arg: Options
        Volby urèují zpùsob výpisu. Volba 'display' vypisuje termy pomocí
        prologovského predikátu display/1 tj. bez pou¾ití operátorové
        notace, volba 'write' stejnì jako printf/1.
Arg: MessageString
        Øetìzec mù¾e obsahovat kromì promìnných a atomù, které mají být
        vypsány, rovnì¾ atomy se speciálním významem ('nl' a 'tab').
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
Text:   Vypí¹e MessageString do standardního výstupního proudu, který
        je dán strukturou pcStdOut/1. Predikát je nezbytný pøi práci
        se soubory a pro práci knihovního debuggeru.
Arg: MessageString
        Øetìzec mù¾e obsahovat kromì promìnných a atomù, které mají být
        vypsány, rovnì¾ atomy se speciálním významem ('nl' a 'tab').
*/

fprintf(Masage):-                       % presmeruj na stdout
        pcStdOut(StdOut),
        telling(Old),tell(StdOut),
         printf(Masage),
        told,tell(Old).

%------------------------------------------------------------------------------
/**     fprintf(+Stream,+MessageString)
Text:   Vypí¹e MessageString do výstupního proudu Stream.
Arg: MessageString
        Øetìzec mù¾e obsahovat kromì promìnných a atomù, které mají být
        vypsány, rovnì¾ atomy se speciálním významem ('nl' a 'tab').
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
