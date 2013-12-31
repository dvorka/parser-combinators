%------------------------------------------------------------------------------
%
%                                  Error
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Predikáty pro chybová hlá¹ení a o¹etøení chyb.
        Chybová hlá¹ení jsou vypisována do výstupního proudu specifikovaného
        ve struktuøe pcStdErr/1. Proto¾e je to rys závislý na konkrétní
        implementaci interpretu, je pcStdErr/1 definována v odpovídajícím 
        zavadìèi knihovny (prolog/pc/pcSWI.pl, prolog/pc/pcBC.pl, apod.).
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                                 Error
%------------------------------------------------------------------------------
/** pcError(+Message)
Text:   Vypí¹e do proudu definovanovaného ve struktuøe pcStdErr/1, který
        je pova¾ován za standardní chybový výstup, hlá¹ku Message.
Arg:    Message
        Argument má stejný formát jako v pøípapadì printf/1.
Example:
        ?- pcError(error,['Wrong variable mode.',nl,nl]).
        Error :
         Wrong variable mode.

        Yes
*/

pcError(Masage):-
        pcStdErr(StdErr),
        telling(Old),tell(StdErr),
         printf(Masage),
        told,tell(Old).

%------------------------------------------------------------------------------
/**     pcError(+Error, +AdditionalParams)
Text:   Vypí¹e chybové hlá¹ení pro konkrétní chybu pomocí pcError/1.
Arg:    Error
        Atom identifikující chybu. 'error' se pou¾ívá pro bì¾ný výpis
        chyby. V tomto pøípadì obsahuje parametr AdditionalParams
        pouze hlá¹ku, která má být vypsána do standardního chybového
        proudu.
p       Dále jsou definovány identifikátory pro konkrétní typy chyb.
        V tìchto pøípadech se struktura AdditionalParams v jednotlivých
        pøípadech li¹í - viz zdrojový soubor.
Arg:    AdditionalParams
        Pøídavné parametry vztahující se ke konkrétní chybì.
*/
% Identifikatory chyb:
%       error                   ... chyba
%       eof                     ... neocekavany konec vstupniho souboru
% - mod ll1
%       ll1IllegalChar          ... nepripustny znak ve vyhledu: not(empty(P))
%       ll1Follow               ... nepripustny znak ve vyhledu: empty(P)
% - mod pseudoll1
%       ll1AltMissing           ... pro dany vyhled neexistuje alternativa
%                                   v kombinatoru alternativni kompozice

pcError(error,Masage):-
        pcErrorPrefix(s("")),
        pcError(Masage).
pcError(eof,[Input,Predicate]):-
        pcErrorPrefix(Input),
        pcError(['Unexpected end of input (',Predicate,').',nl]).

% LL1
pcError(ll1AltMissing,[Input,LA]):-
        pcErrorPrefix(Input),
        (isAscii(LA)
          -> ascii2Atom(LA,A),
             pcError(['There is no alternative for lookahead: ',LA,'/"',A,'"'])
          ;  pcError(['There is no alternative for lookahead: ',LA])).
pcError(ll1IllegalChar,[Input,LA,P]):-
        pcErrorPrefix(Input),
        (isAscii(LA)
          -> ascii2Atom(LA,A),
             pcError(['Illegal input symbol: ',LA,'/"',A,'" (',P,')',nl])
          ;  pcError(['Illegal input symbol: ',LA,' (',P,')',nl])).
pcError(ll1Follow,[Input,LA,FOLLOW]):-
        pcErrorPrefix(Input),
        (isAscii(LA)
          -> ascii2Atom(LA,A),
             pcError(['Illegal input symbol: ',LA,'/"',A,'" (FOLLOW case)',nl,'      FollowStack=',FOLLOW,nl,nl])
          ;  pcError(['Illegal input symbol: ',LA,' (FOLLOW case)'
                          ,nl,'      FollowStack=',FOLLOW,nl,nl])).

% pseudoLL1
pcError(pseudoll1WarnIllegalChar,[Input,LA,P]):-
        pcDeBugName(pseudoll1Warn)
        ->
        pcErrorWarningPrefix(Input),
        (isAscii(LA)
          -> ascii2Atom(LA,A),
             pcError(['Illegal input symbol: ',LA,'/"',A,'" (',P,')',nl])
          ;  pcError(['Illegal input symbol: ',LA,' (',P,')',nl]))
        ;
        true.
pcError(pseudoll1Follow,[Input,LA,FOLLOW]):-
        pcDeBugName(pseudoll1Warn)
        ->
        pcErrorWarningPrefix(Input),
        (isAscii(LA)
          -> ascii2Atom(LA,A),
             pcError(['Illegal input symbol: ',LA,'/"',A,'" (FOLLOW case)',nl,'      FollowStack=',FOLLOW,nl,nl])
          ;  pcError(['Illegal input symbol: ',LA,' (FOLLOW case)'
                          ,nl,'      FollowStack=',FOLLOW,nl,nl]))
        ;
        true.
pcError(pseudoll1Eof,[Input,Predicate]):-
        pcDeBugName(pseudoll1Warn)
        ->
        pcErrorWarningPrefix(Input),
        pcError(['Unexpected end of input (',Predicate,').',nl])
        ;
        true.


% unknown error identifiers
pcError(Idf,_):-
        pcErrorPrefix(s("")),
        pcError(['Unknown error identifier: ',Idf,nl,nl]),
        trace.

% auxiliary predicates
pcErrorPrefix(I):-
        pcStdErr(StdErr),
        fprintf(StdErr,[nl,'Error ']),
        (pcShowPosition(StdErr,I) ; true),      % uzivatelske mody
        fprintf(StdErr,[': ',nl,'   ']).

% warning to stderr
pcErrorWarningPrefix(I):-
        pcStdErr(StdErr),
        fprintf(StdErr,[nl,'Warning ']),
        (pcShowPosition(StdErr,I) ; true),      % uzivatelske mody
        fprintf(StdErr,[': ',nl,'   ']).

%- EOF ------------------------------------------------------------------------
