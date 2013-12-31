%------------------------------------------------------------------------------
%
%                                  Error
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Predik�ty pro chybov� hl�en� a o�et�en� chyb.
        Chybov� hl�en� jsou vypisov�na do v�stupn�ho proudu specifikovan�ho
        ve struktu�e pcStdErr/1. Proto�e je to rys z�visl� na konkr�tn�
        implementaci interpretu, je pcStdErr/1 definov�na v odpov�daj�c�m 
        zavad��i knihovny (prolog/pc/pcSWI.pl, prolog/pc/pcBC.pl, apod.).
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                                 Error
%------------------------------------------------------------------------------
/** pcError(+Message)
Text:   Vyp�e do proudu definovanovan�ho ve struktu�e pcStdErr/1, kter�
        je pova�ov�n za standardn� chybov� v�stup, hl�ku Message.
Arg:    Message
        Argument m� stejn� form�t jako v p��papad� printf/1.
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
Text:   Vyp�e chybov� hl�en� pro konkr�tn� chybu pomoc� pcError/1.
Arg:    Error
        Atom identifikuj�c� chybu. 'error' se pou��v� pro b�n� v�pis
        chyby. V tomto p��pad� obsahuje parametr AdditionalParams
        pouze hl�ku, kter� m� b�t vyps�na do standardn�ho chybov�ho
        proudu.
p       D�le jsou definov�ny identifik�tory pro konkr�tn� typy chyb.
        V t�chto p��padech se struktura AdditionalParams v jednotliv�ch
        p��padech li�� - viz zdrojov� soubor.
Arg:    AdditionalParams
        P��davn� parametry vztahuj�c� se ke konkr�tn� chyb�.
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
