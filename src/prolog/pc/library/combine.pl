%------------------------------------------------------------------------------
%
%                            Kombinatory parseru
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Kombin�tory parser� jsou predik�ty vy���ho ��du, kter� z jednoho
        nebo v�ce parser� vytv��ej� parser nov�.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     <&@>(+Parser1,+Parser2Fun, ?Wrapper)
Text:   Z�kladn� kombin�tor prov�d�j�c� obecnou sekven�n� kompozici parser�
        Parser1 a Parser2. Nejd��ve je na vstup aplikov�n Parser1.
        V z�skan� struktu�e LOS je v ka�d� jej� polo�ce na zbytek
        aplikov�n Parser2.
p       Oba v�sledky jsou kone�n� spojeny predik�tem Fun/3.
Arg:    Parser2Fun
        Parser2 - Fun, Fun je predik�t se signaturou:
v       functor(+P1Result, +P2NotParsed>P2Result, -Result)
Example:
        ?- s("-27E3")+L :-> symbolA("-") <&@> digitA - <&>>^^^ .
        L = [s("7E3") > '-'>'2']
        Yes
*/

<&@>(P1,P2-Fun,ll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(Fun,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
<&@>(P1,P2-Fun,pseudoll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(Fun,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).

<&@>(P1,P2-_,empty+L):-
        <&>>(P1,P2,empty+L).
<&@>(P1,P2-_,first+FIRST):-                      % vyuziva empty
        <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
<&@>(P1,P2-_,eFirst+eFirst(Empty,FIRST)):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<&@>(P1,P2-_,eFirst(assert)+eFirst(Empty,FIRST)):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

% <&@>(+Parser1, +Parser2, ?Wrapper)
<&@>(P1,P2-F,I+L):-
    I+L1 :-> P1,
    <&@>^(P2-F,L1,L-[]).

% <&@>^(+Parser2, +LOSof1, -ComposedDifLOS)
% - predicate gets LOS of the Parser1, on each LOS-member it calls Parser2 
%   and then does concatenation of both results: (N>R1>R2).
<&@>^(P2-F,[N1>R1|L1s],L12-D):-
    N1+L2 :-> P2,                       % call P2 on NotParsedRest
    <&@>^^(R1-F,L2,L12-D_),             % bind results of P1 and P2 using >/2
    <&@>^(P2-F,L1s,D_-D).
<&@>^(_,[],D-D).

% <&@>^^(+ResultOfP1, +LOSOfP2, +LOSOfP1P2 - -D)
% - predicate for the result concatenation
<&@>^^(R1-F, [N2>R2|L2s], [R|L12s]-D):-
        :-@ [F,R1,N2>R2,R],
        <&@>^^(R1, L2s, L12s-D).
<&@>^^(_, [], D-D).

%------------------------------------------------------------------------------
/**     <&>>(+Parser1, +Parser2, ?Wrapper)
Text:   Z�kladn� kombin�tor prov�d�j�c� sekven�n� kompozici parser�
        Parser1 a Parser2. Nejd��ve je na vstup aplikov�n Parser1.
        V z�skan� struktu�e LOS je v ka�d� jej� polo�ce na zbytek
        aplikov�n Parser2.
p       Oba v�sledky jsou kone�n� spojeny oper�torem >/2 - reprezentace
        n-tic v knihovn�.
Example:
        ?- s("-27E3")+L :-> symbolA("-") <&>> digitA.
        L = [s("7E3") > '-'>'2']
        Yes
*/

<&>>^^^(R1,N2>R2,[N2>(R1>R2)]).

<&>>(P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(<&>>^^^ ,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
<&>>(P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(<&>>^^^ ,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).

<&>>(P1,P2,empty+L):-
    empty+L1 :-> P1,
    empty+L2 :-> P2,
    pcAND(L1,L2,L).
<&>>(P1,P2,first+FIRST):-                      % vyuziva empty
    <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
<&>>(P1,P2,eFirst+eFirst(Empty,FIRST)):-
    eFirst+eFirst(Empty1,L1) :-> P1,           % pokud P1 prijima epsilon ->
    (Empty1=true -> first+L2 :-> P2            % do FIRST patri rovnez P2
                 ;  L2=[]),
    append(L1,L2,FIRST),
    <&>>(P1,P2,empty+Empty).
<&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)):-
        eFirst(<&>>(P1,P2),Empty,FIRST)
         ;
        ( eFirst(assert)+eFirst(Empty1,L1) :-> P1,
         ( Empty1=true -> eFirst(assert)+eFirst(Empty2,L2) :-> P2,
                          append(L1,L2,FIRST)
                       ;  ( eFirst(P2,Empty2,_) ; empty+Empty2:->P2 ),
                          FIRST=L1)
        ),
        pcAND(Empty1,Empty2,Empty),
        eFirstAdd(<&>>(P1,P2),Empty,FIRST).

% <&>>(+Parser1, +Parser2, ?Wrapper)
<&>>(P1,P2,I+L):-
    I+L1 :-> P1,
    <&>>^(P2,L1,L-[]).

% <&>>^(+Parser2, +LOSof1, -ComposedDifLOS)
% - predicate gets LOS of the Parser1, on each LOS-member it calls Parser2 
%   and then does concatenation of both results: (N>R1>R2).
<&>>^(P2,[N1>R1|L1s],L12-D):-
    N1+L2 :-> P2,                       % call P2 on NotParsedRest
    <&>>^^(R1,L2,L12-D_),               % bind results of P1 and P2 using >/2
    <&>>^(P2,L1s,D_-D).
<&>>^(_,[],D-D).

% <&>>^^(+ResultOfP1, +LOSOfP2, +LOSOfP1P2 - -D)
% - predicate for the result concatenation
<&>>^^(R1, [N2>R2|L2s], [N2>(R1>R2)|L12s]-D):-
        <&>>^^(R1, L2s, L12s-D).
<&>>^^(_, [], D-D).

%------------------------------------------------------------------------------
/**     <&(+Parser1, +Parser2, ?Wrapper)
Text:   Kombin�tor pro sekven�n� kompozici, kter� stejn� jako
        <&>> aplikuje na vstup oba parsery. Na rozd�l od <&>> je v�stupem
        <& pouze v�sledek z�skan� aplikac� Parser1. Nen� tedy prov�d�no
        spojen� v�sledk�.
Example:
        ?- s("-27E3")+L :-> symbolA("-") <& digitA.
        L = [s("7E3") > '-']
        Yes
*/

<&^^^(R1,N2>_,[N2>R1]).

<&(P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(<&^^^ ,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
<&(P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(<&^^^ ,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).

<&(P1,P2,empty+L):-
        <&>>(P1,P2,empty+L).
<&(P1,P2,eFirst+eFirst(Empty,FIRST)):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<&(P1,P2,first+FIRST):-
        <&>>(P1,P2,first+FIRST).
<&(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

% <&(+Parser1, +Parser2, ?Wrapper)
<&(P1,P2,I+L):-
    I+L1 :-> P1,
    <&^(P2,L1,L-[]).

<&^(P2,[N1>R1|L1s],L12-D):-
    N1+L2 :-> P2,
    <&^^(R1,L2,L12-D_),
    <&^(P2,L1s,D_-D).
<&^(_,[],D-D).

% <&^^(+ResultOfP1, +LOSOfP2, +LOSOfP1P2 - -D)
% - predikat pro vytvareni vysledku
<&^^(R1,[N2>_|L2s],[N2>R1|L12s]-D):-
        <&^^(R1,L2s,L12s-D).
<&^^(_,[],D-D).

%------------------------------------------------------------------------------
/**     &>(+Parser1, +Parser2, ?Wrapper)
Text:   Kombin�tor pro sekven�n� kompozici, kter� stejn� jako
        <&>> aplikuje na vstup oba parsery. Na rozd�l od <&>> je v�stupem
        &> pouze v�sledek z�skan� aplikac� Parser2. Nen� tedy prov�d�no
        spojen� v�sledk�.
Example:
        ?- s("-27E3")+L :-> symbolA("-") &> digitA.
        L = [s("7E3") > '2']
        Yes
*/

&>^^^(_,N2>R2,[N2>R2]).

&>(P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(&>^^^ ,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
&>(P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(&>^^^ ,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).

&>(P1,P2,empty+L):-
        <&>>(P1,P2,empty+L).
&>(P1,P2,eFirst+eFirst(Empty,FIRST)):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
&>(P1,P2,first+FIRST):-
        <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
&>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

% &>(+Parser1, +Parser2, ?Wrapper)
&>(P1,P2,I+L):-
    I+L1 :-> P1,
    &>^(P2,L1,L).

&>^(P2,[N1>_|L1s],L):-
    N1+L2 :-> P2,
    &>^(P2,L1s,L2s),            % L2 neni zadnym zpusobem transformovan
    append(L2,L2s,L).           % a neni tedy mozne jej zaroven s jinou
&>^(_,[],[]).                   % akci prevadet na difList -> built-in append

%------------------------------------------------------------------------------
/**     <&>(+Parser1, +Parser2, ?Wrapper)
Text:   Kombin�tor prov�d�j�c� sekven�n� kompozici parser� Parser1
        a Parser2. Nejd��ve je na vstup aplikov�n Parser1.
        V z�skan� struktu�e LOS je v ka�d� jej� polo�ce na zbytek
        aplikov�n Parser2.
p       Oba v�sledky jsou nakonec spojeny oper�torem |.
Example:
        ?- s("-27E3")+L :-> symbolA("-") <&> digitA.
        L = [s("7E3") > ['-','2']]
        Yes
*/

<&>^^^(R1,N2>R2,[N2>[R1|R2]]).

<&>(P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(<&>^^^ ,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
<&>(P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(<&>^^^ ,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).

<&>(P1,P2,empty+L):-
        <&>>(P1,P2,empty+L).
<&>(P1,P2,eFirst+eFirst(Empty,FIRST)):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<&>(P1,P2,first+FIRST):-
        <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
<&>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

% <&>(+Parser1, +Parser2, ?Wrapper)
<&>(P1,P2,I+L):-
    I+L1 :-> P1,
    <&>^(P2,L1,L-[]).

% Apply P2 on each NotParsed rest of P1
<&>^(P2,[N1>R1|L1s],L12-D):-
    N1+L2 :-> P2,
    <&>^^(R1,L2,L12-D_),
    <&>^(P2,L1s,D_-D).
<&>^(_,[],D-D).

% <&>^^(+ResultOfP1, +LOSOfP2, +LOSOfP1P2 - -D)
% - predicate for the result concatenation
<&>^^(R1, [N2>R2|L2s], [N2>[R1|R2]|L12s]-D):-
        <&>^^(R1, L2s, L12s-D).
<&>^^(_, [], D-D).

%------------------------------------------------------------------------------
/**     <:>(+Parser1, +Parser2, ?Wrapper)
Text:   Kombin�tor prov�d�j�c� alternativn� kompozici parser� Parser1
        a Parser2. Na vstup jsou aplikov�ny oba parsery a oba tedy vr�t�
        struktury LOS. Jejich konkatenace je v�stupem tohoto kombin�toru.
Example:
        ?- s("-27E3")+L :-> symbolA("-") <:> symbol("-").
        L = [s("7E3") > '-',  "7E3" > 45,]
        Yes
*/

<:>(P1,P2,ll1(A^D^F,LA,FOLLOW,I)+L):-
        ll1Code(<:>(P1,P2),P)
         ->
        ll1(A^D^F,LA,FOLLOW,I)+L :-> P
         ;
        (% "shallow link" pres <:>,:>,<: -> vytvoreni <::> -> dbf (P)
         shallowAltLinker(F,ll1,<:>(P1,P2),P),
         (D\=assert
          ;
          %deBug(ll1,['-:-> ',P,nl]),
          assert(ll1Code(<:>(P1,P2),P))),
         ll1(A^D^F,LA,FOLLOW,I)+L :-> P).

<:>(P1,P2,pseudoll1(A^O^D^F,LA,FOLLOW,I)+L):-
        pseudoll1Code(<:>(P1,P2),P)
         ->
        pseudoll1(A^O^D^F,LA,FOLLOW,I)+L :-> P
         ;
        (% shallow backtrack pres <:>,:>,<: -> vytvoreni <::> -> dbf (P)
         shallowAltLinker(F,pseudoll1,<:>(P1,P2),P),
         (D\=assert
          ;
                        %deBug(pseudoll1,['-:-> ',P,nl]),
          assert(pseudoll1Code(<:>(P1,P2),P))),
         pseudoll1(A^O^D^F,LA,FOLLOW,I)+L :-> P).

<:>(P1,P2,empty+L):-
        empty+L1 :-> P1,
        empty+L2 :-> P2,
        pcOR(L1,L2,L).
<:>(P1,P2,first+L):-
        first+L1 :-> P1,
        first+L2 :-> P2,
        append(L1,L2,L).
<:>(P1,P2,eFirst+eFirst(Empty,FIRST)):-
        <:>(P1,P2,empty+Empty),
        <:>(P1,P2,first+FIRST).
<:>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)):-
        eFirst(<:>(P1,P2),Empty,FIRST)
         ;
        (eFirstGetPut(P1,Empty1,FIRST1),       % prevence zacykleni
         eFirstGetPut(P2,Empty2,FIRST2),
         pcOR(Empty1,Empty2,Empty),append(FIRST1,FIRST2,FIRST),
         eFirstAdd(<:>(P1,P2),Empty,FIRST)).

<:>(P1,P2,I+L):-
    I+L1 :-> P1,
    I+L2 :-> P2,
    append(L1,L2,L).

%------------------------------------------------------------------------------
/**     <:(+Parser1, +Parser2, ?Wrapper)
Text:   Kombin�tor pro alternativn� kompozici parser�, kter� prov�d�
        zkr�cen� vyhodnocen� zleva. Pokud Parser1 usp�je, neprov�d�
        se ji� k�d parseru Parser2. V�stupem kombin�toru je tedy v�dy
        v�stup pouze jednoho z dvojice parser�.
Example:
        ?- s("-27E3"+L) :-> symbolA("-") <: symbol("-").
        L = [s("7E3") > '-']
        Yes
*/

<:(P1,P2,ll1(A^D^F,LA,FOLLOW,I)+L):-
        ll1Code(<:(P1,P2),P)
         ->
        ll1(A^D^F,LA,FOLLOW,I)+L :-> P
         ;
        (% "shallow link" pres <:>,:>,<: -> vytvoreni <::> -> dbf (P)
         shallowAltLinker(F,ll1,<:(P1,P2),P),
         (D\=assert
          ;
          %deBug(ll1,['-:-> ',P,nl]),
          assert(ll1Code(<:(P1,P2),P))),
         ll1(A^D^F,LA,FOLLOW,I)+L :-> P).

<:(P1,P2,pseudoll1(A^O^D^F,LA,FOLLOW,I)+L):-
        pseudoll1Code(<:(P1,P2),P)
         ->
        pseudoll1(A^O^D^F,LA,FOLLOW,I)+L :-> P
         ;
        (% "shallow link" pres <:>,:>,<: -> vytvoreni <::> -> dbf (P)
         shallowAltLinker(F,pseudoll1,<:(P1,P2),P),
         (D\=assert
          ;
          %deBug(pseudoll1,['-:-> ',P,nl]),
          assert(pseudoll1Code(<:(P1,P2),P))),
         pseudoll1(A^O^D^F,LA,FOLLOW,I)+L :-> P).

<:(P1,P2,empty+L):-
        <:>(P1,P2,empty+L).
<:(P1,P2,eFirst+eFirst(Empty,FIRST)):-
        <:>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<:(P1,P2,first+FIRST):-
        <:>(P1,P2,eFirst+eFirst(_,FIRST)).
<:(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)):-
        <:>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

<:(P1,P2,alt(ndet,I)+L):-
        <:>(P1,P2,alt(ndet,I)+L).

% <:(+Parser1, +Parser2, ?Wrapper)
<:(P1,P2,I+L):-
    I+L_ :-> P1,
    ( L_=[] -> I+L :-> P2 ; L=L_ ).

%------------------------------------------------------------------------------
/**     :>(+Parser1, +Parser2, ?Wrapper)
Text:   Kombin�tor pro alternativn� kompozici parser�, kter� prov�d�
        zkr�cen� vyhodnocen� zprava. Pokud Parser2 usp�je, neprov�d�
        se ji� k�d parseru Parser1. V�stupem kombin�toru je tedy v�dy
        v�stup pouze jednoho z dvojice parser�.
Example:
        ?- s("-27E3")+L :-> symbolA("-") :> symbol("-").
        L = [s("7E3") > 45]
        Yes
*/

:>(P1,P2,ll1(A^D^F,LA,FOLLOW,I)+L):-
        ll1Code(:>(P1,P2),P)
         ->
        ll1(A^D^F,LA,FOLLOW,I)+L :-> P
         ;
        (% "shallow link" pres <:>,:>,<: -> vytvoreni <::> -> dbf (P)
         shallowAltLinker(F,ll1,:>(P1,P2),P),
         (D\=assert
          ;
          %deBug(ll1,['-:-> ',P,nl]),
          assert(ll1Code(:>(P1,P2),P))),
         ll1(A^D^F,LA,FOLLOW,I)+L :-> P).

:>(P1,P2,pseudoll1(A^O^D^F,LA,FOLLOW,I)+L):-
        pseudoll1Code(:>(P1,P2),P)
         ->
        pseudoll1(A^O^D^F,LA,FOLLOW,I)+L :-> P
         ;
        (% "shallow link" pres <:>,:>,<: -> vytvoreni <::> -> dbf (P)
         shallowAltLinker(F,pseudoll1,:>(P1,P2),P),
         (D\=assert
          ;
         %deBug(pseudoll1,['-:-> ',P,nl]),
          assert(pseudoll1Code(:>(P1,P2),P))),
         pseudoll1(A^O^D^F,LA,FOLLOW,I)+L :-> P).

:>(P1,P2,empty+L):-
        <:>(P1,P2,empty+L).
:>(P1,P2,eFirst+eFirst(Empty,FIRST)):-
        <:>(P1,P2,eFirst+eFirst(Empty,FIRST)).
:>(P1,P2,first+FIRST):-
        <:>(P1,P2,eFirst+eFirst(_,FIRST)).
:>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)):-
        <:>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

:>(P1,P2,alt(ndet,I)+L):-
        <:>(P1,P2,alt(ndet,I)+L).

% :>(+Parser1, +Parser2, ?Wrapper)
:>(P1,P2,I+L):-
    I+L_ :-> P2,
    ( L_=[] -> I+L :-> P1 ; L=L_ ).

%------------------------------------------------------------------------------
%                         Mody ll1 a pseudoll1
%------------------------------------------------------------------------------
/**     <::>(+ParserEnv)
Text:   Kombin�tor alternativn� kompozice ur�en� v�hradn� pro m�dy
        ll1/4 a pseudoll1/4. Pracuje s mno�inami FIRST jednotliv�ch alternativ,
        co� spole�n� s pou�it�m v�hled� umo��uje prov�d�t pom�rn� efektivn�
        syntaktickou anal�zu. Zpravidla se nepou��v� p��mo, ale je generov�n
        bu� a� za b�hu, nebo optimaliz�torem (optimizeRegExprParser/3).
Arg:    ParserEnv
        Struktura tohoto parametru z�vis� na zp�sobu reprezentace
        (condFIRST / enumFIRST / bstFIRST) viz modeaux.pl.
*/

<::>(PE,ll1(A^D^F,LA,FOLLOW,I)+L):-
                                %deBug(ll1,[nl,'<::> Entering ...',nl]),
        % check lookAhead
        findCondEnumBstFIRST(PE,LA,Parser),
                                %deBug(ll1,[' <::> LookAhead: ',LA,nl,Parser,nl]),
        (Parser\=pcEmpty
          -> ll1(A^D^F,LA,FOLLOW,I)+L :-> Parser
          ;
                                %deBug(ll1,['<::> using FOLLOW: ',nl,FOLLOW,nl]),
             % Existuje epsilon prechod?
             findCondEnumBstFIRST(PE,pcEpsilon,ParserEpsilon),
             (ParserEpsilon\=pcEmpty
               -> % kontrola vyhledu pro epsilon prechod
                  ( findInFOLLOW(A,D,FOLLOW,LA) ->
                    ll1(A^D^F,LA,FOLLOW,I)+L :-> ParserEpsilon
                   ; 
                    pcError(ll1Follow,[I,LA,FOLLOW]),
                    L=[]) % vyhled neni ve FOLLOW
               ;  % neexistuje ani epsilon prechod
                  pcError(ll1IllegalChar,[I,LA,<::> / 2]),
                  L=[])
        ),
                                %ll1DeBug(Parser,ParserEpsilon,L,PE),
        true.

<::>(PE,pseudoll1(A^O^D^F,LA,FOLLOW,I)+L):-
                                %deBug(pseudoll1,['<::> Entering',nl]),
        % Check lookAhead (factorize join spojil duplicitni vyhledy)
        findCondEnumBstFIRST(PE,LA,Parser),
                                %deBug(pseudoll1,['<::> Lookahead: ',LA,nl,Parser,nl]),
        (Parser=pcEmpty                                         % nenalezen
          -> Lp=[]
          ;  pseudoll1(A^O^D^F,LA,FOLLOW,I)+Lp :-> Parser),     % ok
        % Bylo neco vypocteno nebo si zabacktrackujeme pres epsilon prechod?
        (Lp\=[]
        -> L=Lp                 % ok
        ;  ( % Bud nebyl parser nalezen nebo vypocet dle Parser skoncil s []
                                %deBug(pseudoll1,['<::> using FOLLOW: ',nl,FOLLOW,nl]),
             % Existuje epsilon prechod? (factorize join spojil duplicitni vyhledy)
             findCondEnumBstFIRST(PE,pcEpsilon,ParserEpsilon),
             (ParserEpsilon\=pcEmpty
               -> % kontrola vyhledu pro epsilon prechod
                  (O=noFOLLOW
                    -> pseudoll1(A^O^D^F,LA,FOLLOW,I)+L :-> ParserEpsilon
                    ;  (findInFOLLOW(A,D,FOLLOW,LA)
                        ->
                        pseudoll1(A^O^D^F,LA,FOLLOW,I)+L :-> ParserEpsilon
                        ;
                        pcError(pseudoll1Follow,[I,LA,FOLLOW]),
                        L=[]))
               ;  % Neexistuje ani epsilon prechod...
                  pcError(pseudoll1WarnIllegalChar,[I,LA,<::> / 2]),
                  L=[]))
        ),
                                %ll1DeBug(Parser,ParserEpsilon,L,PE),
        true.

<::>(PE,empty+L):-
        % prozkoumani environmentu a test na pritomnost atom pcEpsilon
        findCondEnumBstFIRST(PE,pcEpsilon,P),
        (P=pcEmpty -> L=false ; L=true).        % pcEmpty: parser neexistuje
<::>(P,first+FIRST):-
        env2First(P,FIRST).
<::>(P,eFirst+eFirst(Empty,FIRST)):-
        <::>(P,empty+Empty),
        env2First(P,FIRST).
<::>(P,eFirst(assert)+eFirst(Empty,FIRST)):-
        eFirst(<::>(P),Empty,FIRST)
         ;
        <::>(P,eFirst+eFirst(Empty,FIRST)),
        eFirstAdd(<::>(P),Empty,FIRST).

%       Kombinator <::> imlicitni klauzuli nema, protoze jeho pouziti je 
% pripustne v modech ll1 a pseudoll1. V pripade pokusu pouzit jiny mod
% skonci vypocet s chybou...



<::>(_,I+_):-
        pcError(error,['Using implicit clause of the <::>/2 in mode: ',nl,I,nl,nl,
                       'You are probably not in ll1/pseudoll1 mode.',nl,nl]),
        trace.

%------------------------------------------------------------------------------
/**     <:^>(+Parser1, +Parser2, ?Wrapper)
Text:   N�hrada za kombin�tor <:> v prost�ed� konstruktoru
        <::> zabra�uj�c� zbyte�n�m pokus�m o dodate�n� optimalizace v m�dech
        ll1/4 a pseudoll1/4.
*/
% Pouze implicitni klauzule - tento kombinator je pouzivan vyhradne
% v modech ll1 a pseudoll1 v tele alternativ uvnitr konstruktoru <::>.
% Nejsou proto nutne ani eFirst mody, protoze vse je jiz spocteno prave
% v <::>.

<:^>(P1,P2,I+L):-
        %deBug(pseudoll1,['<:^> Entering',nl]),
        I+L1 :-> P1,
        I+L2 :-> P2,
        append(L1,L2,L). % v pseudoll1 je vice nez jedna derivace pripustna


%------------------------------------------------------------------------------
/**     <:^(+Parser1, +Parser2, ?Wrapper)
Text:   N�hrada za kombin�tor <: v prost�ed� konstruktoru
        <::> zabra�uj�c� zbyte�n�m pokus�m o dodate�n� optimalizace v m�dech
        ll1/4 a pseudoll1/4.
*/
% Pouze implicitni klauzule - tento kombinator je vsak pouzivan vyhradne
% v modech ll1 a pseudoll1

<:^(P1,P2,I+L):-
        %deBug(pseudoll1,['<:^ Entering',nl]),
        I+L_ :-> P1,
        ( L_=[] -> I+L :-> P2 ; L=L_ ).

%------------------------------------------------------------------------------
/**     :>^(+Parser1, +Parser2, ?Wrapper)
Text:   N�hrada za kombin�tor :> v prost�ed� konstruktoru
        <::> zabra�uj�c� zbyte�n�m pokus�m o dodate�n� optimalizace v m�dech
        ll1/4 a pseudoll1/4.
*/
% Pouze implicitni klauzule - tento kombinator je vsak pouzivan vyhradne
% v modech ll1 a pseudoll1

:>^(P1,P2,I+L):-
        %deBug(pseudoll1,[':>^ Entering',nl]),
        I+L_ :-> P2,
        ( L_=[] -> I+L :-> P1 ; L=L_ ).


%------------------------------------------------------------------------------
%                       Aux - sekvencni kompozice
%------------------------------------------------------------------------------
% seqCompAux( +Fun, +Parser1, +Parser2, +LL1Selector+ -LOS)
% - Fun is predicate used for result composition:
%       fun(+Result1, +NotParsed2>Result2, -ResultComposition)

% ll1
%  - LOS muze obsahovat nejvyse jednu derivaci.
% lazy
seqCompAux(Fun,P1,P2,ll1(lazy^D,LA,FOLLOW,I)+L):-
        % leva vetev
        ll1(lazy^D,LA,[P2|FOLLOW],I)+L1 :-> P1,         % push P2, invoke P1
        % prava vetev
        (L1=[ll1(lazy^D,LA1,_,I1)>R1]
          -> ll1(lazy^D,LA1,FOLLOW,I1)+L2 :-> P2,       % invoke P2
             (L2=[N2>R2] -> :-@ [Fun,R1,N2>R2,L]        % eg. L=[N2>(R1>R2)]
                         ;  L=[])
          ;  L=[]).
% early
seqCompAux(Fun,P1,P2,ll1(early^D,LA,FOLLOW,I)+L):-
        % epsilon test prave vetve
        (D=assert -> eFirstGetPut(P2,Empty2,First2)
                  ;  eFirstGet(P2,Empty2,First2)),
        (Empty2=true -> append(First2,FOLLOW,FollowP1)  % FOLLOW stack je cond
                     ;  FollowP1=First2),               % list
        % leva vetev
        ll1(early^D,LA,FollowP1,I)+L1 :-> P1,           % invoke P1
        % prava vetev
        (L1=[ll1(early^D,LA1,_,I1)>R1]
          -> ll1(early^D,LA1,FOLLOW,I1)+L2 :-> P2,      % invoke P2
             (L2=[N2>R2] -> :-@ [Fun,R1,N2>R2,L]        % eg. L=[N2>(R1>R2)]
                         ;  L=[])
          ;  L=[]).

% pseudoll1
% lazy
seqCompAux(Fun,P1,P2,pseudoll1(lazy^Opt,LA,FOLLOW,I)+L):-
        % leva vetev
        % FOLLOW je reprezentovana jako seznam parseru -> push P2, invoke P1
        pseudoll1(lazy^Opt,LA,[P2|FOLLOW],I)+L1 :-> P1,    
        % prava vetev
        seqCompAuxCompose(Fun,P2,FOLLOW,L1,L-[]).

% early
seqCompAux(Fun,P1,P2,pseudoll1(early^O^D^F,LA,FOLLOW,I)+L):-
        % epsilon test prave vetve
        (D=assert -> eFirstGetPut(P2,Empty2,First2)
                  ;  eFirstGet(P2,Empty2,First2)),
        % FOLLOW je reprezentavana jako seznam podminek, stejne jako FIRST
        % ve strukture eFirst/3
        (Empty2=true -> append(First2,FOLLOW,FollowP1)
                     ;  FollowP1=First2),
        % leva vetev
        pseudoll1(early^O^D^F,LA,FollowP1,I)+L1 :-> P1,      % invoke P1
        % prava vetev
        seqCompAuxCompose(Fun,P2,FOLLOW,L1,L-[]).

% Auxiliary pseudoll1:
% - zkombinovani vsech vysledku v LOS
seqCompAuxCompose(Fun,P2,FOLLOW,[pseudoll1(Opt,LA,_,I1)>R1|L1s],L12-D):-
        pseudoll1(Opt,LA,FOLLOW,I1)+L2 :-> P2,       % change FOLLOW and invoke P2
        seqCompAuxCompose_(Fun,R1,L2,L12-D_),
        seqCompAuxCompose(Fun,P2,FOLLOW,L1s,D_-D).
seqCompAuxCompose(_,_,_,[],D-D).
seqCompAuxCompose(A,B,C,E, F):-
        deBugAssert(fail,[nl,'seqCompAuxCompose failed:',nl,A,nl,B,nl,C,nl,E,nl,F,nl,nl]).

seqCompAuxCompose_(Fun,R1,[N2>R2|L2s],[NR|L12s]-D):-
        :-@ [Fun,R1,N2>R2,[NR]],
        seqCompAuxCompose_(Fun,R1,L2s,L12s-D).
seqCompAuxCompose_(_,_,[],D-D).
seqCompAuxCompose_(A,B,C, E):-
        deBugAssert(fail,[nl,'seqCompAuxCompose_ failed:',nl,A,nl,B,nl,C,nl,E,nl,nl]).

%------------------------------------------------------------------------------
% Poznamky:
% - staticky vypocet empty, FIRST a mod ll1/pseudoll1 (presmerovani):
%
%   Relace -->:   X -> Y ... X vyuziva Y
%       (<&@>, <&>>, <&, &>, <&>)/3   -->   <&>>/3
%       (<&@>, <&>>, <&, &>, <&>)/2   -->   <&>>/3
%       (<:, :>)                      -->   <:>/3
%       <::>/1                        -->   <::>/2
%       <:^>, <:^, :>^                -->   na miste
%- EOF ------------------------------------------------------------------------
