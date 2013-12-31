%------------------------------------------------------------------------------
%
%                           Optimalizace parseru
%
%				Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/** Module:
Text:   Optimalizace parserù na úrovni základních konstruktorù
        a pomocné predikáty pro konstrukci rozkladových tabulek módù
        ll1/4 a pseudoll1/4.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                                Optimizer
%------------------------------------------------------------------------------
/**     optimizeRegExprParserFile(+Input, +Output)
Text:   Optimalizuje parser ulo¾ený v souboru Input a ulo¾í jej do souboru
        Output - viz optimizeRegExprParser/4.
*/

optimizeRegExprParserFile(Input,Output):-
        printf([nl,nl,'Reading parser from:    ',Input]),
        openFile(Input,OldStream,read),
         read(Parser),
        closeFile(OldStream,read),
        optimizeRegExprParser(Parser,ParserO),
        saveFile(Output,ParserO,term).

%------------------------------------------------------------------------------
/**     optimizeRegExprParser(+Repr, +Mode, +P, -OptP)
Text:   Optimalizuje parser P, výsledek vydá v OptP. Optimalizace
        spoèívá pøedev¹ím v transformacích, které umo¾ní pou¾ít
        místo tradièní varianty kombinátoru alternativní kompozice
        konstruktor <::>. V pøípadì módu pseudoll1/4 jsou alternativy
        s mno¾inami FIRST, které nejsou disjunktní, sluèovány
        (je vypsáno rovnì¾ varování, pokud je aktivní ladící
        jméno 'optimize' viz debug.pl). Mód ll1/4 kolize nepøipou¹tí
        a tak se pøi jejím objevení optimalizace zastaví s chybou
        a výpisem tokenu, který ji zpùsobil.
Arg:    Repr
        Reprezentace okolí konstruktoru <::>. Pøípustné
        jsou atomy 'enum', 'set', 'bst' nebo 'cond'.
Arg:    Mode
        Cílový mód pro který má optimalizace probìhnout - 'pseudoll1'
        nebo 'll1'.
*/
% Optimalizaci parseru je nutne rozdelit na dve varianty: pro ll1 mod
% a pseudoll1 mod podle toho, zda vyhledy alternativ v alternativni
% kompozici jsou disjunktni nebo ne a jak se v takovem pripade zachovat.
%
% ll1 mod
%  - FIRST mny alternativ museji byt disjunktni, v pripade ze se zjisti
%    opak je nahlasena chyba a pozastaven vypocet.
%  - vsechny varianty kombinatoru alternativni kompozice jsou nahrazeny
%    konstruktorem <::> (vzhledem k disjunktnosti ztraci pouziti <: a :>
%    smysl). Bezprostredne navazujici kombinatory <:>, <: i :> se tedy 
%    linkuji v jedne fazi do <::>.
%
% pseudoll1 mod
%  - FIRST mny alternativ nemuseji byt disjunktni. Z toho vyplyvaji
%    nasledujici dusledky:
%    i) Pouziti <: a :> ma i nadale smysl. Proto se linkuje zvlast
%       <:>, zvlast <: a zvlast :>. Pokud pri linkovani konstruktoru
%       daneho typu mnoziny FIRST jednotlivych alternativ nejsou disjunktni,
%       provadi se jejich opetovne slouceni pomoci predikatu odpovidajiciho
%       typu <:^> (<:>),  <:^ (<:) a :>^ (:>). Slucovani je mozne pouzit 
%       pouze u vstupu ve forme ASCII kodu - vyhledu je konecne mnoho.
%       U condFIRST neni obecne mozne prunik detekovat viz nize.
%    ii) Po separatni "kondenzaci" se provadi jeste druhy pruchod, ktery
%       optimalizuje bezprostredne vnorene konstruktory <::> vytvorene 
%       behem pruchodu prvniho.
%
% Volba reprezentace *FIRST dle typu vstupu:
%  'enum'
%    ... vstupem jsou ASCII kody - konecny pocet vyhledu, provadi se slucovani
%        mnozin FIRST.
%  'set'
%    ... vstupem jsou ASCII kody - konecny pocet vyhledu, vnitrne se provede
%        slouceni v reprezentaci 'enum', pak je vygenerovana repr. 'set'.
%  'bst'
%    ... vstupem jsou ASCII kody - konecny pocet vyhledu, vnitrne se provede
%        slouceni v reprezentaci 'enum', pak je vygenerovan binarni 
%        vyhledavaci strom.
%  'cond'
%    ... vstupem libovolne termy - libovolny pocet vyhledu, reprezentace
%        mnozinou podminek. Mnoziny FIRST vzhledem k poctu vyhledu nelze
%        korektne sloucit. Slouceni lze vsak provest pro libovolnou mnozinu
%        termu se kterymi pracujeme. Implicitne se tedy provadi slouceni
%        pro ASCII kody, protoze rozklad takoveho typu vstupu je
%        nejobvyklejsi. * PRO OBECNOU MNOZINU TERMU VSAK NELZE ZARUCIT
%        VZAJEMNOU DISJUNKTNOST MNOZIN FIRST *.
%        Volby 'check'/'join' plati tedy pouze ve vyse uvedenem, omezenem
%        smyslu.

optimizeRegExprParser(R,M,Parser,OptimizedParser):-
        printf([nl,tab,tab,tab,'* Optimizer *',nl,
                'Representation: ',R,nl,'Mode: ',M,nl,
                'PASS 1...',nl]),
        optRep(R,M,Parser,OptParser),
        (M=pseudoll1
         -> printf([nl,'PASS 2...',nl]),
            optRep2(OptParser,OptimizedParser)
         ;  OptimizedParser=OptParser),
        printf(['Successfuly optimized!',nl,nl]).
        
% optRep(+Representation, +Parser, -OptimizedParser)
% - optimize Regular Expression Parser

% PASS 1:
optRep(R,M,<*>(P),<*>(Po)):-          optRep(R,M,P,Po).
optRep(R,M,<+>(P),<+>(Po)):-          optRep(R,M,P,Po).
optRep(R,M,<?>(P),<?>(Po)):-          optRep(R,M,P,Po).

optRep(R,M,<<*>>(P),<<*>>(Po)):-      optRep(R,M,P,Po).
optRep(R,M,<<+>>(P),<<+>>(Po)):-      optRep(R,M,P,Po).
optRep(R,M,<<?>>(P),<<?>>(Po)):-      optRep(R,M,P,Po).

optRep(R,M,<^>(P),<^>(Po)):-          optRep(R,M,P,Po).
optRep(R,M,<>(P),<>(Po)):-            optRep(R,M,P,Po).

optRep(R,M,<&>(LP,RP),<&>(LPo,RPo)):- optRep(R,M,LP,LPo),optRep(R,M,RP,RPo).
optRep(R,M,<&(LP,RP),<&(LPo,RPo)):-   optRep(R,M,LP,LPo),optRep(R,M,RP,RPo).
optRep(R,M,&>(LP,RP),&>(LPo,RPo)):-   optRep(R,M,LP,LPo),optRep(R,M,RP,RPo).

optRep(R,M,<@(P,F),<@(Po,F)):-        optRep(R,M,P,Po).
optRep(R,M,<?(P,F),<?(Po,F)):-        optRep(R,M,P,Po).
optRep(R,M,<?-(P,F),<?-(Po,F)):-      optRep(R,M,P,Po).
optRep(R,M,<@@(P,F),<@@(Po,F)):-      optRep(R,M,P,Po).

% Zde se chovani lisi dle modu:
% ll1:
optRep(R,ll1,<:(LP,RP),Linked):-  optRepShallowLinker(R,ll1,<:(LP,RP),Linked).
optRep(R,ll1,:>(LP,RP),Linked):-  optRepShallowLinker(R,ll1,:>(LP,RP),Linked).
optRep(R,ll1,<:>(LP,RP),Linked):- optRepShallowLinker(R,ll1,<:>(LP,RP),Linked).
% pseudoll1:
optRep(R,pseudoll1,<:(LP,RP),Linked):-
        optRepShallowLinker(R,pseudoll1,<:(LP,RP),Linked).
optRep(R,pseudoll1,:>(LP,RP),Linked):-
        optRepShallowLinker(R,pseudoll1,:>(LP,RP),Linked).
optRep(R,pseudoll1,<:>(LP,RP),Linked):-
        optRepShallowLinker(R,pseudoll1,<:>(LP,RP),Linked).

% explicitne neosetrene parsery tvori listy
optRep(_,_,P,P).

% PASS 2:                                                       (pseudoll1)
% - druhy pruchod slouzi pro optimalizaci vnorenych kostruktoru <::>
%   v modu pseudoll1

optRep2(<*>(P),<*>(Po)):-          optRep2(P,Po).
optRep2(<+>(P),<+>(Po)):-          optRep2(P,Po).
optRep2(<?>(P),<?>(Po)):-          optRep2(P,Po).
optRep2(<<*>>(P),<<*>>(Po)):-      optRep2(P,Po).
optRep2(<<+>>(P),<<+>>(Po)):-      optRep2(P,Po).
optRep2(<<?>>(P),<<?>>(Po)):-      optRep2(P,Po).
optRep2(<^>(P),<^>(Po)):-          optRep2(P,Po).
optRep2(<>(P),<>(Po)):-            optRep2(P,Po).
optRep2(<&>(LP,RP),<&>(LPo,RPo)):- optRep2(LP,LPo),optRep2(RP,RPo).
optRep2(<&(LP,RP),<&(LPo,RPo)):-   optRep2(LP,LPo),optRep2(RP,RPo).
optRep2(&>(LP,RP),&>(LPo,RPo)):-   optRep2(LP,LPo),optRep2(RP,RPo).
optRep2(<@(P,F),<@(Po,F)):-        optRep2(P,Po).
optRep2(<?(P,F),<?(Po,F)):-        optRep2(P,Po).
optRep2(<?-(P,F),<?-(Po,F)):-      optRep2(P,Po).
optRep2(<@@(P,F),<@@(Po,F)):-      optRep2(P,Po).

optRep2(<::>(Env),<::>(Linked)):-
        pass2Linker(Env,Linked).

optRep2(P,P).

% AUXILIARY
% Slinkovani vnorenych kombinatoru alt. komp. a vytvoreni konstruktoru <::>:
% ll1
optRepShallowLinker(R,ll1,AltComposition,<::>(Environ)):-
        % rekurzivni zpracovani parseru pod urovni <:> v optGetAlts
        optGetAlts(ll1,optRep(R,ll1),AltComposition,ParserList),
         optAlts2FstAlts(ParserList,CondFIRST),   % zapouzdrena condFIRST
          % cond reprezentace neni slucovana      % joiner se nepouziva
          (R=cond -> optFactorizeCond(check,ghost,CondFIRST,Factorized)      % out condFIRST
                  ;  optFactorizeByFIRST(check,ghost,CondFIRST,Factorized)), % out enumFIRST
           convertCondEnumBstFIRST(R,Factorized,Environ). % -> do cilove reprezentace

% pseudoll1 - varianty dle jednotlivych linkeru (<:^>) & spol.)
optRepShallowLinker(R,pseudoll1,AltComposition,<::>(Environ)):-
        optGetAlts(pseudoll1,optRep(R,pseudoll1),AltComposition,ParserList),
         optAlts2FstAlts(ParserList,CondFIRST),
          (AltComposition= <:>(LP,RP) -> Joiner= <:^> ;
           AltComposition= <:(LP,RP)  -> Joiner= <:^  ;
           AltComposition= :>(LP,RP)  -> Joiner= :>^  ),
          (R=cond -> optFactorizeCond(join,Joiner,CondFIRST,Factorized)      % out condFIRST
                  ;  optFactorizeByFIRST(join,Joiner,CondFIRST,Factorized)), % out enumFIRST
           convertCondEnumBstFIRST(R,Factorized,Environ).

%------------------------------------------------------------------------------
/**     optGetAlts(+Mode, +Optimizer, +Parser, -AltList)
Text:   Predikát pøedpokládá, ¾e Parser lze unifikovat s jedním z kombinátorù
        alternativní kompozice (<:>, <:, :>). Shromá¾dí bezprostøednì vnoøené
        alternativy a vydá je v seznamu AltList.
Arg:    Optimizer
        Predikát, jím¾ má být Parser optimalizován.
Arg:    Mode
        Cílový mód, pro který se sbírání alternativ provádí viz
        komentáø optimizeRegExprParser.
*/

optGetAlts(Mode,Opt,P,A):-
        optGetAlts_(Mode,Opt,P,A-[])
         ;
        pcError(error,['optGetAlts failed - parser probably is not alternative:',nl,P,nl,nl]).

% ll1
optGetAlts_(ll1,Opt,P,A-D):-
        (P= <:>(LP,RP) ; P= <:(LP,RP) ; P= :>(LP,RP))
        -> optGetAlts_(ll1,Opt,LP,A-D1),
           optGetAlts_(ll1,Opt,RP,D1-D)
        ;  % zde se provede zanoreni a optimalizace subparseru
           :-@ [Opt,P,OP],
           A=[OP|D].

% pseudoll1 ... linkovani stejnych typu
optGetAlts_(pseudoll1,Opt,P,A):-
        % rozskok dle typu
        P= <:>(LP,RP) -> optGetAlts_(<:>,pseudoll1,Opt,P,A)
         ;
        P= <:(LP,RP) ->  optGetAlts_(<:,pseudoll1,Opt,P,A)
         ;
        P= :>(LP,RP) ->  optGetAlts_(:>,pseudoll1,Opt,P,A).

optGetAlts_(<:>,pseudoll1,Opt,P,A-D):-
        P= <:>(LP,RP)
        -> optGetAlts_(<:>,pseudoll1,Opt,LP,A-D1),
           optGetAlts_(<:>,pseudoll1,Opt,RP,D1-D)
        ;  % zde se provede zanoreni a optimalizace subparseru
           :-@ [Opt,P,OP],
           A=[OP|D].
optGetAlts_(<:,pseudoll1,Opt,P,A-D):-
        P= <:(LP,RP)
        -> optGetAlts_(<:,pseudoll1,Opt,LP,A-D1),
           optGetAlts_(<:,pseudoll1,Opt,RP,D1-D)
        ;  % zde se provede zanoreni a optimalizace subparseru
           :-@ [Opt,P,OP],
           A=[OP|D].
optGetAlts_(:>,pseudoll1,Opt,P,A-D):-
        P= :>(LP,RP)
        -> optGetAlts_(:>,pseudoll1,Opt,LP,A-D1),
           optGetAlts_(:>,pseudoll1,Opt,RP,D1-D)
        ;  % zde se provede zanoreni a optimalizace subparseru
           :-@ [Opt,P,OP],
           A=[OP|D].

%------------------------------------------------------------------------------
/**     optAlts2FstAlts(+AltList, -FirstSetAltList)
Text:   Pro jednotlivé alternativy v seznamu AltList dopoète mno¾iny FIRST
        a získanou strukturu vydá v reprezentaci condFIRST vèetnì zapouzdøení.
*/
% - AltList je prologovsky seznam parseru
% - FirstSetAltList ma format:
%   condFIRST( [ [FirstSet1, Parser1]
%                 ...
%                [FirstSetN, ParserN]])
%   FirstSetI: [==(1),==(2)]
% - ke kazde alternative ze seznamu je dopoctena mna FIRST v pozadovane
%   reprezentaci
% - pokud je soucasti mnoziny FIRST epsilon, je do seznamu podminek
%   pridana nasledujici dvojice (Parser prijima epsilon):
%       [[==(pcEpsilon)],Parser] ... seznam s jednou podminkou

optAlts2FstAlts([P|T],condFIRST(Alts)):-
        % FIRST je v reprezentaci condFIRST bez zapouzdreni
        eFirst(assert)+eFirst(Epsilon,FIRST):-> P,
        (Epsilon=true   % epsilon flag
          -> Alts=[[[==([pcEpsilon])|FIRST],P]|FT]
          ;  Alts=[[FIRST,P]|FT]),
        optAlts2FstAlts(T,condFIRST(FT)).
optAlts2FstAlts([],condFIRST([])).

%------------------------------------------------------------------------------
/**     optFactorizeByFIRST(+Opt, +Joiner, +CondFIRST, -Factor)
Text:   Kontroluje disjunktnost mno¾in FIRST ve vstupním seznamu alternativ.
        Parsery, jejich¾ mny FIRST nejsou disjunktní, jsou v seznamu
        buï spojeny (pseudoll1/4) nebo je v tomto pøípadì nahlá¹ena
        chyba a vydán prázdný seznam (ll1/4).
Arg:    CondFIRST
        Je seznam alternativ spoleènì s jejich mno¾inami FIRST v reprezentaci
        condFIRST vytvoøený predikátem optAlts2FstAlts/3.
Arg:    Opt
        Atom 'join' v módu pseudoll1/4 nebo 'check' v modu ll1/4, viz popis
        predikátu.
Arg:    Joiner
        Predikát pro sluèování alternativ daný volbou 'join'.
*/
% Pripad 1:
%  Algoritmus:
%  - vstupem je condFIRST reprezentace v zapouzdreni
%  - provede se rozdrobeni do reprezentace enum (crumbling)
%     [ [LA1,P1],[LA2,P1],[LA3,P1],[LA5,P2] ]
%  - setrideni rozdrobenych vyhledu (sort/2) (odstranuje duplicity)
%  - podle modu:
%     join
%      kondenzace - bezprostredne nasledujici parsery se stejnym vyhledem
%      jsou spojeny kombinatorem, ktery je ziskan z argumentu Joiner
%     check
%      pokud je vyse popsana dvojice nalezena, je vypsana chyba - kolize
%      vyhledu
%  - na vystup je vydana joined/checked *FIRST (enum)
% Pripad 2:
%  - v pripade volby join a kombinatoru <: a :> zalezi na poradi
%    -> neni mozne tedy provadet trideni
%  - Algoritmus:
%   - vstupem je condFIRST reprezentace v zapouzdreni
%   - reprezentace condFIRST se konvertuje na set
%   - provede se kondezace
%   - nasleduje crumbling
%   - na vystup je vydana joined/checked *FIRST (set)

% Pripad 2:
optFactorizeByFIRST(join,<:^,condFIRST(C),setFIRST(Ordered)):-
        convertCondEnumBstFIRST(set,condFIRST(C),setFIRST(S)),
        optFactorizePartial(S,Condensed),
        optFactCondForceOrder(<:^,C,Condensed,Ordered).

optFactorizeByFIRST(join,:>^,condFIRST(C),setFIRST(Ordered)):-
        convertCondEnumBstFIRST(set,condFIRST(C),setFIRST(S)),
        optFactorizePartial(S,Condensed),
        optFactCondForceOrder(<:^,C,Condensed,Ordered).

% Pripad 1:
optFactorizeByFIRST(Opt,Joiner,condFIRST(C),enumFIRST(Condensed)):-
        % pouze rozdrobeni (duplicity se nedetekuji)
        convertCondEnumBstFIRST(enum,condFIRST(C),enumFIRST(Crumbled)),
        sort(Crumbled,SCrumbled),       % odstrani duplicity
        optFactorizeCondensation(Opt,Joiner,SCrumbled,Condensed).

% Pripad 1 Aux:
% join
optFactorizeCondensation(join,Joiner,[[LA1,P1],[LA2,P2]|T],O):-
        (LA1=LA2
          -> P=..[Joiner,P1,PP],
             O=[[LA,P]|TT], 
             (LA1=pcEpsilon
              -> deBug(optimize,[nl,'Warning:',nl,' LL(1): more then one epsilon alternative in:',nl,'  ',P1,nl,'  ',P2,nl,nl])
              ;  deBug(optimize,[nl,'Warning:',nl,' LL(1): collision between:',nl,'  ',P1,nl,'  ',P2,tab,' in token: ',LA1,nl,nl]))
          ;  O=[[LA1,P1],[LA,PP]|TT]),
        optFactorizeCondensation(join,Joiner,[[LA2,P2]|T],[[LA,PP]|TT]).
optFactorizeCondensation(join,_,[[LA2,P2]],[[LA2,P2]]).
optFactorizeCondensation(join,_,[],[]).                

% check
optFactorizeCondensation(check,J,[[LA1,P1],[LA2,P2]|T],O):-
        (LA1=LA2
          -> 
             (LA1=pcEpsilon
              -> pcError(error,['LL(1): more then one epsilon alternative in:',nl,'  ',P1,nl,'  ',P2,nl,nl])
              ;  pcError(error,['LL(1): collision between:',nl,'  ',P1,nl,'  ',P2,tab,' in token: ',LA1,nl,nl])),
             trace,
             O=[[LA1,P1],[LA2,P2]|T]
          ;  O=[[LA1,P1],[LA2,P2]|T]),
        optFactorizeCondensation(check,J,[[LA2,P2]|T],[[LA2,P2]|T]).
optFactorizeCondensation(check,_,[[LA2,P2]],[[LA2,P2]]).
optFactorizeCondensation(check,_,[],[]).                

% error detection
optFactorizeCondensation(V,_,I,_):-
        deBugAssert(fail,['Something wrong in optFactorizeCondensation',
                         nl,V,nl,I,nl,nl]).

% Pripad 2 Aux:
% optFactorizePartial(+CondFIRST,-FactorizedCondFIRST)
% - Algoritmus:
%   vybere se hlava seznamu alternativ a provadeji se iterace "nabalovani"
%   alternativ ostatnich na ni. Pokud zadna z alternativ neni behem iterace
%   pripojena, je konecne vytvoren uzaver - Faktor je kompletni.
optFactorizePartial([H|T],Closure):-
        optFactorizePartial_(H,T,Factor,Rest),    % sluc podle vyhledu H
        % pribylo neco k uzaveru?
        (H=Factor -> optFactorizePartial(Rest,Factors),         % ne
                     Closure=[Factor|Factors]
                  ;  optFactorizePartial([Factor|Rest],Factors),% ano
                     % jeste pridame v dalsich iteracich
                     Closure=Factors).
optFactorizePartial([],[]).

% optFactorizePartial(+Template,+List,-Factor,-ListRest)
% - pruchodem List jsou k Template pripojeny vsechny derivace s nimiz ma
%   Template neprazdny prunik v mne FIRST. Vysledny Factor je vydan ve ctvrtem
%   argumentu, nepripojene parsery v argumentu poslednim.
optFactorizePartial_([F,P],[[FA,PA]|T],Factor,Rest):-
        disjunctSets(F,FA)
         -> optFactorizePartial_([F,P],T,Factor,R),
            Rest=[[FA,PA]|R]
         ;  deBug(optimize,[nl,'Warning:',nl,' LL(1): collision between:',nl,'  ',P,nl,'  ',PA,nl,nl]),
            unionSets(F,FA,OF),
            (isList(P) -> PL=P ; PL=[P]), (isList(PA) -> PAL=PA ; PAL=[PA]),
            append(PL,PAL,OP),
            optFactorizePartial_([OF,OP],T,Factor,Rest).
optFactorizePartial_(Factor,[],Factor,[]).

%------------------------------------------------------------------------------
% optFactorizeCond(+Opt, +Joiner, +CondFIRST, -Factor)
% - analogie predchoziho predikatu, cilovou reprezentaci je ovsem opet
%   condFIRST (libovolny pocet vyhledu), optimalizuje se pro ASCII kody.
% - slucovani zachovava poradi
% - predchozi predikat nelze pouzit, protoze prevodem do enum reprezentace
%   a zpet se ztraci nizka prostorova slozitost cond reprezentace.
%  a naopak
%   tento predikat nelze pouzit misto predchoziho vzhledem k mozne ztrate
%   efektivity pri slucovani alternativ plynouci z velikosti mny vyhledu cond

optFactorizeCond(Opt,Joiner,condFIRST(Cond),condFIRST(Ordered)):-
        genCompleteFirstSet(255,X),
        optFactCond(Opt,X,condFIRST(Cond),condFIRST(Aux)),
        % vynuceni puvodniho poradi v alternativnich kompozicich <: a :>
        optFactCondForceOrder(Joiner,Cond,Aux,Ordered).

% Pribyl parametr s termy pro nez se kontrola provadi. Parametr muze obsahovat
% prologovsky seznam s libovolnymi termy.
optFactCond(Opt,[A|AA],condFIRST(Cond),condFIRST(Done)):-
        % najdi alternativy s FIRST obsahujici A
        (
         optFindAllCondFits(condFIRST(Cond),A,Factor,Rest) ; deBugAssert(fail,[nl,'Error:',nl,' optFactCond/4 failed',nl,nl])
        ),
        length(Factor,Length),
        (Length=<1 ->
         optFactCond(Opt,AA,condFIRST(Cond),condFIRST(Done))
         ;
         (Opt=join
          -> (A=pcEpsilon
              -> deBug(optimize,[nl,'Warning:',nl,' LL(1): more then one epsilon alternative in:',nl,'  ',Factor,nl,nl])
              ;  deBug(optimize,[nl,'Warning:',nl,' LL(1): collision between:',nl,'  ',Factor,nl,tab,' in token: ',A,nl,nl])),
              % vytvoreni faktoru (link podminek, Join parseru)
              optFactCondMakeNew(Factor,[NC,NP]),
              optimizeCond(NC,OptC),
              % ukaz co jsi vytvoril
              deBug(optimize,['Joined: ',nl,[OptC,NP],nl,nl]),
              % rekurzivne s novou mnozinou
              optFactCond(Opt,AA,condFIRST([[OptC,NP]|Rest]),condFIRST(Done))
          ;   % Opt=check
              (A=pcEpsilon
               -> pcError(error,['LL(1): more then one epsilon alternative in:',nl,'  ',Factor,nl,nl])
               ;  pcError(error,['LL(1): collision between:',nl,'  ',Factor,nl,tab,' in token: ',A,nl,nl])),
              trace,
              % hledani kolizi ve zbytku
              optFactCond(Opt,AA,condFIRST(Cond),condFIRST(Done))
         )
        ).
optFactCond(_,[],condFIRST(Cond),condFIRST(Cond)).

% optFactCondMakeNew(+Factor,-New):-
% - delka factoru >= 2
% - slouci a zoptimalizuji se mny FIRST jednotlivych alternativ
% - parsery se posbiraji do seznamu. Jejich join se provede az nasledne
%   v optFactorizeCond
% - factor muze obsahovat polozky kde P je bud seznam parseru nebo
%   parser, proto nutne osetreni

optFactCondMakeNew([[C,Pi]|T],[CCC,PPP]):-
        (isList(Pi) -> P=Pi ; P=[Pi]),
        optFactCondMakeNew(T,TT),
        (TT=[CC,PP] -> append(P,PP,PPP),append(C,CC,CCC)
                    ;  CCC=C, PPP=P).
optFactCondMakeNew([],[]).

% optFactCondForceOrder(+Joiner,+NeOrigCondFIRST,+Unsorted,-Sorted)
% - upravi seznamy, ktere pripravil optFactCondMakeNew tak, aby
%   bylo zachovano puvodni poradi v <: / :> / <:> a provede join.
% - Unsorted:
%   [ [FIRST1, [P1,P2,P3]],
%     [FIRST2, P5 ],
%     [FIRST3, [P6,P7,P8]]]
% - vzorem pro trideni pred joinem je puvodni condFIRST tj. NotEncapsCondFirst

optFactCondForceOrder(Joiner,Orig,[[O,Unsort]|T],[[O,SPs]|TT]):-
        (isList(Unsort)
          ->
             optFactCondSort(Joiner,Orig,Unsort,SPs)
          ;  % v parametru neni seznam ale parser
             Unsort=SPs),
        optFactCondForceOrder(Joiner,Orig,T,TT).
optFactCondForceOrder(_,_,[],[]).

% optFactCondSort(+Joiner,+OrigCondFirst,+ParserList,-SortedParserList)
% - prochazi se puvodni condFIRST, kazdy prvek z ni se pokusim vytahnout
%   z unsorted -> taham v poradi v jakem to bylo v puvodni...
% - ve spravnem poradi se uz spoji...
optFactCondSort(Joiner,[[_,P]|T],Unsort,Sort):-
        optFactCondSort(Joiner,T,Unsort,TT),
        (member(P,Unsort)
          -> (TT=[] -> Sort=P ; Sort=..[Joiner,P,TT])
          ;  Sort=TT).
optFactCondSort(_,[],_,[]).

% optFindAllCondFits(+EncapsCondFIRST,+LA,Factor,-RestEncapsCondFIRST)
% - posbira vsechny polozky do jejichz FIRST patri vyhled LA
optFindAllCondFits(condFIRST([[C,P]|T]),LA,Factor,R):-
        (firstSetFitCond(C,LA)
         -> Factor=[[C,P]|Ps],R=RR
         ;  Factor=Ps,R=[[C,P]|RR]),
        optFindAllCondFits(condFIRST(T),LA,Ps,RR).
optFindAllCondFits(condFIRST([]),_,[],[]).


%------------------------------------------------------------------------------
%                           Optimize: Pass 2
%------------------------------------------------------------------------------
% Komentar:
% - optimalizace "vytahuje" odpovidajici alternativy:
%   *enum
%     <::>([LA, <::>([[LA1,P1],[LA,P2],[LA,P3]]))
%   ... z vnoreneho environmentu se vytahne alternativa s danym vyhledem 
%       tj. LA, zbyle dve se se stejne namaji sanci uplatnit a tak se odstrani
%     <::>([LA, P2])
%   *bst
% - ad enum
%------------------------------------------------------------------------------
% pass2Linker(+Env, -Linked)
% - argumenty jsou environmenty z <::> (condFIRST & spol.)

pass2Linker(condFIRST(C),condFIRST(C)).                         % neprovadi se
pass2Linker(setFIRST(C),setFIRST(C)).                           % neprovadi se
pass2Linker(bstFIRST(B),bstFIRST(Linked)):-
        bst2List(B,E),
        pass2Linker(enumFIRST(E),enumFIRST(L)),
        list2Bst(L,Linked).

pass2Linker(enumFIRST(E),enumFIRST(Linked)):-
        % Z bezprostredne vnoreneho <::> vybali alternativu dle jejiho
        % vyhledu, ostatni alternativy se totiz beztak neuplatni a jsou zde 
        % tedy zbytecne. Vytazene telo alternativy optimalizuje rekurzivne.
        pass2Flatten(E,Linked).

%------------------------------------------------------------------------------
% pass2Flatten(E,Flat)
% - E je seznam z tela enumFIRST

pass2Flatten([[LA,<::>(enumFIRST(Env))]|T],[[LA,PP]|TT]):-
        % nalezeni alternativy pro vyhled
        (member([LA,P],Env) ; deBugAssert(fail,['pass2Flatten/2 alt failed',nl,nl])),
        % optimalizace tela
        optRep2(P,PP),
        % zbytek
        pass2Flatten(T,TT).
pass2Flatten([[LA,P]|T],[[LA,PP]|TT]):-
        optRep2(P,PP),
        pass2Flatten(T,TT).
pass2Flatten([],[]).

%- EOF ------------------------------------------------------------------------
