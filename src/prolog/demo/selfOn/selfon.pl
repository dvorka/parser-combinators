%------------------------------------------------------------------------------
%
%             		   Online self application
%
%				Martin Dvorak
%                                   1998
%------------------------------------------------------------------------------
/** Module: Online self application
Text:   Ukázka online self aplikace konstruktorù parserù. Gramatika
        v Backus-Naurovì formì je pøelo¾ena do 'asociativního
        environmentu gramatiky' - obdoby rozkladové tabulky, který
        je pøímo interpretovatelný pomocí konstruktorù parserù.
p       Pøesnìji, BNF gramatika je pøelo¾ena na parser tj. parser vydává
        místo rozkladového stromu parser pro jazyk generovaný gramatikou,
        který lze pou¾ít bez zásahu do databáze (konzultování generovaného
        kódu jako u offline) - co¾ vysvìtluje název tohoto pøíkladu.
*/
%------------------------------------------------------------------------------
% Komentar:
% - tento priklad vychazi z clanku:
%    Jeroen Fokker: Functional Parsers,
%     Advanced Functional Programming, LNCS 925, Springer-Verlag, 1995
%------------------------------------------------------------------------------
% Inicializace & knihovny

:- ['../../loadSWI'].          % SWI Prolog
%:- ['../../loadBP-2'].        % BinProlog

%------------------------------------------------------------------------------
%                                  Demo
%------------------------------------------------------------------------------

% Vytvoreni a pouziti parseru pascalovske blokove struktury
go1:-
        BNF        ="BLOCK ::= ` begin` BLOCK ` end` BLOCK | .",
        StartSymbol="BLOCK",
        Input      =s(" begin end begin begin begin end end end begin end"),

                parserGenerator(BNF, StartSymbol, Input+L),

        (L=[],printf(['Result: ',nl,L,nl])
         ;
         L=[_>T|_],gstShow(T)).

% Preklad BNF gramatiky do environmentu
go2:-
        bnf2Env('bnf/block.bnf','env/block.env').
go3:-
        bnf2Env('bnf/prolog.bnf','env/prolog.env').

% Premapovani bnf do Prologu
go4:-
        bnf2Src('bnf/block.bnf','_block.pl').
go5:-
        bnf2Src('bnf/prolog.bnf','_prolog.pl').

go:-
        printf(['Choose go1, go2, go3, go4 or go5.',nl]).

%------------------------------------------------------------------------------
%                          Vstupni bod programu
%------------------------------------------------------------------------------
/**     parserGenerator(+BNFstring, +StartSymbol, ?Wrapper)
Text:   Predikát online self aplikace.
*/

parserGenerator(BNFstring,StartSymbol,W):-
        % vytvor environment gramatiky
        bnf(s(BNFstring)+[_>GE|_]),
        % pomoci vytvoreneho environmentu analyzuj vstupni text
        % a vyber reseni
        W :->
                parseGrammar(GE,StartSymbol)<> .

%------------------------------------------------------------------------------
%                             Environmenty
%------------------------------------------------------------------------------
/**     assoc(+Environment, +Value, -Mapping)
Text:   Environment je seznamem dvojic tvaru 'value>itsImage'. Je
        reprezentací relace:
v       r: value --> image
        Predikát se pou¾ívá pro asociaci hodnoty Value se svým obrazem.
        Vstupem je Value, výstupem je mapování - obraz.
*/

assoc([V>M|_],V,M).
assoc([_>_|T],V,M):-
        assoc(T,V,M).

%------------------------------------------------------------------------------
/**     mapEnv(+Function, +Environment, -NewEnvironment)
Text:   Aplikuje funkci na v¹echny obrazy v environmentu.
Example:
        ?- mapEnv(+(1), [1 > 2, 2 > 5], X).
        X= [1 > 3, 2 > 6]
        Yes
*/

mapEnv(F,E,O):-
    mapList( fstTuple *>* (sndTuple=>F) ,E,O).

%------------------------------------------------------------------------------
%                               Gramatiky
%------------------------------------------------------------------------------
% Gramatika je z naseho pohledu mapovani mezi neterminalnimi symboly
% na leve strane pravidla a pravou stranou pravidla. Nasleduji parsery
% urcene pro analyzu BNF retezcu.
% Napriklad:
%               " BLOCK ::= `{` BLOCK `}` | COMMAND | . "
%------------------------------------------------------------------------------
% term(?Wrapper)
% - terminalni symbol v BNF retezci
% - ` je obraceny apostrof
term(W):-
 W :->
        #>(symbol("`") &> termParser <& symbol("`"))<> .


%------------------------------------------------------------------------------
% nonTerm(?Wrapper)
% - neterminalni symbol v BNF retezci
nonTerm(W):-
 W :->
        #>nonTermParser<> .


%------------------------------------------------------------------------------
% alt(?Wrapper)
% - analyza jedne alternativy z prave strany pravidla
alt(W):-
 W :->
        (term <: nonTerm)<*> .

%------------------------------------------------------------------------------
% rightSide(?Wrapper)
% - prava strana pravidla - seznam alternativ
rightSide(W):-
 W :->
        alt separatedBy #>symbol("|").

%------------------------------------------------------------------------------
% rule(?Wrapper)
% - pravidlo gramatiky
rule(W):-
 W :->
        nonTerm <&>> (#>token("::=") &> rightSide) <& #>symbol(".").

%------------------------------------------------------------------------------
% bnf(?Wrapper)
% - parser BNF retezce
% - vysledkem rozkladu je "asociativni" environment gramatiky
% - priklad:
%       ?- bnf(s(" BLOCK ::= `begin` BLOCK `end` BLOCK |  .")+L)
bnf(W):-
 W :->
        rule<*> .

%------------------------------------------------------------------------------
% Definice parseru pro reprezentaci terminalu a neterminalu uvnitr BNF
% retezce:

isNotAntiApostroph([X]):-
        X\==96.

%------------------------------------------------------------------------------
% termParser(?Wrapper)
% - vysledkem je dvojice: value>'terminal'
termParser(W):-
 W :->
        fulfil(isNotAntiApostroph)<+> <@ (id *>* const(terminal)).

%------------------------------------------------------------------------------
% termParser(?Wrapper)
% - vysledkem je dvojice: value>'nonTerminal'
% - neterminal dostava prefix 'nt', aby nebyl pokladan za promennou viz nize
nonTermParser(W):-
 W :->
        fulfil(isUprChar)<+>
                     <@ (nTRSem *>* const(nonTerminal)).

%------------------------------------------------------------------------------
% nTRSem(+NonTerminalString, -Result)
% - transformace jmena neterminalniho symbolu:
%       Jmeno neterminalu je modifikovano tak, aby bylo jednak pouzitelne 
%   ve zdrojovych souborech jako jmeno predikatu a dale aby se minimalizovala 
%   moznost vyskytu kolizi se jmeny vestavenych predikatu - pripojenim 
%   prefixu 'nt'.
nTRSem(I,R):-
    string2Atom([110,116|I],R).

%------------------------------------------------------------------------------
% Priklad gramatickeho pravidla:
%  BLOCK ::= `begin` BLOCK `end` BLOCK |  .
%       neterminaly ... ntBLOCK
%       terminaly   ... begin, end
% - tecka znaci konec pravidla
% - prazdna alternativa epsilon
%------------------------------------------------------------------------------
%                           Syntakticke stromy
%------------------------------------------------------------------------------
% Syntakticky strom muze mit libovony pocet vetvi. Proto pouzijeme
% reprezentaci vrcholu:
%                       t(Value, ListOfSons)

% mkNode(?Value, ?SonList, ?Node)
% - konstruktor vrcholu
mkNode(Value,SonList,t(Value,SonList)).
% - alternativa s prohozenymi argumenty (dve verze - kolony)
mkNodeS(SonList,Value,t(Value,SonList)).

%------------------------------------------------------------------------------
% gstShow(+Tree)
% - zobrazeni syntaktickeho stromu leziciho na levem boku
gstShow(Bst):-
        Width=80, Tab=3,
        gstShowRuler(Width,Tab),                                % pravitko
        gstShow_(Bst,0,Tab),
        gstShowRuler(Width,Tab).                                % pravitko

gstShow_(t(Val,Sonlist),Off,Ti):-
        Offset is Off+Ti,
        gstShowSons(Sonlist,Offset,Ti),
        tab(Off),write(Val),nl.     

gstShowSons([H|T],Offset,Ti):-       % zobrazeni synu od posledniho k prvnimu
        gstShowSons(T,Offset,Ti),
        gstShow_(H,Offset,Ti).
gstShowSons(t(Val,Sonlist),Offset,Ti):-
        gstShow_(t(Val,Sonlist),Offset,Ti).
gstShowSons([],_,_).

gstShowRuler(Total,D):-
        DD is D-1,
        gstShow__(Total,DD).
gstShow__(Total,D):-
        (Total>D
          -> write('I'), tab(D),
             T is Total-D-1,
             gstShow__(T,D)
          ;  nl).

%------------------------------------------------------------------------------
%                         Parsery misto gramatik
%------------------------------------------------------------------------------
% Pro danou gramatiku a startovni symbol chceme zkonstruovat parser jazyka,
% ktery gramatika popisuje. Proto je nutna dalsi transformace.

% parseTNT(+GrammarEnv, +TerminalOrNonTerminal > +ID, ?Wrapper)
% - analyzuj Terminal nebo NeTerminal

% terminalni symboly:
%  - pokusi se nacist ze vstupni retezce terminal
%  - ze ziskaneho vysledku je zkonsruovan listovy vrchol -> to je
%    semanticka hodnota seznamu uspesnych rozkladu.
%  - priklad:
%       ?- parseTNT(_,"identifer">terminal,s("identifer ")+L)
%          L= [s(" ") > t('identifier',[])]
%          Yes
parseTNT(_,TS>terminal,W):-
 W :->
        token(TS)  <@ (string2Atom => mkNodeS([])).

% neterminalni symboly:
% - nacte ze vstupniho retezce neterminalni symbol
% - pomoci environmentu gramatiky GE, ktery je jeho parametrem, se pokusi
%   vybrat dalsi pravidlo k rozkladu. Vybrane pravidlo je nasledne 
%   zkonvertovano na parser.
% - po navratu z parseru je zkonstruovan vrchol stromu
parseTNT(GE,NTS>nonTerminal,I+L):-
        % ziskej pravidlo
        assoc(GE,NTS>nonTerminal,RightSide),
        % ziskej zbytek rozkladu
        I+L_ :->
                parseRightSide(GE,RightSide),
        % zkonstruuj vnitrni uzel
        mapList(fstTuple *>* (sndTuple=>mkNode(NTS)),L_,L).

%------------------------------------------------------------------------------
% parseAlt(+GrammarEnv, Alternativa, ?Wrapper)
% - jedna alternativa: [ P1, P2, P3 ]
% - Pi je jeden terminal/non-terminal z pravidla
% - na kazdou polozku je volan parserTNT/3
parseAlt(GE,Alternative,W):-
        mapList(addArg(parseTNT(GE)),Alternative,ParserList),
        % vytvoreni konvoje <&>
        sequence(ParserList,Parser),
        % aplikace parseru
        W :->
                Parser.

%------------------------------------------------------------------------------
% parseRightSide(+GrammarEnvironmet, +RightSide, ?Wrapper)
% - vytvoreni parser pro pravou stranu pravidla: [[P1,P2,P3],[P5,P6],...]
% - prava strana je seznam alternativ - jedna z nich je pouzita
%   -> vytvorime konvoj <:>
parseRightSide(GE,RightSide,W):-
        mapList(addArg(parseAlt(GE)),RightSide,ParserList),
        % vytvoreni konvoje <:>
        choice(ParserList,Parser),  		
        W :->
                Parser.

%------------------------------------------------------------------------------
% parseGrammar(+GrammarEnvironment,+StartNonTerminal,?Wrapper)
% - gramatika
parseGrammar(GE,StartNonTerminal,W):-
        % konverze startovniho neterminalu na odpovidajici funkci
        nTRSem(StartNonTerminal,SNT),
        % odstartuj parsing - zacni ve startovnim neterminalu
        parseTNT(GE,SNT>nonTerminal,W).

%------------------------------------------------------------------------------
%                               Contrib
%------------------------------------------------------------------------------
/**     bnf2Env(+BnfFile, +EnvFile)
Text:   Pøelo¾í gramatiku v Backus-Naurovì formì do 'asociativního
        environmentu gramatiky' a ulo¾í jej do souboru EnvFile.
*/

bnf2Env(BnfFile,EnvFile):-
        printf([nl,'Parsing file ',BnfFile,' containing BNF to file ',EnvFile,'...']),
         invokeFile(bnf,BnfFile,[_>E|_]),
          saveFile(EnvFile,E,term),
        printf([' done!',nl]).

%------------------------------------------------------------------------------
/**     bnf2Src(+BnfFile, +SrcFile)
Text:   Pøelo¾í gramatiku v Backus-Naurovì formì do 'asociativního
        environmentu gramatiky' a ten pøemapuje na knihovní kostruktory
        a parsery. Získaný prologovský kód ulo¾í do souboru SrcFile.
*/

bnf2Src(BnfFile,SrcFile):-
        printf([nl,'Compiling BNF file ',BnfFile,' to Prolog source ',SrcFile,'...']),
         invokeFile(bnf,BnfFile,[_>E|_]),
          openFile(SrcFile,O,write),
           env2Src(E),
          closeFile(O,write),
        printf([' done!',nl]).

%------------------------------------------------------------------------------

env2Src(Env):-
     clause2Src(Env,_).

%------------------------------------------------------------------------------
clause2Src([],[]).
clause2Src([Head>Body|T],[Rule|NT]):-
    goal2Src(Head,HeadSrc),
    body2Src(Body,BodySrc),
    RuleHead =.. [HeadSrc,I+L],
    Rule=(RuleHead:- I+L :-> BodySrc),
     write(Rule),
     write('.'),
     nl,
    clause2Src(T,NT).

%------------------------------------------------------------------------------
body2Src(Alts,AltsSrc):-
    body2Src_(Alts,AltsSrcList),
    choice(AltsSrcList,AltsSrc).

body2Src_([],[]).
body2Src_([A|As],[(ASrc)|AsSrc]):-
    alt2Src(A,ASrc),
    body2Src_(As,AsSrc).

%------------------------------------------------------------------------------
alt2Src([],succeed([])).

alt2Src(Alt,AltSrc):-
    alt2Src_(Alt,AltSrcList),
    sequence(AltSrcList,AltSrc).

alt2Src_([],[]).
alt2Src_([I|Is],[OI|OIs]):-
    goal2Src(I,OI),
    alt2Src_(Is,OIs).

%------------------------------------------------------------------------------
goal2Src(Item>terminal,token(Item)).    % terminal must be parsed using token
goal2Src(Item>nonTerminal,Item).      	% non-terminal identifier is an atom
    
%- EOF ------------------------------------------------------------------------
    