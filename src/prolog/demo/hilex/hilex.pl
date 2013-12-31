%------------------------------------------------------------------------------
%
%                                 * Hilex *
%
%                               Martin Dvorak
%                                    2000
%------------------------------------------------------------------------------
/**     Module: HiLex
Text:   HiLex je program urèený pro pøevádìní souborù se zdrojovými texty
        do jejich HTML formy se syntax highlightingem. 
        Vhodný je pøedev¹ím pro zvý¹ení èitelnost fragmentù zdrojových textù 
        vkládaných do dokumentace vytváøené v tomto formátu.

        Struktura vstupního textu a po¾adovaný zpùsob zapouzdøení se
        specifikuje v konfiguraèních souborech, z nich¾
        je vygenerován zapouzdøovaè tvoøený knihovními parsery a konstruktory, 
        který je mo¾né buï pøímo aplikovat nebo ulo¾it pro pozdìj¹í pou¾ití.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
% Komentar:
%       Nejrychlejsim nastavenim je pseudoll1=yes, environemnt=cond,
% optimize=yes (druhy pruchod, kdy jsou jiz vygenerovany rozkladove tabulky).
% Ukazuje se totiz, ze pro rychlost je nejvice rozhodujici
% prostorova slozitost. Zbyle tri reprezentace (enum,set,bst) jsou 
% optimalizovany na rychlost (tj. rychle vyhledavani) za cenu vetsich 
% pametovych naroku. Tato cena je vsak prilis vysoka.
%       Paradoxne druhou nejrychlejsi konfiguraci je pseudoll1=no, optimize=no.
% Vse ale zalezi na pouzitem interpretu a predevsim na jeho praci s pameti 
% (copy,GC), z vyse uvedenych duvodu.
%      pseudoll1 mod je vhodny predevsim pri vytvareni novych zapozdrovacu,
% a neuspesnem zpracovani vstupniho textu, protoze je schopen presne
% lokalizovat misto vyskytu chyby tj. neosetrene resp. zavadne syntakticke
% konstrukce. Dalsi viz hlxOptionIs/2.
%       Pro rychlejsi beh je take vhodne ulozit jednou vypocteny kod pomoci
% modeSave/0 a pri naslednem pouzivani jej opet zavest pomoci modeLoad/0.
%------------------------------------------------------------------------------
%                              Config & knihovny
%------------------------------------------------------------------------------

 :-op(500,xfy,hlxOptionIs).

 % Volby:
 debug       hlxOptionIs no.         % (yes,no), rovnez pc/library/debug.pl
 pseudoll1   hlxOptionIs yes.        % (yes,no)
 environment hlxOptionIs cond.       % reprezentace env v <::>, mod pseudoll1
                                     % (cond,enum,set,bst)
 optimize    hlxOptionIs yes.        % optimalizace zapouzdrovacu (yes,no)
                                     % parsery jsou optimalizovany pro mod
                                     % pseudoll1, spusteni v beznem modu
                                     % nema zadny efekt, protoze:
        % Pokud je zapouzdreni provedeno s optimize=yes, pouziva se
        % konstruktor <::>/2, ktery je definovan pouze pro mody ll1/pseudoll1.
        % Spusteni v s/file/... tedy skonci na assert.

 foo         hlxOptionIs nothing.

 % Knihovny:
 :- ['../../loadSWI'].               % SWI Prolog
 makeStandaloneApplication(hilex):- qsave_program('hilex').

 %:- ['../../loadBP-2'].             % BinProlog

%-----------------------------------------------------------------------------
%                                    Demo
%------------------------------------------------------------------------------

% Konfiguracni soubor *.hlx -> zapouzdrovac *.coc:
cocHilex:-      hilex('hlx/hilex.hlx','coc/hilex.coc').
cocProlog:-     hilex('hlx/prolog.hlx','coc/prolog.coc').
cocJava:-       hilex('hlx/java.hlx','coc/java.coc').
cocCpp:-        hilex('hlx/cpp.hlx','coc/cpp.coc').
cocPascal:-     hilex('hlx/pascal.hlx','coc/pascal.coc').
cocHilexTable:-      hilex('hlx/hilextable.hlx','coc/hilextable.coc').
cocPrologTable:-     hilex('hlx/prologtable.hlx','coc/prologtable.coc').
cocJavaTable:-       hilex('hlx/javatable.hlx','coc/javatable.coc').
cocCppTable:-        hilex('hlx/cpptable.hlx','coc/cpptable.coc').
cocPascalTable:-     hilex('hlx/pascaltable.hlx','coc/pascaltable.coc').
cocPrologdoc:-       hilex('hlx/prologdoc.hlx','coc/prologdoc.coc').

% Konverze zdrojovych souboru do HTML:
htmlHilex:-     hilex('coc/hilex.coc','inputs/input.hlx','examples/hilex.htm').
htmlProlog:-    hilex('coc/prolog.coc','inputs/input.pl','examples/prolog.htm').
htmlJava:-      hilex('coc/java.coc','inputs/input.java','examples/java.htm').
htmlCpp:-       hilex('coc/cpp.coc','inputs/input.cpp','examples/cpp.htm').
htmlPascal:-    hilex('coc/pascal.coc','inputs/input.pas','examples/pascal.htm').
                
% Davky:
hlxCocAll:-     cocHilex, cocProlog, cocJava, cocCpp, cocPascal.
hlxCocTables:-  cocHilexTable, cocPrologTable, cocJavaTable, cocCppTable, cocPascalTable.
hlxHtmlAll:-    htmlHilex, htmlProlog, htmlJava, htmlCpp, htmlPascal.

%------------------------------------------------------------------------------
%                                Benchmark
%------------------------------------------------------------------------------

hilexBech(lpa):-
 pcBenchmark('LPA',hlxCoconize('hilex/prolog.coc',
                               'hilex/bench1.txt','hilex/output.htm')).
hilexBech(swi):-
 pcBenchmark('SWI',hlxCoconize('hilex/prolog.coc',
                               'hilex/bench1.txt','hilex/output.htm')).
hilexBech(bp):-
 pcBenchmark('BP', hlxCoconize('hilex/prolog.coc',
                               'hilex/bench1.txt','hilex/output.htm')).

%------------------------------------------------------------------------------
%                            Vstupni bod programu
%------------------------------------------------------------------------------
/**     hilex
Text:   Nápovìda programu.
*/
hilex:-
        printf(['Hilex help:',nl,
                ' hilex',nl,
                '  ... online help',nl,
                ' hilex(+FileHLX, +FileCOC)',nl,
                '  ... create coconizer *.coc from configuration *.hlx file',nl,
                ' hilex(+LanguagueOrCoconizerFile, +InputFile, +OutputFile)',nl,
                '  ... encapsulate file InputFile using LanguagueOrCoconizerFile',nl,
                '      file (or use prepared coconizers: prolog,cpp,hilex,pascal,java)',nl,
                '      and store result into OutputFile',nl,
                'Examples:',nl,
                ' ?- hilex(prolog.hlx, prolog.coc).',nl,
                ' ?- hilex(prolog.coc, source.coc, dest.html).',nl,
                ' ?- hilex(java, source.java, dest.html).',nl,
                'Bye!',nl]).

%------------------------------------------------------------------------------
/**     hilex(+FileHLX, +FileCOC)
Text:   Z konfiguraèního souboru FileHLX vygeneruje zapouzdøovaè
        do souboru FileCOC.
*/

hilex(I,C):-
    hlxFile2Coconizer(I,C).

/**     hilex(+LanguagueOrCoconizerFile, +InputFile, +OutputFile)
Text:   Pøevede zdrojový soubor InputFile do HTML pomocí zapozdøovaèe
        LanguagueOrCoconizerFile a výsledek ulo¾í do OutputFile.
        V¹echny argumenty jsou atomy.
Arg: LanguagueOrCoconizerFile
        Pokud je vázán na jeden z atomù: 'prolog', 'cpp', 'hilex', 'pascal'
        nebo 'java', je pou¾it pøipravený zapouzdøovaè. V opaèném
        pøípadì se pøedpokládá, ¾e je argument vázán na cestu
        k souboru *.coc, který zapouzdøovaè obsahuje, nebo na
        samotný konfiguraèní soubor *.hlx.
*/

hilex(C,I,O):-
    name(C,CS),
    (
     (C==prolog;C==cpp;C==hilex;C==pascal;C==java)
       ->
      mAppend(["coc/",CS,".coc"],CS_), name(CoconizerFile,CS_),
      hlxCoconize(CoconizerFile,I,O)
       ;
      append(_,".coc",CS) -> hlxCoconize(C,I,O)
       ;
      append(Name,".hlx",CS) -> append(Name,".coc",CName),name(ACName,CName),
                                hilex(C,ACName), hlxCoconize(ACName,I,O)
    ).

%------------------------------------------------------------------------------
%                 Vytvoreni parseru podle regularniho vyrazu
%-----------------------------------------------------------------------------
% hlxChar(?Wrapper)
% - prijima znak, ktery neni specialni tj. ruzny od \*+?[]|().

hlxChar(W):-
        nonSymbols([92|"*+?[]|()"],W).          % ascii2Atom(92,'\')

%-----------------------------------------------------------------------------
% hlxBackChar(?Wrapper)
% - akceptuje znak, jemuz predchazi zpetne lomitko
% - priklad:
%    \a ... a
%    \\ ... backslash
% - specialni sekvence:
%    \n ... new line (0x0a)
%    \t ... tabulator
%    \r ... CR (0x0d)
%    \e ... ESC (0x1B)
%    \s ... white space (CR,LF,TAB,SPACE)
%    \S ... znak ruzny od white space
%    \l ... lower case a-z
%    \u ... upper case A-Z
%    \w ... \l a \u
%    \d ... digit
%    \D ... znak, ktery neni cislici
%    \a ... digit+w
%    \x00;  ... dana hexadecimalni hodnota (note ';')
%    \i000; ... dana decimalni hodnota (note ';')
%    \o000; ... dana octa hodnota (note ';')

hlxBackChar(W):-
 W :->
        symbol([92])                    % ascii2Atom(92,'\')
        &>
        (
         symbol("n") <@ const(symbol([10])) % korektni jak pod DOS: 0xD,*0xA*
          <:                                % tak UNIX: *0xA*
         symbol("t") <@ const(symbol([9]))
          <:
         symbol("r") <@ const(symbol([13]))
          <:
         symbol("e") <@ const(symbol([27]))
          <:
         symbol("s") <@ const(whiteSpace)
          <:
         symbol("S") <@ const(nonFulfil(isWhiteSpace))
          <:
         symbol("l") <@ const(lower)
          <:
         symbol("u") <@ const(upper)
          <:
         symbol("w") <@ const(letter)
          <:
         symbol("d") <@ const(digit<@ascii)
          <:
         symbol("D") <@ const(nonFulfil(isDigit))
          <:
         symbol("a") <@ const(numAlpha)
          <:
         (symbol("x") &> hexadecimal) <& symbol(";") <@ alter(X,symbol([X]))
          <:
         (symbol("i") &> natural) <& symbol(";") <@ alter(X,symbol([X]))
          <:
         (symbol("o") &> octal) <& symbol(";") <@ alter(X,symbol([X]))
          <:
         item <@ alter(X,symbol([X]))
        ).

%-----------------------------------------------------------------------------
% hlxCharList(?Wrapper)
% - akceptuje seznam pripustnych znaku v regularnim vyrazu
% - priklady:
%       [^ab\cf-g]      ... znaky ruzne od abcfg
%       [\\\(\)c]       ... znaky \()c
%       [^ab\c]         ... znaky ruzne od abc

hlxCharList(W):-
 W :->
        brackets( symbol("^") <?@> switch-const(nonSwitch)
                   <&>
                  (hlxCharInterval <: hlxCharacter)<+> ).

% hlxCharInterval(?Wrapper)
% - akceptuje interval
% - priklad:
%       a-d

hlxCharInterval(W):-
 W :->
        hlxCharacter <&>> symbol("-") &> hlxCharacter.

% hlxCharacter(?Wrapper)
% - jeden znak

hlxCharacter(W):-
 W :->
        hlxChar
         <:
        hlxListBackChar.

% hlxListBackChar(?Wrapper)
% - akceptuje znak, jemuz predchazi zpetne lomitko
% - priklad:
%    \a ... a
%    \\ ... backslash
%   pouze specialni sequence:
%    \n ... new line
%    \t ... tabulator

hlxListBackChar(W):-
 W :->
        symbol([92])                    % ascii2Atom(92,'\')
        &>
        (
         nonSymbols("nt")
          <:
         symbol("n") <@ const(10)       % korektni jak pod OS DOS: 0xD,*0xA*
          <:                            % tak UNIX: *0xA*
         symbol("t") <@ const(9)
        ).

%------------------------------------------------------------------------------
% hlxSwitch(?Wrapper)
% - vytvari parser pro dany switch

hlxSwitch(W):-
 W :->
       (hlxChar         <@ hlxSwitch_Char2P
         <:
        hlxBackChar     <@ toList
         <:
        hlxCharList     <@ hlxSwitch_CharList2P).


% hlxSwitch_Char2P(+Character, -Parser)
% - transformuje znak na parser (poznamka: [46] = ".")
% - priklad:
%    hlxSwitch_Char2P("a",symbol("a")).

hlxSwitch_Char2P(C,P):-
    C=46                                              % ascii2Atom(46,'.')
     -> P=[nonSymbols([10])]                          % DOS: nonSymbols([13])
     ;  (C=10 -> P=[symbol([10])]                     % DOS: token([13,10])
              ;  P=[symbol([C])]).                      

% hlxSwitch_CharList2P(+List, -Parser)
% - transformuje vystup parseru hlxCharList/4 na parser seznamu pripustnych
%   znaku
% - priklady:
%  [switch,"a",98>99,"d"]    -> symbols("ad") <: fulfil(sIsBetween([98],[99]))
%  [nonSwitch,"a",98>99,"d"] -> nonSymbols("ad") <: fulfil(sIsNotBetween([98],[99]))

hlxSwitch_CharList2P([switch|CharList],[P]):-
    hlxSwitch_Concat(CharList,String,IntervalP,switch),
    (IntervalP=[] -> P=symbols(String)
                  ;  (String=[] -> P=IntervalP
                                ;  P= <:(symbols(String),IntervalP))).
hlxSwitch_CharList2P([nonSwitch|CharList],[P]):-
    hlxSwitch_Concat(CharList,String,IntervalP,nonSwitch),
    (IntervalP=[] -> P=nonSymbols(String)
                  ;  (String=[] -> P=IntervalP
                                ;  P= <:(nonSymbols(String),IntervalP))).

% hlxSwitch_Concat(+ListOfCharsAndIntervals, -String, -IntervalP, +Flag)
% - transformuje znaky v ListOfCharsAndIntervals na retezec String a
%   seznam intervalu podminek IntervalP
% - Flag
%       switch v nonSwitch
% - priklad:
%       [97,98>99,100] -> ...,[97,100],[hlxInInterval(98,99),...],...

hlxSwitch_Concat([],[],[],_).
hlxSwitch_Concat([V|T],S,I,F):-
    hlxSwitch_Concat(T,Ss,Is,F),
    (
     V=(From>To)
      ->
     (
      S=Ss,
      ( F=switch
         ->
        (Is=[] -> I=fulfil(sIsBetween([From],[To]))
               ;  I= <:(fulfil(sIsBetween([From],[To])),Is) )
         ;
        (Is=[] -> I=fulfil(sIsNotBetween([From],[To]))
               ; I= <:(fulfil(sIsNotBetween([From],[To])),Is) )
      )
     )
      ;
     (S=[V|Ss], I=Is)
    ).

%------------------------------------------------------------------------------
% hlxRepeater(+Det, ?Wrapper)
% - transformuje symbol iteratoru na jemu odpovidajici ekvivalent mezi
%   mutatory. Pokud je Det vazana na promennou det, jsou pouzity iteratory
%   vydavajici v seznamu uspesnych rozkladu nejvyse jeden maximalni vysledek.

hlxRepeater(det,W):-
 W :->
       (symbol("*") <@ const('<*>'))
         <:
        symbol("+") <@ const('<+>')
         <:
        symbol("?") <@ const('<?>').

hlxRepeater(ndet,W):-
 W :->
       (symbol("*") <@ const('<**>'))
         <:
        symbol("+") <@ const('<++>')
         <:
        symbol("?") <@ const('<??>').

%------------------------------------------------------------------------------
% hlxRepeaterize( [Parser]>Repeater, OutputPredicate )
% - transformuje vstup [Parser]>Repeater na Repeater(Parser) kde
%    Repeater ... je <*>, <+> nebo <?>
%    Parser ...   je napriklad symbols("ab")<*> <&> symbol("b")
% - priklady:
%       [symbol("a")] > <*> -> <*>(symbol("a"))
%       [symbol("a")] > []  -> symbol("a")

hlxRepeaterize([P]>Rptr,[L]):-
    Rptr=[] -> L=P ; L=..[Rptr,P].
            
%------------------------------------------------------------------------------
% hlxConcat(+ListOfParsers, Parser)
% - spoji parsery v seznamu ListOfParsers pomoci kombinatoru sekvencni
%   kompozice.
% - priklad:
%    hlxConcat([[symbol("a")],[symbol("b")]],token("ab"))
%    hlxConcat([[symbol("a")],[nonSymbol("b")]],symbol("a")<:>nonSymbol("b"))

hlxConcat([],[]).
hlxConcat(I,P):-
    ((I=[[symbol(_)]|_];I=[[token(_)]|_])
      -> hlxLinkSymbol(I,T,S),(S=[_] -> HP=symbol(S) ; HP=token(S))
      ;  I=[[HP]|T]),
    hlxConcat(T,TP),
    (TP=[] -> P=HP ; P= <&>(HP,TP)).
    
hlxLinkSymbol(I,T,S):-
     I=[[symbol([C])]|Tt] -> hlxLinkSymbol(Tt,T,R), S=[C|R]
      ;
     I=[[token(Cs)]|Tt] -> hlxLinkSymbol(Tt,T,R), append(Cs,R,S)
      ;
     S=[], I=T.
hlxLinkSymbol([],[],[]).

%------------------------------------------------------------------------------
/** hlxRegExpr2Parser(+RegularExpressionString,-Parser)
Text:   Parser øetìzce RegularExpressionString obsahujícího regulární
        výraz. Jeho výsledkem je Parser, který pøijímá sentence vyhovující
        danému regulárnímu výrazu.
*/

hlxRegExpr2Parser(String,Parser):-
        hlxRegExpr(ndet,quickS(String)+[_>[Parser]]).

% Precedence 0
hlxRegExprPrec0(D,W):-
 W :->
        hlxSwitch
        <:
        parentheses(hlxRegExpr(D)).

% Precedence 1
hlxRegExprPrec1(D,W):-
 W :->
        (hlxRegExprPrec0(D) <&>> hlxRepeater(D)<?>) <@ hlxRepeaterize.

% Precedence 2
hlxRegExpr(D,W):-
 (D=det -> C='<:' ; C='<:>'),
 W :->
        (chainR(id,
                hlxRegExprPrec1(det)<*> <@ hlxConcat,
                symbol("|") <@ const(C))<>) <@ toList.

%------------------------------------------------------------------------------
%        Prevod konfiguracniho souboru na zapouzdrovac: *.hlx -> *.coc
%------------------------------------------------------------------------------
% hlxFile(?Wrapper)
% - parser konfiguracniho souboru *.hlx

hlxFile(W):-
 W :->
        ( hlxJunk &> (hlxRecord <@ hlxRecord2SingleEnv))<+> .

%------------------------------------------------------------------------------
% hlxString(?Wrapper)
% - akceptuje "Hilex" retezec
% - priklad:
%       """abc""" -> "abc"

hlxString(W):-
 W :->
        ((token([92,34]) <@ const(34) <: nonSymbols(""""))<*>)
         enclosedIn                             % ascii(",34), ascii(\,92)
        symbol("""").

% hlxStringA(?Wrapper)
% - varianta predchazejiciho parseru, ktera vraci jako vyslednou hodnotu atom

hlxStringA(W):-
 W :->
    hlxString <@ string2Atom.

%------------------------------------------------------------------------------
% hlxRecordName(?Wrapper)
% - akceptuje klicove slovo zaznamu

hlxRecordName(W):-
 W :->
        tokenA("Element") <: tokenA("Setup").

%------------------------------------------------------------------------------
% hlxKeyword(?Wrapper)
% - prijima klicova slova pripustna uvnitr zaznamu

hlxKeyword(W):-
 W :->
       (tokenA("Action") <: tokenA("Descr") <:
        tokenA("Font") <: tokenA("Size") <: tokenA("Color") <:
        tokenA("Underscore") <: tokenA("Italic") <: tokenA("Bold") <:
        tokenA("RawBeg") <: tokenA("RawEnd") <:
        tokenA("Prefix") <: tokenA("Postfix")).

%------------------------------------------------------------------------------
% hlxRecord(?Wrapper)
% - parser zaznamu

hlxRecord(W):-
 W :->
        funProcDef( procHead(hlxRecordName,parentheses(#>(hlxStringA))),
                    brace(semicolonListOf(cmdAri(#>hlxKeyword,
                                                 #>symbol("="),
                                                 #>hlxString))
                          <&
                          (#>symbol(";"))<?>)). 

%------------------------------------------------------------------------------
% hlxComment(?Wrapper)
% - komentar

hlxComment(W):-
        lineComment("#",W).

%------------------------------------------------------------------------------
% hlxJunk(?Wrapper)

hlxJunk(W):-
 W :->
        (whiteSpace<# <: hlxComment)<*> .
        
%------------------------------------------------------------------------------
%     Semanticka analyza syntaktickeho stromu vydaneho parserem hlxFile/1
%------------------------------------------------------------------------------
% hlxOrder(+Item1, +Item2)
% - predikat pro usporadani environmentu:  true if Item1<Item2

hlxOrder(L>_,B>_):-
    hlxOrder(['Descr','RawBeg','RawEnd','Raw','Font','Size','Color','Underscore','Italic','Bold'],L,B).
hlxOrder([],_,_).
hlxOrder([H|T],L,B):-
    H=L -> true
        ;  (H=B -> fail ; hlxOrder(T,L,B)).

%------------------------------------------------------------------------------
% hlxSetup2SingleEnv(+ParsedSetupRecord, ?PagePrefixAndPostfix)
% - trasformace zaznamu 'Setup'

hlxSetup2SingleEnv([],["",""]).
hlxSetup2SingleEnv(['Prefix'>String|T],[String,Postfix]):-
    hlxSetup2SingleEnv(T,[_,Postfix ]).
hlxSetup2SingleEnv(['Postfix'>String|T ],[Prefix,String]):-
    hlxSetup2SingleEnv(T,[Prefix,_ ]).

%------------------------------------------------------------------------------
% hlxRecord2Env_(+Input,-TempEnv,+Flag).
% - redukce record environmentu a pridani chybejicich polozek se standardnimi
%   hodnotami
% - Flag je priznak pro distribuci hodnot pri rekurzivnim volani:
%   s ... start                 === action,descr
%   f ... was font item         === font,size,color
%   o ... was another item      === underscore,italic,bold

% Kterakoli ze zapouzdrovacich voleb -> default === nothing
hlxRecord2Env_(['Descr'>String],
               ['Descr'>String,'RawBeg'>"",'RawEnd'>""],_).

% Polozka RawBeg musi byt nasledovana RawEnd -> OK (zbytek seznamu je ignorovan)
hlxRecord2Env_(['RawBeg'>RawBeg,'RawEnd'>RawEnd|_],
               ['RawBeg'>RawBeg,'RawEnd'>RawEnd ],_).

% V pripade vetsiho poctu deskriptoru jsou tyto retezeny
hlxRecord2Env_(['Descr'>String1,Key>String2|T],TTT,_):-
    (
     Key='Descr'
      ->
     append(String2,String1,String),
     TTT=TT
      ;
     String=String2,
     TTT=['Descr'>String1|TT]
    ),
    hlxRecord2Env_([Key>String|T],TT,s).

% Vytvoreni RawBeg a RawEnd v TmpRaw tuple
hlxRecord2Env_([Key>String1|T],TT,_):-
    (
     Key='Font' -> append("<FONT FACE=""",String1,String_)
     ;
     Key='Size' -> append("<FONT SIZE=""",String1,String_)
     ;
     Key='Color' -> append("<FONT COLOR=""",String1,String_)    % trick [34|String1]
    ),
    append(String_,"""",String),
    hlxRecord2Env_(['TmpRaw'>[String,"</FONT>"]|T],TT,f).

hlxRecord2Env_([Key>Value|T],TT,F):-
    member(Key,['Underscore','Italic','Bold'])
     ->
    (
     name('TRUE',Value)
      ->
     (
      Key='Underscore' -> RawBeg="<U>",RawEnd="</U>"
      ;
      Key='Italic' ->  RawBeg="<I>",RawEnd="</I>"
      ;
      Key='Bold' ->  RawBeg="<B>",RawEnd="</B>"
     ),
     hlxRecord2Env_(['TmpRaw'>[RawBeg,RawEnd]|T],TT,o)
      ;
     hlxRecord2Env_(T,TT,F)
    ).

hlxRecord2Env_(['TmpRaw'>[String1,RawEnd],Key>String2|T],TT,F):-
    (
     Key='Size' -> mAppend([String1," SIZE=""",String2,""""],String),RawEnd_=RawEnd,F_=f
     ;
     Key='Color' ->  mAppend([String1," COLOR=""",String2,""""],String),RawEnd_=RawEnd,F_=f
     ;
     Key='Underscore' ->
      ( (F=f ->  append(String1,"><U>",String) ; append(String1,"<U>",String)),
        append("</U>",RawEnd,RawEnd_), F_=o
      )
     ;
     Key='Italic' ->
      ( (F=f ->  append(String1,"><I>",String) ; append(String1,"<I>",String)),
        append("</I>",RawEnd,RawEnd_), F_=o
      )
     ;
     Key='Bold' ->
      ( (F=f ->  append(String1,"><B>",String) ; append(String1,"<B>",String)),
        append("</B>",RawEnd,RawEnd_), F_=o
      )
    ),
    hlxRecord2Env_(['TmpRaw'>[String,RawEnd_]|T],TT,F_).

% dno rekurze
hlxRecord2Env_(['TmpRaw'>[RawBeg,RawEnd]],['RawBeg'>RawBeg_,'RawEnd'>RawEnd],F):-
    (
     F=f -> append(RawBeg,">",RawBeg_) ; RawBeg=RawBeg_
    ).

hlxRecord2Env_([],[],_):-
    printf([nl,'   Error: record is empty -> please add some field...',nl,nl]).

%------------------------------------------------------------------------------
% hlxSingleEnv2SingleP(+ReducedEnvironment, -Parser)
% - Vstupem je redukovany environment zaznamu. Predikat vytvori parser
%   provadejici zapouzdreni lexikalniho elementu k nemuz se zaznam vztahuje.

hlxSingleEnv2SingleP(['Descr'>Descr,'RawBeg'>RawBeg,'RawEnd'>RawEnd],Parser):-
    hlxRegExpr2Parser(Descr,P),
% Pribaleni Raw*: [RawBeg,Token,RawEnd], narovnani a nahrazeni specialnich
%                 znaku je provedeno az po urceni nejdelsiho tokenu
    Parser = (P <@ hlxAddRaw(RawBeg,RawEnd)).
% Narovnani vnoreneho retezce a nahrazeni specialnich znaku
%   Parser = (P <@ hlxFlat(html,RawBeg,RawEnd)).
% Narovnani vnoreneho retezce, nahrazeni specialnich znaku a urceni vahy
%   Parser = (P <@ hlxFlatAndWeight(html,RawBeg,RawEnd)).

%------------------------------------------------------------------------------
% hlxRecord2SingleEnv(+ParsedRecord, -Parser)
% - v pripade klicoveho slova 'Element' transformuje zaznam na parser
% - v pripade klicoveho slova 'Setup' transformuje zaznam na zapouzdrovac
%   cele stranky

hlxRecord2SingleEnv(('Setup'>Name)>TupleList,setup(Prefix,Postfix)):-
     printf([nl,'Setup: ',Name]),
    hlxSetup2SingleEnv(TupleList,[Prefix,Postfix]).

hlxRecord2SingleEnv(('Element'>Name)>TupleList,P):-
    quickSortG(hlxOrder,TupleList,SL),
     printf([nl,tab,Name]),
    hlxRecord2Env_(SL,E,_),!,           % redukce environmentu
    hlxSingleEnv2SingleP(E,P).

%------------------------------------------------------------------------------
% hlxPrintString(+String,+Flag)
% - zobrazi vnoreny prologovsky retezec jako napriklad:
%       [97,[97,[97]],97,97] -> 'aaaaa'
% - Flag
%       html ... specialni znaky jsou transformovany na HTML sekvence

hlxPrintString(I,Flag):-
    I=[] -> true
     ;
    (I=[H|T] -> X=H ; X=I),
     ( integer(X) -> ( name(A,[X]), ( Flag=html -> (
                                                     A='<' -> write('&lt;');
                                                     A='>' -> write('&gt;');
                                                     A='&' -> write('&amp;');
                                                     A='"' -> write('&quot;');
                                                     write(A)
                                                   )
                                                ;
                                                   write(A)
                                    ),
                      hlxPrintString(T,Flag))
                  ;
                      hlxPrintString(X,Flag), hlxPrintString(T,Flag)
     ).

%------------------------------------------------------------------------------
% hlxAddRaw(+RawBeg, +RawEnd, +String, -Triple)
hlxAddRaw(RawBeg,RawEnd,String,[RawBeg,String,RawEnd]).

%------------------------------------------------------------------------------
% hlxEncapsulate(+RawBeg, +RawEnd, +Token, -SemActionMandatoryFlag)
% - tento predikat je pouzivan jako semanticka akce v generovanych predikatech
% - do aktualniho vystupniho proudu vypise zapouzdreni a token samotny

hlxEncapsulate(RawBeg,RawEnd,R,o):-
    printString(RawBeg),
     hlxPrintString(R,html),
    printString(RawEnd).

%------------------------------------------------------------------------------
% hlxFlatAndWeight(+Flag,+Prefix,+Postfix,+E,-TermWithWeight)
% - Pokud je prijmuto nekolik lexikalnich elementu v jednom kroku, vybere
%   nejdelsi z nich. V pripade rovnosti rozhoduje poradi v konfiguracnim
%   souboru. Tento predikat urcuje vahu jednoho lexikalniho elementu E.
% - Flag:
%       html ... vystup pro HTML
% - Prefix a Postfix:
%       zapouzreni E

hlxFlatAndWeight(Flag,Prefix,Postfix,I,[Weight,[Prefix,String,Postfix]]):-
        hlxFlatAndWeight_(Flag,I,String),
        length(String,Weight).

hlxFlat(Flag,Prefix,Postfix,I,[Prefix,String,Postfix]):-
        hlxFlatAndWeight_(Flag,I,String).

hlxFlatAndWeight_(HTML,I,O):-
    I=[] -> O=[]
     ;
    (I=[H|T] -> X=H ; X=I),
     (integer(X) -> ( name(A,[X]), ( HTML=html -> ( A='<' -> Hh="&lt;";
                                                    A='>' -> Hh="&gt;";
                                                    A='&' -> Hh="&amp;";
                                                    A='"' -> Hh="&quot;";
                                                    Hh=[X] )
                                               ;
                                                   Hh=[X]
                                   ),
                      hlxFlatAndWeight_(HTML,T,Tt),
                      append(Hh,Tt,O))
                 ;
                    hlxFlatAndWeight_(HTML,X,OX),
                    hlxFlatAndWeight_(HTML,T,OT),
                    append(OX,OT,O)
     ).

%------------------------------------------------------------------------------
% hlxMakeCoconizer(+CoconizerEnv, +Coconizer, -Setup)
% - transformuje seznam parseru elementu na zapouzdrovac a setup
hlxMakeCoconizer([],noCocon,_).
hlxMakeCoconizer([H|T],Coconizer,Setup):-
    hlxMakeCoconizer(T,C,S),
    ( H='*' -> Setup=S, Coconizer=C
            ;  ( H=setup(Pre,Pos)
                  -> Setup=setup(Pre,Pos), Coconizer=C
                  ;  Setup=S,
                     ( C=noCocon -> Coconizer=H
                                 ;  Coconizer= <:>(H,C) ))).

%------------------------------------------------------------------------------
% hlxOptimizeCoconizer(+Coconizer, -OptimizedCoconizer)
% - zoptimalizuje zapouzdrovac
hlxOptimizeCoconizer(Coconizer,CoconizerO):-
        printf([nl,'Optimizing coconizer...',nl,nl]),
        environment hlxOptionIs Representation,
        optimizeRegExprParser(Representation,pseudoll1,Coconizer,CoconizerO).

%------------------------------------------------------------------------------
% hlxFile2Coconizer(+InputHLXFile, +OutputCOCFile)
% - pro dany *.hlx soubor vygeneruje zapozdrovac do *.coc souboru
hlxFile2Coconizer(InputFile,OutputFile):-
    (debug hlxOptionIs yes,assert(pcDeBugName(debug)) ; (retractall(pcDeBugName(_)),assert(pcDeBugName(noname)))),
    loadFile(InputFile,I),
    printf([nl,'Making configuration file records:']),
     environment hlxOptionIs Env,
     (pseudoll1Cocon hlxOptionIs yes
       -> invokePseudoLL1(early,noFOLLOW,assert,Env,offset,hlxFile,s(I),CocLos)
       ;  hlxFile(quickS(I)+CocLos)),
     % kontrola, zda se podarilo konfiguracni soubor analyzovat
     (CocLos=[_>L]
       -> printf(['     ... Done!',nl,nl,'Creating coconizer...'])
       ;  pcError(error,['File ',InputFile,' corrupted.',nl,'Exiting!',nl,nl]),abort),
     % vytvoreni
     hlxMakeCoconizer(L,Coc,Setup),
     Cocon= ( <^>(Coc) <@ hlxShowWinner
               <:
              fulfil(isWhiteSpace)<+> <@ hlxPrintWhiteSpace ),
     % optimalizace
     (hlxOptionIs(optimize,yes) -> hlxOptimizeCoconizer(Cocon,Coconizer);Coconizer=Cocon),
    printf(['Done!',nl,nl]),
    ( var(Setup) -> Setup=setup("","") ; true ),
    saveFile(OutputFile,coconizer(Setup,Coconizer),term).

%------------------------------------------------------------------------------
% hlxShowWinner([Prefix,String,Postfix],w)
% - v prubehu zapouzdrovani je tento predikat pouzivan k vypsani elementu
%   do aktualniho vystupniho proudu
hlxShowWinner([Prefix,String,Postfix],w):-
    printString(Prefix),
     hlxPrintString(String,html),
    printString(Postfix).

%------------------------------------------------------------------------------
% hlxCoconize(+CoconizerFile, +InputFile, +OutputFile)
% - provede zapouzdreni souboru InputFile pomoci zapouzdrovace ulozeneho
%   v souboru CoconizerFile. Vysledek je ulozen do OutputFile.
hlxCoconize(CoconizerFile,InputFile,OutputFile):-
    (debug hlxOptionIs yes,assert(pcDeBugName(debug)) ; (retractall(pcDeBugName(_)),assert(pcDeBugName(noname)))),
    printf([nl,tab,tab,tab,'* HILEX *',nl,nl,'Reading coconizer from:    ',CoconizerFile]),
    openFile(CoconizerFile,OldStream,read),
     read(Term),
    closeFile(OldStream,read),
    Term = coconizer(setup(Prefix,Postfix),Coconizer),
        printf([nl,'Reading input from:        ',InputFile]),
    loadFile(InputFile,I),
        printf([nl,'Output file:               ',OutputFile,nl,'Stand by while coconizing...',nl,nl]),
    openFile(OutputFile,OldStream,write),
    printString(Prefix),
     % pouzijeme mod pseudoll1: (offset pro orezavani vysledku)
     environment hlxOptionIs Env,
      (pseudoll1 hlxOptionIs yes
       -> invokePseudoLL1(early,noFOLLOW,assert,Env,offset,(Coconizer)<+>,s(I),_)
       ;  invokeOffset((Coconizer)<+>,quickS(I),_)),
    printString(Postfix),
    closeFile(OldStream,write),
    printf(['Done!',nl,nl]).

%------------------------------------------------------------------------------
% hlxCoconizeString(+CoconizerFile, +String)
% - provede zapouzdreni Stringu pomoci zapouzdrovace ulozeneho
%   v souboru CoconizerFile. Vysledek vypise na standardni vystup
hlxCoconizeString(CoconizerFile,I):-
    (debug hlxOptionIs yes,assert(pcDeBugName(debug)) ; (retractall(pcDeBugName(_)),assert(pcDeBugName(noname)))),
    openFile(CoconizerFile,OldStream,read),
     read(Term),
    closeFile(OldStream,read),
    Term = coconizer(setup(Prefix,Postfix),Coconizer),
    printString(Prefix),
     % pouzijeme mod pseudoll1: (offset pro orezavani vysledku)
     environment hlxOptionIs Env,
      (pseudoll1 hlxOptionIs yes
       -> % lze zvolit (noFOLLOW v useFOLLOW+(earlyvlazy)) + (assert v off)
          invokePseudoLL1(lazy,noFOLLOW,assert,Env,offset,(Coconizer)<+>,s(I),_)
       ;  invokeOffset((Coconizer)<+>,quickS(I),_)),
    printString(Postfix).

%------------------------------------------------------------------------------
% hlxPrintWhiteSpace(+I, -O)
hlxPrintWhiteSpace(I,w):-
    printString(I).

%- EOF ------------------------------------------------------------------------
