%------------------------------------------------------------------------------
%
%                               * PrologDoc *
%
%			        Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/** Module: PrologDoc
Text:   PrologDoc je nástroj urèený pro generování dokumentace programù,
        pøípadnì i jednotlivých souborù, napsaných v jazyce Prolog, ve
        formátu HTML a LaTeX.
p       Dokumentace, kterou PrologDoc vytváøí, zahrnuje jednotlivé
        zdrojové soubory a predikáty programu, dále obsahuje rejstøík
        v¹ech definovaných operátorù, souborù a predikátù, které byly
        popsány prostøednictvím dokumentaèních komentáøù.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                              Config & knihovny
%------------------------------------------------------------------------------

 :-op(500,xfy,pdOptionIs).

 useFile pdOptionIs yes.         % zpracovavat primo soubor (tj. quickFile mod)
                                 % pokud je zakomentovano, je pouzit mod quickS
                                 % (yes,no)
 hilitExamples pdOptionIs yes.   % syntax highlighting prikladu (Hilex)
                                 % (yes,no)

 % HTML barvy:
 htmlText  pdOptionIs '#ffffff'. % text
 htmlBg    pdOptionIs '#485068'. % pozadi
 htmlLink  pdOptionIs '#c6e2ff'. % link
 htmlIArg  pdOptionIs '#99ffcc'. % argument ve vstupnim modu
 htmlOArg  pdOptionIs '#ffa837'. % argument ve vystupnim modu
 htmlIOArg pdOptionIs '#fffacd'. % argument ve vstupne vystupnim modu

 % TeX volby:
 texStandalone pdOptionIs 'yes'. % generovat take standalone.tex
 texFontSize   pdOptionIs '11'.  % velikost pisma (pt)
 texNewPage    pdOptionIs 'yes'. % indexy zacinat na nove strane (yes,no)
 texRefs       pdOptionIs 'yes'. % reference v indexech (yes,no)

 foo pdOptionIs nothing.

 % Knihovny:
 :- hilitExamples pdOptionIs no ; ['../hilex/hilex']. % Hilex (zavede PClib)

%------------------------------------------------------------------------------
%                                   Demo
%------------------------------------------------------------------------------

go:-
        prologDoc(html,single,'prologdoc.pl').

%------------------------------------------------------------------------------
/**     prologdoc
Text:   Nápovìda.
*/

prologdoc:-
 printf([nl,'PrologDoc help:',nl,
         nl,
         'prologDoc(+DocFormat, +Recursive, +SourceFile)',nl,
         ' For another options see source file and pdOptionIs/2.',nl,
         ' DocFormat:',nl,
         '  html   ... HTML source',nl,
         '  latex  ... LaTeX source',nl,
         ' Recursive:',nl,
         '  follow ... recursive using directives (shell script)',nl,
         '  single ... only single file',nl,
         ' Example:',nl,
         '  ?- prologDoc(html,single,''prologdoc.pl'')',nl,
         nl,
         'stripPrologDoc(+SrcFile, +DestFile)',nl,
         ' Removes PrologDoc comments.',nl,nl
        ]).

prologdoc:-
        prologDoc.

%------------------------------------------------------------------------------
%                          Vstupni bod programu
%------------------------------------------------------------------------------
/**     prologDoc(+DocFormat, +Recursive, +SourceFile)
Text:   Vygeneruje dokumentaci souboru SourceFile.
Arg:    DocFormat
        Formát generované dokumentace - atom 'html' nebo 'latex'.
Arg:    Recursive
        Pokud je argument vázán na atom 'follow', je dokumentace generována
        rekurzivnì pro soubory vnoøené pomocí direktiv. Volba 'single'
        je urèena pro vytváøení dokumentace jednotlivého souboru.
*/

prologDoc(Format,single,File):-
        run(single,File,'source.pd'),
        pdGenDoc(Format,'source.pd').

prologDoc(_,follow,_):-
        printf([nl,'PrologDoc - ''follow'' option:',nl,nl,
                   'For the recursive creation of documentation via tracing include',nl,
                   'directives in Prolog source files please use shell script',nl,
                   '"prologdoc.sh".',nl,nl,'Bye!',nl,nl]).

prologDoc(_,Recursive,_):-
        pcError(error,['Unknown option (',Recursive,') or another error.']).

%------------------------------------------------------------------------------
%                   Predikaty volane ze skriptu prologdoc.sh
%------------------------------------------------------------------------------
% run(single, +InputFile, +OutputIntermediateFile)
% - zpracovani jednotliveho souboru
run(single,InputFile,OutputFile):-
	printf(['Generating PrologDoc source: ',InputFile,' -> ',OutputFile,' ... ']),
        assert(pdCurrentFile(InputFile)),
        (
         useFile pdOptionIs yes
         -> openFile(OutputFile,Old,write),
             openStream(InputFile,Handle,OldHandle,read),
              atStream(Handle,Pos),
              quickFile(Handle,Pos)+_
                        :-> pdPrologFile,
             closeStream(OldHandle,read),
            closeFile(Old,write)
         ; loadFile(InputFile,Content),
            openFile(OutputFile,Old,write),        
             quickS(Content)+_
                        :-> pdPrologFile,
            closeFile(Old,write)
        ),
	printf(['Bye!']).

% run(strip, +InputFile, +OutputFile)
% - odstraneni dokumentacnich komentaru
run(strip,InputFile,OutputFile):-
        (InputFile=OutputFile
         ->
          pcError(error,['Source and destination file are the same (',InputFile,'),',nl,nl])
         ;
	 printf(['Stripping PrologDoc source: ',InputFile,' -> ',OutputFile,' ... ']),
         (useFile pdOptionIs yes
          -> openStream(InputFile,Handle,OldHandle,read),
              openFile(OutputFile,Old,write),
               atStream(Handle,Pos),
               quickFile(Handle,Pos)+_ :->
                         pdStripPrologFile,
               closeFile(Old,write),
             closeStream(OldHandle,read)
          ; loadFile(InputFile,Content),
             openFile(OutputFile,Old,write),
              quickS(Content)+_ :->
                         pdStripPrologFile,
             closeFile(Old,write)
         ),
	 printf(['Bye!'])).

% run(+InputFile, +OutputIntermediateFile)
% - vytvareni mezikodu s rekurzivni sledovanim zavadecich direktiv
run(InputFile,OutputFile):-
	printf(['Generating PrologDoc source: ',InputFile,' -> ',OutputFile,' ... ']),
        name(InputFile,IF),pdAnalyzePath(quickS(IF)+[_>(Path>_)|_]),
        assert(pdCurrentFile(InputFile)),
        (
         useFile pdOptionIs yes
         -> openStream(InputFile,Handle,OldHandle,read),
             openFile(OutputFile,Old,write),
              atStream(Handle,Pos),
              quickFile(Handle,Pos)+_ :->
                        pdPrologFile <@ genDivings(Path,'step.pd'),
             closeFile(Old,write),
            closeStream(OldHandle,read)
         ; loadFile(InputFile,Content),
            openFile(OutputFile,Old,write),        
             quickS(Content)+_ :->
                        pdPrologFile <@ genDivings(Path,'step.pd'),
            closeFile(Old,write)
        ),
	printf(['Bye!']).

%------------------------------------------------------------------------------
% genDivings(+Path,+FileName,-Out,-Out)
% - generovani jmen souboru pro rekurzivni zanoreni z retezcu v zavadecich
%   direktivach
% - FileName je soubor direktiv. Soubor je nacten na vstup a direktivy,
%   ktere obsahuje, jsou parsovany a jmena souboru jsou doplnena o relativni
%   cesty a pripony.
% - ze seznamu uspesnych rozkladu jsou konecne ziskana jmena zdrojovych
%   souboru ve kterych se bude pri generovani pokracovat
genDivings(Path,FileName,X,X):-
         % generate nesting information (for building tree)
         genDivingsNest(Path,X),
         % generate files to dive
         openFile(FileName,Old,write),
          genDivings(Path,X),
         closeFile(Old,write).

genDivings(Path,[H|T]):-
        ( H=10
          ;  H=p
          ;  printf([Path,H,nl])),  % pripona pridana pri zpracovani direktivy
             genDivings(Path,T).
genDivings(_,[]).

genDivingsNest(Path,[H|T]):-
        ( H=10
          ;  H=p
          ;  pdCurrentFile(Nest),
             printf(['pdNestedIn(''',Path,H,''',''',
                                     Nest,''').',nl])),
             genDivingsNest(Path,T).
genDivingsNest(_,[]).

%------------------------------------------------------------------------------
%                           Prolog source file
%------------------------------------------------------------------------------
/**     pdPrologFile(?Wrapper)
Text:   Parser prologovského zdrojového soubory s komentáøovými sekcemi.
*/

pdPrologFile(W):-
 W :->
        (prologDocSection
          <:
         prologOpDirective
          <:
         prologLoadDirective
          <:
         prologMComment
          <:
         untilEOL)<+> .

%------------------------------------------------------------------------------
/**     prologOpDirective(?Wrapper)
Text:   Parser prologovských direktiv.
*/

prologOpDirective(W):-
 W :->
        (#>token(":-") &> #>token("op")
          &> parentheses(
                         (#>natural <& #>symbol(",")<#)
                          <&>> (tokensA(["yfx","xfy","xfx","yfy","yf","fy","xf","fx"])
                                 <& #>symbol(",")<#)
                          <&>> (
                                prologIdf <@ alter(X,[X])         % op(0,fx,a).
                                 <:
                                brackets(commaListOf prologIdf))  % op(0,fx,[a,b,c]).
                        )
         <& symbol(".")) <@ processOpDir.

processOpDir(Prec>(Type>Op),p):-
        processOpDir(Op,Prec,Type).
processOpDir([],_,_).
processOpDir([Op|T],Prec,Type):-
        printf(['pdOperator(',Op,',',Type,',',Prec,').',nl]),
        processOpDir(T,Prec,Type).

%------------------------------------------------------------------------------
% pdAnalyzePath(?Wrapper)
% - v ceste vstupniho souboru se oddeli cesta od jeho jmena
%   a vyda se dvojice path>fileName
% - analyzuji se pouze relativni cesty - nepredpoklada se, ze by uzivatel 
%   pouzil cestu od korene filesystemu
pdAnalyzePath(W):-
 W :->
        (nonSymbols("/")<+>) separatedBy symbol("/") <@ evalAnalyzePath.

evalAnalyzePath(R,APath>AFile):-
        evalAP(R,Path>File),
        name(APath,Path), name(AFile,File).

evalAP([File],[]>File).
evalAP([H|T],Path>File):-
        evalAP(T,P>File),
        append(H,[47|P],Path).                    % ascii(/,47)

%------------------------------------------------------------------------------
%                          Prolog Doc section
%------------------------------------------------------------------------------
/**     prologDocSection(?Wrapper)
Text:   Parser komentáøové sekce PrologDoc.
*/

prologDocSection(W):-
 W :->                                  
       (pdInnards enclosedIn (#>token("/**")) and (#>token("*/")))
                <@ generateSource.

pdInnards(I+L):-
 I+L :->
        ((#>pdPrologHead <& untilEOL <@ alter(X,prototype(X)))
          <&>>
         pdSections)
          <:
         pdModule.

pdPrologHead(W):-
 W :->
        prologIdf
         <&>>
        parentheses(commaListOf pdPrologHeadArg)<?> .

pdPrologHeadArg(W):-
 W :->
        #>(symbolA("+") <: symbolA("-") <: symbolA("?"))
         <&>>
        #>pdPrologVar.

pdPrototype(W):-
 W :->
        #>pdPrologHead.

% Sekce muze byt uvozena jednim z nasledujich retezcu:
% "Text:" "Arg:" "Operator:" "Example:"
pdSections(W):-
 W :->
        ((tokenA("Text:")
           <&>> (#>(nonSymbol([10])<*>) <& symbol([10]))
            <&> pdSectionBody)
         <:
        (tokenA("Arg:")
          <&>> (#>pdPrologVar <& untilEOL)     % argument name
           <&>> pdSectionBody)
         <:
        ((tokenA("Operator:") <& untilEOL) <&>> pdSectionBody)
         <:
        ((tokenA("Example:") <& untilEOL) <&>> pdSectionBody)
         <:
        symbol([10]))<*> .

% pdSectionBody(?Wrapper)
% - v textove sekci mohou byt pouzity formatovaci znacky:
pdSectionBody(W):-
 W :->                                        % the first line of the section
        ((whiteSpace<:symbol("v")<:symbol("p"))
           <&> (#>(nonSymbol([10])<*>) <& symbol([10])))<*> .   % other lines must begin with ws
                                       

pdModule(W):-
 W :->
       ( (#>tokenA("Module:") <&>> getLine<?>)
         <&>>
         ((tokenA("Text:") <&>> #>getLine <&> pdSectionBody)
           <:
          ((tokenA("Author:") <: tokenA("Ver:"))
             <&>> #>getLine) <@ const(10)                       % ignored
           <:
           symbol([10])
         )<*>
        ).

%------------------------------------------------------------------------------
% prologMComment(?Wrapper)
% - prologovsky multiline komentar
prologMComment(W):-
 W :->
        (token("/*")
         <&>
          (symbol("*")<+> <&> nonSymbols("/")
            <:
           nonSymbol("*"))<*>
         <&>
         symbol("*")<+>
         <&>
         symbol("/")) <@ const(10).                     % ja jsem pan Nikdo

%------------------------------------------------------------------------------
% generateSource/2
% - generovani zdrojoveho mezikodu
generateSource(('Module:'>Name)> Sections,p):-
        pdCurrentFile(AFile), name(AFile,File),
        sortSections(Sections,[Text,_,_]),
        C=pdModule(File,Name,Text),
        printf([C,'.',nl]).

generateSource(prototype(Pred>Args)>Sections,p):-
        pdCurrentFile(AFile), name(AFile,File),
        length(Args,Ar),
        ( Sections=[] -> Text=[], A=[], Example=[]
                      ;  sortSections(Sections,[Text,A,Example])),
        C=prologDocData(File,           % file where it's defined (used as key)
                        Pred,           % predicate name (used as key)
                        Ar,             % arity
                        Args,           % list of args
                        Text,           % text sections
                        A,              % argument sections
                        Example),       % example sections
        printf([C,'.',nl]).

sortSections([10|T],O):-                % empty line
        sortSections(T,O).
sortSections([Type>S|T],O):-
        ( Type='Text:' -> O=[[S|IT],IA,IE] ;
           Type='Arg:'  -> O=[IT,[S|IA],IE] ;
            Type='Example:' -> O=[IT,IA,[S|IE]]),
        sortSections(T,[IT,IA,IE]).
sortSections([],[[],[],[]]).

%------------------------------------------------------------------------------
% prologLoadDirective(?Wrapper)
% - parser direktivy zavadejici zdrojovy soubor
% - pouze direktivy typu :- ['...'].
prologLoadDirective(W):-
 W :->
        (#>token(":-") &> brackets(#>(prologLoadDirectivePath <: prologIdf)))
          <@ processLoadDirective.

prologLoadDirectivePath(W):-                       % atom in apostrophes
 W :->
        ((                              % ascii(',39).
         (token("\\'") <: token("''")) <@ const(39) <: nonSymbol("'")
        )<*>)
        enclosedIn
         symbol("'") and symbol("'") <@ string2Atom.

processLoadDirective(A,Apl):-
        name(A,S),
        (append(_,".pl",S),Apl=A ; append(S,".pl",SApl),name(Apl,SApl)).

%------------------------------------------------------------------------------

pdPrologVar(W):-
 W :->
        ((upper <: symbol("_")) <&> fulfil(isDigitChar_)<*>).

%------------------------------------------------------------------------------
%                         Generovani dokumentace
%------------------------------------------------------------------------------

:- ['pdgenhtml'].       % generator dokumentace - HTML
:- ['pdgenlatex'].      % generator dokumentace - LaTeX

%------------------------------------------------------------------------------
/**     pdGenDoc(+TargetFormat,+Source)
Text:   Vygeneruje dokumentaci ve formátu TargetFormat z mezikódu,
        který je ulo¾en v souboru Source.
Arg:    TargetFormat
        Argument urèující formát generované dokumentace, mù¾e být vázán
        na atom 'html' nebo 'latex'.
*/

pdGenDoc(TargetFormat,Source):-
        TargetFormat=html  -> generateHtmlDoc(Source)
        ;
        TargetFormat=latex -> generateLatexDoc(Source)
        ;
        TargetFormat=vlnka -> generateLatexDoc(Source)
        ;
        pcError(error,['Unknown target language: ',
                       TargetFormat,
                       ' (only html, latex and vlnka is supported)',nl,nl]).

%------------------------------------------------------------------------------
% Data format: prologDocData/7
%  prologDocData(
%       FileL,            ... file where it's defined (string)
%       Pred,             ... predicate name (atom)
%       Arity,            ... arity (atom)
%       Args,             ... list of: type(atom) > Name(string)
%       Text,             ... text sections
%       Arg,              ... argument sections
%       Example           ... example sections
%           ),      
%
% Text and example sections:
% [                             ... list of text sections
%       [ line1, ..., lineN ]   ... section1
%       ...
%       [ line1, ..., lineN ]   ... sectionM
% ]
%
% Arg section:
% [                             
%       Arg1 > [ line1, ..., lineN ]   
%       ...
%       ArgM > [ line1, ..., lineN ]   
% ]
%
% line* and Arg* are lists
%
%------------------------------------------------------------------------------
% Data description: pdModule(File,Name,Text)
%       File ... filename (string)
%       Name ... module name (string)
%       Text ... description (list of strings in list) [["",""]]
%------------------------------------------------------------------------------
% Data description: pdOperator(Op,Type,Precedence)
%       Op         ... name of the operator (atom)
%       Type       ... asociativity (atom)
%       Precedence ... precedence
%------------------------------------------------------------------------------
% Data description: pdCurrentFile(File)
%       File       ... name of the processed file (atom)
%------------------------------------------------------------------------------
/**     stripPrologDoc(+InputFile, +OutputFile)
Text:   Odstraní ze zdrojových souborù dokumentaèní komentáøe. Zdrojový
        a cílový soubor se musí li¹it.
*/

stripPrologDoc(InputFile,OutputFile):-
        run(strip,InputFile,OutputFile).

%------------------------------------------------------------------------------
/**     pdStripPrologFile(?Wrapper)
Text:   Odstraní ze zdrojových souborù PrologDoc komentáøe.
*/

pdStripPrologFile(W):-
 W :->
        (stripPrologDocSection
          <:
         getLine <@ showString => showAtom(10) => const(10))<+> .


%------------------------------------------------------------------------------
stripPrologDocSection(W):-
 W :->                                  
       (pdInnards enclosedIn (#>token("/**")) and (#>token("*/")))
                <@ const(p).

%- EOF ------------------------------------------------------------------------
