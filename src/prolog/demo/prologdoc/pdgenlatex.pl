%------------------------------------------------------------------------------
%
%                     PrologDoc - LaTeX code generation
%
%			        Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/** Module: PrologDoc gen LaTeX
Text:   Èást programu PrologDoc, která generuje z mezikódu dokumentaci
        pro LaTeX.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     generateLatexDoc(+SrcFile)
Text:   Generátor LaTeX dokumentace. Soubory dokumentace jsou vytváøeny
        v podadresáøi ./doc.
*/

generateLatexDoc(SrcFile):-
        consult(SrcFile),
        nl,genShowTree,nl,
        genLatexIndexes,
        (clause(pdFile(_,_,_),_)
          -> genLatexNodes,genLatexRootNode,genLatexStandalone
          ;  true),
        pdCleanDbf,nl.

generateLatexDoc:-
        generateLatexDoc('source.pl').

generateSmallLatexDoc:-
        assert(smallDoc  pdOptionIs yes),       % don't break index pages
        generateLatexDoc('src.pd').

%------------------------------------------------------------------------------
/**     genLatexIndexes
Text:   Vygeneruje index souborù, index predikátù a index v¹ech definovaných
        operátorù.
*/
genLatexIndexes:-
        genLatexFileIndex,
        genLatexPredicateIndex,
        genLatexOpIndex.

%------------------------------------------------------------------------------
%                               fileindex.tex
%------------------------------------------------------------------------------
/**     genLatexFileIndex
Text:   Vygeneruje index v¹ech souborù. Bìhem generování jsou pøiøazeny
        jednotlivým souborùm a predikátùm, které jsou v nich definovány,
        unikátní identikaèní èísla. Tato èísla se v dal¹ích fázích vytváøení
        dokumentace pou¾ívají pro pojmenovávání referencí.
*/

genLatexFileIndex:-
        write('Generating file index ... '),
        openFile('doc/fileindex.tex',Old,write),
         write('\\section{Index souborù}'),nl,
          genLatexFPIndex(ID),
          genLatexFMIndex(ID),
         write('\\endinput'),nl,
        closeFile(Old,write),
        printf(['Done!',nl]).

genLatexFPIndex(IDO):-
        clause(prologDocData(_,_,_,_,_,_,_),_)
             -> findall([F,P,A],prologDocData(F,P,A,_,_,_,_),R),
                fPLatexIndex(R,noname,1,IDO)
             ;  IDO=1.

% fPLatexIndex
% - file predicate index
fPLatexIndex([],_,ID,ID).
fPLatexIndex([[F,P,A]|T],O,ID,IDO):-
        (O=F -> Old=O, IDF is ID                % remove duplicities
             ;  Old=F, IDF is ID+1,
                % file
                string2Latex(F,FName), assert(pdFile(F,IDF,FName)),
                printf(['\\noindent {\\bf ',FName,'}\\hfill \\ref{',IDF,'}',' str. \\pageref{',IDF,'}',nl,nl,'\\noindent',nl]),
                (clause(pdModule(F,_,MT),_) -> genLatexTexts(MT) ;  true),
                printf([nl,'\\vskip 3mm',nl])
        ),
        % predicate
        IDP is IDF+1,
                                                % atom2Latex(P,PName),
        assert(pdPredicate(P/A,IDP,P,F)),       % PName
        fPLatexIndex(T,Old,IDP,IDO).

% genLatexFMIndex
% - module index (neobsahujici predikat)
genLatexFMIndex(ID):-
        clause(pdModule(_,_,_),_)
         -> findall(F,pdModule(F,_,_),Modules), fMLatexIndex(Modules,ID)
         ;  true.
             
fMLatexIndex([],_).
fMLatexIndex([F|T],ID):-
        ( clause(pdFile(F,_,_),_)
           -> IDF = ID
           ;  IDF is ID+1,
              string2Latex(F,FName), assert(pdFile(F,IDF,FName)),
              printf(['\\noindent {\\bf ',FName,'}\\hfill \\ref{',IDF,'}',' str. \\pageref{',IDF,'}',nl,nl,'\\noindent',nl]),
              pdModule(F,_,MT), 
              genLatexTexts(MT),
              printf([nl,'\\vskip 3mm',nl])
        ),
        fMLatexIndex(T,IDF).
        
%------------------------------------------------------------------------------
%                               predindex.tex
%------------------------------------------------------------------------------
/**     genLatexPredicateIndex
Text:   Vygeneruje index v¹ech definovaných predikátù. Pro propojení
        pomocí odkazù vyu¾ívá globální informace pøipravené predikátem
        genLatexFileIndex/0.
*/

genLatexPredicateIndex:-
        write('Generating predicate index ... '),
        ( clause(pdPredicate(_,_,_,_),_)
           -> findall(P/A,pdPredicate(P/A,_,_,_),Ps),sort(Ps,Sorted),
              openFile('doc/predindex.tex',Old,write),
               write('\\section{Index predikátù}'),nl,
               write('\\begin{tabular}{p{5cm}p{3cm}}'),nl,
               write('Predikát & Odstavec\\\\'),nl,
               write('\\hline'),nl,
               write('\\end{tabular}'),
                genLatexPIndex(Sorted),
               write('\\endinput'),nl,
              closeFile(Old,write)
           ;  true),
        printf(['Done!',nl]).

% genLatexPIndex(+OutputFormat,+PredicateNames).
% - generate file index and assert checked names and IDs
genLatexPIndex([P/A|T]):-
        pdPredicate(P/A,IDP,_,_), % File
        printf([nl,nl,'\\noindent\\begin{tabular}{p{5cm}p{3cm}}',nl,
                '\\verb|',P,'/',A,'| & \\ref{',IDP,'}, str. \\pageref{',IDP,'}\\\\',nl,
                '\\end{tabular}']),
        % '(',File,')'
        genLatexPIndex(T).
genLatexPIndex([]).

%------------------------------------------------------------------------------
%                               opindex.tex
%------------------------------------------------------------------------------
/**     genLatexOpIndex
Text:   Vygeneruje index v¹ech definovaných operátorù. Pro propojení
        pomocí odkazù vyu¾ívá globální informace pøipravené predikátem
        genLatexFileIndex/0.
*/

genLatexOpIndex:-
        write('Generating operator index ... '),
        texRefs pdOptionIs TexRefs,
        openFile('doc/opindex.tex',Old,write),
         write('\\section{Index operátorù}'),nl,
          (
           clause(pdOperator(_,_,_),_)
           -> findall([Pr,O],pdOperator(O,_,Pr),Os),sort(Os,Sorted),
              (TexRefs='yes'
               -> printf(['\\begin{tabular}{p{3cm}p{2cm}p{2cm}p{3cm}}',nl,
                         'Operátor & Precedence & Asociativita & Odstavec\\\\',nl])
               ;  printf(['\\begin{tabular}{p{3cm}p{2cm}p{2cm}}',nl,
                         'Operátor & Precedence & Asociativita\\\\',nl])),
              write('\\hline'),nl,
              write('\\end{tabular}'),nl,
               genLatexOIndex(TexRefs,Sorted)
           ;  true
          ),
        write('\\endinput'),nl,
        closeFile(Old,write),
        printf(['Done!',nl]).

% genLatexPIndex(+TexRefs,+PredicateNames).
genLatexOIndex(TexRefs,[[Prec,H]|T]):-
        pdOperator(H,Asoc,Prec),
        (clause(pdPredicate(H/_,IDP,LatexName,_),_)
          -> (
               TexRefs='yes'
              -> printf([nl,nl,'\\noindent\\begin{tabular}{p{3cm}p{2cm}p{2cm}p{3cm}}',nl,
                        '\\verb|',LatexName,'| & ',Prec,' & ']),
                 latexShowColorAsoc(Asoc),
                 printf([' & \\ref{',IDP,'}, str. \\pageref{',IDP,'}\\\\',nl,'\\end{tabular}'])
              ;  printf([nl,nl,'\\noindent\\begin{tabular}{p{3cm}p{2cm}p{2cm}}',nl,
                        '\\verb|',LatexName,'| & ',Prec,' & ']),
                 latexShowColorAsoc(Asoc),
                 printf([' \\\\',nl,'\\end{tabular}']))
          ;  (
              TexRefs='yes'
              -> 
              printf([nl,nl,'\\noindent\\begin{tabular}{p{3cm}p{2cm}p{2cm}p{3cm}}',nl,
                         '\\verb|',H,'| & ',Prec,' & ']),
                 latexShowColorAsoc(Asoc),
                 printf([' & . \\\\',nl,'\\end{tabular}'])
              ;  printf([nl,nl,'\\noindent\\begin{tabular}{p{3cm}p{2cm}p{2cm}}',nl,
                         '\\verb|',H,'| & ',Prec,' & ']),
                 latexShowColorAsoc(Asoc),
                 printf([' \\\\',nl,'\\end{tabular}']))
        ),
        genLatexOIndex(TexRefs,T).
genLatexOIndex(_,[]).

%------------------------------------------------------------------------------
%                               nodes.tex
%------------------------------------------------------------------------------
/**     genLatexNodes
Text:   Vygeneruje dokumentaci v¹ech definovaných predikátù dle souborù ve
        kterých jsou ulo¾eny.
*/

genLatexNodes:-
        write('Generating nodes ... '),nl,
        openFile('doc/nodes.tex',Old,write),
          genLatexNodeByNode,
         pdLatexPostfix,
        closeFile(Old,write),
        printf([nl,'Done!',nl]).

genLatexNodeByNode:-
        pdFile(F,_,_),                    % navraceni vyhradne pres tento pred.
         genLatexNbnMkFileName(F,FN,TeX), % vytvor jmeno nodu
         % vypis jmena nodu do nodes.tex
         printf(['\\input{',FN,'}',nl]),
         % vygenerovani nodu
         openFile(TeX,O,write),
          genLatexOneNode(F),
         closeFile(O,write),
        fail.
genLatexNodeByNode.

genLatexNbnMkFileName(F,FileName,TeX):-
         pdAnalyzePath(s(F)+[_ > (_ > File)]),
         % vytvor z jmena prologovskeho souboru tex: s.pl -> doc/s.tex
         name(File,SFile),
         (append(SName,".pl",SFile) ; SName=SFile), % *.pl ; *.?
         append(SName,".tex",SFileName),name(FileName,SFileName),
         append("doc/",SFileName,STeX), name(TeX,STeX),
          % ukaz node uzivateli
          fprintf(user,[' ',TeX,nl]),!.

% genLatexOneNode(+File)
% - generovani dokumentace souboru File
genLatexOneNode(F):-
        pdFile(F,ID,FLatex),
        printf(['\\label{',ID,'}',nl]),
         pdLatexPrefix(FLatex),
          (clause(pdModule(F,_,MT),_) -> genLatexTexts(MT) ; true),
          (clause(prologDocData(F,_,_,_,_,_,_),_)
           -> write('\\vskip 3mm\\noindent\\textsc{Index predikátù}'),nl,nl,
               genLatexNodePredIndex(F),
              write('\\vskip 3mm\\noindent\\textsc{Predikáty}'),nl,nl,
              genLatexNodeBody(F)
           ;  true),!.

% genLatexNodePredIndex(File)
% - generovani indexu predikatu v souboru File
genLatexNodePredIndex(File):-
        pdPredicate(K,ID,Latex,File),     % navraceni vyhradne pres tento pred.
        genLatexNodePredIndexItem(K,ID,Latex,File),
        fail.
genLatexNodePredIndex(_).
        
% genLatexNodeBody(File)
% - predikaty jednoho souboru
genLatexNodeBody(F):-
        prologDocData(F,P,Ar,Arg,T,A,E), % navraceni vyhradne pres tento pred.
        genLatexNodeBodyItem([F,P,Ar,Arg,T,A,E]),
        fail.
genLatexNodeBody(_).

% genLatexNodePredIndexItem/1
% - ziskani a zobrazeni jednoho indexu
genLatexNodePredIndexItem(Key/A,_,Latex,File):- % ID
        prologDocData(File,Key,A,_,_,_,_),
        printf(['\\verb|',Latex,'/',A,'|',nl,nl]),
        !.     % prevent backtrack in case of repeat fail

% genLatexNodeBodyItem/1
% - popis jednoho predikatu
genLatexNodeBodyItem([_,P,Ar,Args,T,A,E]):-
        pdPredicate(P/Ar,ID,Latex,_),
        printf(['\\noindent',nl,'\\label{',ID,'}',nl,
                '\\verb|',Latex,'/',Ar,' |\\hrulefill \\verb| ',Latex]),
         (Args=[] ; write('('), genLatexColorArgs(Args), write(')')),
        write('|'),nl,nl,
        write('\\vskip -2mm\\noindent\\begin{quote}'),  % 3mm
        genLatexTexts(T),
                              write('\\end{quote}'),
        write('\\vskip -3mm\\noindent\\begin{description}'),nl,
          genLatexIsOperator(P),
          genLatexArguments(A),
          genLatexExample(E),
                       write('\\item \\end{description}'),nl,nl,
         !.     % prevent backtrack in case of repeat fail in genLatexNodeBody
        
%------------------------------------------------------------------------------
% genLatexIsOperator(Predicate)
% - pokud je definovan jako operator, vypise jeho precedenci a asociativitu
genLatexIsOperator(P):-
        clause(pdOperator(P,Asoc,Prec),_)
         ->
        printf(['\\item {\\it Operátor:}\\quad ',Prec,', ']),
        latexShowColorAsoc(Asoc),
        nl.
genLatexIsOperator(_).               % nedefinovan

% genLatexColorArgs(Args)
% - Args je seznam argumentu, predikat vygeruje jejich barevnou a
%   symbolickou reprezentaci dle modu.
genLatexColorArgs([H]):-
        genLatexColorArg(H).
genLatexColorArgs([H|T]):-
        T\=[],
        genLatexColorArg(H),write(','),genLatexColorArgs(T).
genLatexColorArg(Type>A):-
        name(Aa,A),printf([Type,Aa]).

% genLatexTexts(Text)
% - text je seznam prologovskych retezcu
genLatexTexts([H|_]):-
         genLatexText(H).
genLatexTexts([]).

genLatexText([H|T]):-
        % p ... paragraph
        H=[112|TT] -> nl,nl,
                      string2Latex(TT,A),
                      write(A),nl,
                      genLatexText(T) ;
        % v ... verbatim
        H=[118|TT] -> write('\\begin{verbatim}'),
                      name(A,TT),                   % string2Latex(TT,A),
                      write(A),nl,
                      genLatexText(verbatim,T) ;
        string2Latex(H,A),
        write(A),nl,
        genLatexText(T).
genLatexText([]).

genLatexText(verbatim,[H|T]):-       % predchazelo verbatim
        % v ... verbatim
        H=[118|TT] -> name(A,TT),                   % string2Latex(TT,A),
                      write(A),nl,
                      genLatexText(verbatim,T) ;
        % p ... paragraph
        H=[112|TT] -> write('\\end{verbatim}'),nl,nl,
                      string2Latex(TT,A),
                      write(A),nl,
                      genLatexText(T) ;
        write('\\end{verbatim}'),
        string2Latex(H,A),
        write(A),nl,
        genLatexText(T).
genLatexText(verbatim,[]):-
        write('\\end{verbatim}').

% genLatexArguments(Text)
% - text je seznam prologovskych retezcu
genLatexArguments([Arg>Text|T]):-
        name(A,Arg),
        printf([nl,'\\item {\\it ',A,':}',nl]),genLatexText(Text),nl,
        genLatexArguments(T).
genLatexArguments([]).

% genLatexExample(Text)
% - text je seznam prologovskych retezcu
genLatexExample([H|_]):-
        write('\\item {\\it Pøíklad:}\\vskip -3mm'),nl,
        write('\\vbox{\\begin{verbatim}'),nl,
         genLatexExam(H),
        write('\\end{verbatim}}'),nl.
genLatexExample([]).

genLatexExam([H]):-
        name(A,H),write(A).
genLatexExam([H|T]):-
        T\=[],
        name(A,H),write(A),nl,genLatexExam(T).

%------------------------------------------------------------------------------
%                          documentation.tex
%------------------------------------------------------------------------------

genLatexRootNode:-
        (clause(pdModule(IF,IN,IT),_)
          -> (IN=[] -> N=[] ; N=IN),IT=T ; IF=[],N=[],T=[[]]),
        openFile('doc/documentation.tex',Old,write),
          printf(['\\pagenumbering{roman} \\maketitle \\vskip 2cm',nl]),
          printf(['\\noindent',nl,'Dokumentace byla vygenerována programem \\emph{PrologDoc} poèínaje souborem {\\tt ']),
          genLatexShowHyphenatedPath(IF),
          printf(['}, který je koøenem hierarchie souborù se zdrojovým kódem.',nl,
                  'Je tvoøena pøehledem predikátù dle souborù a indexy souborù, predikátù a operátorù.',
                  nl,'\\vskip 3mm\\noindent',nl]),
          genLatexTexts(T),

          printf([nl,
                  '\\newpage \\tableofcontents',
                  nl,
                  '\\newpage \\pagestyle{headings} \\section{Pøehled predikátù} \\pagenumbering{arabic} \\setcounter{page}{1}',nl,
                  'Tato èást obsahuje pøehled predikátù podle souborù.',nl,nl,
                  nl,
                  '\\begin{table}',nl,
                  '\\caption{Struktura zdrojových souborù} \\label{StrukturaZdrojovychSouboru}',nl,
                  '\\begin{center}',nl]),
          genLatexShowTree,
          (smallDoc pdOptionIs yes -> Break=' ' ; Break='\\newpage '),

          printf([nl,
                  '\\end{center}',nl,
                  '\\end{table}',nl,
                  '\\input{nodes}',nl,
                  Break,'\\input{fileindex}',nl,
                  Break,'\\input{predindex}',nl,
                  Break,'\\input{opindex}',nl,
                  '\\endinput',nl]),
        closeFile(Old,write).

%------------------------------------------------------------------------------
% genLatexShowHyphenatedPath(+String)
% - cesta je parsovana a jsou vkladany znaky pro povoleni zalomeni cesty
genLatexShowHyphenatedPath(I):-
 s(I)+_ :->
        (symbol("/") <@ showConst(['/\\-'])
          <:
         nonSymbol("/")<+> <@ showString)<*> .

%------------------------------------------------------------------------------
%                              standalone.tex
%------------------------------------------------------------------------------

genLatexStandalone:-
        texFontSize pdOptionIs Font,
        texStandalone pdOptionIs Y,
        (Y=no
          ;
        (clause(pdModule(IF,IN,IT),_)
          -> (IN=[] -> N=[] ; N=IN),IT=T ; IF=[],N=[],T=[[]]),
        append("Dokumentace:\\\\",N,Title), name(TitleA,Title),
         openFile('doc/standalone.tex',Old,write),
           printf(['\\documentclass[',Font,'pt]{article}',nl,
                   '\\usepackage{czech}',nl,
                   '\\title{',TitleA,'\\\\}',nl,
                   '\\date{$\\star$}',nl,
                   '\\addtolength{\\voffset}{-45pt}',nl,
                   '\\addtolength{\\topmargin}{17pt}',nl,
                   '\\addtolength{\\textheight}{45mm}',nl,nl,
                   '\\addtolength{\\hoffset}{-1.5cm}',nl,
                   '\\setlength{\\marginparwidth}{20mm}',nl,
                   '\\setlength{\\evensidemargin}{1pt}',nl,
                   '\\addtolength{\\textwidth}{30mm}',nl,
                   '\\begin{document}',nl,
                   ' \\input{documentation}',nl,
                   '\\end{document}\\endinput',nl]),
         closeFile(Old,write),
         printf([nl,nl,'To create .dvi run LaTeX on "standalone.tex"',nl])).

%------------------------------------------------------------------------------
%                                  Aux
%------------------------------------------------------------------------------
% pdLatex*/0
% - bezny prefix a postfix texovych dokumentu

pdLatexPrefix(Title):-
        printf([nl,'\\subsection{',Title,'}',nl]).

pdLatexPostfix:-
        printf([nl,'\\endinput',nl]).

%------------------------------------------------------------------------------
% pdCleanDbf
% - odstrani z databaze vsechny pridane predikaty
pdCleanDbf:-
        retractall(pdFile(_,_,_)),
        retractall(pdCurrentFile(_)),
        retractall(pdModule(_,_,_)),
        retractall(pdOperator(_,_,_)),
        retractall(prologDocData(_,_,_,_,_,_,_)).

%------------------------------------------------------------------------------
/**     string2Latex(+String,+LatexAtom)
Text:   V øetìzci String jsou zapouzdøeny rezervované znaky TeXu do
        prostøedí verbatim. Výsledek je vydán ve formì atomu.
*/

string2Latex(I,A):-
        string2LaTeX(I,S),
        name(A,S).
        
string2LaTeX([],[]).
string2LaTeX([H|T],O):-        % update string2LaTeX/3 TOO!
        (member(H,[38,34,60,62,64,43,45,42,95,92,35,94,63,58,124,37])
                  % &  "  <  >  @  +  -  * _  \  #  ^  ?  :  |  
         -> O=[92,118,101,114,98,39,H|OT],        % \verb'
            string2LaTeX(verbatim,T,OT)
         ;  O=[H|OT],
            string2LaTeX(T,OT)).
         
                               % update string2LaTeX/2 TOO!
string2LaTeX(verbatim,[],[39,92,114,101,108,97,120,32]).
string2LaTeX(verbatim,[H|T],O):-        
        (member(H,[38,34,60,62,64,43,45,42,95,92,35,94,63,58,124,37])
                  % &  "  <  >  @  +  -  * _  \  #  ^  ?  :  |  
         -> O=[H|OT],
            string2LaTeX(verbatim,T,OT)
         ;  O=[39,H|OT],
%         ;  O=[39,92,114,101,108,97,120,32,H|OT], % '\relax
            string2LaTeX(T,OT)).       

%------------------------------------------------------------------------------
/**     atom2Latex(+Atom,+LatexAtom)
Text:   V atomu Atom jsou zapouzdøeny rezervované znaky TeXu do
        prostøedí verbatim. Výsledek je vydán rovnì¾ ve formì atomu.
*/

atom2Latex(A,O):-
        name(A,S), string2Latex(S,O).

%------------------------------------------------------------------------------
% latexShowColorAsoc(+Type)
% - barevne vypise typ asociativity
% - Type mohou byt atomy xf, yf, fx, fy, xfx, xfy, yfx, yfy

latexShowColorAsoc(Asoc):-
        name(Asoc,S),
        latexShowColAsoc(S).

latexShowColAsoc([120|T]):-        % x
        write('{\\it x\\/}'),
        latexShowColAsoc(T).
latexShowColAsoc([121|T]):-        % y
        write('{\\it y\\/}'),
        latexShowColAsoc(T).
latexShowColAsoc([102|T]):-        % f
        write('f'),
        latexShowColAsoc(T).
latexShowColAsoc([]).

%------------------------------------------------------------------------------
% genShowTree
% - strom vnoreni (pdNestedIn/2) -> raw verze.

genShowTree:-
         pdNestedInRoot(Nest),
         printf(['o']),nl,
         genShowTree_1("",[Nest]).

genShowTree_1(TabS,[Nest|T]):-
        (T=[] -> append(TabS,"    ",SonTabS),
                 append(TabS,"`---+- ",SiblTabS)
              ;  append(TabS,"|   ",SonTabS),
                 append(TabS,"|---+- ",SiblTabS)),
                                        name(SiblTab,SiblTabS),
        write(SiblTab),genShowTreeFile(Nest),nl,
        (clause(pdNestedIn(_,_),_)
         -> findall(File,pdNestedIn(File,Nest),Bag),
            genShowTree_1(SonTabS,Bag) ; true),
        genShowTree_1(TabS,T).
genShowTree_1(_,[]).


genShowTreeFile(APathF):-
        write(APathF).
% may be one day `sh tree`
genShowTreeFile(APathF):-
        name(APathF,PathF),
        pdAnalyzePath(s(PathF)+L),
        (L=[_>(_>Out)]
                    ;  Out=APathF),
        write(Out).
        
%------------------------------------------------------------------------------
% genLatexShowTree
% ukaz strom vnoreni (pdNestedIn/2).

genLatexShowTree:-
        printf([nl,'\\begin{verbatim}',nl]),
         genShowTree,
        printf([nl,'\\end{verbatim}',nl]).

%- EOF ------------------------------------------------------------------------
