%------------------------------------------------------------------------------
%
%                     PrologDoc - HTML code generation
%
%			        Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/** Module: PrologDoc gen HTML
Text:   Èást programu PrologDoc, která generuje z mezikódu dokumentaci
        ve formátu HTML.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     generateHtmlDoc(+SrcFile)
Text:   Generátor HTML dokumentace. Soubory dokumentace jsou vytváøeny
        v podadresáøi ./doc.
*/

generateHtmlDoc(SrcFile):-
        consult(SrcFile),
        nl,genShowTree,nl,
        genHtmlIndexes,
        (clause(pdFile(_,_,_),_)
          -> write('Generating nodes ... '),nl,
             genHtmlNodes,
             genHtmlRootNode,
             genHtmlAboutNode
          ;  true),
        pdCleanDbf,nl.

generateHtmlDoc:-
        generateHtmlDoc('source.pl').
%------------------------------------------------------------------------------
/**     genHtmlIndexes
Text:   Vygeneruje index souborù, index predikátù a index v¹ech definovaných
        operátorù.
*/
genHtmlIndexes:-
        genHtmlFileIndex,
        genHtmlPredicateIndex,
        genHtmlOpIndex.

%------------------------------------------------------------------------------
%                               fileindex.html
%------------------------------------------------------------------------------
/**     genHtmlFileIndex
Text:   Vygeneruje index v¹ech souborù. Bìhem generování jsou pøiøazeny
        jednotlivým souborùm a predikátùm, které jsou v nich definovány,
        unikátní identikaèní èísla. Tato èísla se v dal¹ích fázích vytváøení
        dokumentace pou¾ívají pro pojmenovávání kotev a linkù.
*/
% Data description: pdFile(Key,ID,HTMLName)
%       Key      ... string containing file name (string)
%       ID       ... uniq identification number of the file (integer)
%       HTMLName ... file name with replace HTML reserved symbols (atom)

% Data description: pdPredicate(Key,ID,HTMLName,File)
%       Key      ... predicate name (atom)
%       ID       ... uniq identification number of the predicate (integer)
%       HTMLName ... file name with replace HTML reserved symbols (atom)
%       File     ... key of file where is this predicate defined (string)

genHtmlFileIndex:-
        write('Generating file index ... '),
        openFile('doc/fileindex.html',Old,write),
         pdHtmlPrefix('Index souborù'),write('<UL>'),
          genHtmlFPIndex(ID),
          genHtmlFMIndex(ID),
         write('</UL>'),pdHtmlPostfix,
        closeFile(Old,write),
        printf(['Done!',nl]).

genHtmlFPIndex(IDO):-
        clause(prologDocData(_,_,_,_,_,_,_),_)
             -> findall([F,P,A],prologDocData(F,P,A,_,_,_,_),R),
                fPHtmlIndex(R,noname,1,IDO)
             ;  IDO=1.

% fPHtmlIndex
% - file predicate index
fPHtmlIndex([],_,ID,ID).
fPHtmlIndex([[F,P,A]|T],O,ID,IDO):-
        (O=F -> Old=O, IDF is ID                % remove duplicities
             ;  Old=F, IDF is ID+1,
                % file
                string2Html(F,FName), assert(pdFile(F,IDF,FName)),
                printf(['<A HREF="node',IDF,'.html">',FName,'</A>']),
                (clause(pdModule(F,_,MT),_) -> write('<BR>'),genHtmlTexts(MT) ;  true),
                write('<BR><BR>')
        ),
        % predicate
        IDP is IDF+1,
        atom2Html(P,PName), assert(pdPredicate(P/A,IDP,PName,F)),
        fPHtmlIndex(T,Old,IDP,IDO).

% genHtmlFMIndex
% - module index
genHtmlFMIndex(ID):-
        clause(pdModule(_,_,_),_)
         -> findall(F,pdModule(F,_,_),Modules), fMHtmlIndex(Modules,ID)
         ;  true.
             
fMHtmlIndex([],_).
fMHtmlIndex([F|T],ID):-
        ( clause(pdFile(F,_,_),_)
           -> IDF = ID
           ;  IDF is ID+1,
              string2Html(F,FName), assert(pdFile(F,IDF,FName)),
              printf(['<A HREF="node',IDF,'.html">',FName,'</A><BR>']),
              pdModule(F,_,MT), genHtmlTexts(MT),write('<BR><BR>')
        ),
        fMHtmlIndex(T,IDF).
        
%------------------------------------------------------------------------------
%                               predindex.html
%------------------------------------------------------------------------------
/**     genHtmlPredicateIndex
Text:   Vygeneruje index v¹ech definovaných predikátù. Pro propojení
        pomocí odkazù vyu¾ívá globální informace pøipravené predikátem
        genHtmlFileIndex/0.
*/

genHtmlPredicateIndex:-
        write('Generating predicate index ... '),
        ( clause(pdPredicate(_,_,_,_),_)
           -> findall(P/A,pdPredicate(P/A,_,_,_),Ps),sort(Ps,Sorted),
              openFile('doc/predindex.html',Old,write),
               pdHtmlPrefix('Index predikátù'),write('<UL>'),
                genHtmlPIndex(Sorted),
               write('</UL>'),pdHtmlPostfix,
              closeFile(Old,write)
           ;  true),
        printf(['Done!',nl]).

% genHtmlPIndex(+PredicateNames).
% - generate file index and assert checked names and IDs
genHtmlPIndex([P/A|T]):-
        pdPredicate(P/A,IDP,HtmlName,File),
        pdFile(File,IDF,_),
        printf(['<A HREF="node',IDF,'.html#pred',IDP,'">',
                 HtmlName,'/',A,'</A><BR>']),
        genHtmlPIndex(T).
genHtmlPIndex([]).

%------------------------------------------------------------------------------
%                               opindex.html
%------------------------------------------------------------------------------
/**     genHtmlOpIndex
Text:   Vygeneruje index v¹ech definovaných operátorù. Pro propojení
        pomocí odkazù vyu¾ívá globální informace pøipravené predikátem
        genHtmlFileIndex/0.
*/

genHtmlOpIndex:-
        write('Generating operator index ... '),
        openFile('doc/opindex.html',Old,write),
         pdHtmlPrefix('Index operátorù'),
          (
           clause(pdOperator(_,_,_),_)
           -> findall([Pr,O],pdOperator(O,_,Pr),Os),sort(Os,Sorted),
              write('<BLOCKQUOTE><TABLE><TR ALIGN="CENTER" BGCOLOR="#000000"><TD>Operátor</TD><TD>Precedence</TD><TD>Asociativita</TD></TR>'),
              genHtmlOIndex(Sorted),
              write('</TABLE></BLOCKQUOTE>')
           ;  true
          ),
         pdHtmlPostfix,
        closeFile(Old,write),
        printf(['Done!',nl]).

% genHtmlPIndex(+PredicateNames).
genHtmlOIndex([[Prec,H]|T]):-
        pdOperator(H,Asoc,Prec),
        (clause(pdPredicate(H/_,IDP,HtmlName,File),_)
          -> clause(pdFile(File,IDF,_),_),
             printf(['<TR ALIGN="CENTER"><TD><A HREF="node',IDF,'.html#pred',IDP,'">',
                     HtmlName,'</A></TD><TD>',Prec,'</TD><TD>']),
             htmlShowColorAsoc(Asoc),
             write('</TD></TR>')
          ;  atom2Html(H,HtmlName),
             printf(['<TR ALIGN="CENTER"><TD>',HtmlName,'</TD><TD>',Prec,'</TD><TD>']),
             htmlShowColorAsoc(Asoc),
             write('</TD></TR>')
        ),
        genHtmlOIndex(T).
genHtmlOIndex([]).

%------------------------------------------------------------------------------
%                               node*.html
%------------------------------------------------------------------------------
/**     genHtmlNodes
Text:   Vygeneruje dokumentaci v¹ech definovaných predikátù dle souborù ve
        kterých jsou ulo¾eny.
*/

genHtmlNodes:-
        pdFile(F,_,_),          % navraceni vyhradne pres tento pred.
        genHtmlOneNode(F),
        fail.
genHtmlNodes.


% genHtmlOneNode(+File)
% - generovani dokumentace souboru File
genHtmlOneNode(F):-
        pdFile(F,ID,FHtml),name(ID,IDS),
        mAppend(["doc/node",IDS,".html"],NodeS),name(Node,NodeS),
        printf([' ',Node,nl]),
        openFile(Node,Old,write),
         pdHtmlPrefix(FHtml),
          (clause(pdModule(F,_,MT),_) -> genHtmlTexts(MT) ; true),
          (clause(prologDocData(F,_,_,_,_,_,_),_)
           ->
             write('<H3>Index predikátù</H3><BLOCKQUOTE>'),
              genHtmlNodePredIndex(F),
             write('</BLOCKQUOTE><BR><BR><H3>Predikáty</H3>'),
              genHtmlNodeBody(F)
           ;  true),
         pdHtmlPostfix,
        closeFile(Old,write),!.

% genHtmlNodePredIndex(File)
% - generovani indexu predikatu v souboru File
genHtmlNodePredIndex(File):-
        pdPredicate(K,ID,Html,File),     % navraceni vyhradne pres tento pred.
        genHtmlNodePredIndexItem(K,ID,Html,File),
        fail.
genHtmlNodePredIndex(_).
        
% genHtmlNodeBody(File)
% - predikaty jednoho souboru
genHtmlNodeBody(F):-
        prologDocData(F,P,Ar,Arg,T,A,E), % navraceni vyhradne pres tento pred.
        genHtmlNodeBodyItem([F,P,Ar,Arg,T,A,E]),
        fail.
genHtmlNodeBody(_).

% genHtmlNodePredIndexItem/1
% - ziskani a zobrazeni jednoho indexu
genHtmlNodePredIndexItem(Key/A,ID,Html,File):-
        prologDocData(File,Key,A,_,_,_,_),
        printf(['<A HREF="#pred',ID,'">',Html,'/',A,'</A><BR>']),
        !.     % prevent backtrack in case of repeat fail

% genHtmlNodeBodyItem/1
% - popis jednoho predikatu
genHtmlNodeBodyItem([_,P,Ar,Args,T,A,E]):-
        pdPredicate(P/Ar,ID,Html,_),
        printf(['<A NAME="pred',ID,'"><B>',Html]),
         (Args=[] ; write('('), genHtmlColorArgs(Args), write(')')),
         write('</B></A><BR>'),
        genHtmlTexts(T),
         write('<BLOCKQUOTE>'),
          genHtmlIsOperator(P),
          genHtmlArguments(A),
          genHtmlExample(E),
         write('</BLOCKQUOTE><BR><BR>'),
         !.     % prevent backtrack in case of repeat fail in genHtmlNodeBody
        
%------------------------------------------------------------------------------
% genHtmlIsOperator(Predicate)
% - pokud je definovan jako operator, vypise jeho precedenci a asociativitu
genHtmlIsOperator(P):-
        clause(pdOperator(P,Asoc,Prec),_)
         ->
        printf(['<I>Operátor:</I> ',Prec,', ']),
        htmlShowColorAsoc(Asoc),
        write('<BR>').
genHtmlIsOperator(_).               % nedefinovan

% genHtmlColorArgs(Args)
% - Args je seznam argumentu, predikat vygeruje jejich barevnou a
%   symbolickou reprezentaci dle modu.
genHtmlColorArgs([H]):-
        genHtmlColorArg(H).
genHtmlColorArgs([H|T]):-
        T\=[],
        genHtmlColorArg(H),write(','),genHtmlColorArgs(T).
genHtmlColorArg(Type>A):-
        (Type='+' -> htmlIArg pdOptionIs Col ;
          Type='-' -> htmlOArg pdOptionIs Col ;
           Type='?' -> htmlIOArg pdOptionIs Col),
        name(Aa,A),printf(['<FONT COLOR="',Col,'">',Type,Aa,'</FONT>']).

% genHtmlTexts(Text)
% - text je seznam prologovskych retezcu
genHtmlTexts([H|_]):-
         genHtmlText(H).
genHtmlTexts([]).

genHtmlText([H|T]):-
        % p ... paragraph
        H=[112|TT] -> write('<BR><BR>'),
                      string2Html(TT,A),
                      write(A),nl,
                      genHtmlText(T) ;
        % v ... verbatim
        H=[118|TT] -> write('<BLOCKQUOTE><PRE>'),
                      string2Html(TT,A),
                      write(A),nl,
                      genHtmlText(verbatim,T) ;
        string2Html(H,A),
        write(A),nl,
        genHtmlText(T).
genHtmlText([]).

genHtmlText(verbatim,[H|T]):-       % predchazelo verbatim
        % v ... verbatim
        H=[118|TT] -> string2Html(TT,A),
                      write(A),nl,
                      genHtmlText(verbatim,T) ;
        % p ... paragraph
        H=[112|TT] -> write('</PRE></BLOCKQUOTE><BR><BR>'),
                      string2Html(TT,A),
                      write(A),nl,
                      genHtmlText(T) ;
        write('</PRE></BLOCKQUOTE>'),
        string2Html(H,A),
        write(A),nl,
        genHtmlText(T).
genHtmlText(verbatim,[]):-
        write('</PRE></BLOCKQUOTE>').

% genHtmlArguments(Text)
% - text je seznam prologovskych retezcu
genHtmlArguments([H|T]):-
        printf(['<TABLE>']),genHtmlArgum([H|T]),printf(['</TABLE>']).
genHtmlArguments([]).

genHtmlArgum([Arg>Text|T]):-                      
        name(A,Arg),
        printf(['<TR><TD VALIGN="top"><I>',A,'</I></TD><TD>']),genHtmlText(Text),printf(['</TD></TR>']),
        genHtmlArgum(T).
genHtmlArgum([]).

% genHtmlExample(Text)
% - text je seznam prologovskych retezcu
genHtmlExample([H|_]):-
        write('<I>Pøíklad:</I>'),
        (hilitExamples pdOptionIs yes
         -> genHtmlExam(hilex,H)
         ;  genHtmlExam(poor,H)).
genHtmlExample([]).

genHtmlExam(hilex,H):-
        genHtmlExam(hilex,H,A),
        (hlxCoconizeString('../hilex/coc/prologdoc.coc',A) ; true).

genHtmlExam(poor,H):-
        write('<PRE>'),
        genHtmlExam(H),
        write('</PRE>').

% Primy vypis
genHtmlExam([H]):-
        string2Html(H,A),write(A).
genHtmlExam([H|T]):-
        T\=[],
        string2Html(H,A),write(A),nl,genHtmlExam(T).

% Hilex vypis
genHtmlExam(hilex,[A],AA):-
        append(A,[10],AA).
genHtmlExam(hilex,[A|T],AAA):-
        T\=[],
        genHtmlExam(hilex,T,AA),
        append(A,[10|AA],AAA).

%------------------------------------------------------------------------------
%                               index.html
%------------------------------------------------------------------------------

genHtmlRootNode:-
        (clause(pdModule(IF,IN,IT),_)
          -> (IN=[] -> N=[] ; N=[58,32|IN]),IT=T ; N=[],T=[[]]),
        append("Dokumentace",N,Title), name(TitleA,Title),
        openFile('doc/index.html',Old,write),
         pdHtmlPrefix(TitleA),
          string2Html(IF,HF),
          printf(['<P>Dokumentace byla vygenerována poèínaje souborem <CODE>',HF,
                  '</CODE>, který je koøenem hierarchie souborù tvoøících zdrojový kód.<BR><BR>']),
           genHtmlShowTree,
          genHtmlTexts(T),
          printf(['<P> Dokumentaci tvoøí:<CENTER><TABLE><TR><TD>',
                  '<A HREF="fileindex.html">Index souborù</A><BR>',
                  '<A HREF="predindex.html">Index predikátù</A><BR>',
                  '<A HREF="opindex.html">Index operátorù</A></TD></TR></TABLE></CENTER><BR>']),
        pdHtmlPdLinks(Links),
        printf(['<HR NOSHADE WIDTH="50%">',Links,'<CENTER><FONT SIZE="-1"><I>prologdoc.pl, PCLibrary<BR>Martin Dvorak<BR>2000</I></FONT></CENTER></BODY></HTML>']),
        closeFile(Old,write).

%------------------------------------------------------------------------------
%                               about.html
%------------------------------------------------------------------------------

genHtmlAboutNode:-
        (clause(pdModule(_,IN,IT),_)
          -> (IN=[] -> N=[] ; N=[58,32|IN]),IT=T ; N=[],T=[[]]),
        append("About",N,Title), name(TitleA,Title),
        openFile('doc/about.html',Old,write),
         pdHtmlPrefix(TitleA),
          printf(['<P><CENTER>Dokumentaci vygeneroval program <CODE>PrologDoc',
                  '</CODE>, který byl vytvoøen pomocí knihovny kombinátorù parserù.',
                  '<BR><BR><FONT SIZE="-1"><I>prologdoc.pl, PCLibrary<BR>Martin Dvorak<BR>2000</I></FONT></CENTER><BR>']),
         pdHtmlPostfix,
        closeFile(Old,write).

%------------------------------------------------------------------------------
%                                  Aux
%------------------------------------------------------------------------------
% pdHtml*/0
% - common HTML page parts

pdHtmlPrefix(Title):-
        pdHtmlPdLinks(Links),
        printf([nl,'<HTML><HEAD><TITLE>',Title,'</TITLE>',nl,
                '<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=iso-8859-2">',
                '<BODY TEXT="#FFFFFF" BGCOLOR="#485068" LINK="#C6E2FF" VLINK="#C6E2FF" ALINK="#008000">',
                Links,
                '<HR NOSHADE WIDTH="50%"><TABLE WIDTH="100%"><TR BGCOLOR="#000000" ALIGN="CENTER"><TD><BR><H2>',
                Title,'</H2></TD></TR></TABLE>']).

pdHtmlPdLinks('<CENTER>[ <A HREF="index.html">Home</A> | <A HREF="fileindex.html">Soubory</A> | <A HREF="opindex.html">Operátory</A> | <A HREF="predindex.html">Predikáty</A> | <A HREF="about.html">About</A> ]</CENTER><BR>').
                
pdHtmlPostfix:-
        pdHtmlPdLinks(Links),
        printf(['<HR NOSHADE WIDTH="50%">',Links,'</BODY></HTML>']).

%------------------------------------------------------------------------------
/**     string2Html(+String,+HtmlAtom)
Text:   V øetìzci String jsou nahrazeny rezervované znaky HTML. Výsledek
        je vydán ve formì atomu.
Example:
        ?- string2Html("<&>",'&lt;&amp;&gt;')
        Yes
*/

string2Html(I,A):-
        string2HTML(I,S),
        name(A,S).
        
string2HTML([],[]).
string2HTML([H|T],O):-
        (H=60 -> O=[38,108,116,59|OT] ;                 % &lt;
         H=62 -> O=[38,103,116,59|OT] ;                 % &gt;
         H=38 -> O=[38,97,109,112,59|OT] ;              % &amp;
         H=34 -> O=[38,113,117,111,116,59|OT] ;         % &quot;
         O=[H|OT]),
         string2HTML(T,OT).

%------------------------------------------------------------------------------
/**     atom2Html(+Atom,+HtmlAtom)
Text:   V atomu Atom jsou nahrazeny rezervované znaky HTML. Výsledek
        je vydán rovnì¾ ve formì atomu.
Example:
        ?- string2Html("<&>",'&lt;&amp;&gt;')
        Yes
*/

atom2Html(A,O):-
        name(A,S), string2Html(S,O).

%------------------------------------------------------------------------------
% htmlShowColorAsoc(+Type)
% - barevne vypise typ asociativity
% - Type mohou byt atomy xf, yf, fx, fy, xfx, xfy, yfx, yfy

htmlShowColorAsoc(Asoc):-
        name(Asoc,S),
        htmlShowColAsoc(S).

htmlShowColAsoc([120|T]):-        % x
        htmlOArg pdOptionIs Color,
        printf(['<FONT COLOR="',Color,'">x</FONT>']),
        htmlShowColAsoc(T).
htmlShowColAsoc([121|T]):-        % y
        htmlIArg pdOptionIs Color,
        printf(['<FONT COLOR="',Color,'">y</FONT>']),
        htmlShowColAsoc(T).
htmlShowColAsoc([102|T]):-        % f
        write('f'),
        htmlShowColAsoc(T).
htmlShowColAsoc([]).

%------------------------------------------------------------------------------
% genHtmlShowTree
% - strom vnoreni (pdNestedIn/2).

genHtmlShowTree:-
        printf([nl,'<BLOCKQUOTE><FONT COLOR="#fffacd"><PRE>',nl]),
         genShowTree,
        printf([nl,'</PRE></FONT></BLOCKQUOTE>',nl]).

%- EOF ------------------------------------------------------------------------
