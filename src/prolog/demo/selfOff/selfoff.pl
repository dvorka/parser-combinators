%------------------------------------------------------------------------------
%
%                              Offline self application
%
%                   Prevod BNF gramatiky do zdroveho souboru PC
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module: Offline self application
Text:   Uk�zka offline self aplikace konstruktor� parser�. Ze vstupn�
        gramatiky v Backus-Naurov� form� je vygenerov�n zdrojov� text
        parseru. Ten lze n�sledn� zav�st do interpretu a pou��t pro
        anal�zu jazyka definovan�ho gramatikou.
p       Parser se generuje p��m�m mapov�n�m oper�tor� BNF na konstruktory
        z knihovny a proto kvalita jeho k�du z�vis� na pou�it� gramatice.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
% Inicializace & knihovny

:- ['../../loadSWI'].          % SWI Prolog
%:- ['../../loadBP-2'].        % BinProlog

%------------------------------------------------------------------------------
%                                Demo
%------------------------------------------------------------------------------
% Zdrojovy kod je generovan do souboru code/block.pl a code/prolog.pl

go:- goBlock, goProlog.

goBlock:-
        bnf2SrcLoader('bnf/block.bnf','code/block.pl').

goProlog:-
        bnf2SrcLoader('bnf/prolog.bnf','code/prolog.pl').

%------------------------------------------------------------------------------
%                         Vstupni bod programu
%------------------------------------------------------------------------------
% bnf2SrcLoader(+SrcFile, +DstFile)
bnf2SrcLoader(SrcFile,DstFile):-
	printf(['Converting...']),
         loadFile(SrcFile, Buffer),             % input file into buffer
         openFile(DstFile,Old,write),           % redirect output to file

	  write('% Machine generated file!'),nl,nl,
          write('% Swi Prolog loader'),nl,
          write(':- [''../../../loadSWI''].'),nl,nl,
          write('% Example goal:'),nl,
          write('% ?- nINTEGER(s("357")+L).'),nl,nl,

          bnfBNF2Src(Buffer),

	  nl,write('% EOF'),nl,
         closeFile(Old,write),                  % close output file
	printf(['done!',nl]).

%------------------------------------------------------------------------------
%                               Code
%------------------------------------------------------------------------------
% Backus-Naur form:
%       example of grammar rule:
%        BLOCK ::= `begin` BLOCK `end` BLOCK | .
%
%       nonterminals ... in uppercase
%       terminals    ... enclosed in ` `
%------------------------------------------------------------------------------
% terminal will be list

bnfTerminal(W):-
 W :->
        (symbol("`") &> nonSymbol("`")<+> <& symbol("`")).

%------------------------------------------------------------------------------
% nonterminal will be atom

bnfNonterminal(W):-
 W :->                                              % ascii(n,110)
        fulfil(isUprChar)<+> <@ alter(R,[110|R]) => string2Atom.

%------------------------------------------------------------------------------

bnfRuleBody(W):-
 W :->
       ( #>(bnfNonterminal <: bnfTerminal) <+> % <?@> []-id
        <&>
       ( #>symbol("|") &> ( #> (bnfNonterminal <: bnfTerminal) )<+> )<*>
        <&
       #>symbol(".")).
        
%------------------------------------------------------------------------------

bnfRule(W):-
 W :->
        #>bnfNonterminal <&> #>token("::=") &> bnfRuleBody.
%------------------------------------------------------------------------------

bnfGrammar(W):-
 W :->
        bnfRule<+> .

bnfGrammarRepeat(W):-
 W :->
        bnfRule<\/> .

%------------------------------------------------------------------------------
%                        Parser source code generator
%------------------------------------------------------------------------------

bnfRuleSubBody2Src([[H|T]|R],<&>(token([H|T]),SrcR)):-
	bnfRuleSubBody2Src(R,SrcR).
bnfRuleSubBody2Src([[H|T]],token([H|T])).
bnfRuleSubBody2Src([NonTerminal|T],<&>(NonTerminal,SrcT)):-
	bnfRuleSubBody2Src(T,SrcT).
bnfRuleSubBody2Src([Parser],Parser).

%------------------------------------------------------------------------------

bnfRuleBody2Src([SubBody],SubBodySrc):-
	bnfRuleSubBody2Src(SubBody,SubBodySrc).
bnfRuleBody2Src([SubBody|T],<:>(SubBodySrc,ParserSrc)):-	
	bnfRuleSubBody2Src(SubBody,SubBodySrc),
	bnfRuleBody2Src(T,ParserSrc).

%------------------------------------------------------------------------------

bnfRule2Src([NonTerminal|Body]):-
	bnfRuleBody2Src(Body,Parser),
	Head=..[NonTerminal,W],
	Goal=(Head :- :->(W,Parser)),
	write(Goal),write('.'),nl.
	
%------------------------------------------------------------------------------

bnfGrammar2Src([Rule|T]):-
	bnfRule2Src(Rule),
	bnfGrammar2Src(T).
bnfGrammar2Src([]).

%------------------------------------------------------------------------------

bnfBNF2Src(File):-
	bnfGrammar(quickS(File)+[_>Grammar]),
	bnfGrammar2Src(Grammar).

bnfBNF2SrcRepeat(File):-
	bnfGrammarRepeat(quickS(File)+[_>Grammar]),
	bnfGrammar2Src(Grammar).

%- EOF ------------------------------------------------------------------------
