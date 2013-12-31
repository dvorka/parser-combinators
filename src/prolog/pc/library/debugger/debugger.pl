%------------------------------------------------------------------------------
%
%                        Debugger kombinatoru parseru
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module: Debugger
Text:   Debugger kombinátorù parserù.
*/
%------------------------------------------------------------------------------
% Poznamky:
% - tento soubor je zavaden standardnim zavadecem knihovny '../pc*.pl'
%   pri praci s knihovnou lze tedy debugger kdykoli pouzit
% - nastaveni ladicich bodu pomoci spy/1 se projevi pouze u vestavenych
%   predikatu. Uzivatelske predikaty jsou totiz intepretovany, a tak
%   k jejich volani nedochazi - pouzit ladici body debuggeru (option break)
%   viz napoveda.
% - detekce selhani parseru je provadena vzdy, informace o lokalizaci chyby
%   se vsak vypise pouze je-li aktivni volba verbose (pri "tichem behu" 
%   se zobrazi pouze prompt ladiciho bodu)
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                                  Demo
%------------------------------------------------------------------------------

goDebugger1:-
        deBugger(pcDbgDemo1,[break],[digit]).
goDebugger2:-
        deBugger(pcDbgDemo2,[break,verbose,stack],[token]).

pcDbgDemo1:-
        s("12e-3")+_ :-> double.
pcDbgDemo2:-
        off(0,s("abcd"))+_  :-> token("abc")<+> <: symbols("efg")<*> .

%------------------------------------------------------------------------------
/**     deBugger
Text:   Nápovìda.
*/

deBugger:-
        findall(X,pcDbgValidOption(X),Options),
        fprintf([nl,
                 'Parser combinators debugger help:',nl,
                 ' deBugger(+Goal)',nl,
                 ' deBugger(+Goal, +OptionsList)',nl,
                 ' deBugger(+Goal, +OptionsList, +BreakPointList)',nl,
                 nl,
                 'Options:',nl,' ',Options,nl,
                 nl,
                 'Example:',nl,
                 ' ?- deBugger( (s("a")+L :-> symbol("a") <@ show),',nl,
                 ' |            [break,display,verbose],',nl,
                 ' |            [<@,symbol]).',nl]).

%------------------------------------------------------------------------------
%                          Vstupni bod debuggeru
%------------------------------------------------------------------------------
/**     deBugger(+Goal, +Options, +BreakPoints)
Text:   Debugger navr¾ený speciálnì pro parsery vytvoøené v
        konstruktorù umo¾òující krokování,
        prùbì¾né vypisování volaných parserù, sledování
        stopy výpoètu, nastavování ladících bodù, pøístup do struktur
        parserù za bìhu a detekci jejich selhání.
Arg: Options
        Prologovský seznam voleb, který mù¾e obsahovat atomy:
v       verbose, stack, break, trace, display, goals
        Atom 'verbose' urèuje, zda se má vypisovat ka¾dý volaný cíl,
        'stack' vypisování zásobníku volání parserù, 'trace'
        krokování, 'display' zpùsob výpisu operátorù
        a pokud je uveden atom 'break', dojde k zastavení
        pøi volání parseru, který je uveden v seznamu BreakPoints.
Arg: BreakPoints
        Prologovský seznam funktorù parserù a konstruktorù, který je
        seznamem ladících bodù.
Example:
        ?- deBugger( (s("a")+L :-> symbol("a") <@ show),
        |            [break],
        |            [<@,symbol] ).

        ?- deBugger(pcDbgDemo2,
        |           off(0,s("abcd"))+_ :->
        |            token("abc")<+> <: symbols("efg")<*>,
        |           [break,verbose,stack],
        |           [token]).
*/
/**     deBugger(+Goal, +Options)
Text:   Debugger pro kombinátory parserù - viz deBugger/3.
*/
/**     deBugger(+Goal)
Text:   Debugger pro kombinátory parserù - viz deBugger/3.
*/

deBugger(Goal):-
        deBugger(Goal,[],[]).

deBugger(Goal,Options):-
        deBugger(Goal,Options,[]).

deBugger(Goal,Options,BreakPoints):-
        fprintf(['Parser combinators debugger is now running...',nl,
                  ' Debug options: ',Options,nl]),
        pcDbgCheckOptionsValidity(Options),
        pcSolve(Goal,0,[],Options,BreakPoints,_).

%------------------------------------------------------------------------------
%                                Kod
%------------------------------------------------------------------------------
/**     pcDbgValidOption(-ListOfOptions)
Text:   Struktura pøípustných voleb debuggeru.
*/

pcDbgValidOption(stack).        % vypis zasobniku pri kazdem volani
pcDbgValidOption(trace).        % krokovani
pcDbgValidOption(break).        % stop na ladicich bodech uvedenych
                                % v parametru BreakPoints
pcDbgValidOption(display).      % vypis v neoperatorove notaci
pcDbgValidOption(verbose).      % podrobne vypisy volani
pcDbgValidOption(goals).        % vypisy volani neparserovych cilu

pcDbgValidOption(noname).       % E klazule

%------------------------------------------------------------------------------
% pcDbgIsValidOption(+Option)
% - uspeje, pokud Option je platna volba

pcDbgIsValidOption(O):-
        pcDbgValidOption(O).

%------------------------------------------------------------------------------
% pcDbgCheckOptionsValidity(+OptionList)
% - zkontroluje platnost voleb ze seznamu OptionList

pcDbgCheckOptionsValidity([]).
pcDbgCheckOptionsValidity([H|T]):-
        pcDbgValidOption(H)
        -> pcDbgCheckOptionsValidity(T)
        ;  pcError(['Error: invalid debugger option: ',H,nl]) -> fail.

%------------------------------------------------------------------------------
%                               Solver
%------------------------------------------------------------------------------
% pcSolve(+Goal, +Depth, +ParserStack, +Options, +BreakPoints, -OBreakPoints)
% - jadro metainterpretu pro ladeni kombinatoru parseru

% bottom
pcSolve(true,_,_,_,B,B):-
        !.
        
% konjunkce
pcSolve((A,B),Depth,Stack,O,Br,BrOO):-
        !,NewDepth is Depth+1,
	functor(A,F,Arity),
	pcDbgMsg(['[',NewDepth,'] &   ',F,'/',Arity,nl],O),
         pcSolve(A,NewDepth,Stack,O,Br,BrO),
	( functor(B,BF,BArity), 
          BF\=',' -> pcDbgMsg(['[',NewDepth,'] &   ',BF,'/',BArity,nl],O) ; true ),
         pcSolve(B,NewDepth,Stack,O,BrO,BrOO).

% disjunkce
pcSolve((A;B),Depth,Stack,O,Br,BrOO):-
        !,NewDepth is Depth+1,
        (
	 functor(A,F,Arity),
	 pcDbgMsg(['[',NewDepth,'] v   ',F,'/',Arity,nl],O),
         pcSolve(A,NewDepth,Stack,O,Br,BrOO)
          ;
	 functor(B,F,Arity),
	 pcDbgMsg(['[',NewDepth,'] v ',F,'/',Arity,nl],O),
         pcSolve(B,NewDepth,Stack,O,Br,BrOO)).

% :-@ call/n
% - standardni verze :-@/1 se nepouziva: explicitnim osetrenim metainterpret
%   dostava pod kontrolu cile vyvolane predikatem :-@/1
% - cil je vytvoren, nasleduje push noveho cile na zasobnik stopy,
%   nakonec je cil vyvolan
pcSolve(:-@([Term|Args]),Depth,Stack,O,Br,BrOO):-
        !,NewDepth is Depth+1,                  % aktualni hloubka je NewDepth
	functor(Term,F,_),
         % vytvoreni cile
         Term =.. [F|T],                        
         append([F|T],Args,GoalL),
         Goal =.. GoalL,
	pcDbgMsg(['[',NewDepth,'] :-@ ',F,nl],O), % zanoreni z urovne NewDepth
         pcSolve(Goal,NewDepth,Stack,O,Br,BrOO),
	pcDbgMsg(['[',NewDepth,'] @-: ',F,nl],O). % vynoreni na uroven NewDepth

% :->
% - standardni verze :->/2 se nepouziva: explicitnim osetrenim debugger
%   dostava pod kontrolu parsery, ktere jsou jim vyvolany. V pripade
%   smart varianty je "odchyceni" reseno v ramci "beznych cilu".
% - je vytvoren cil, nasleduje push volaneho parseru na zasobnik stopy,
%   nakonec je parser vyvolan
pcSolve(:->(Wrapper,Parser),Depth,Stack,O,Br,BrOO):-
        !,NewDepth is Depth+1,
	functor(Parser,F,_),
         % vytvoreni cile
	 Parser=..ParserList,                   
         append(ParserList,[Wrapper],GoalList),
	 Goal=..GoalList,
        % pokud je volba 'stack' aktivni -> zobrazi obsah stopy
        pcDbgShowStack([F|Stack],O),
        % kontrola, zda je parser ladicim bodem (vstup)
        pcDbgCheckBreakpoint(Wrapper,Parser,Depth,Stack,O,Br,BrO),
        % solve parser
	pcDbgMsg(['[',NewDepth,'] :-> ',Parser,nl],O), % zanoreni z urovne NewDepth
         (pcSolve(Goal,NewDepth,[F|Stack],O,BrO,BrO2)
           ;
          % detekce selhani parseru
          pcDbgMsg(['Error: parser failed: ',nl,'   ',Goal,nl,'Enter c. to continue.',nl],O),pcDbgRead(_)),
	pcDbgMsg(['[',NewDepth,'] <-: ',F,nl],O),      % vynoreni na uroven NewDepth
        % kontrola ladiciho bodu (opusteni)
        pcDbgCheckBreakpoint(Wrapper,Parser,Depth,Stack,O,BrO2,BrOO).

% bezny cil & smart & rucne zapsane parsery
pcSolve(A,Depth,Stack,O,Br,BrOO):-
        !,NewDepth is Depth+1,
	functor(A,F,Arity),
        ( 
	 clause(A,B)                            
          ,                                          % bezny cil muze selhat
         (  % ? je to smart verze parseru ?          % <*>(W :-> P) ... u prvniho parametru pripojen wrapper
           (arg(1,A,Arg),nonvar(Arg),Arg=Wrapper:->FooP,nonvar(FooP)
            ;
            % ? je to neoperatorova verze parseru ?  % posl. arg. je W = I+L
            Arity==1,arg(Arity,A,Wrapper),nonvar(Wrapper),functor(Wrapper,'+',2))

            -> % smart verze

           % volba stack je aktivni -> zobrazeni obsahu zasobniku
           pcDbgShowStack([F|Stack],O),
           % kontrola ladicich bodu (vstup)
           pcDbgCheckBreakpoint(Wrapper,A,Depth,Stack,O,Br,BrO),
           % solve goal
	   pcDbgMsg(['[',NewDepth,'] 8-> ',A,nl],O),
            (pcSolve(B,NewDepth,[F|Stack],O,BrO,BrO2)
              ;
             % detekce selhani parseru
             pcDbgMsg(['Error: smart parser failed: ',nl,'   ',A,nl],O),
             pcDbgCheckBreakpointChoose(Wrapper,A,Depth,Stack,O,[],_,Trace),Trace
            ),
	   pcDbgMsg(['[',NewDepth,'] <-8 ',F,nl],O),
           % kontrola ladiciho bodu (opusteni)
           pcDbgCheckBreakpoint(Wrapper,A,Depth,Stack,O,BrO2,BrOO)

            ;  % bezny cil

	   pcDbgMsg(['[',NewDepth,'] >   ',F,'/',Arity,nl],O),
            pcSolve(B,NewDepth,Stack,O,Br,BrOO),
	   pcDbgMsg(['[',NewDepth,'] <   ',F,'/',Arity,nl],O)
         )
          ;                                     
         % built-in predikat
         (member(goals,O)
          -> pcDbgMsg(['[',NewDepth,'] built-in > ',A,nl],O)
	  ;  pcDbgMsg(['[',NewDepth,'] built-in > ',F,'/',Arity,nl],O)),!,
         Br=BrOO,
         A              % prime volani cile A namisto jeho tela
        ).

%------------------------------------------------------------------------------
% pcDbgMsg(+Message, +Options)
% - pri volbe 'verbose' vypisuje Message

pcDbgMsg(Msg):-
        pcDbgMsg(Msg,[verbose]).
pcDbgMsg(Msg,Options):-
        (member(display,Options) -> Show=display ; Show=write),
        member(verbose,Options)
         -> pcStdOut(StdOut),fprintf(StdOut,Show,Msg)
         ;  true.

%------------------------------------------------------------------------------
% pcDbgShowStack(+Stack, +Options)
% - pri volbe 'stack' zobrazuje obsah zasobniku

pcDbgShowStack(Stack,Options):-
        member(stack,Options)
         ->
        fprintf(['--- TOS ---',nl]),
        pcDbgStackListing(Stack),
        fprintf(['--- BOS --- ... stack when calling V',nl]).
pcDbgShowStack(_,_).

%------------------------------------------------------------------------------
% pcDbgStackListing(+Stack).

pcDbgStackListing([H|T]):-
        pcDbgMsg([H]),nl,
        pcDbgStackListing(T).
pcDbgStackListing([]).

%------------------------------------------------------------------------------
% pcLosPrinter(+WantAtomized, +ItemNum, +LOS)
% - LOS pretty printer

pcLosPrinter(_,_,LOS):-
        var(LOS),!,fprintf([unbound,nl]).
pcLosPrinter(Atomized,ItemNum,[N>R|T]):-
        fprintf(['[',ItemNum,']   ']),
        (Atomized=yes -> pcDbgPrintAtomizedInput(N) ; fprintf([N])),
        fprintf(['>',R,nl]),
        NewItemNum is ItemNum+1,
        pcLosPrinter(Atomized,NewItemNum,T).
pcLosPrinter(_,ItemNum,[]):-
        fprintf(['[',ItemNum,']   []',nl]).
        
%------------------------------------------------------------------------------
% pcDbgCheckBreakpoint(Wrapper,Parser,Depth,Stack,Options,BreakPoints,OutBreakPoints)
% - volan z pcSolve
% - zkontroluje, zda Parser je aktualnim ladicim bodem. Pokud ano, provede
%   odpovidajici akce

pcDbgCheckBreakpoint(Wrapper,Parser,Depth,Stack,Options,BreakPoints,OB):-
        (
         member(trace,Options),
         functor(Parser,F,Arity)
         ;
         member(break,Options),
         functor(Parser,F,Arity),
         member(F,BreakPoints)
        )
        ->
        fprintf([nl,'Breakpoint ',F,'/',Arity,' at depth ',Depth,' (h. for help)',nl]),
        pcDbgCheckBreakpointChoose(Wrapper,Parser,Depth,Stack,Options,BreakPoints,OB,Trace),!,
        Trace.
pcDbgCheckBreakpoint(_,_,_,_,_,B,B).

%------------------------------------------------------------------------------

pcDbgCheckBreakpointChoose(W,P,D,S,O,B,OOB,T):-
         fprintf(['(deBugger) ']), pcDbgRead(Choose),
         (pcDbgChoosen(Choose,S,P,W,B,OB) ; B=OB),
         ( Choose==n
           -> OOB=OB, (T==trace -> true; T=true)
           ;  ( Choose==f
                -> OOB=[], (T==trace -> true; T=true)
                ;  ( Choose==t
                     -> OOB=OB, T=trace
                     ;  pcDbgCheckBreakpointChoose(W,P,D,S,O,OB,OOB,T)))).

%- Akce specifikovane v pcDbgCheckBreakpoint ----------------------------------
% pcDbgChoosen(+ChoosenOption,+Stack,+Parser,+Wrapper,+Breakpoints,-Breakpoints)

% continue (next)
pcDbgChoosen(n,_,_,_,B,B).

% big help
pcDbgChoosen(hh,_,_,_,B,B):-
        deBugger.

% help
pcDbgChoosen(h,_,_,_,B,B):-
          %----------------------------------------+--------------------------------------
 fprintf(['Help:',nl,
          ' s  ... content of debugger stack',nl,
          ' p  ... show parser',nl,
          ' l  ... show LOS structure of the parser   al ... show LOS with atomized rest',nl,
          ' i  ... show input                         ai ... show atomized input',nl,
          ' b  ... show breakpoints',nl,
          ' ab ... add breakpoint                     rb ... remove breakpoint',nl,
          '                                           cb ... clear all breakpoints',nl,
          ' t  ... run internal debugger (trace)',nl,
          ' sb ... spy',nl,
          ' se ... nospy                              sa ... nospyall',nl,
          ' li ... predicate source code (listing)',nl,
          ' is ... interpret&PClib statistics         gc ... run garbage collector',nl,
          ' c  ... call goal (?-)',nl,
          ' h  ... help                               hh ... deBugger predicate help',nl,
          ' n  ... continue in debugging (next)       f  ... finish parse - no stops',nl,
          '                                                  (brekpoints ignored)',nl,
          ' a  ... abort',nl
         ]).

% stack
pcDbgChoosen(s,Stack,_,_,B,B):-
        fprintf(['--- TOS ---',nl]),
        pcDbgStackListing(Stack),
        fprintf(['--- BOS ---',nl]).

% LOS
pcDbgChoosen(l,_,_,_+L,B,B):-
        fprintf(['---     LOS    ---',nl]),
         pcLosPrinter(no,1,L),
        fprintf(['--- End of LOS ---',nl]).

% LOS with rest atomized
pcDbgChoosen(al,_,_,_+L,B,B):-
        fprintf(['---     LOS    ---',nl]),
         pcLosPrinter(yes,1,L),
        fprintf(['--- End of LOS ---',nl]).

% input
pcDbgChoosen(i,_,_,I+_,B,B):-
        fprintf([I,nl]).

% atomized input
pcDbgChoosen(ai,_,_,Ii+_,B,B):-
        pcDbgPrintAtomizedInput(Ii),nl.


% show breakpoints
pcDbgChoosen(b,_,_,_,B,B):-
        B=[] -> fprintf(['No breakpoints.',nl])
             ;  fprintf(['Breakpoints:',nl]),mapList(shownl,B,_),nl.

% clear all breakpoints
pcDbgChoosen(cb,_,_,_,_,[]):-
        fprintf(['Breakpoints removed.',nl]).

% add breakpoint
pcDbgChoosen(ab,_,_,_,B,[Choose|B]):-
        fprintf(['Enter breakpoint to add: ']), pcDbgRead(Choose),
        fprintf(['Breakpoints:',nl]),mapList(shownl,[Choose|B],_),nl.

% remove breakpoint
pcDbgChoosen(rb,_,_,_,B,DB):-
        fprintf(['Enter breakpoint to delete: ']), pcDbgRead(Choose),
        deleteList(B,Choose,DB),
        fprintf(['Breakpoints:',nl]),mapList(shownl,DB,_),nl.

% trace
pcDbgChoosen(t,_,_,_,B,B):-
        trace.

% abort
pcDbgChoosen(a,_,_,_,_,_):-
        abort.

% spy
pcDbgChoosen(sb,_,_,_,B,B):-
        fprintf(['Enter predicate to spy/1: ']), pcDbgRead(Choose),
        spy(Choose),nl.

% nospy
pcDbgChoosen(se,_,_,_,B,B):-
        fprintf(['Enter predicate to nospy/1: ']), pcDbgRead(Choose),
        nospy(Choose),nl.

% nospy
pcDbgChoosen(sa,_,_,_,B,B):-
        fprintf(['nospyall']),nospyall,nl.

% listing
pcDbgChoosen(li,_,_,_,B,B):-
        fprintf(['Enter predicate to list: ']), pcDbgRead(Choose),
        listing(Choose),nl.

% statistics
pcDbgChoosen(is,_,_,_,B,B):-
        pcStatistic.

% GC
pcDbgChoosen(gc,_,_,_,B,B):-
        pcGC.

% call goal
pcDbgChoosen(c,_,_,_,B,B):-
        fprintf(['Enter predicate to call: ?- ']), pcDbgRead(Choose),
        (Choose,nl ; fprintf([nl,'* Goal: ',Choose,' failed *',nl])).

% parser
pcDbgChoosen(p,_,P,_,B,B):-
        P== _ :-> Parser
         -> fprintf([Parser,nl])
         ;  fprintf([P,nl]).

%------------------------------------------------------------------------------
% pcDbgPrintAtomizedInput(+PcInput)

pcDbgPrintAtomizedInput(Ii):-
        pcGetInput(Ii,I)
         -> name(A,I),fprintf([A])
         ;  fprintf(['Error: Unknown input descriptor:',nl,Ii,nl]).

%------------------------------------------------------------------------------
% pcDbgRead(X)
% - prologovsky read/1 s presmerovanim vstupu (nutne pri praci se soubory (IN))

pcDbgRead(X):-
        seeing(O),
        pcStdIn(Keyb),see(Keyb),
        read(X),
        see(O).

%- EOF ------------------------------------------------------------------------
