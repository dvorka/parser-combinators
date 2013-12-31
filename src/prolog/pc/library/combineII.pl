%------------------------------------------------------------------------------
%
%                          More parser combinators II
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Kombinátory parserù II.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     lchainedBy(+ElementParser, +SeparatorParser, ?Wrapper)
Text:   Analyzuje polo¾ky oddìlené separátory, které mají význam
        binárních operátorù s levou asociativitou. Sémantické
        akce odpovídající jednotlivým separátorum jsou aplikovány
        po zpracování celého vstupu.
*/
%       symbol <@ const(add)
% - parse elements separated by some symbol - left association
% - semantic action is done after parsing and result is returned in LOS
%   ?- lchainedBy(digit,symbol("+"),"1+2-3",L).
%    		  -> ...1 > [ +>2, ->3 ]...
%    		  -> fold
%		  LOS = [ [] > 0, ...

lchainedBy(P,S,W):-
 W :->
        (P <&>> (S <&>> P)<**>) <@ chainFoldL.

chainFoldL(Out>[],Out).
chainFoldL(InVal>[Op>I|IT],Out):-
        :-@ [Op,InVal,I,OutVal],
        chainFoldL(OutVal>IT,Out).

%------------------------------------------------------------------------------
/**     rchainedBy(+ElementParser, +SeparatorParser, ?Wrapper)
Text:   Analyzuje polo¾ky oddìlené separátory, které mají význam
        binárních operátorù s levou asociativitou. Sémantické
        akce odpovídající jednotlivým separátorum jsou aplikovány
        po zpracování celého vstupu.
*/
%       symbol <@ const(add)
% - parse elements separated by some symbol - right association
% - semantic action is done after parsing and result is returned in LOS
%   ?- rchainedBy(digit,symbol("+"),"1+2+3",L).  -> ...[ 1>+, 2>+ ] > 3... -> fold

rchainedBy(P,S,W):-
 W :->
        ((P <&>> S)<**> <&>> P) <@ chainFoldR.

chainFoldR([]>InVal,InVal).             % case of foldR
chainFoldR([I>Op|IT]>InVal,Out):-
        chainFoldR(IT>InVal,Out1),
        :-@ [Op,I,Out1,Out].

%------------------------------------------------------------------------------
/**     chainR(?Evaluator, +ElementParser, +Separator, ?Wrapper)
Text:   Analyzuje polo¾ky oddìlené separátory, které mají význam
        binárních operátorù s levou asociativitou. Sémantické
        akce odpovídající jednotlivým separátorùm jsou aplikovány
        v prùbìhu analýzy vstupu. Pro vyhodnocení je pou¾íván
        Evaluator.
Example:
        ?- s("1+2-3")+L :-> chainR(id).
        L = 1 > ((+>2) > (->3) > [])
        Yes
        ?- s("1+2-3")+L :-> chainR(eval).
        L = 0
        Yes
*/

chainR(E,P,S,W):-
 W :->
        (P <&>> (S<&>>P)<*@*>epsilon-eChainR(E))
                <@ evalChainR(E).

eChainR(E,(Op>Num)>(OpR>NumR),Op>R):-
    P=..[OpR,Num,NumR], :-@ [E,P,R].
eChainR(_,ON>[],ON).                    % bottom

evalChainR(E,Num>(OpR>NumR),R):-
    P=..[OpR,Num,NumR], :-@ [E,P,R].
evalChainR(_,Num>[],Num).               % if there is no separator

%------------------------------------------------------------------------------
/**     chainL(?Evaluator, +ElementParser, +Separator, ?Wrapper)
Text:   Analyzuje polo¾ky oddìlené separátory, které mají význam
        binárních operátorù s levou asociativitou. Sémantické
        akce odpovídající jednotlivým separátorùm jsou aplikovány
        v prùbìhu analýzy vstupu. Pro vyhodnocení je pou¾íván
        Evaluator.
Example:
        ?- s("1+2-3")+L :-> chainL(id).
        L = ((1>+) > (2>+) > 3)
        Yes
        ?- s("1+2-3")+L :-> chainL(eval).
        L = 0
        Yes
*/

chainL(E,P,S,W):-
 W :->
        (P<&>>S)<*@*>P-eChainL(E).      % tricky: <*@>P-

eChainL(E,(Num>Op)>NumR,R):-
    P=..[Op,Num,NumR], :-@ [E,P,R].

% Tradicnim zpusobem by nebylo mozne, protoze inicialni hodnota prichazi az
% po vynoreni z rekurze. Proto pouzijeme maly trik a nahradime terminator
% parserem inicialni hodnoty. Parser se tim padem znacne zjednodusi
% (funkcionalni nezvladli) zustala tam otazka otevrena
%------------------------------------------------------------------------------
/**     getLine(?Wrapper)
Text:   V seznamu úspì¹ných rozkladù vrátí v¹echny znaky a¾ po oddìlovaè
        nového øádku. Ten je ze vstupu odebrán, av¹ak do výsledku
        ulo¾en není.
*/
% - get one line (without line delimiter)

getLine(W):-
 W :->
        nonSymbol([10])<*> <& symbol([10]).
        
%------------------------------------------------------------------------------
/**     getEOF(?Wrapper)
Text:   V seznamu úspì¹ných rozkladù vrátí èást vstupního textu,
        která nebyla dosud zpracována.
*/

getEOF(W):-
 W :->
        item<*> .

%------------------------------------------------------------------------------
/**     skipLine(?Wrapper)
Text:   Na rozdíl od getLine vrátí v seznamu úspì¹ných rozkladù jako
        výsledek atom 'line'.
*/

skipLine(W):-
 W :->
        nonSymbol([10])<*> &> symbol([10]) <@ const(line).

%------------------------------------------------------------------------------
/**     untilEOL(?Wrapper)
Text:   Ignoruje v¹e a¾ po oddìlovaè øádku vèetnì. V seznamu úspì¹ných
        rozkladù vrací atom '10'.
*/
% - ignores everything till the end of line and returns atom 10

untilEOL(W):-
 W :->
        nonSymbol([10])<*> &> symbol([10]).

%------------------------------------------------------------------------------
/**     <&&>(+Parser1, +Parser2, ?Wrapper)
Text:   Kombinátor provádìjící sekvenèní kompozici parserù
        Parser1 a Parser2, který je urèen pro kombinování parserù,
        jejich¾ výsledkem má být øetìzec. Výsledky obou parserù jsou
        o¹etøeny tak, aby je bylo mo¾né zøetìzit pomocí append/3.
        Tím pádem nevznikají øetìzce, které je nutné následnì
        narovnávat napøíklad pomocí coloneFlattenString/2.
Example:
        ?- s("abcd")+L :->
        |       (token("abc") <&> symbol("d")).
        L = [s([])>[[97, 98, 99]|100]]
        Yes
        ?- s("abcd")+L :->
        |       (token("abc") <&&> symbol("d") <&> epsilon).
        L = [s([])>[97, 98, 99, 100]]
        Yes
*/

<&&>^^^(R1,N2>R2,[N2>R12]):-
        (append(R1,R2,R12)                       % oba vysledky jsou 'typu' seznam
          ;
         ((R1=[] ; R1=[_|_]),RR1=R1 ; RR1=[R1]), % nevysla-li prvni varianta, 
         ((R2=[] ; R2=[_|_]),RR2=R2 ; RR2=[R2]), % musi se term prevest
         append(RR1,RR2,R12)).                   % na seznam

<&&>(P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(<&&>^^^ ,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
<&&>(P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L):-
        seqCompAux(<&&>^^^ ,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).

<&&>(P1,P2,empty+L):-
        <&>>(P1,P2,empty+L).
<&&>(P1,P2,first+FIRST):-                      % vyuziva empty
        <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
<&&>(P1,P2,eFirst+eFirst(Empty,FIRST)):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<&&>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

% <&&>(+Parser1, +Parser2, ?Wrapper)
<&&>(P1,P2,I+L):-
    I+L1 :-> P1,
    <&&>^(P2,L1,L-[]).

% <&&>^(+Parser2, +LOSof1, -ComposedDifLOS)
% - predicate gets LOS of the Parser1, on each LOS-member it calls Parser2 
%   and then does concatenation of both results: (N>R1>R2).
<&&>^(P2,[N1>R1|L1s],L12-D):-
    N1+L2 :-> P2,                       % call P2 on NotParsedRest
    <&&>^^(R1,L2,L12-DD),               % bind results of P1 and P2 using >/2
    <&&>^(P2,L1s,DD-D).
<&&>^(_,[],D-D).

% <&&>^^(+ResultOfP1, +LOSOfP2, +LOSOfP1P2 - -D)
% - predicate for the result concatenation
<&&>^^(R1, [N2>R2|L2s], [N2>R12|L12s]-D):-
        (append(R1,R2,R12)                       % oba vysledky jsou 'typu' seznam
          ;
         ((R1=[] ; R1=[_|_]),RR1=R1 ; RR1=[R1]), % nevysla-li prvni varianta, 
         ((R2=[] ; R2=[_|_]),RR2=R2 ; RR2=[R2]), % musi se term prevest
         append(RR1,RR2,R12)                     % na seznam
        ),
        <&&>^^(R1, L2s, L12s-D).
<&&>^^(_, [], D-D).

%------------------------------------------------------------------------------
/**     nestedIn(+ParFunCon, +OpenAndClose, ?Wrapper)
Text:   Kombinátor nestedIn umo¾òuje analyzovat strukturu vnoøených
        blokù s mo¾ností aplikace sémantických operací na výsledky.
Arg:    OpenAndClose
        '+Open and +Close' ... ka¾dý blok je uzavøen mezi tokeny
        'Open' a 'Close' (and/2 je pouze operátor spojující parametry)
Arg:    ParFunCon
        '+Parser - +Fun - +Constant' ... 'Parser' akceptuje vnitøní
        èást bloku, 'Fun' provádí její tranformaci a 'Constant'
        je term reprezentující list pøijímané stromové struktury.
Example:
        pascalNestedComment(W):-
        W :->
           brace(nonSymbols("{}")<*>
            &>
             nonSymbols("{}")-const(comment)-nil
              nestedIn
               symbol("{") and symbol("}")<?@>comment-id).
*/

nestedIn(P-F-C,Open and Close,W):-
 W :->
       ((Open &>
                (P <: (P-F-C nestedIn Open and Close) )<+>
                <& Close)
           <&>> ( (P <: (P-F-C nestedIn Open and Close) )<+>
                  <: return(C))
       ) <@ F.

%------------------------------------------------------------------------------
/**     pascalNestedComment(?Wrapper)
Text:   Parser pro Pascal like vnoøené komentáøe. Struktura komentáøe
        je sice analyzována, ale na výstup je vydán pouze atom 'comment'.
*/
% Prolog, Pascal, ...

pascalNestedComment(W):-
 W :->  brace(nonSymbols("{}")<*>
               &>
              nonSymbols("{}")-const(comment)-nil
               nestedIn
                symbol("{") and symbol("}")<?@>comment-id).

%------------------------------------------------------------------------------
/**     chainPassage(+Scanner, +Parser, ?Wrapper)
Text:   Scanner rozlo¾í vstup na polo¾ky relevantní  z hlediska parseru.
        Úlohou scanneru je rovnì¾ zapouzdøit vhodným zpùsobem tokenizovaný
        vstup - tak lze ovlivòovat mód parseru. Výsledek Parseru
        je i výstupem dvouprùchodového rozkladu.
Example:
        ?- s("3*2")+L :->
        |   (itemA<*> <@ alter(X,s(X)) => shownl)
        |    chainPassage
        |     expressioner.
        s([3,*,2])
        L=[s("")>6]
        Yes
*/

chainPassage(Scanner,Parser,I+L):-
 I+Chains :->
        (Scanner <@ chainParser(Parser) => gotaChainResult),
 flattenList(Chains,L).

chainParser(Parser,I,L):-
 I+L :->
        Parser.

gotaChainResult([_>R|_],R).
gotaChainResult([],pcEmpty).

%- EOF ------------------------------------------------------------------------
