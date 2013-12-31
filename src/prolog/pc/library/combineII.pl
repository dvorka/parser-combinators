%------------------------------------------------------------------------------
%
%                          More parser combinators II
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Kombin�tory parser� II.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     lchainedBy(+ElementParser, +SeparatorParser, ?Wrapper)
Text:   Analyzuje polo�ky odd�len� separ�tory, kter� maj� v�znam
        bin�rn�ch oper�tor� s levou asociativitou. S�mantick�
        akce odpov�daj�c� jednotliv�m separ�torum jsou aplikov�ny
        po zpracov�n� cel�ho vstupu.
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
Text:   Analyzuje polo�ky odd�len� separ�tory, kter� maj� v�znam
        bin�rn�ch oper�tor� s levou asociativitou. S�mantick�
        akce odpov�daj�c� jednotliv�m separ�torum jsou aplikov�ny
        po zpracov�n� cel�ho vstupu.
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
Text:   Analyzuje polo�ky odd�len� separ�tory, kter� maj� v�znam
        bin�rn�ch oper�tor� s levou asociativitou. S�mantick�
        akce odpov�daj�c� jednotliv�m separ�tor�m jsou aplikov�ny
        v pr�b�hu anal�zy vstupu. Pro vyhodnocen� je pou��v�n
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
Text:   Analyzuje polo�ky odd�len� separ�tory, kter� maj� v�znam
        bin�rn�ch oper�tor� s levou asociativitou. S�mantick�
        akce odpov�daj�c� jednotliv�m separ�tor�m jsou aplikov�ny
        v pr�b�hu anal�zy vstupu. Pro vyhodnocen� je pou��v�n
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
Text:   V seznamu �sp�n�ch rozklad� vr�t� v�echny znaky a� po odd�lova�
        nov�ho ��dku. Ten je ze vstupu odebr�n, av�ak do v�sledku
        ulo�en nen�.
*/
% - get one line (without line delimiter)

getLine(W):-
 W :->
        nonSymbol([10])<*> <& symbol([10]).
        
%------------------------------------------------------------------------------
/**     getEOF(?Wrapper)
Text:   V seznamu �sp�n�ch rozklad� vr�t� ��st vstupn�ho textu,
        kter� nebyla dosud zpracov�na.
*/

getEOF(W):-
 W :->
        item<*> .

%------------------------------------------------------------------------------
/**     skipLine(?Wrapper)
Text:   Na rozd�l od getLine vr�t� v seznamu �sp�n�ch rozklad� jako
        v�sledek atom 'line'.
*/

skipLine(W):-
 W :->
        nonSymbol([10])<*> &> symbol([10]) <@ const(line).

%------------------------------------------------------------------------------
/**     untilEOL(?Wrapper)
Text:   Ignoruje v�e a� po odd�lova� ��dku v�etn�. V seznamu �sp�n�ch
        rozklad� vrac� atom '10'.
*/
% - ignores everything till the end of line and returns atom 10

untilEOL(W):-
 W :->
        nonSymbol([10])<*> &> symbol([10]).

%------------------------------------------------------------------------------
/**     <&&>(+Parser1, +Parser2, ?Wrapper)
Text:   Kombin�tor prov�d�j�c� sekven�n� kompozici parser�
        Parser1 a Parser2, kter� je ur�en pro kombinov�n� parser�,
        jejich� v�sledkem m� b�t �et�zec. V�sledky obou parser� jsou
        o�et�eny tak, aby je bylo mo�n� z�et�zit pomoc� append/3.
        T�m p�dem nevznikaj� �et�zce, kter� je nutn� n�sledn�
        narovn�vat nap��klad pomoc� coloneFlattenString/2.
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
Text:   Kombin�tor nestedIn umo��uje analyzovat strukturu vno�en�ch
        blok� s mo�nost� aplikace s�mantick�ch operac� na v�sledky.
Arg:    OpenAndClose
        '+Open and +Close' ... ka�d� blok je uzav�en mezi tokeny
        'Open' a 'Close' (and/2 je pouze oper�tor spojuj�c� parametry)
Arg:    ParFunCon
        '+Parser - +Fun - +Constant' ... 'Parser' akceptuje vnit�n�
        ��st bloku, 'Fun' prov�d� jej� tranformaci a 'Constant'
        je term reprezentuj�c� list p�ij�man� stromov� struktury.
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
Text:   Parser pro Pascal like vno�en� koment��e. Struktura koment��e
        je sice analyzov�na, ale na v�stup je vyd�n pouze atom 'comment'.
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
Text:   Scanner rozlo�� vstup na polo�ky relevantn�  z hlediska parseru.
        �lohou scanneru je rovn� zapouzd�it vhodn�m zp�sobem tokenizovan�
        vstup - tak lze ovliv�ovat m�d parseru. V�sledek Parseru
        je i v�stupem dvoupr�chodov�ho rozkladu.
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
