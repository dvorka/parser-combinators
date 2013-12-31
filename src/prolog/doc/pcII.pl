%------------------------------------------------------------------------------
%
%
%                          Knihovna kombinatoru parseru
%
%                   Priklady z textu prace: Kapitola 4 a Kapitola 5
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Poznamky:
% - prosim konzultujte tento soubor pomoci ../docII.pl
% - konvence ve zdrojovych souborech lze nalezt v ../pc/README.conventions
%------------------------------------------------------------------------------
% Prehled prikladu:
go:-
        printf(['Available examples:',nl]),
        listing(examples).

:- nl,write(' * Type go/0 for available examples *'),nl,nl.
%------------------------------------------------------------------------------
%                       "Vlastnosti operatorove notace"
%------------------------------------------------------------------------------
% Priklad:
goOperator1:-
        printf(['Example 1:',nl,' ']),
        P=symbolA("A"), Q=symbolA("a"), F=shownl,
 s("Aaa")+_ :->
        (P<**> <&> Q <@ F).

% [[A]|a]
% Yes

%------------------------------------------------------------------------------
% Priklad:
goOperator2:-
        printf(['Example 2:',nl,' ']),
        P=symbolA("A"), Q=symbolA("a"), F=shownl,
 s("Aaa")+_ :->
        P<**> <&> Q <@ F.

% No

%------------------------------------------------------------------------------
% Priklad:
goOperator3:-
        printf(['Example 3:',nl,' ']),
        P=symbolA("A"), Q=symbolA("a"), F=shownl,
 s("Aaa")+_ :->
        (P<**> <&> Q) <@ F.

% [[A]|a]
% Yes

%------------------------------------------------------------------------------

%        ----------------------------
%       | viz ../pc/library/smart.pl |
%        ----------------------------

%------------------------------------------------------------------------------
%                           "Zkracene vyhodnoceni"
%------------------------------------------------------------------------------
% letter1(?Wrapper)
letter1(W):-
 W :-> 
       (lower <:> upper).

%------------------------------------------------------------------------------

%        ------------------------------
%       | viz ../pc/library/combine.pl |
%        ------------------------------

%------------------------------------------------------------------------------
% letter(?Wrapper)
letter(W):-
 W :-> 
       lower <: upper.

%------------------------------------------------------------------------------
% bin1(?Wrapper)
bin1(W):-
 W :->
        (symbolA("0") <: symbolA("1"))<<*>>
                <@ foldR(evalBin,0>0) => alter(Val>_,Val).

evalBin(I,Val>Pow,Value>Power):-
        Power is Pow+1, Value is Val+(2^Pow)*I.

%------------------------------------------------------------------------------

%        ---------------------------------
%       | viz ../pc/library/mutatePart.pl |
%        ---------------------------------

%------------------------------------------------------------------------------
% bin(?Wrapper)
bin(W):-
 W :->
        (symbolA("0") <: symbolA("1"))<*>
                <@ foldR(evalBin,0>0) => alter(Val>_,Val).

%------------------------------------------------------------------------------
%                  "Zestihlovani seznamu uspesnych rozkladu"
%------------------------------------------------------------------------------
% <?(+Parser, +Condition, ?Wrapper)
<?(P,Cond,I+FL):-
        I+L :-> P,
        filter(sieve(Cond),L,FL).

sieve(Cond,_>R):- :-@ [Cond,R].

%------------------------------------------------------------------------------
% nestedIn(+Parser - +Fun - +Constant, +Open and +Close, ?Wrapper)
nestedIn(P-F-C,Open and Close,W):-
 W :->
       (
        (
          (Open &>
                (P <: (P-F-C nestedIn Open and Close) )<+>
                <& Close)
           <&>> ( (P <: (P-F-C nestedIn Open and Close) )<+>
                  <: return(C))
        ) <@ F).

%------------------------------------------------------------------------------
% fooNestedBlocks(?Wrapper)
fooNestedBlocks(W):-
  W :->
        (tokenA("c;")-funNB-nil
          nestedIn
           symbol("{") and symbol("}")).

funNB(A>B,nest(A,B)).

%------------------------------------------------------------------------------
% Priklad:
goTrim1:-
 s("{c;{c;{c;}c;}}c;{c;}")+L :->
        fooNestedBlocks,
                        docShow(4,[L]).

% L = [s([])>nest([c;, nest([c;, nest([c;], [c;])], nil)], [c;, nest([c;], nil)])]
% Yes

%------------------------------------------------------------------------------
% fooNestedBlocksDepth(?Wrapper)
fooNestedBlocksDepth(W):-
  W :->
        ((token("c;") <@ const(0))-funNBDepth-[0]
          nestedIn
           symbol("{") and symbol("}")).

funNBDepth(A>B,X):-
        maxList(A,MA), maxList(B,MB), max3(MA,MB,M), X is M+1.
maxList([H|T],Max):- foldL(max3,H,T,Max).

max3(X,Y,M):- X>Y -> M=X ; M=Y.
        
%------------------------------------------------------------------------------
% Priklad:
goTrim2:-
 s("{c;{c;{c;}c;}}c;{c;}")+L :->
        fooNestedBlocksDepth,
                        docShow(5,[L]).

% L = [s([])>3]
% Yes
        
%------------------------------------------------------------------------------
% pascalNestedComment(?Wrapper)
pascalNestedComment(W):-
 W :->  brace(nonSymbols("{}")<*>
               &>
              nonSymbols("{}")-const(comment)-nil
               nestedIn
                symbol("{") and symbol("}")<?@>comment-id).

%------------------------------------------------------------------------------
% Priklad:
goTrim3:-
 s("{ This {is} { nested {comment}}!} clrsrc;")+L :->
        pascalNestedComment,
                L=[s(N)>R],name(A,N),docShow(6,[s(A)>R]).

% L = [s(" clrsrc;") >comment]
% Yes
                
%------------------------------------------------------------------------------
%                               "Leva faktorizace"
%------------------------------------------------------------------------------
% fooLeftFactorization(?Wrapper)
fooLeftFactorization1(W):-
 W :->
        (double <&>> symbolA("*") <&>> double
          <:
         double <&>> symbolA("/") <&>> double) <@ evalFact.
           

evalFact(X>(Op>Y),R):- Op == '*', R is X*Y ; R is X/Y.

%------------------------------------------------------------------------------
% fooLeftFactorization(?Wrapper)
fooLeftFactorization(W):-
 W :->
        (double <&>>
           (symbolA("*") <: symbolA("/"))
                                <&>> double) <@ evalFact.

%------------------------------------------------------------------------------
% Priklad:
goFact1:-
 s("3.14/2.72")+L :->
        fooLeftFactorization1,
                printf(['Example 7:',nl,' ',L,nl]).

% L = [s([])>1.15441]
% Yes
                    
%------------------------------------------------------------------------------
% Priklad:
goFact2:-
 s("3.14/2.72")+L :->
        fooLeftFactorization,
                printf(['Example 8:',nl,' ',L,nl]).

% L = [s([])>1.15441]
% Yes
                    
%------------------------------------------------------------------------------
%                          "Aritmeticke vyrazy"
%------------------------------------------------------------------------------
exprA(W):-
        termA(W).
termA(W):-
 W :->  (exprA <&>> symbolA("*") <&>> factA)
          <:
        (exprA <&>> symbolA("/") <&>> factA).
factA(W):-
  W :->  int
          <:
         poorIdf <&> parentheses(commaListOf exprA)<?>
          <:
         parentheses(exprA).

%------------------------------------------------------------------------------
termB(W):-
 W :->    
        (factA <&>>
         ((symbolA("*")<:symbolA("/")) <&>> factA)<**>)
           <@ shownl.

%------------------------------------------------------------------------------
termC(W):-
 W :->    
        (factA <&>>
         ((symbolA("*")<:symbolA("/")) <&>> factA)<**>)
           <@ chainFoldl.

%------------------------------------------------------------------------------
% lchainedBy(+Parser, +Separator, ?Wrapper)
lchainedBy(P,S,W):-
 W :->
        ( P <&>> (S <&>> P)<*> ) <@ chainFoldl.

chainFoldl( Out>[], Out ).
chainFoldl( InVal>[Op>I|IT], Out ):-
        :-@ [Op,InVal,I,OutVal],
        chainFoldl( OutVal>IT, Out ).

%------------------------------------------------------------------------------
exprD(W):-
 W :->  factD
         lchainedBy 
          (symbolA("*")<:symbolA("/")).

factD(W):-
  W :->  int
          <:
         poorIdf <&> parentheses(commaListOf exprD)<?>
                                        <@ evalFact
          <:
         parentheses(exprD).

%------------------------------------------------------------------------------
% Priklad:
goChain0:-
 s("1*prumer(12,6,3)*(8/10)")+L :-> exprD,
                        docShow(9,[L]).

prumer(A,B,C,P):- P is (A+B+C)/3.

%------------------------------------------------------------------------------
% Priklad:
goChain1:-
 s("10+2-7")+L :->
        natural lchainedBy (symbolA("+")<@const(+) <:
                            symbolA("-")<@const(-)),
                        docShow(10,[L]).

% L = [s([])>5]
% Yes

%------------------------------------------------------------------------------
% rchainedBy(+Parser, +Separator, ?Wrapper)
rchainedBy(P,S,W):-
 W :->
        ((P <&>> S)<*> <&>> P) <@ chainFoldr.

chainFoldr( []>InVal, InVal ).          % init
chainFoldr( [I>Op|IT]>InVal, Out ):-
        chainFoldr( IT>InVal, Out1 ),
        :-@ [Op,I,Out1,Out].            % count value when returning

%------------------------------------------------------------------------------
% Priklad:
goChain2:-
 s("2^^2^^3")+L :->
        natural rchainedBy tokenA("^^"),
                        docShow(11,[L]).

% L = [s([])>256]
% Yes

%------------------------------------------------------------------------------

%       DocIII a:

%        ------------------------------------
%       | viz ../demo/expr/geneze/1chains.pl |
%        ------------------------------------

%        ---------------------------------
%       | viz ../demo/expr/geneze/2gen.pl |
%        ---------------------------------

%------------------------------------------------------------------------------
% docShow(+ExampleNumber,+ResultList)
docShow(Number,L):-
        printf(['Example ',Number,':',nl]),
        docShow(L).

docShow([H|T]):-
        printf([' ',H,nl]),
        docShow(T).
docShow([]).

%------------------------------------------------------------------------------

examples:-
        goOperator1,
        (goOperator2 ; (write('This example is designed to fail!'),nl)),
        goOperator3,
        goTrim1, goTrim2, goTrim3,
        goFact1, goFact2,
        goChain1, goChain2.

%- EOF ------------------------------------------------------------------------
