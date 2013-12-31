%------------------------------------------------------------------------------
%
%                          Knihovna kombinatoru parseru
%
%                Priklady z textu prace: Kapitola 1 - Kapitola 3
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Poznamky:
% - prosim konzultujte tento soubor pomoci ../docI.pl
% - konvence ve zdrojovych souborech lze nalezt v ../pc/README.conventions
%------------------------------------------------------------------------------
% Prehled prikladu:
go:-
        printf(['Available examples:',nl]),
        listing(examples).

:- nl,write(' * Type go/0 for available examples *'),nl,nl.
%------------------------------------------------------------------------------
%                             "Curryfikace"
%------------------------------------------------------------------------------
% :-@ [+Term | +ListOfAdditionalArguments]

%        -----------------
%       | viz ../pc/hop/* |
%        -----------------

%------------------------------------------------------------------------------
% Priklad:
goCall:-
        G=.. [mod,23,7,X],
        call(G),
                                docShow(1,[G,X]).

%------------------------------------------------------------------------------
% Priklad:
goCurry1:-
        :-@ [append,[1,3,5],R],
                                docShow(2,[R]).
% R= append([1,3,5])
% Yes

%------------------------------------------------------------------------------
% Priklad:
goCurry2:-
        :-@ [append,[1,3,5],T],
        :-@ [T,[7,9],R],
                                docShow(3,[T,R]).
% T= append([1,3,5])
% R= [1,3,5,7,9]
% Yes
        
%------------------------------------------------------------------------------
%                      "Aplikace z funkcionalniho sveta"
%------------------------------------------------------------------------------

%        --------------------------
%       | viz ../pc/library/fun.pl |
%        --------------------------

%------------------------------------------------------------------------------
% Priklad:
goMap:-
        mapList(append("Surname: "),["Kowalski","Warren"],R),
                                docShow(4,[R]).

% R= ["Surname: Kowalski","Surname: Warren"]
% Yes

%------------------------------------------------------------------------------
% Priklad:
goFilter:-
        filter(isOdd,[1,2,3],R),
                                docShow(5,[R]).
% R= [1,3]
% Yes

%------------------------------------------------------------------------------
% Priklad:
goZip:-
        zip(+,[1,2,3],[4,5,6],R),
                                docShow(6,[R]).
% R= [5,7,9]
% Yes

%------------------------------------------------------------------------------
% Priklad:
goFun1:-
        :-@ [mapList(filter(isOdd) =-> zip(*,[1,2,3])),
        [[5,6,7],[8,9]],
        R],
                                docShow(7,[R]).
% R= [[5, 21], []]
% Yes.

%------------------------------------------------------------------------------
% Priklad:
goFun2:-
        :-@ [mapList(+) =-> foldR(append,[]),[[0,1],[2]],R],
                                docShow(8,[R]).
% R= [+(0), +(1), +(2)]
% Yes

%------------------------------------------------------------------------------
% Priklad:
goFun3:-
        foldR(append =-> mapList(+(3)),[],[[3],[5,6]],R),
                                docShow(9,[R]).

% R= [6, 8, 9] 
% Yes

%------------------------------------------------------------------------------
%                            "Reprezentace n-tic"
%------------------------------------------------------------------------------
% *>*(+FunN, +FunR, +Input, +NewTuple)
*>*(FunN,FunR,I,(N>R)):-
    :-@[FunN,I,N],
    :-@[FunR,I,R].

%------------------------------------------------------------------------------
% fstTuple(+Tuple, -N)
fstTuple(N>_,N).

%------------------------------------------------------------------------------
% sndTuple(+Tuple, -R)
sndTuple(_>R,R).

%------------------------------------------------------------------------------
% tuple2List(?ResultTuple, ?List)
tuple2List((R1>R2),[R1|R2]).

%------------------------------------------------------------------------------
% fstTupleEmpty(?Tuple)
fstTupleEmpty([]>_).

%------------------------------------------------------------------------------
% fstTupleSEmpty(?Tuple)
fstTupleSEmpty(s([])>_).

%------------------------------------------------------------------------------
% Priklad:
goTuple:-
        mapList(fstTuple *>* (const(mod) *>* sndTuple),["x">"y","y">"z"],R),
                                docShow(10,[R]).
        
% R= ["x"> (mod>"y"), "y"> (mod>"z")]
% Yes
%------------------------------------------------------------------------------
%                            "Primitivni parsery"
%------------------------------------------------------------------------------

%        -------------------------
%       | viz ../pc/library/is.pl |
%        -------------------------

%------------------------------------------------------------------------------
% epsilon(+Input + -LOS)
epsilon(I+[I>[]]).

%------------------------------------------------------------------------------
% return(+ReturnValue, +Input + -LOS)
return(V,I+[I>V]).

%------------------------------------------------------------------------------
% epsilon1(+Input + -LOS)
epsilon1(W):-
        return([],W).

%------------------------------------------------------------------------------
% terminate(+Input + -LOS)
terminate(_+[]).

%------------------------------------------------------------------------------
% item(+Input + -LOS)
item(s([S|Is])+[s(Is)>S]).
item(s([])+[]).

%------------------------------------------------------------------------------
% fulfil(+Condition, +Input + -LOS)
fulfil(C,I+L):-
        item(I+Li) -> (Li=[N>R],:-@ [C,[R]] -> return(R,N+L)
                                            ;  terminate(I+L)).

%------------------------------------------------------------------------------
% symbol(+Symbol, +Input + -LOS)
symbol(S,W):-
       fulfil(==(S),W).

%------------------------------------------------------------------------------
% lower(+Input + -LOS)
lower(W):-
       fulfil(isLwrChar,W).

%------------------------------------------------------------------------------
% lower(+Input + -LOS)
upper(W):-
       fulfil(isUprChar,W).

%------------------------------------------------------------------------------
% symbols(+Symbols, +Input + -LOS)
symbols(A,I+L):-
        item(I+Li) -> (Li=[N>R],member(R,A) -> return(R,N+L)
                                            ;  terminate(I+L)).

%------------------------------------------------------------------------------
% whiteSpace(+Input + -LOS)
whiteSpace(W):-
        symbols([32,9,13,10],W).

%------------------------------------------------------------------------------
%                           "Kombinatory parseru"
%------------------------------------------------------------------------------
% <&>>(+Parser1, +Parser2, ?Wrapper)
<&>>(P1,P2,I+L):-
        I+L1 :-> P1,
        <&>>^(P2,L1,L-[]).

% <&>>^(Parser2, LosOfP1, ComposedLosOfP1P2)
<&>>^(_,[],D-D).
<&>>^(P2,[N>R|L1s],L12-D):-
    N+L2 :-> P2,
    mapListDL(fstTuple *>* (const(R) *>* sndTuple),L2,L12-D_),
    <&>>^(P2,L1s,D_-D).

%------------------------------------------------------------------------------
% Priklad:
goComb1:-
        <&>>(symbol("="),symbol(":") <&>> symbol("="),s("=:=")+L),
                                docShow(11,[L]).

% L= [s([])> (O'=> (O':>O'=))]
% Yes

%------------------------------------------------------------------------------
% Priklad:
goComb2:-
 s("=:=")+L :-> (symbol("=") <&>> symbol(":") <&>> symbol("=")),
                                docShow(12,[L]).

% L= [s([])> (O'=> (O':>O'=))]
% Yes

%------------------------------------------------------------------------------
% <:>(+Parser1, +Parser2, ?Wrapper)
<:>(P1,P2,I+L):-
    I+L1 :-> P1,
    I+L2 :-> P2,
    append(L1,L2,L).

%------------------------------------------------------------------------------
% letter1(?Wrapper)
letter1(W):-
 W :-> 
       (lower <:> upper).

%------------------------------------------------------------------------------
% digit1(+Input + -LOS)
digit1(W):-
       fulfil(isDigit,W).

%------------------------------------------------------------------------------
% nat1(?Wrapper)
nat1(W):-
 W :->
        (digit1 <&>> nat1
          <:>
	 epsilon).

%------------------------------------------------------------------------------
% Priklad:
goComb3:-
        s("256")+L :-> nat1,
                                docShow(13,[L]).

% L= [s("")> (O'2> (O'5> (O'6>[]))),
% |   s("6")> (O'2> (O'5>[])),
% |   s("56")> (O'2>[]),
% |   s("256")>[]]
% Yes

%------------------------------------------------------------------------------
%                          "Mutatory parseru"
%------------------------------------------------------------------------------
% <>(+Parser, ?Wrapper)
<>(P,I+L):-
 I+L_ :->
        P,
 ( L_==[] -> L=[] ; L_=[L__|_],L=[L__]).
 
%------------------------------------------------------------------------------
% whole(+Parser, ?Wrapper)
whole(P,I+L):-
 I+Lt :->
        P,
 filter(fstTupleSEmpty,Lt,L).

%------------------------------------------------------------------------------
% <@(+Parser, +Function, ?Wrapper)
<@(P,F,I+L):-
    I+L_ :-> P,
    mapList(fstTuple *>* (F =-> sndTuple),L_,L).

%------------------------------------------------------------------------------
% digit(+Input + -LOS)
digit(W):-
 W :->
        (fulfil(isDigit) <@ ascii2Atom).

%------------------------------------------------------------------------------
% point2D1(?Wrapper)
point2D1(W):-
 W :->
        (symbol("[")
          <&>>
         nat1 <&>> symbol(",") <&>> nat1
          <&>>
         symbol("]")).

%------------------------------------------------------------------------------
% <&(+Parser1, +Parser2, ?Wrapper)
<&(P1,P2,W):-
 W :->
        (P1 <&>> P2 <@ fstTuple).

%------------------------------------------------------------------------------
% &>(+Parser1, +Parser2, ?Wrapper)
&>(P1,P2,W):-
 W :->
        (P1 <&>> P2 <@ sndTuple).

%------------------------------------------------------------------------------
% enclosedIn(+InnardsParser, +Openning and +Closing, ?Wrapper)
enclosedIn(P,SO and SC,W):-
 W :->
        (SO &> P <& SC).

%------------------------------------------------------------------------------
% parentheses(+Innards, ?Wrapper)
parentheses(P,W):-
        W :-> P enclosedIn symbol("(") and symbol(")").        

%------------------------------------------------------------------------------
% brackets(+Innards, ?Wrapper)
brackets(P,W):-
        W :-> P enclosedIn symbol("[") and symbol("]").        

%------------------------------------------------------------------------------
% brace(+Innards, ?Wrapper)
brace(P,W):-
        W :-> P enclosedIn symbol("{") and symbol("}").        

%------------------------------------------------------------------------------
% prologEnvelope(+Innards, ?Wrapper)
prologEnvelope(P,W):-
        W :-> P enclosedIn (symbol(":") &> symbol("-"))
                             and
                            symbol(".").

%------------------------------------------------------------------------------
% token(+String, ?Wrapper)
token([H|T],W):-
 W :->
       (symbol([H]) <&>> token(T) <@ tuple2List).
token([],W):-
 W :->
       return([]).

%------------------------------------------------------------------------------
% <&>(+Parser1, +Parser2, ?Wraper)
<&>(P1,P2,W):-
 W :->
        (P1 <&>> P2 <@ tuple2List).

%------------------------------------------------------------------------------
% nat(?Wrapper)
nat(W):-
 W :->
        ( digit <&> nat <:> epsilon <> ).
        
% natural2(?Wrapper)
natural2(W):-
 W :->
        (digit <&> nat <@ foldL(evalNatural,0)).

evalNatural(Acc,I,Result):-
        Result is Acc*10 + I.

%------------------------------------------------------------------------------
% point2D(?Wrapper)
point2D(W):-
 W :->
        brackets(natural2 <&>> symbol(",") &> natural2).

%------------------------------------------------------------------------------
% Priklad:
goMut:-
 s("[12,36]")+L :-> point2D,
                                docShow(14,[L]).

% L= [s([])> (12>36)]
% Yes

%------------------------------------------------------------------------------
%                             "Generatory parseru"
%------------------------------------------------------------------------------
% convoy(+Binder, +ListOfParsers, -Convoy)
convoy(_,[P],P).
convoy(B,[H|T],Convoy):-
	Convoy=..[B,H,TConvoy],
	convoy(B,T,TConvoy).
convoy(_,[],terminate).

%------------------------------------------------------------------------------
% convoy(+Binder, +Terminator, +ListOfParsers, -Convoy)
convoy(B,T,Ps,C):-
        foldR(convoy_(B),T,Ps,C).
convoy_(B,P1,P2,P):- P=..[B,P1,P2].

%------------------------------------------------------------------------------
% sequence(+ListOfParsers, -Convoy)
sequence(Ps,C):-
    convoy(<&>,Ps,C).

%------------------------------------------------------------------------------
% sequence(+ListOfParsers, +Terminator, -Convoy)
sequence(Ps,T,C):-
    convoy(<&>,T,Ps,C).

%------------------------------------------------------------------------------
% choice(+ListOfParsers, -Convoy)
choice(Ps,C):-
    convoy(<:>,Ps,C).

%------------------------------------------------------------------------------
% choice(+ListOfParsers, +Terminator, -Convoy)
choice(Ps,T,C):-
    convoy(<:>,T,Ps,C).

%------------------------------------------------------------------------------
% Priklad:
% - generator parseru BNF pravidla

terminal(W):-
 W :->  (fulfil(isDigitChar_) <&> terminal
          <:>
         fulfil(isDigitChar_) <&> epsilon).

nonTerminal(W):-
 W :->  (angled(terminal) <@ mkNonTerminal).
        

rule(W):-
 W :->  ((whole nonTerminal <&>>
          (token(" ::= ") &> (ruleBody <@ choice)) <& symbol(".")
         ) <@ mkRule).

ruleBody(W):-
 W :->  (((ruleAlt <@ sequence) <& symbol("|")) <&> ruleBody
          <:>
         ruleAlt <&> epsilon <@ sequence).

ruleAlt(W):-
 W :->
        (minals <&> (symbol(" ") &> ruleAlt)
          <:>
         minals <&> epsilon
          <:>
         symbol(" ") <@ mkEpsilon).

minals(W):-
 W :-> ((terminal<@ mkTerminal)<:>nonTerminal).
        

% Pomocne predikaty
mkEpsilon(_,[epsilon]).
mkRule(H>T,:-(H,T)).
mkTerminal(S,token(S)).
mkNonTerminal(S,A):-
        name(A,S).
angled(P,W):-
        W :-> P enclosedIn (symbol("<")) and (symbol(">")).

%------------------------------------------------------------------------------
% Priklad:
goGenerator:-
        s("<block> ::= begin <block> end| .")+L :->
                rule,
                                docShow(15,[L]).
% L = [s([])> (block:-token([98, 101, 103, 105, 110])<&>block<&>token([101, 110, 100])<:>epsilon)]
% Yes

%------------------------------------------------------------------------------
% keywords(+ListOfKeywords,-Parser)
keywords(Strings,P):-
        mapList(tokenS,Strings,Ps),
        choice(Ps,P).

tokenS(S,token(S)).

%------------------------------------------------------------------------------
%                     "Transformace syntaktickeho stromu"
%------------------------------------------------------------------------------
% symbolA(+Symbol, +Input + -LOS)
symbolA(S,W):-
 W :->
	(fulfil(==(S)) <@ ascii2Atom).

%------------------------------------------------------------------------------
% tokenA(+String, ?Wrapper)
tokenA(String,W):-
 W :->
	(token(String) <@ string2Atom).

%------------------------------------------------------------------------------

%        -----------------------------
%       | viz ../pc/library/colone.pl |
%        -----------------------------

%------------------------------------------------------------------------------
% Priklad:
goColone:-
 s("los")+L :->
        (tokenA("los") <@ fun(a1,aN) ->> b1 ->> b2 ->> bN),
                                docShow(16,[L]).

%------------------------------------------------------------------------------
% Priklad:
goColone1:-
 s("los")+L :->
        (tokenA("los") <@ fun(a1,aN) ->> b1 ->> b2 => fun(c1,cN) ->> d1 ->> dN),
                                docShow(17,[L]).

%------------------------------------------------------------------------------
% Priklad:
goColone2:-
 s("5")+L :->
        (natural <@ *(3) => / ->> 15 => *(30)),
                                docShow(18,[L]).

% L= [s([])>30]
% Yes

%------------------------------------------------------------------------------
% Priklad:
goColone3:-
 s("los")+L :->
        (tokenA("los") <@ fun1 => fun2(a,b) ->> c ->> d ->> e => fun3(g)),
                                docShow(19,[L]).

% L= [s([])>fun3(g, fun2(a, b, fun1(los), c, d, e))]
% Yes

%------------------------------------------------------------------------------
%                            "Jednoducha iterace"
%------------------------------------------------------------------------------
% prologVar1(?Wrapper)
prologVar1(W):-
 W :->
        ((upper <:> symbol("_")) <&> prologVarBody).

prologVarBody(W):-
 W :->  (fulfil(isDigitChar_) <&> prologVarBody
          <:>
         epsilon).

%------------------------------------------------------------------------------
% <**>(+Parser, ?Wrapper)
<**>(P,W):-
 W :->
        (P <&> P<**>
          <:>
         epsilon).

%------------------------------------------------------------------------------
% <<*>>(+Parser, ?Wrapper)
<<*>>(P,W):-
 W :->
        (P <&> P<<*>>
          <:>
         epsilon <>).

%------------------------------------------------------------------------------
% prologVar(?Wrapper)
prologVar(W):-
 W :->
        ((upper <:> symbol("_")) <&> fulfil(isDigitChar_)<<*>>).
        
%------------------------------------------------------------------------------
% <*@*>(+Parser,+Bottom-Function,?Wrapper)
<*@*>(P,B-F,W):-
 W :->
        (P <&>> P<*@*>B-F <@ F
          <:>
         B).

%------------------------------------------------------------------------------
% <**>>(+Parser,+Bottom-Function,?Wrapper)
<**>>(P,W):-
 W :->
        (P <*@*>return(nil)-id).

%------------------------------------------------------------------------------
% <**>^(+Parser,+Bottom-Function,?Wrapper)
<**>^(P,W):-
 W :->
        (P <*@*>epsilon-tuple2List).

%------------------------------------------------------------------------------
% <*@*>(+Parser,+Bottom-Function,?Wrapper)
<<*@>>(P,B-F,W):-
 W :->
        (P <&>> P<<*@>>B-F <@ F
          <:>
         B <>).

%------------------------------------------------------------------------------
% #>(+Parser,?Wrapper)
#>(P,W):-
 W :->
        ( whiteSpace<<*@>>return(whiteSpace)-sndTuple &> P ).

% #>(+Parser,?Wrapper)
<#(P,W):-
 W :->
        ( P <& whiteSpace<<*@>>return(whiteSpace)-sndTuple ).

%------------------------------------------------------------------------------
% Priklad:
goSimpleIter1:-
 s("[ _flippedList  ]")+L :->
        brackets(#> prologVar <#),
                                L=[N>R],name(A,R),docShow(20,[[N>A]]).

% L= [s("")>"_flippedList"]
% Yes

%------------------------------------------------------------------------------
% nonSymbol(+Symbol, ?Wrapper)
nonSymbol(S,W):-
       fulfil(\==(S),W).

%------------------------------------------------------------------------------
% nonSymbols(+Symbols, ?Wrapper)
nonSymbols(A,I+L):-
        item(I+[N>R]),
        ( member([R],A) -> terminate(I+L)
                        ;  return(R,N+L) ).

%------------------------------------------------------------------------------
% lineComment(+String,?Wrapper)
lineComment(S,W):-
 W :->
        (token(S)
          &>
           nonSymbol([10])<<*@>>return(comment)-sndTuple).

%------------------------------------------------------------------------------
% *LComment(?Wrapper).
prologLComment(W):-
        W :-> lineComment("%").
haskellLComment(W):-
        W :-> lineComment("--").
makeLComment(W):-
        W :-> lineComment("#").
        
%------------------------------------------------------------------------------
% Priklad:
goSimpleIter2:-
 mAppend(["% this is comment",[10]," t(1)."],I),
 s(I)+L :->
        prologLComment,
                                docShow(21,[L]).

% L= [s([10|" t(1)."])>comment]
% Yes
        
%------------------------------------------------------------------------------
% <++>(+Parser, ?Wrapper)
<++>(P,W):-
 W :->
        (P <&> P<**>).

%------------------------------------------------------------------------------
% <<+>>(+Parser, ?Wrapper)
<<+>>(P,W):-
 W :->
        (P <&> P<<*>>).

%------------------------------------------------------------------------------
% natural(?Wrapper)
natural(W):-
 W :->
        (digit<<+>> <@ foldL(evalNatural,0)).

%------------------------------------------------------------------------------
% <?@?>(+Parser, +No - +Yes, ?Wrapper)
<?@?>(P,No-Yes,W):-
 W :->
        ( P <@ Yes
           <:>
          return(No)).

%------------------------------------------------------------------------------
% <??>(+Parser, ?Wrapper)
<??>(P,W):-
 W :->
        ( P
           <:>
          epsilon).

%------------------------------------------------------------------------------
% <<?@>>(+Parser, +No - +Yes, ?Wrapper)
<<?@>>(P,No-Yes,W):-
 W :->
        ( P <@ Yes
           <:>
          return(No) <>).

%------------------------------------------------------------------------------
% int(?Wrapper)
int(W):-
 W :->
        ( symbol("-")<?@?>1-const(-1)
           <&>>
          natural <@ alter(Sig>Val,Sig*Val) => eval).

%------------------------------------------------------------------------------
% fractionalPart(?Wrapper)
fractionalPart(W):-
 W :->
        ( digit<<+>> <@ foldR(evalFract,0.0) ).

evalFract(I,Acc,Result):-
        Result is (I+Acc)/10.

%------------------------------------------------------------------------------
% double(?Wrapper)
double(W):-
 W :->
     ((int
        <&>>
       (symbol(".") &> fractionalPart)<<?@>>0.0-id
        <&>>
       (symbols("eE") &> int)<<?@>>0.0-id) <@ evalDouble).

evalDouble(I>(F>E),R):-
        I<0 -> R is (I+F*(-1))*10^E ; R is (I+F)*10^E.

%------------------------------------------------------------------------------
% Priklad:
goSimpleIter3:-
 s("-1.2e-3")+L :->
        double,
                docShow(22,[L]).

% L= [s([])> -0.0012]
% Yes

%------------------------------------------------------------------------------
%                           "Iterace se separatory"
%------------------------------------------------------------------------------
% prologHead(?Wrapper)
prologHead(W):-
 W :->
       (prologIdf
         <&>>
        parentheses( prologTerm <&> (symbol(",")<&>> prologTerm)<<*>> )
           <?@?>noArg-id).

%------------------------------------------------------------------------------
% separatedBy(+Parser, +Separator, ?Wrapper)
separatedBy(P,S,W):-
 W :->
        (P <&> ( S &> P )<<*>>).

%------------------------------------------------------------------------------
% commaListOf(?Wrapper)
commaListOf(P,W):-
 W :->
        P separatedBy symbol(",").

%------------------------------------------------------------------------------
% semicolonListOf(?Wrapper)
semicolonListOf(P,W):-
 W :->
        P separatedBy symbol(";").

%------------------------------------------------------------------------------
% separated0By(+Parser, +Separator, ?Wrapper)
separated0By(P,S,W):-
 W :->
        (P separatedBy S
          <:>
         epsilon).

%------------------------------------------------------------------------------
% determinant(?Wrapper)
determinant(W):-
 W :->
        (   commaListOf #>double<#
           separatedBy
            (symbol("|") <&> #>symbol("|"))
          enclosedIn
           symbol("|") and symbol("|")
        ).
        
%------------------------------------------------------------------------------
% Priklad:
goSepIter:-
 mAppend(["| 1, -5, 7e-3 |",[10],
          "| 13, 6,    0 |",[10],
          "| -9, 3,  1.5 |"],I),
 s(I)+L :-> determinant,
                docShow(23,[L]).

% L= [s([])>[[1, -5, 0.007], [13, 6, 0], [-9, 3, 1.5]]]
% Yes

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
        goCall, goCurry1, goCurry2,
        goMap, goFilter, goZip,
        goFun1, goFun2, goFun3,
        goTuple,
        goComb1, goComb2, goComb3,
        goMut,
        goGenerator,
        goColone, goColone1, goColone2, goColone3,
        goSimpleIter1, goSimpleIter2, goSimpleIter3,
        goSepIter.

%- EOF ------------------------------------------------------------------------
