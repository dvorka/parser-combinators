%------------------------------------------------------------------------------
%
%            Vyrazy se zkracenym vyhodnocenim a pouzitim databaze
%
%			        Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Verze parseru výrazù s pou¾itím databáze.
*/
%------------------------------------------------------------------------------
% Inicializace & knihovny

:- ['../../loadSWI'].          % SWI Prolog
%:- ['../../loadBP-2'].        % BinProlog

%------------------------------------------------------------------------------
%                                   Demo
%------------------------------------------------------------------------------
% Ukazky:
%       1. parser a vyhodnocovac aritmetickych vyrazu: goExpr/0
%       2. parser podmnoziny jazyka Prolog: goProlog/0
%------------------------------------------------------------------------------
%                       1. demo: aritmeticke vyrazy
%------------------------------------------------------------------------------

goExpr:-
        % Arguments:
        Evaluator=arieval,    % (arieval,id)

        Expr=   "++max(4,3--^5)"
                %"prumer(1,3^2)^2+2*3^6-7/8*9"
                %"++max(4,3--^5)"
                %"3^2^2+2*3^6-7/8*9"
                %"++10+3--+5"
                %"++!1"
                ,

        Parser=
        expression([
                    [yfx("+",'+'),yfx("-",'-')],
                    [yfx("*",'*'),yfx("/",'/'),yfx("//",'//')],
                    [xfx("mod",'mod')],
                    [xfy("^",'^')],
                    [fx("++",'++'),xf("--",'--')],
                    [fx("!",'!')]
                   ],
                   factAri(Evaluator)),

        % Invoke:
        quickS(Expr)+L :-> Parser,

        printf([nl,'LOS: ',L,nl]).


go:-
        printf(['Call goExpr/0 or goProlog/0.',nl]).
%------------------------------------------------------------------------------
% factAri(?Wrapper)
% - cislo, identifikator, funkce nebo uzavorkovany vyraz
factAri(E,W):-
  W :-> (
         int
          <: 
         (poorIdf <&> parentheses(commaListOf expr)<?>) <@ evalFact(E)
          <: 
         parentheses(expr)).

%------------------------------------------------------------------------------
/**     arieval(+Expression, -Result)
Text:   Vyhodnocovaè aritmetických výrazù.
*/

arieval(X,X):- integer(X).
arieval(+(X,Y),R):- R is X+Y.
arieval(-(X,Y),R):- R is X-Y.
arieval(*(X,Y),R):- R is X*Y.
arieval(/(X,Y),R):- R is X/Y.
arieval(//(X,Y),R):- R is X//Y.
arieval(mod(X,Y),R):- R is X mod Y.
arieval(^(X,Y),R):- ^^(X,Y,R).
arieval(++(X),R):- inc(X,R).
arieval(--(X),R):- dec(X,R).
arieval(!(X),R):- X=0 -> R=1 ; R=0.
arieval(prumer(X,Y),R):- R is (X+Y)/2.
arieval(max(X,Y),R):- X>=Y -> R=X ; R=Y.
arieval(min(X,Y),R):- X=<Y -> R=X ; R=Y.
arieval(X,X).

%------------------------------------------------------------------------------
%                 2. demo: parser podmnoziny jazyka Prolog
%------------------------------------------------------------------------------

goProlog:-
        expression([[fx(":-",':-'),fx("?-",'?-')],
                    [xfx(":-",':-')],
                    [xfy(";",';')],
                    [xfy("->",'->')],
                    [xfy(",",',')],
                    [fy("not",'not')]
                   ],
                   factProlog(id),
                   quickS("a(b,c):-d->f,g;h")+L),
                   (L=[_>R] -> nl,write('LOS: '),display(R) ;
                               nl,write('LOS empty')),nl.

%------------------------------------------------------------------------------
% factProlog(?Wrapper)
factProlog(E,W):-
  W :-> (
         (prologIdf <&> parentheses(commaListOf expr)<?>) <@ evalFact(E)
          <: 
         parentheses(expr)).

%------------------------------------------------------------------------------
% |                                                                          |
% |    Parser vyrazu s libovolnym poctem urovni precedence operatoru typu:   |
% |                       xf fx yf fy xfx xfy yfx yfy                        |
% |                                                                          |
%------------------------------------------------------------------------------
/**     expression(+Operators,+FactAndEvaluator,?Wrapper)
Text:   Parser výrazù obsahujících unární i binární operátory s libovolným
        poètem priorit. Z u¾ivatelského zápisu je vygenerován parser,
        který je následnì pou¾it pro rozklad vstupního textu. Dále
        je pro zpracování v dobì rozkladu specifikován vyhodnocovaè.
        Pøi rozkladu jsou jednak vyu¾ívány kombinátory alternativní kompozice
        s èásteèným vyhodnocením a dále ukládání do databáze.
Arg:    Operators:
        Viz exp.pl.
Arg:    FactiAndEvaluator
*/

expression(Operators,FactAndEvaluator,Wrapper):-
        arg(1,FactAndEvaluator,Evaluator),
        op2Env(Operators,OpEnvironment),
        foldR( exprFold(Evaluator),
               FactAndEvaluator,
               OpEnvironment,
               Parser),
        deBug(expr,[nl,'Parser: ',Parser,nl]),
        asserta(pcExpression(Parser)),
         expr(Wrapper),                                 % parser is global
        retract(pcExpression(Parser)).

expr(W):-
        pcExpression(Parser),
        W :->
                Parser.

exprFold(E,I,Acc,level(E,I,Acc)).

%------------------------------------------------------------------------------
%                            Jedna uroven priorit
%------------------------------------------------------------------------------

% jedna prioritni  uroven
level(E,O,LP,W):-
 W :->
       (yfyB(level(E,O,LP)) <: yfxB(level(E,O,LP)) <:
        xfyB(level(E,O,LP)) <: xfxB(level(E,O,LP)) <:
        unary(level(E,O,LP))).

% do nizsich urovni lze proklouznout pomoci unary

%------------------------------------------------------------------------------
%                               Binarni operatory
%------------------------------------------------------------------------------

xfxB(CP,I+L):-
 CP=level(E,[_,_,FY,YF,XFX,_,_,_],LP),
 (XFX=terminate -> L=[] ;
  I+L :->
       (FY<*> <&>>
         (LP <&>> XFX <&>> LP) <&>>
        YF<*>)                                                  <@ evalXfx(E)).
        
yfyB(CP,I+L):-
 CP=level(E,[_,_,FY,YF,_,YFY,_,_],LP),
 (YFY=terminate -> L=[] ;
 I+L :->
        ((yfxB(CP) <: xfyB(CP) <: xfxB(CP) <: unary(CP))
          <&>>
         ((YFY <&>> (yfxB(CP) <: xfyB(CP) <: xfxB(CP) <: LP))
           <&>> YF<*>) <+>)                                    <@ evalYfx(E)).

xfyB(CP,I+L):-
 CP=level(E,[_,_,FY,_,_,_,XFY,_],LP),
 (XFY=terminate -> L=[] ;
 I+L :-> (
        (FY<*> <&>> (LP<&>>XFY))<+>
         <&>>
        (yfyB(CP) <: yfxB(CP) <: xfxB(CP) <: unary(CP))
       )                                                        <@ evalXfy(E)).

yfxB(CP,I+L):-
 CP=level(E,[_,_,_,YF,_,_,_,YFX],LP),
 (YFX=terminate -> L=[] ;
 I+L :-> (
        (xfyB(CP) <: xfxB(CP) <: unary(CP))
         <&>>
        ((YFX<&>>LP) <&>> YF<*>)<+>
        ) <@ evalYfx(E)).

%------------------------------------------------------------------------------
%                             Unarni operatory
%------------------------------------------------------------------------------

% x unary operators
unary(CP,W):-
 CP=level(E,[FX,XF,FY,YF,_,_,_,_],LP),
 ( FX=terminate,XF=terminate,FY=terminate,YF=terminate
  -> W :-> LP
  ;
 W :->(
           FY<*> <&>>
             (FX <&>> LP <@ evalUnaryFX(E)
               <:
              LP <&>> XF<?> <@ evalUnaryXF(E)) <&>>
           YF<*>                    <@ evalUnary(E))).

%------------------------------------------------------------------------------
%                  Semanticke akce pro jednotlive typy operatoru
%------------------------------------------------------------------------------
% fact
evalFact(Eval,L,R):-
        T=..L,
        :-@ [Eval,T,R].

%------------------------------------------------------------------------------
% xfx
evalXfx(Eval,FYL>((N1>(Op>N2))>YFL),R):-
        P=..[Op,N1,N2],
        foldR(sfx,P,FYL,FY),
        foldL(sfxFlip,FY,YFL,T),
        :-@ [Eval,T,R].

%------------------------------------------------------------------------------
% xfy
evalXfy(Eval,List>Ini,R):-
        foldR(xfyFF,Ini,List,T),
        :-@ [Eval,T,R].
xfyFF(FYL>(N>Op),Acc,R):-
        P =..[Op,N,Acc],
        foldR(sfx,P,FYL,R).

%------------------------------------------------------------------------------
% yfx
evalYfx(Eval,AccI>List,R):-
        foldL(eYfx,AccI,List,T),
        :-@ [Eval,T,R].
eYfx(Acc,(Op>N)>YFL,R):-
        P =..[Op,Acc,N],
        foldL(sfxFlip,P,YFL,R).

%------------------------------------------------------------------------------
% unary
evalUnary(Eval,FYL>(P>YFL),R):-   % [[fy1,fy2],P,[yf1,yf2]]
        foldR(sfx,P,FYL,FY),
        foldL(sfxFlip,FY,YFL,T),
        :-@ [Eval,T,R].
evalUnaryFX(Eval,F>X,R):-
        T=..[F,X], :-@ [Eval,T,R].
evalUnaryXF(Eval,X>F,R):-
        F=[] -> X=R ; T=..[F,X], :-@ [Eval,T,R].

%------------------------------------------------------------------------------
%                       Operator environment generator
%------------------------------------------------------------------------------
/**     op2Env(+Operators, -Environment)
Text:   Generuje z u¾ivatelského výètu operátorù, tak jak byl popsán
        v expression/3 environment, který násldnì pou¾ívá k rozkladu.
*/

op2Env([],[]).
op2Env([PrecLevel|T],[EnvLevel|ET]):-
        op2EnvLev(PrecLevel,            % jedna prioritni uroven
                  % fx        xf        fy        yf
                  [terminate,terminate,terminate,terminate,
                  % xfx       yxf       xfy       yfx
                   terminate,terminate,terminate,terminate],
                  EnvLevel),  
        op2Env(T,ET).

op2EnvLev([],E,E).
% operatory: fx xf fy yf xfx yfy xfy yfx
op2EnvLev([fx(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(FX,O,V,FXo), op2EnvLev(T,[FXo,XF,FY,YF,XFX,YFY,XFY,YFX],E).
op2EnvLev([xf(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(XF,O,V,XFo), op2EnvLev(T,[FX,XFo,FY,YF,XFX,YFY,XFY,YFX],E).
op2EnvLev([fy(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(FY,O,V,FYo), op2EnvLev(T,[FX,XF,FYo,YF,XFX,YFY,XFY,YFX],E).
op2EnvLev([yf(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(YF,O,V,YFo), op2EnvLev(T,[FX,XF,FY,YFo,XFX,YFY,XFY,YFX],E).
op2EnvLev([xfx(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(XFX,O,V,XFXo), op2EnvLev(T,[FX,XF,FY,YF,XFXo,YFY,XFY,YFX],E).
op2EnvLev([yfy(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(YFY,O,V,YFYo), op2EnvLev(T,[FX,XF,FY,YF,XFX,YFYo,XFY,YFX],E).
op2EnvLev([xfy(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(XFY,O,V,XFYo), op2EnvLev(T,[FX,XF,FY,YF,XFX,YFY,XFYo,YFX],E).
op2EnvLev([yfx(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(YFX,O,V,YFXo), op2EnvLev(T,[FX,XF,FY,YF,XFX,YFY,XFY,YFXo],E).

op2EnvP(P,S,V,Po):-
        P=terminate -> Po= (token(S)<@const(V))
                     ; Po= (<:(token(S)<@const(V),P)).

%- EOF ------------------------------------------------------------------------
