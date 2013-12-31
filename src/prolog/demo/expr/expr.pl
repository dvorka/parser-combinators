%------------------------------------------------------------------------------
%
%                            (Aritmeticke) vyrazy
%
%			        Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Z�kladn� verze parseru v�raz�.
*/
%------------------------------------------------------------------------------
% Inicializace & knihovny

:- ['../../loadSWI'].          % SWI Prolog
%:- ['../../loadBP-2'].        % BinProlog

%------------------------------------------------------------------------------
%                                   Code
%------------------------------------------------------------------------------
/**     expression(+Operators,+Evaluator,?Wrapper)
Text:   Parser v�raz� obsahuj�c�ch un�rn� i bin�rn� oper�tory s libovoln�m
        po�tem priorit. Z u�ivatelsk�ho z�pisu je vygenerov�n parser,
        kter� je n�sledn� pou�it pro rozklad vstupn�ho textu. D�le
        je pro zpracov�n� v dob� rozkladu specifikov�n vyhodnocova�.
Arg:    Operators:
        V��et p��pustn�ch oper�tor� je seznam seznam� definic jednotliv�ch
        oper�tor�. Definice jednoho oper�toru je ulo�ena ve struktu�e,
        jej� funktor ud�v� typ asociativity (xf, yf, fx, fy, xfx, xfy,
        yfx, yfy), prvn� argument je token ve vstupn�m textu a druh�
        parametr funktor fce, kter� se  m� pou��t p�i vyhodnocov�n�.
        Definice oper�tor� o stejn� precedenci jsou sdru�eny v jednom seznamu.
        Seznamy oper�tor� o stejn� precedenci jsou kone�n� ulo�eny v
        celkov�m v��tu, zde jsou se�azeny od nejvy��� precedence k nejni���.
p       Nap��klad:
v               [[yfx("+",'+'),yfx("-",'-')],
v                [yfx("*",'*'),yfx("/",'/'),yfx("//",'//')],
v                [fx("!",'!')]]
Arg:    Evaluator
        Jm�no predik�tu, kter� m� prov�d�t vyhodnocov�n�.
Example:
        ?- expression([[yfx("+",'+'),yfx("-",'-')],
        |              [yfx("*",'*'),yfx("/",'/')],
        |              [xfy("^",'^')]],
        |              arieval,
        |              quickS("3^2^2+2*3^6-7/8*9")+L).
*/

expression(Operators,Evaluator,Wrapper):-
        op2Env(Operators,OpEnvironment),
        deBug(expr,[nl,'Environment: ',nl,OpEnvironment,nl]),
        expr(OpEnvironment,Evaluator,Wrapper).

expr(OpEnvironment,Evaluator,W):-
        foldR( exprFold(Evaluator),
               fact(OpEnvironment,Evaluator),
               OpEnvironment,
               Parser),
        deBug(expr,[nl,'Parser: ',nl,Parser,nl]),
        W :->
                Parser.

exprFold(E,I,Acc,level(E,I,Acc)).

%------------------------------------------------------------------------------
/** fact(?Wrapper)
*/
% - objekt: cislo, identifikator, funkce nebo (...)
fact(O,E,W):-
  W :-> (
         int
          <:         % !!!
         (poorIdf <&> parentheses(commaListOf expr(O,E))<?>) <@ evalFact(E)
          <:         % !!!
         parentheses(expr(O,E))).              % put it into nest

%------------------------------------------------------------------------------

xfxB(CP,W):- 
 CP=level(E,[_,_,FY,YF,XFX,_,_,_],LP),
 W :->
       (FY<**> <&>>
         (LP <&>> XFX <&>> LP) <&>>
        YF<**>)                               <@ evalXfx(E).
        
yfyB(CP,W):-
 CP=level(E,[_,_,FY,YF,_,YFY,_,_],LP),
 W :-> (
        (((FY<**>
            <&>>
          ((xfxB(CP) <:> yfxB(CP) <:> xfyB(CP) <:> LP) <&>> YFY))<+>
            <&>>
          (xfxB(CP) <:> xfyB(CP) <:> yfxB(CP) <:> unary(CP))) <@ evalXfy(E))
        <:>
        (((xfxB(CP) <:> yfxB(CP) <:> xfyB(CP) <:> unary(CP))
          <&>>
         ((YFY <&>> (xfxB(CP) <:> xfyB(CP) <:> yfxB(CP) <:> LP))
           <&>> YF<**>) <+>)                                  <@ evalYfx(E))
       ).

xfyB(CP,W):-
 CP=level(E,[_,_,FY,_,_,_,XFY,_],LP),
 W :-> (
        (FY<**> <&>> (LP<&>>XFY))<+>
         <&>>
        (yfxB(CP) <:> xfxB(CP) <:> yfyB(CP) <:> unary(CP))
       ) <@ evalXfy(E).

yfxB(CP,W):-
 CP=level(E,[_,_,_,YF,_,_,_,YFX],LP),
 W :-> (
        (xfyB(CP) <:> xfxB(CP) <:> unary(CP))
         <&>>
        ((YFX<&>>LP) <&>> YF<**>)<+>
        ) <@ evalYfx(E).

%------------------------------------------------------------------------------
%                             Unarni operatory
%------------------------------------------------------------------------------

unary(CP,W):-
 CP=level(E,[FX,XF,FY,YF,_,_,_,_],LP),
 W :->(
           FY<**> <&>>
             (FX<?> <&>> LP <@ evalUnaryFX(E)
               <:>
              LP <&>> XF<?> <@ evalUnaryXF(E)) <&>>
           YF<**>                    <@ evalUnary(E)).

%------------------------------------------------------------------------------
%                          Jedna prioritni uroven
%------------------------------------------------------------------------------

level(E,O,LP,W):-
 W :->
       (yfyB(level(E,O,LP)) <:> xfyB(level(E,O,LP)) <:>
        yfxB(level(E,O,LP)) <:> xfxB(level(E,O,LP)) <:>
        unary(level(E,O,LP))).

%------------------------------------------------------------------------------
%                               Evaluatory
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
        F=[] -> X=R ; T=..[F,X], :-@ [Eval,T,R].
evalUnaryXF(Eval,X>F,R):-
        F=[] -> X=R ; T=..[F,X], :-@ [Eval,T,R].

%------------------------------------------------------------------------------
%                               Extra evaluator
%------------------------------------------------------------------------------
/**     arieval(+Expression, -Result)
Text:   Vyhodnocova� aritmetick�ch v�raz�.
*/

arieval(X,o):-
        integer(X).

% standard functions
arieval(+(X,Y),R):- R is X+Y.
arieval(-(X,Y),R):- R is X-Y.
arieval(*(X,Y),R):- R is X*Y.
arieval(/(X,Y),R):- R is X/Y.
arieval(//(X,Y),R):- R is X//Y.
arieval(mod(X,Y),R):- R is X mod Y.
arieval(^(X,Y),R):- ^^(X,Y,R).
arieval(++(X),R):- inc(X,R).
arieval(--(X),R):- dec(X,R).

% vlastni funkce
arieval(!(X),R):- dec(X,R).
arieval(?(X),R):- dec(X,R).

arieval(X,X).

%------------------------------------------------------------------------------
%                       Operator environment generator
%------------------------------------------------------------------------------
/**     op2Env(+Operators, -Environment)
Text:   Generuje z u�ivatelsk�ho v��tu oper�tor�, tak jak byl pops�n
        v expression/3 environment, kter� je n�sledn� pou��v�n k rozkladu.
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
                     ; Po= (<:>(token(S)<@const(V),P)).

%- EOF ------------------------------------------------------------------------
