%------------------------------------------------------------------------------
%
%      Parser vyrazu s libovolnym poctem urovni precedence operatoru typu
%                         xf fx yf fy xfx xfy yfx yfy
%
%			        Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/** Module:
Text:   Parser v�raz� s libovoln�m po�tem �rovn� precedence oper�tor�
        typu: xf, fx, yf, fy, xfx, xfy, yfx a yfy.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/** expression(+OperatorEnvironment, +FactAndEvaluator, ?Wrapper)
Text:   Parser v�raz� obsahuj�c�ch un�rn� i bin�rn� oper�tory s libovoln�m
        po�tem priorit. Z u�ivatelsk�ho z�pisu je vygenerov�n parser,
        kter� je n�sledn� pou�it pro rozklad vstupn�ho textu. D�le
        je pro zpracov�n� v dob� rozkladu specifikov�n vyhodnocova�.
Arg:    Operators:
        V��et p��pustn�ch oper�tor� je seznam seznam� definic jednotliv�ch
        oper�tor�. Definice jednoho oper�toru je ulo�ena ve struktu�e,
        jej� funktor ud�v� typ asociativity (xf, yf, fx, fy, xfx, xfy,
        yfx, yfy), prvn� argument je token ve vstupn�m textu a druh�
        parametr funktor funkce, kter� se  m� pou��t p�i vyhodnocov�n�.
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
% - OperatorEnvironment:
%       [ level1                % operatory s nejnizsi prioritou
%         ...
%         levelN                % operatory s nejvyssi prioritou
%       ]
%
%   level*:
%       je seznam polozek
%               typOperatoru(String,Value)
%       kde je
%           typOperatoru ... xf,fx,yf,fy,xfx,yfx,xfy nebo yfy
%           String       ... prijimany operator
%           Value        ... hodnota vydavana parserem operatoru (typicky
%                            funktor predikatu odpovidajici dane operaci)
%
%   Priklad:
%        [ [yfx("+",add),yfx("-",sub)],         % vyssi precedence
%          [xfy("^",pow)],                      % nizsi precedence
%        ]
%
% - FactAndEvaluator
%       ma tvar         precedence0(Evaluator)
%
%   Priklad:
%       factProlog(id)
%
% - precedence0 
%       Parser operandu tj. elementu s nejnizsi precedenci
%
% - Evaluator
%       Slouzi pro zpracovani analyzovaneho vstupu. Muze to byt napriklad 
%   predikat generujici pseudokod, zjednodusovac nebo vyhodnocovac. Na vstup
%   dostava analyzovany vyraz zapsany ve standardni notaci 
%   (napriklad add(1,5)), v poslednim argumentu je ocekavan vysledek.
%
%   Priklad:
%       id      ... je vydan syntakticky strom (neprovadi se zadne 
%                   transformace)
%       ariexpr ... pripraveny jednoduchy vyhodnocovac aritmetickych vyrazu

expression(Operators,FactAndEvaluator,Wrapper):-
        arg(1,FactAndEvaluator,Evaluator),
        op2Env(Operators,OpEnvironment),
        foldR( exprFold(Evaluator),
               FactAndEvaluator,
               OpEnvironment,
               Parser),
        asserta(pcExpression(Parser)),
         expr(Wrapper),
        retract(pcExpression(Parser)).

exprFold(E,I,Acc,level(E,I,Acc)).

%------------------------------------------------------------------------------
% - vstupem je uzivatelem pripraveny enviroment operatoru

expressionHandMade(OpParser,Wrapper):-
        asserta(pcExpression(OpParser)),
         expr(Wrapper),
        retract(pcExpression(OpParser)).

%------------------------------------------------------------------------------
expr(W):-
        pcExpression(Parser),
        W :->
                Parser.

%------------------------------------------------------------------------------
%                            Jedna uroven priorit
%------------------------------------------------------------------------------
% - jedna uroven precedence
level(E,O,LP,W):-
 W :->
       (yfyB(level(E,O,LP)) <: yfxB(level(E,O,LP)) <:
        xfyB(level(E,O,LP)) <: xfxB(level(E,O,LP)) <:
        unary(level(E,O,LP))).

%------------------------------------------------------------------------------
%                               Binarni operatory
%------------------------------------------------------------------------------

xfxB(CP,I+L):-                        
 CP=level(E,l(_,_,FY,YF,XFX,_,_,_),LP),
 (XFX=terminate -> L=[] ;
  I+L :->
       (FY<*> <&>>
         (LP <&>> XFX <&>> LP) <&>>
        YF<*>)                                                  <@ evalXfx(E)).
        
yfyB(CP,I+L):-
 CP=level(E,l(_,_,_,YF,_,YFY,_,_),LP),                          % FY zakomentovano
 (YFY=terminate -> L=[] ;
 I+L :->
        ((yfxB(CP) <: xfyB(CP) <: xfxB(CP) <: unary(CP))
          <&>>
         ((YFY <&>> (yfxB(CP) <: xfyB(CP) <: xfxB(CP) <: LP))
           <&>> YF<*>) <+>)                                     <@ evalYfx(E)).

xfyB(CP,I+L):-
 CP=level(E,l(_,_,FY,_,_,_,XFY,_),LP),
 (XFY=terminate -> L=[] ;
 I+L :-> (
        (FY<*> <&>> (LP<&>>XFY))<+>
         <&>>
        (yfyB(CP) <: yfxB(CP) <: xfxB(CP) <: unary(CP))
       )                                                        <@ evalXfy(E)).

yfxB(CP,I+L):-
 CP=level(E,l(_,_,_,YF,_,_,_,YFX),LP),
 (YFX=terminate -> L=[] ;
 I+L :-> (
        (xfyB(CP) <: xfxB(CP) <: unary(CP))
         <&>>
        ((YFX<&>>LP) <&>> YF<*>)<+>
        ) <@ evalYfx(E)).

%------------------------------------------------------------------------------
%                             Unarni operatory
%------------------------------------------------------------------------------

unary(CP,W):-
 CP=level(E,l(FX,XF,FY,YF,_,_,_,_),LP),
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
%             Generator parseru vyrazu z uzivatelem daneho environmentu
%------------------------------------------------------------------------------
/**     op2Env(+Operators, -Environment)
Text:   Generuje z u�ivatelsk�ho v��tu oper�tor�, tak je pops�n
        v expression/3 okol�, kter� je n�sledn� pou��v�no p�i rozkladu.
*/

op2Env([],[]).
op2Env([PrecLevel|T],[EnvLevel|ET]):-
        op2EnvLev(PrecLevel,            % jedna prioritni uroven
                  % fx        xf        fy        yf
                  l(terminate,terminate,terminate,terminate,
                  % xfx       yxf       xfy       yfx
                    terminate,terminate,terminate,terminate),
                  EnvLevel),  
        op2Env(T,ET).

op2EnvLev([],E,E).
% operatory: fx xf fy yf xfx yfy xfy yfx
op2EnvLev([fx(O,V)|T],l(FX,XF,FY,YF,XFX,YFY,XFY,YFX),E):-
        op2EnvP(FX,O,V,FXo), op2EnvLev(T,l(FXo,XF,FY,YF,XFX,YFY,XFY,YFX),E).
op2EnvLev([xf(O,V)|T],l(FX,XF,FY,YF,XFX,YFY,XFY,YFX),E):-
        op2EnvP(XF,O,V,XFo), op2EnvLev(T,l(FX,XFo,FY,YF,XFX,YFY,XFY,YFX),E).
op2EnvLev([fy(O,V)|T],l(FX,XF,FY,YF,XFX,YFY,XFY,YFX),E):-
        op2EnvP(FY,O,V,FYo), op2EnvLev(T,l(FX,XF,FYo,YF,XFX,YFY,XFY,YFX),E).
op2EnvLev([yf(O,V)|T],l(FX,XF,FY,YF,XFX,YFY,XFY,YFX),E):-
        op2EnvP(YF,O,V,YFo), op2EnvLev(T,l(FX,XF,FY,YFo,XFX,YFY,XFY,YFX),E).
op2EnvLev([xfx(O,V)|T],l(FX,XF,FY,YF,XFX,YFY,XFY,YFX),E):-
        op2EnvP(XFX,O,V,XFXo), op2EnvLev(T,l(FX,XF,FY,YF,XFXo,YFY,XFY,YFX),E).
op2EnvLev([yfy(O,V)|T],l(FX,XF,FY,YF,XFX,YFY,XFY,YFX),E):-
        op2EnvP(YFY,O,V,YFYo), op2EnvLev(T,l(FX,XF,FY,YF,XFX,YFYo,XFY,YFX),E).
op2EnvLev([xfy(O,V)|T],l(FX,XF,FY,YF,XFX,YFY,XFY,YFX),E):-
        op2EnvP(XFY,O,V,XFYo), op2EnvLev(T,l(FX,XF,FY,YF,XFX,YFY,XFYo,YFX),E).
op2EnvLev([yfx(O,V)|T],l(FX,XF,FY,YF,XFX,YFY,XFY,YFX),E):-
        op2EnvP(YFX,O,V,YFXo), op2EnvLev(T,l(FX,XF,FY,YF,XFX,YFY,XFY,YFXo),E).

op2EnvP(P,S,V,Po):-
        P=terminate -> Po= (token(S)<@const(V))
                     ; Po= (<:(token(S)<@const(V),P)).

%------------------------------------------------------------------------------
%                        Predpripravene vyhodnocovace
%------------------------------------------------------------------------------
/** arieval(+Expr, -Result)
Text:   Jednoduch� nerekurzivn� vyhodnocova� aritmetick�ch v�raz�, kter� lze
        snadno dopl�ovat o vlastn� funkce.
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

% pomocne predikaty
add(X,Y,R):-    R is X+Y.              % use add (plus is built-in SWI Prolog)
sub(X,Y,R):-    R is X-Y.
mul(X,Y,R):-    R is X*Y.
div(X,Y,R):-    R is X/Y.
idiv(X,Y,R):-   R is X//Y.
inc(X,R):-      integer(X), R is X+1.
dec(X,R):-      integer(X), R is X-1.

%- EOF ------------------------------------------------------------------------
