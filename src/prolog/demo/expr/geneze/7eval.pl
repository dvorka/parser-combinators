%------------------------------------------------------------------------------
%
%                          Arithmetic expressions
%
%			        Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Poznamka:
% Test - vsechny operatory staticky, jedna uroven precedence + vyhodnocovac.
%------------------------------------------------------------------------------
% Init & libraries

:- ['../../loadSWI'].          % SWI Prolog

%------------------------------------------------------------------------------
%                                  Demo
%------------------------------------------------------------------------------

% Jako vyhodnocovac je zde pouzita identita - demostracni priklad.

go:-
        go(id).

go(E):-
        write('1 *'),nl,
        s("1")+_ :-> level(E) <@ shownl, nl,

        write('xfx(1, 2) *'),nl,
        s("1xfx2")+_ :-> level(E) <@ shownl, nl,
        write('yfx(1, 2) *'),nl,
        s("1yfx2")+_ :-> level(E) <@ shownl, nl,
        write('xfy(1, 2) *'),nl,
        s("1xfy2")+_ :-> level(E) <@ shownl, nl,
        write('yfy(1, 2) *'),nl,
        s("1yfy2")+_ :-> level(E) <@ shownl, nl,
        
        write('xfy(1, xfy(2, xfy(3, 5))) *'),nl,
        s("1xfy2xfy3xfy5")+_ :-> level(E) <@ shownl, nl,      % xfy
        write('yfx(yfx(yfx(1, 2), 3), 5) *'),nl,
        s("1yfx2yfx3yfx5")+_ :-> level(E) <@ shownl, nl,      % yfx
        write('yfy(1, yfy(2, yfy(3, 5))) *'),nl,
        s("1yfy2yfy3yfy5")+_ :-> level(E) <@ shownl, nl,      % yfy

        write('xfy(1, yfx(xfx(2, 3), 5)) *'),nl,
        s("1xfy2xfx3yfx5")+_ :-> level(E) <@ shownl, nl,      % center xfx
        write('fail *'),nl,
        s("1yfx2xfx3xfy5")+_ :-> level(E) <@ shownl, nl,      % center xfx

        write('yfy(xfx(1, 2), xfx(3, 5)) *'),nl,
        s("1xfx2yfy3xfx5")+_ :-> level(E) <@ shownl, nl,      % center yfy
        write('yfy(yfx(1, 2), xfy(3, 5))'),nl,
        s("1yfx2yfy3xfy5")+_ :-> level(E) <@ shownl, nl,

        write('xfy(1, xfy(2, yfy(3, 5))) *'),nl,
        s("1xfy2xfy3yfy5")+_ :-> level(E) <@ shownl, nl,

        write('yfx(yfx(xfy(1, xfy(2, 3)), 5), 6) *'),nl,
        s("1xfy2xfy3yfx5yfx6")+_ :-> level(E) <@ shownl, nl,

        write('xfy(1, xfy(2, yfy(3, yfx(5, 6)))) *'),nl,
        s("1xfy2xfy3yfy5yfx6")+_ :-> level(E) <@ shownl, nl,

        % unary operators
        write(' *'),nl,
        s("fyfy1xfyf")+_ :-> level(E) <@ shownl, nl.

%------------------------------------------------------------------------------
%                                   Code
%------------------------------------------------------------------------------
/** fact(?Wrapper)
*/
% - object: number, identifier, function or (...)
fact(E,W):-
  W :-> (
         int
          <:                                  % !!!
         (poorIdf <&> parentheses(commaListOf level(E))<?>) <@ evalFact(E)
          <:         % !!!
         parentheses(level(E))). % put it into nest

%------------------------------------------------------------------------------

xfxB(E,W):-
 W :->
       (tokenA("fy")<**> <&>>
         (fact(E) <&>> tokenA("xfx") <&>> fact(E)) <&>>
        tokenA("yf")<**>)                               <@ evalXfx(E).
        
yfyB(E,W):-
 W :-> ((
         ((tokenA("fy")<**>
            <&>>
          ((xfxB(E) <:> yfxB(E) <:> xfyB(E) <:> fact(E)) <&>> tokenA("yfy")))<+>
            <&>>
          (xfxB(E) <:> xfyB(E) <:> yfxB(E) <:> unary(E)))       <@ evalXfy(E)
        )
        <:>
        (
         ((xfxB(E) <:> yfxB(E) <:> xfyB(E) <:> unary(E))
          <&>>
         ((tokenA("yfy") <&>> (xfxB(E) <:> xfyB(E) <:> yfxB(E) <:> fact(E)))
           <&>> tokenA("yf")<**>) <+>)                          <@ evalYfx(E)
        )
       ).

xfyB(E,W):-
 W :-> (
        (tokenA("fy")<**> <&>> (fact(E)<&>>tokenA("xfy")))<+>
         <&>>
        (yfxB(E) <:> xfxB(E) <:> yfyB(E) <:> unary(E))
       ) <@ evalXfy(E).

        
yfxB(E,W):-
 W :-> (
        (xfyB(E) <:> xfxB(E) <:> unary(E))
         <&>>
        ((tokenA("yfx")<&>>fact(E)) <&>> tokenA("yf")<**>)<+>
        ) <@ evalYfx(E).

level(E,W):-
 W:->
        (yfyB(E) <:>  xfyB(E) <:> yfxB(E) <:> xfxB(E) <:> unary(E)).

%------------------------------------------------------------------------------
%                             Unarni operatory
%------------------------------------------------------------------------------
% x unary operators
unary(E,W):-
    W :->(
           tokenA("fy")<**> <&>>
             (tokenA("fx")<?> <&>> fact(E) <@ evalUnaryFX(E)
               <:>
              fact(E) <&>> tokenA("xf")<?> <@ evalUnaryXF(E)) <&>>
           tokenA("yf")<**>                    <@ evalUnary(E)).

%------------------------------------------------------------------------------
%                               Evaluator
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

% own functions
arieval(!(X),R):- dec(X,R).
arieval(?(X),R):- dec(X,R).
arieval(max(X,Y),R):- R=X.
arieval(min(X,Y),R):- R=X.

arieval(X,X).

%- EOF ------------------------------------------------------------------------
