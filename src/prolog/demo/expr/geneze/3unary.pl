%------------------------------------------------------------------------------
%
%                                Expressions
%
%			        Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Poznamka:
%       Test - unarni operatory staticky, jedna uroven precedence.
%------------------------------------------------------------------------------
% Init & libraries

:- ['../../loadSWI'].          % SWI Prolog

%------------------------------------------------------------------------------
%                               Demo
%------------------------------------------------------------------------------

go1:-
 s("fx1yf")+L :-> whole(unary(fact)),
                                        printf([L,nl,nl]).
% [[]>yf(fx(1))]
% Yes

go2:-
 s("fy1yf")+L :-> whole(unary(fact)),
                                        printf([L,nl,nl]).
% [[]>yf(fy(1)), []>yf(fy(1))]
% Yes

go3:-
 s("fy1xfyf")+L :-> whole(unary(fact)),
                                        printf([L,nl,nl]).
% [[]>yf(fy(xf(1)))]
% Yes

go4:-
 s("fy(fx1yf)xf")+L :-> whole(unary(fact)),
                                        printf([L,nl,nl]).

% [[]>fy(xf(yf(fx(1)))), []>xf(fy(yf(fx(1))))]
% Yes

%------------------------------------------------------------------------------
%                                   Code
%------------------------------------------------------------------------------
/** fact(?Wrapper)
*/
% - object: number, identifier, function or (...)
fact(W):-
  W :-> (
         int
          <:                                  % !!!
         (poorIdf <&> parentheses(commaListOf unary(fact))<?>) <@ evalFact
          <:         % !!!
         parentheses(unary(fact))). % put it into nest

evalFact(L,R):-
        R=..L.

%------------------------------------------------------------------------------
%                               Unarni operatory
%------------------------------------------------------------------------------

% x unary operators
unary(P,W):-
    W :->(
           tokenA("fy")<**> <&>>
             (tokenA("fx") <&>> P <@ evalUnaryFX
               <:>
              P <&>> tokenA("xf")<?> <@ evalUnaryXF
             ) <&>>
           tokenA("yf")<**>                    <@ evalUnary).

% [[fy1,fy2],P,[yf1,yf2]]
evalUnary(FYL>(P>YFL),FYYF):-
        foldR(sfx,P,FYL,FY),
        foldL(sfxFlip,FY,YFL,FYYF).

evalUnaryFX(F>X,R):-
        R=..[F,X].
evalUnaryXF(X>F,R):-
        F=[] -> X=R ; R=..[F,X].

%- EOF ------------------------------------------------------------------------
