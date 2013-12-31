%------------------------------------------------------------------------------
%
%                                Expressions
%
%			        Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Poznamka:
%       Test - unarni & binarni operatory staticky, jedna uroven precedence.
%------------------------------------------------------------------------------
% Init & libraries

:- ['../../loadSWI'].          % SWI Prolog

%------------------------------------------------------------------------------
%                                  Demo
%------------------------------------------------------------------------------
go:-
        write('1 *'),nl,
        s("1")+_ :-> level <@ shownl, nl,

        write('xfx(1, 2) *'),nl,
        s("1xfx2")+_ :-> level <@ shownl, nl,
        write('yfx(1, 2) *'),nl,
        s("1yfx2")+_ :-> level <@ shownl, nl,
        write('xfy(1, 2) *'),nl,
        s("1xfy2")+_ :-> level <@ shownl, nl,
        write('yfy(1, 2) *'),nl,
        s("1yfy2")+_ :-> level <@ shownl, nl,
        
        write('xfy(1, xfy(2, xfy(3, 5))) *'),nl,
        s("1xfy2xfy3xfy5")+_ :-> level <@ shownl, nl,      % xfy
        write('yfx(yfx(yfx(1, 2), 3), 5) *'),nl,
        s("1yfx2yfx3yfx5")+_ :-> level <@ shownl, nl,      % yfx
        write('yfy(1, yfy(2, yfy(3, 5))) *'),nl,
        s("1yfy2yfy3yfy5")+_ :-> level <@ shownl, nl,      % yfy

        write('xfy(1, yfx(xfx(2, 3), 5)) *'),nl,
        s("1xfy2xfx3yfx5")+_ :-> level <@ shownl, nl,      % center xfx
        write('fail *'),nl,
        s("1yfx2xfx3xfy5")+_ :-> level <@ shownl, nl,      % center xfx

        write('yfy(xfx(1, 2), xfx(3, 5)) *'),nl,
        s("1xfx2yfy3xfx5")+_ :-> level <@ shownl, nl,      % center yfy
        write('yfy(yfx(1, 2), xfy(3, 5))'),nl,
        s("1yfx2yfy3xfy5")+_ :-> level <@ shownl, nl,

        write('xfy(1, xfy(2, yfy(3, 5))) *'),nl,
        s("1xfy2xfy3yfy5")+_ :-> level <@ shownl, nl,

        write('yfx(yfx(xfy(1, xfy(2, 3)), 5), 6) *'),nl,
        s("1xfy2xfy3yfx5yfx6")+_ :-> level <@ shownl, nl,

        write('xfy(1, xfy(2, yfy(3, yfx(5, 6)))) *'),nl,
        s("1xfy2xfy3yfy5yfx6")+_ :-> level <@ shownl, nl,

        % unary operators
        write(' *'),nl,
        s("fyfy1xfyf")+_ :-> level <@ shownl, nl.

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
         (poorIdf <&> parentheses(commaListOf level)<?>) <@ evalFact
          <:         % !!!
         parentheses(level)). % put it into nest

evalFact(L,R):-
        R=..L.

%------------------------------------------------------------------------------
%                         Pouze binarni operatory
%------------------------------------------------------------------------------

xfxB(W):-
 W :->
       (tokenA("fy")<**> <&>>
         (fact <&>> tokenA("xfx") <&>> fact) <&>>
        tokenA("yf")<**>)                                 <@ xfxF.
        

xfxF(FYL>((N1>(Op>N2))>YFL),R):-
        P=..[Op,N1,N2],
        foldR(sfx,P,FYL,FY),
        foldL(sfxFlip,FY,YFL,R).

yfyB(W):-
 W :-> ((
         ((tokenA("fy")<**>
            <&>>
          ((xfxB <:> yfxB <:> xfyB <:> fact) <&>> tokenA("yfy")))<+>
            <&>>
          (xfxB <:> xfyB <:> yfxB <:> unary(fact))) <@ xfyF
        )
        <:>
        (
         ((xfxB <:> yfxB <:> xfyB <:> unary(fact))
          <&>>
         ((tokenA("yfy") <&>> (xfxB <:> xfyB <:> yfxB <:> fact))
           <&>> tokenA("yf")<**>) <+>)          <@ yfxF     
        )
       ).

xfyB(W):-
 W :-> (
        (tokenA("fy")<**> <&>> (fact<&>>tokenA("xfy")))<+>
         <&>>
        (yfxB <:> xfxB <:> yfyB <:> unary(fact))
       ) <@ xfyF.

xfyF(List>Ini,Res):-
        foldR(xfyFF,Ini,List,Res).
xfyFF(FYL>(N>Op),Acc,R):-
        P =..[Op,N,Acc],
        foldR(sfx,P,FYL,R).
        
yfxB(W):-
 W :-> (
        (xfyB <:> xfxB <:> unary(fact))
         <&>>
        ((tokenA("yfx")<&>>fact) <&>> tokenA("yf")<**>)<+>
        ) <@ yfxF.

yfxF(AccI>List,Res):-
        foldL(yfxFF,AccI,List,Res).
yfxFF(Acc,(Op>N)>YFL,R):-
        P =..[Op,Acc,N],
        foldL(sfxFlip,P,YFL,R).

% Jedna uroven precedence spolecne s unarnimi operatory.

level(W):-
 W:-> (
        yfyB <:>  xfyB <:> yfxB <:> xfxB <:> unary(fact)
      ).

%------------------------------------------------------------------------------
%                             Unarni operatory
%------------------------------------------------------------------------------

% x unary operators
unary(P,W):-
    W :->(
           tokenA("fy")<**> <&>>
             (tokenA("fx")<?> <&>> P <@ evalUnaryFX
               <:>
              P <&>> tokenA("xf")<?> <@ evalUnaryXF) <&>>
           tokenA("yf")<**>                    <@ evalUnary).

% [[fy1,fy2],P,[yf1,yf2]]
evalUnary(FYL>(P>YFL),FYYF):-
        foldR(sfx,P,FYL,FY),
        foldL(sfxFlip,FY,YFL,FYYF).

evalUnaryFX(F>X,R):-
        F=[] -> X=R ; R=..[F,X].
evalUnaryXF(X>F,R):-
        F=[] -> X=R ; R=..[F,X].

%- EOF ------------------------------------------------------------------------
