%------------------------------------------------------------------------------
%
%                                Expressions
%
%			        Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Poznamka:
%       Test - binarni operatory staticky, jedna uroven precedence.
%------------------------------------------------------------------------------
% Init & libraries

 :- ['../../loadSWI'].          % SWI Prolog

%------------------------------------------------------------------------------
%                                 Demo
%------------------------------------------------------------------------------

go:-
        write('1 *'),nl,
        s("1")+_ :-> expr <@ shownl, nl,

        write('xfx(1, 2) *'),nl,
        s("1xfx2")+_ :-> expr <@ shownl, nl,
        write('yfx(1, 2) *'),nl,
        s("1yfx2")+_ :-> expr <@ shownl, nl,
        write('xfy(1, 2) *'),nl,
        s("1xfy2")+_ :-> expr <@ shownl, nl,
        write('yfy(1, 2) *'),nl,
        s("1yfy2")+_ :-> expr <@ shownl, nl,
        
        write('xfy(1, xfy(2, xfy(3, 5))) *'),nl,
        s("1xfy2xfy3xfy5")+_ :-> expr <@ shownl, nl,      % xfy
        write('yfx(yfx(yfx(1, 2), 3), 5) *'),nl,
        s("1yfx2yfx3yfx5")+_ :-> expr <@ shownl, nl,      % yfx
        write('yfy(1, yfy(2, yfy(3, 5))) *'),nl,
        s("1yfy2yfy3yfy5")+_ :-> expr <@ shownl, nl,      % yfy

        write('xfy(1, yfx(xfx(2, 3), 5)) *'),nl,
        s("1xfy2xfx3yfx5")+_ :-> expr <@ shownl, nl,      % center xfx
        write('fail *'),nl,
        s("1yfx2xfx3xfy5")+_ :-> expr <@ shownl, nl,      % center xfx

        write('yfy(xfx(1, 2), xfx(3, 5)) *'),nl,
        s("1xfx2yfy3xfx5")+_ :-> expr <@ shownl, nl,      % center yfy
        write('yfy(yfx(1, 2), xfy(3, 5))'),nl,
        s("1yfx2yfy3xfy5")+_ :-> expr <@ shownl, nl,

        write('xfy(1, xfy(2, yfy(3, 5))) *'),nl,
        s("1xfy2xfy3yfy5")+_ :-> expr <@ shownl, nl,

        write('yfx(yfx(xfy(1, xfy(2, 3)), 5), 6) *'),nl,
        s("1xfy2xfy3yfx5yfx6")+_ :-> expr <@ shownl, nl,

        write('xfy(1, xfy(2, yfy(3, yfx(5, 6)))) *'),nl,
        s("1xfy2xfy3yfy5yfx6")+_ :-> expr <@ shownl, nl.

%------------------------------------------------------------------------------
%                               Code
%------------------------------------------------------------------------------
/** fact(?Wrapper)
*/
% - object: number, identifier, function or (...)
fact(W):-
  W :-> (
         int	   			                 
          <:
         (poorIdf <&> (parentheses(commaListOf expr))<?>) <@ evalFact
          <:
         parentheses(expr)).

evalFact(L,R):- printf(['call',L,nl,nl]), append(L,[R],F), :-@ F.

%------------------------------------------------------------------------------
%                       Pouze binarni operatory
%------------------------------------------------------------------------------
% Pod operatorem xfx mohou byt pouze operatory s vyssi prioritou.

xfxB(W):-
 W :->
        (fact <&>> tokenA("xfx") <&>> fact) <@ xfxF.

xfxF(N1>(Op>N2),R):-
        R=..[Op,N1,N2].

% Nalevo i napravo muze byt cokoli ze stejne priority - jakykoli operator.
% Je mozne vydat dve moznosti - retezit s levou asociativitou nebo s pravou 
% asociativitou.

yfyB(W):-
 W :-> ((
         (((xfxB <:> yfxB <:> xfyB <:> fact) <&>> tokenA("yfy"))<+>
          <&>>
         (xfxB <:> xfyB <:> yfxB <:> fact)) <@ xfyF                             % iterovat a foldL
        )
        <:>
        (
         ((xfxB <:> yfxB <:> xfyB <:> fact)
          <&>>
         (tokenA("yfy") <&>> (xfxB <:> xfyB <:> yfxB <:> fact))<+>) <@ yfxF     % iterovat a foldL
        )
       ).

% Nalevo muze byt pouze operator s vyssi prioritou, napravo cokoli

xfyB(W):-
 W :-> (
        (fact<&>>tokenA("xfy"))<+>
         <&>>
        (yfxB <:> xfxB <:> yfyB <:> fact)
       ) <@ xfyF.

xfyF(List>Ini,Res):-
        foldR(xfyFF,Ini,List,Res).
xfyFF(N>Op,Acc,R):-
        R =..[Op,N,Acc].

% Nalevo muze byt libovlny operator, kvuli vzniku cyklu jsme vynechali
% yfy - je to v poradku. Vzdy ziskame jeden spravny rozklad. Ve
% vstupu totiz predchazi yfy operatoru yfx (jsou od nej nalevo)
% a ten zavesi yfx pod sebe. Napravo muze byt pouze vyssi priorita.

yfxB(W):-
 W :-> (
        (xfyB <:> xfxB <:> fact)
         <&>>
        (tokenA("yfx")<&>>fact)<+> 
       ) <@ yfxF.                  

yfxF(AccI>List,Res):-
        foldL(yfxFF,AccI,List,Res).
yfxFF(Acc,Op>N,R):-
        R =..[Op,Acc,N].

% A konecne jedna uroven priorit.

expr(W):-
 W:-> (
        yfyB <:>  xfyB <:> yfxB <:> xfxB <:> fact
      ).

%- EOF ------------------------------------------------------------------------
