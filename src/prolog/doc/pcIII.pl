%------------------------------------------------------------------------------
%
%
%                          Knihovna kombinatoru parseru
%
%                   Priklady z textu prace: Kapitola 5 a dalsi
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Poznamky:
% - prosim konzultujte tento soubor pomoci ../docIII.pl
% - konvence ve zdrojovych souborech lze nalezt v ../pc/README.conventions
%------------------------------------------------------------------------------
% Prehled prikladu:
go:-
        printf(['Available examples:',nl]),
        listing(examples).

:- nl,write(' * Type go/0 for available examples *'),nl,nl.
%------------------------------------------------------------------------------
%                          "Aritmeticke vyrazy"
%------------------------------------------------------------------------------
% Priklad:
goExpr:-
        expression([[fx(":-",':-'),fx("?-",'?-')],
                    [xfx(":-",':-')],
                    [xfy(";",';')],
                    [xfy("->",'->')],
                    [xfy(",",',')],
                    [fy("not",'not')]
                   ],
                   factProlog(id),
                   quickS("a(b,c):-d->f,g;h")+L),
                   docShow(1,[]),
                   (L=[_>R] -> write('LOS: '),display(R) ;
                               write('LOS empty')),nl.

% factProlog(?Wrapper)
factProlog(E,W):-
  W :-> (
         (prologIdf <&> parentheses(commaListOf expr)<?>) <@ evalFact(E)
          <: 
         parentheses(expr)).

%------------------------------------------------------------------------------
%                                 "Mody parseru"
%------------------------------------------------------------------------------
% Priklad:
goModeOff:-
 off(0,s("{ This is off/2 test! } listing(item)."))+L :->
        pascalNestedComment,
                        docShow(2,[L]).
        
%------------------------------------------------------------------------------

%        ----------------------------
%       | viz ../pc/library/adhoc.pl |
%        ----------------------------

%        ---------------------------------
%       | viz ../demo/charset/charset.pl |
%        ---------------------------------

%------------------------------------------------------------------------------
% Priklad:
goModeCompose:-
                        docShow(3,[]),
 off(0,lineCol(1,1,prn(s("564e-2"))))+L
       :-> double,
       printf([nl,' ',L,nl]).

%------------------------------------------------------------------------------
% Priklad:
goMultiPass:-
        multiPass([prelex,lexer,strip,synthPass],P),
                        docShow(4,[P]).

%------------------------------------------------------------------------------
%                       "Deterministicke konstruktory parseru"
%------------------------------------------------------------------------------
% Priklad:
goEmpty:-
 empty+Empty :-> double,
                       docShow(5,[Empty]).

%------------------------------------------------------------------------------
% Priklad:
goFirst:-
 first+First :-> double,
                       docShow(6,[First]).

%------------------------------------------------------------------------------
%                       "Metainterpret pro ladeni"
%------------------------------------------------------------------------------

goDebug:-
                        docShow(7,[]),
        deBugger(pcDbgDemo1,[break],[digit]).

%       a

%        --------------------------------
%       | viz ../pc/degugger/debugger.pl |
%        --------------------------------

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
        goExpr,
        goModeOff,goModeCompose,
        goMultiPass,
        goEmpty, goFirst,
        goDebug.

%- EOF ------------------------------------------------------------------------
