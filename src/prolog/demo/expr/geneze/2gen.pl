%------------------------------------------------------------------------------
%
%                    Generalized arithmetical expressions
%
%			        Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Init & libraries

 :- ['../../loadSWI'].          % SWI Prolog
 %:- ['../../loadBP-2'].        % BinProlog                     

%------------------------------------------------------------------------------
%                                Demo
%------------------------------------------------------------------------------
% 1. AriExpr I expression parser using generator

go1:-
        expr1(s("1+prumer(12,6,3)*(8+9-10)")+L),
                printf([L,nl]).
% L= [[]> 50]
% Yes

go2:-
        expr2(s("1+prumer(12,6,3)*(8+9-10)")+L),
                printf([L,nl]).
% L= [[]> 50]
% Yes

%   ------------------------------------------------------------------------
% 2. New expression parser

go3:-
        expression(s("1+prumer(12,6,3)*(8+9-10)")+L),
                printf([L,nl]).
% L= [[]> 50]
% Yes

go4:-
        expression(s("1+10mod3//2*100+2*5//2")+L),
                printf([L,nl]).

% L= [[]> 5]
% Yes

%   ------------------------------------------------------------------------
 % some demo function
   prumer(A,B,C,P):- P is (A+B+C)/3.

%------------------------------------------------------------------------------
%                               Code
%------------------------------------------------------------------------------
/** fact(+ExpressionParser,?Wrapper)
*/
fact(EP,W):-
  W :-> (
         int	   			                 
          <:
         (poorIdf <&> parentheses(commaListOf EP)<?>) <@ evalFact
          <:
         parentheses(EP)).

evalFact(L,R):- append(L,[R],F), :-@ F.

%------------------------------------------------------------------------------
/** exprGen(+Parser, +OperatorList, ?Wrapper)
*/
% - parser generator for expressions with multiple levels of operator priority
% - P is parser of sequence with higher priority
% - OperatorList is list of operators e.g. ["+","-"]
% - example:
%       ?- generator(digit,["+","-"],"1+2",X).
%       X= [ []> +(1,-(2,3)), ...

exprGen(Operators,P,W):-
    mapList(sfx(tokenA),Operators,OpList),
    selection(OpList,SepParser),
    W :->
        P lchainedBy SepParser.

%------------------------------------------------------------------------------
%                       AriExpr I using generator
%------------------------------------------------------------------------------

term(W):-
 W :->
        exprGen(["*","/"],fact(expr1)).

expr1(W):-
 W :->
        exprGen(["+","-"],term).

% or

expr2(W):-
 W :->
        exprGen(["+","-"],exprGen(["*","/"],fact(expr2))).

%------------------------------------------------------------------------------
%         Arithmetical expressions with multiple levels of priority
%------------------------------------------------------------------------------
/** expression(Wrapper)
*/
% - operators are sorted in list from the LOWEST to the HIGHEST priority
% - new priority level -> add item to list

expression(W):-
    foldR(sfx2(exprGen),fact(expression),
          [["+","-"],["*","/"],["//","mod"]], ExprParser),
    W :->
        ExprParser.

%- EOF ------------------------------------------------------------------------
