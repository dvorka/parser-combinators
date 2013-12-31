%------------------------------------------------------------------------------
%
%                          Arithmetic expressions
%
%			        Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Init & libraries

 :- ['../../loadSWI'].          % SWI Prolog
 %:- ['../../loadBP-2'].        % BinProlog                     

%------------------------------------------------------------------------------
%                                   Demo
%------------------------------------------------------------------------------

go1:-
        expr(s("1+(5+6)*(8+9-10)")+L),
        printf([L,nl]).

% L= [[]> 78]
% Yes

% ---

% some demo function:
prumer(A,B,C,P):- P is (A+B+C)/3.

go2:-
        expr(s("1+prumer(12,6,3)*(8+9-10)")+L),
        printf([L,nl]).

% L= [[]> 50]
% Yes

% ---

go3:-
        expr(s("1+prumer(12,6*2-6*2,prumer(3*4,1+1,1))")+L),
        printf([L,nl]).

% L= [[]> 6.66667]
% Yes

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
         (poorIdf <&> parentheses(commaListOf expr)<?>) <@ evalFact
          <:
         parentheses(expr)).

evalFact(L,R):- append(L,[R],F), :-@ F.

%------------------------------------------------------------------------------
/** term(?Wrapper)
*/
% - chaining priority high: * and / sequence
term(W):-
 W :->
        fact lchainedBy (symbolA("*") <: symbolA("/")).

%------------------------------------------------------------------------------
/** expr(?Wrapper)
*/
% chaining priority low: + and - sequence
expr(W):-
 W :->
        term lchainedBy (symbolA("+") <: symbolA("-")).

%- EOF ------------------------------------------------------------------------
