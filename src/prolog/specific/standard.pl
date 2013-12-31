%------------------------------------------------------------------------------
%
%                    Standard impl of specific predicates
%
%                               Martin Dvorak
%				    1999
%------------------------------------------------------------------------------
%                               Arithmetic
%------------------------------------------------------------------------------
+(X,Y,R):-
        R is X+Y.

-(X,Y,R):-
        R is X-Y.

*(X,Y,R):-
        R is X*Y.

/(X,Y,R):-
        R is X/Y.

mod(X,Y,R):-
        R is X mod Y.

//(X,Y,R):-
        R is X//Y.

%- EOF ------------------------------------------------------------------------
