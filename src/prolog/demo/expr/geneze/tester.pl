%------------------------------------------------------------------------------
%
%                          Arithmetic expressions
%
%			        Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------

% x is operator
:- op(600,fx,xa).
:- op(601,fx,xb).
:- op(602,fx,xc).
:- op(603,fx,xd).
:- op(600,xf,ax).
:- op(601,xf,bx).
:- op(602,xf,cx).
:- op(603,xf,dx).

xa(_).
xb(_).
xc(_).
xd(_).
ax(_).
bx(_).
cx(_).
dx(_).

% y is operator
:- op(600,fy,ya).
:- op(601,fy,yb).
:- op(602,fy,yc).
:- op(603,fy,yd).
:- op(600,yf,ay).
:- op(601,yf,by).
:- op(602,yf,cy).
:- op(603,yf,dy).

ya(_).
yb(_).
yc(_).
yd(_).
ay(_).
by(_).
cy(_).
dy(_).

%------------------------------------------------------------------------------
% Binary operators

% x is operator
:- op(600,xfx,xax).
:- op(601,xfx,xbx).
:- op(602,xfx,xcx).

:- op(600,xfy,xay).
:- op(601,xfy,xby).
:- op(602,xfy,xcy).

:- op(600,yfx,yax).
:- op(601,yfx,ybx).
:- op(602,yfx,ycx).

:- op(600,yfy,yay).
:- op(601,yfy,yby).
:- op(602,yfy,ycy).

xax(_,_).
xbx(_,_).
xcx(_,_).
xay(_,_).
xby(_,_).
xcy(_,_).
yax(_,_).
ybx(_,_).
ycx(_,_).
yay(_,_).
yby(_,_).
ycy(_,_).

%------------------------------------------------------------------------------

go(Expr):-
	display(Expr).

%------------------------------------------------------------------------------
% Poznamky:

% ?- go(ya a ay).                     ay(ya(a))            
%    U  stejnych precedenci rozhoduje poradi (prefix ma prednost pred  postfix)

% ?- go(ya yb a).                     error
%    U rozdilnych precedenci, cim blize jsme k operandu, tim vetsi musi
%    byt priorita

% ?- go(yd yc a ay by).               yd(yc(by(ay(a))))
%    U rozdilnych precedenci, nezalezi zda je operator v post nebo pre,
%    pokud ma veci precedenci, tak ma prednost

% ?- go(yd yc c by cy).               yd(cy(yc(by(c))))
%    Kombinace predchozich prikladu. U ruznych precedenci nezalezi na poradi,
%    ale u stejnych precedenci (c) rozhodl prefix pro yc a uplatnil se drive

%- EOF ------------------------------------------------------------------------
