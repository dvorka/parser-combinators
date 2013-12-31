%------------------------------------------------------------------------------
%
%             Generatoru souboru s prehledem definovanych operatoru
%
%                             Martin Dvorak
%				  1999
%------------------------------------------------------------------------------
% Inicializace & knihovny (BP,SWI)
:- ['../../support/setlist'].
:- ['../../support/file'].
:- ['../../pc/hop/standard'].
:- ['../../pc/library/debug'].

% Inicializace & knihovny (LPA)
%:- consult('../../support/setlist').
%:- consult('../../support/file').
%:- consult('../../pc/hop/standard').
%:- consult('../../pc/library/debug').

%------------------------------------------------------------------------------

go:-
        currentOperators('curop.txt').

%------------------------------------------------------------------------------
/** currentOperators(+File)
Text:   Vygeneruje soubor s pøehledem definovaných operátorù.
*/
currentOperators(File):-
    openFile(File,O,write),
     nl,write('Machine generated file of current_op/3:'),nl,nl,
     bagof([X,Y,Z],current_op(X,Y,Z),R),
     quickSortG(curOpMatch,R,SR),	
     printCurOps(SR),
     nl,write('(by /pc/src/prolog/tools/curop.pl)'),nl,
    closeFile(O,write).

%------------------------------------------------------------------------------
% list
printCurOps([[Prec,Type,Arity]|R]):-
    printf([Prec,'   ',Type,'   ',Arity,nl]),
    printCurOps(R).
printCurOps([]).    

%------------------------------------------------------------------------------
% matcher
curOpMatch([X|_],[Y|_]):-
    X<Y.
    
%- EOF ------------------------------------------------------------------------
