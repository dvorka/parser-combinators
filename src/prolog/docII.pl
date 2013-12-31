%------------------------------------------------------------------------------
%
%                Priklady z textu prace: Kapitola 4 a Kapitola 5
%
%                           Zavadec ./doc/pcII.pl
%                                    SWI
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------

% Definice operatoru
 :-['pc/library/operator'].

% HOP - SWI Prolog
 :-['specific/swi'].
 :-['pc/hop/calln/curry/swi'].
 :-['pc/hop/binder/swi'].

% HOP - BinProlog
 %:-['specific/bp'].
 %:-['pc/hop/calln/curry/bp'].
 %:-['pc/hop/binder/bp'].

% Funkcionalni programovani
 :- ['pc/library/fun'].
% Klasifikace znaku
 :- ['pc/library/is'].
% Konverze
 :- ['pc/library/to'].
% Kolony
 :- ['pc/library/colone'].
% Podpora
 :- ['support/file'].
 :- ['support/setlist'].
% Zakladni primitiva a konstruktory
 :- ['pc/library/terminal'].
 :- ['pc/library/combine'].
 :- ['pc/library/mutate'].
 :- ['pc/library/mutatePart'].
 :- ['pc/library/mutateTrim'].
 :- ['pc/library/mutateFail'].
% "Smart verze"
 :- ['pc/library/smart'].
% Debugger
 :- ['pc/library/debug'].
 :- ['pc/library/debugger/debugger'].
% Konstruktory z docI
 :- ['pc/library/tuple'].
 :- ['pc/library/terminalI'].
 :- ['pc/library/mutateI'].
 :- ['pc/library/combineI'].
 :- ['pc/library/datatype'].
 :- ['pc/library/idf'].

:- retractall(letter(_)),retractall(evalBin(_,_,_)),retractall(bin(_)),
   retractall(<?(_,_,_)),retractall(sieve(_,_)).

% +----------------------------------------+
% | Konstruktory: Kapitola 4 - Kapitola 5  |
% +----------------------------------------+
:- ['doc/pcII'].

%- EOF ------------------------------------------------------------------------
