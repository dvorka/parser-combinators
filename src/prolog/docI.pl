%------------------------------------------------------------------------------
%
%                Priklady z textu prace: Kapitola 1 - Kapitola 3
%
%                           Zavadec ./doc/pcI.pl
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
% Debugger
 :- ['pc/library/debugger/debugger'].
 :- ['pc/library/debug'].

% +---------------------------------------+
% | Konstruktory: Kapitola 1 - Kapitola 3 |
% +---------------------------------------+
 :- ['doc/pcI'].

%- EOF ------------------------------------------------------------------------
