%------------------------------------------------------------------------------
%
%                    * Knihovna kombinatoru parseru *
%
%               Zavadec knihovny kombinatoru parseru & podpory
%                              BinProlog
%
%                            Martin Dvorak
%				 1999
%------------------------------------------------------------------------------
/** Module: Knihovna konstruktorù parserù
Text:   Knihovna konstruktorù je tvoøena sadou kombinátorù, mutátorù,
        generátorù a v¹eobecnì pou¾itelných predikátù urèených pro
        rychlé vytváøení parserù. Kromì toho obsahuje parsery pro
        obvyklé syntaktické konstrukce a podporu pro ladìní, její¾
        souèástí je metainterpret usnadòující efektivní vyhledávání
        a odstraòování chyb z vytváøených syntaktických analyzátorù.
p       Soubor 'loadBP-2.pl' je zavadìèem knihovny pro interpret BinProlog.
*/

% Knihovna
:- ['../../pc/pcBP-2'].

% Implementacne zavisle predikaty (BinProlog)
:- ['../../specific/bp'].

% Podpora
:- ['../../support/file'].
:- ['../../support/setlist'].	

%- EOF ------------------------------------------------------------------------
