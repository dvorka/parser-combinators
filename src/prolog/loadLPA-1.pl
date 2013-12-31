%------------------------------------------------------------------------------
%
%                    * Knihovna kombinatoru parseru *
%
%               Zavadec knihovny kombinatoru parseru & podpory
%                             LPA Prolog
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
p       Soubor 'loadLPA-1.pl' je zavadìèem knihovny pro interpret LPA Prolog.
*/

% Knihovna
:-consult('../pc/pcLPA-1').

% Implementacne zavisle predikaty (LPA Prolog)
:-consult('../specific/lpa').

% Podpora
:-consult('../support/file').
:-consult('../support/setlist').

%- EOF ------------------------------------------------------------------------
