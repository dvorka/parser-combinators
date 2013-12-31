%------------------------------------------------------------------------------
%
%                    * Knihovna kombinatoru parseru *
%
%               Zavadec knihovny kombinatoru parseru & podpory
%                             SWI Prolog
%
%                            Martin Dvorak
%				 1999
%------------------------------------------------------------------------------
/**     Module: Knihovna konstruktorù parserù
Text:   Knihovna konstruktorù je tvoøena sadou kombinátorù, mutátorù,
        generátorù a v¹eobecnì pou¾itelných predikátù urèených pro
        rychlé vytváøení parserù. Kromì toho obsahuje parsery pro
        obvyklé syntaktické konstrukce a podporu pro ladìní, její¾
        souèástí je metainterpret usnadòující efektivní vyhledávání
        a odstraòování chyb z vytváøených syntaktických analyzátorù.
        Soubor 'prolog/loadSWI.pl' je zavadìèem knihovny pro interpret
        SWI Prolog.
*/

% Knihovna
:-['pc/pcSWI'].

% Implementacne zavisle predikaty (SWI Prolog)
:-['specific/swi'].

% Podpora
:- ['support/file'].           
:- ['support/setlist'].

:- nl,write('For info, use ?- pcStatistic. and for debug help type ?- deBugger.'),nl.

%- EOF ------------------------------------------------------------------------
