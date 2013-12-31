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
/** Module: Knihovna konstruktor� parser�
Text:   Knihovna konstruktor� je tvo�ena sadou kombin�tor�, mut�tor�,
        gener�tor� a v�eobecn� pou�iteln�ch predik�t� ur�en�ch pro
        rychl� vytv��en� parser�. Krom� toho obsahuje parsery pro
        obvykl� syntaktick� konstrukce a podporu pro lad�n�, jej�
        sou��st� je metainterpret usnad�uj�c� efektivn� vyhled�v�n�
        a odstra�ov�n� chyb z vytv��en�ch syntaktick�ch analyz�tor�.
p       Soubor 'loadBP-2.pl' je zavad��em knihovny pro interpret BinProlog.
*/

% Knihovna
:- ['../../pc/pcBP-2'].

% Implementacne zavisle predikaty (BinProlog)
:- ['../../specific/bp'].

% Podpora
:- ['../../support/file'].
:- ['../../support/setlist'].	

%- EOF ------------------------------------------------------------------------
