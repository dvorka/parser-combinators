%------------------------------------------------------------------------------
%
%                                  Mode IO
%
%				Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/**     Module:
Text:   V m�dech ll1/pseudoll1 jsou za b�hu vytv��eny rozkladov�
        tabulky. V selektorech t�chto m�d� lze ur�it, zda se m�
        k�d, kter�mi jsou tabulky reprezentov�ny ukl�dat do datab�ze.
p               V p��pad�, �e je ukl�d�n� povoleno, lze tento k�d
        nejen upravovat, prohl�et a roz�i�ovat, ale rovn� ukl�dat
        pro pozd�j�� pou�it�. K t�mto operac�m jsou ur�eny
        predik�ty definovan� v tomto souboru.
*/
%------------------------------------------------------------------------------
%                       eFirst / ll1 / pseudoll1
%------------------------------------------------------------------------------
/**     modeShow
Text:   Vyp�e do standardn�ho v�stupn�ho proudu data ulo�en� do
        datab�ze pro zrychlen� pr�ce v m�dech ll1/4 a pseudoll1/4
        - eFirst/0, ll1Code/0 a pseudoll1Code/0.
*/

modeShow:-
        eFirstShow,
        ll1CodeShow,
        pseudoll1CodeShow.

%------------------------------------------------------------------------------
/**     modeSave
Text:   Zap�e do soubor� v aktu�ln� adres��i data ulo�en� do
        datab�ze pro zrychlen� pr�ce v m�dech ll1/4 a pseudoll1/4
        - odpov�d� pou�it� eFirstSave/0, ll1CodeSave/0 a pseudoll1CodeSave/0.
*/

modeSave:-
        eFirstSave,
        ll1CodeSave,
        pseudoll1CodeSave.

%------------------------------------------------------------------------------
/**     modeLoad
Text:   Zavede ze soubor� v aktu�ln� adres��i data ulo�en� do
        datab�ze pro zrychlen� pr�ce v m�dech ll1/4 a pseudoll1/4
        - odpov�d� pou�it� eFirstLoad/0, ll1CodeLoad/0 a pseudoll1CodeLoad/0.
*/

modeLoad:-
        eFirstLoad,
        ll1CodeLoad,
        pseudoll1CodeLoad.

%------------------------------------------------------------------------------
%                               FIRST set
%------------------------------------------------------------------------------
/** eFirstSave
Text:   Ulo�� vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        eFirst/3 do souboru 'pc_first.pl'.
*/

eFirstSave:-
        eFirstSave('pc_first.pl').

/** eFirstSave(+FileName)
Text:   Ulo�� vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        eFirst/3 do souboru FileName.
*/

eFirstSave(FileName):-
        openFile(FileName,O,write),
         eFirstShow,
        closeFile(O,write).

%------------------------------------------------------------------------------
/** eFirstLoad
Text:   Zavede vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        eFirst/3 ze souboru 'pc_first.pl'.
*/

eFirstLoad:-
        eFirstLoad('pc_first.pl').

/** eFirstLoad(+FileName)
Text:   Zavede vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        eFirst/3 ze souboru FileName.
*/

eFirstLoad(FileName):-
        [FileName].

%------------------------------------------------------------------------------
%                                   ll1
%------------------------------------------------------------------------------
/** ll1CodeShow
Text:   Vyp�e vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        ll1Code/3.
*/

ll1CodeShow:-
        nl,write('%  ll1Code data:'),
        ll1CodeShow_,
        nl.
ll1CodeShow_:-
        listing(ll1Code),
        fail.
ll1CodeShow_.

%------------------------------------------------------------------------------
/** ll1CodeSave
Text:   Ulo�� vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        ll1Code/3 do souboru 'pc_ll1.pl'.
*/

ll1CodeSave:-
        ll1CodeSave('pc_ll1.pl').

/** ll1CodeSave(+FileName)
Text:   Ulo�� vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        ll1Code/3 do souboru FileName.
*/

ll1CodeSave(FileName):-
        openFile(FileName,O,write),
         ll1CodeShow,
        closeFile(O,write).

%------------------------------------------------------------------------------
/** ll1CodeLoad
Text:   Zavede vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        ll1Code/3 ze souboru 'pc_ll1.pl'.
*/

ll1CodeLoad:-
        ll1CodeLoad('pc_ll1.pl').

/** ll1CodeLoad(+FileName)
Text:   Zavede vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        ll1Code/3 ze souboru FileName.
*/

ll1CodeLoad(FileName):-
        [FileName].

%------------------------------------------------------------------------------
%                                   pseudoll1
%------------------------------------------------------------------------------
/** pseudoll1CodeShow
Text:   Vyp�e vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        pseudoll1Code/3.
*/

pseudoll1CodeShow:-
        nl,write('% pseudoll1Code data:'),
        pseudoll1CodeShow_,
        nl.
pseudoll1CodeShow_:-
        listing(pseudoll1Code),
        fail.
pseudoll1CodeShow_.

%------------------------------------------------------------------------------
/** pseudoll1CodeSave
Text:   Ulo�� vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        pseudoll1Code/3 do souboru 'pc_pll1.pl'.
*/

pseudoll1CodeSave:-
        pseudoll1CodeSave('pc_pll1.pl').

/** pseudoll1CodeSave(+FileName)
Text:   Ulo�� vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        pseudoll1Code/3 do souboru FileName.
*/

pseudoll1CodeSave(FileName):-
        openFile(FileName,O,write),
         pseudoll1CodeShow,
        closeFile(O,write).

%------------------------------------------------------------------------------
/** pseudoll1CodeLoad
Text:   Zavede vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        pseudoll1Code/3 ze souboru 'pc_pll1.pl'.
*/

pseudoll1CodeLoad:-
        pseudoll1CodeLoad('pc_pll1.pl').

/** pseudoll1CodeLoad(+FileName)
Text:   Zavede vypo�ten� hodnoty Empty a FIRST parser� ulo�en� ve struktu�e
        pseudoll1Code/3 ze souboru FileName.
*/

pseudoll1CodeLoad(FileName):-
        [FileName].

%- EOF ------------------------------------------------------------------------
