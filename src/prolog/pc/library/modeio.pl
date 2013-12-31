%------------------------------------------------------------------------------
%
%                                  Mode IO
%
%				Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/**     Module:
Text:   V módech ll1/pseudoll1 jsou za bìhu vytváøeny rozkladové
        tabulky. V selektorech tìchto módù lze urèit, zda se má
        kód, kterými jsou tabulky reprezentovány ukládat do databáze.
p               V pøípadì, ¾e je ukládání povoleno, lze tento kód
        nejen upravovat, prohlí¾et a roz¹iøovat, ale rovnì¾ ukládat
        pro pozdìj¹í pou¾ití. K tìmto operacím jsou urèeny
        predikáty definované v tomto souboru.
*/
%------------------------------------------------------------------------------
%                       eFirst / ll1 / pseudoll1
%------------------------------------------------------------------------------
/**     modeShow
Text:   Vypí¹e do standardního výstupního proudu data ulo¾ená do
        databáze pro zrychlení práce v módech ll1/4 a pseudoll1/4
        - eFirst/0, ll1Code/0 a pseudoll1Code/0.
*/

modeShow:-
        eFirstShow,
        ll1CodeShow,
        pseudoll1CodeShow.

%------------------------------------------------------------------------------
/**     modeSave
Text:   Zapí¹e do souborù v aktuální adresáøi data ulo¾ená do
        databáze pro zrychlení práce v módech ll1/4 a pseudoll1/4
        - odpovídá pou¾ití eFirstSave/0, ll1CodeSave/0 a pseudoll1CodeSave/0.
*/

modeSave:-
        eFirstSave,
        ll1CodeSave,
        pseudoll1CodeSave.

%------------------------------------------------------------------------------
/**     modeLoad
Text:   Zavede ze souborù v aktuální adresáøi data ulo¾ená do
        databáze pro zrychlení práce v módech ll1/4 a pseudoll1/4
        - odpovídá pou¾ití eFirstLoad/0, ll1CodeLoad/0 a pseudoll1CodeLoad/0.
*/

modeLoad:-
        eFirstLoad,
        ll1CodeLoad,
        pseudoll1CodeLoad.

%------------------------------------------------------------------------------
%                               FIRST set
%------------------------------------------------------------------------------
/** eFirstSave
Text:   Ulo¾í vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        eFirst/3 do souboru 'pc_first.pl'.
*/

eFirstSave:-
        eFirstSave('pc_first.pl').

/** eFirstSave(+FileName)
Text:   Ulo¾í vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        eFirst/3 do souboru FileName.
*/

eFirstSave(FileName):-
        openFile(FileName,O,write),
         eFirstShow,
        closeFile(O,write).

%------------------------------------------------------------------------------
/** eFirstLoad
Text:   Zavede vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        eFirst/3 ze souboru 'pc_first.pl'.
*/

eFirstLoad:-
        eFirstLoad('pc_first.pl').

/** eFirstLoad(+FileName)
Text:   Zavede vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        eFirst/3 ze souboru FileName.
*/

eFirstLoad(FileName):-
        [FileName].

%------------------------------------------------------------------------------
%                                   ll1
%------------------------------------------------------------------------------
/** ll1CodeShow
Text:   Vypí¹e vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
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
Text:   Ulo¾í vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        ll1Code/3 do souboru 'pc_ll1.pl'.
*/

ll1CodeSave:-
        ll1CodeSave('pc_ll1.pl').

/** ll1CodeSave(+FileName)
Text:   Ulo¾í vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        ll1Code/3 do souboru FileName.
*/

ll1CodeSave(FileName):-
        openFile(FileName,O,write),
         ll1CodeShow,
        closeFile(O,write).

%------------------------------------------------------------------------------
/** ll1CodeLoad
Text:   Zavede vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        ll1Code/3 ze souboru 'pc_ll1.pl'.
*/

ll1CodeLoad:-
        ll1CodeLoad('pc_ll1.pl').

/** ll1CodeLoad(+FileName)
Text:   Zavede vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        ll1Code/3 ze souboru FileName.
*/

ll1CodeLoad(FileName):-
        [FileName].

%------------------------------------------------------------------------------
%                                   pseudoll1
%------------------------------------------------------------------------------
/** pseudoll1CodeShow
Text:   Vypí¹e vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
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
Text:   Ulo¾í vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        pseudoll1Code/3 do souboru 'pc_pll1.pl'.
*/

pseudoll1CodeSave:-
        pseudoll1CodeSave('pc_pll1.pl').

/** pseudoll1CodeSave(+FileName)
Text:   Ulo¾í vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        pseudoll1Code/3 do souboru FileName.
*/

pseudoll1CodeSave(FileName):-
        openFile(FileName,O,write),
         pseudoll1CodeShow,
        closeFile(O,write).

%------------------------------------------------------------------------------
/** pseudoll1CodeLoad
Text:   Zavede vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        pseudoll1Code/3 ze souboru 'pc_pll1.pl'.
*/

pseudoll1CodeLoad:-
        pseudoll1CodeLoad('pc_pll1.pl').

/** pseudoll1CodeLoad(+FileName)
Text:   Zavede vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        pseudoll1Code/3 ze souboru FileName.
*/

pseudoll1CodeLoad(FileName):-
        [FileName].

%- EOF ------------------------------------------------------------------------
