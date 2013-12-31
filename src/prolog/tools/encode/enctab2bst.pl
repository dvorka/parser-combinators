%------------------------------------------------------------------------------
%
%           	      Generovani tabulek pro mod encode
%
%                             Martin Dvorak
%                                 2000
%------------------------------------------------------------------------------
% Komentar:
%       Generovani tabulek pro mod encode ve forme binarnich vyhledavacich
% stromu. Vstupem je tabulka ve forme prologovskeho seznamu. Odpovidajici si
% znaky maji v jednotlivych seznamech stejnou pozici. Nejdrive se tedy
% rozsiri seznam o klice identifikujici pozici elementu a teprve potom je
% vygenerovan strom. Hledani probiha tak, ze se pro vstupni znak
% najde nejprve klic, pomoci toho se v dalsim BST vyhleda znak vystupni.
%------------------------------------------------------------------------------
% Inicializace & knihovny

:- ['../../loadSWI'].          % SWI Prolog
%:- ['../../loadBP-2'].        % BinProlog

%------------------------------------------------------------------------------
%                                Demo
%------------------------------------------------------------------------------

go:-
        openFile('table.bst',Old,write),
         goCycleChar,goCycleKey,
        closeFile(Old,write).

goCycleChar:-
        encodeTable(TableName,Table),encodeTab2Bst(Table,CharBst,_),
        printf([encodeBstChar(TableName,CharBst),'.',nl]),
        fail.
goCycleChar.
goCycleKey:-
        encodeTable(TableName,Table),encodeTab2Bst(Table,_,KeyBst),
        printf([encodeBstKey(TableName,KeyBst),'.',nl]),
        fail.
goCycleKey.

%------------------------------------------------------------------------------
%                             encodeTab2Bst/2
%------------------------------------------------------------------------------

encodeTab2Bst(Table,CharBst,KeyBst):-
        encodeTab2BstAddKeys(1,Table,CharTable,KeyTable),        % pridani klicu
        list2Bst(CharTable,CharBst),list2Bst(KeyTable,KeyBst),!. % bst

% encodeTab2BstAddKeys(+Key,+Original,+SearchByChar,+SearchByKey):-
encodeTab2BstAddKeys(Key,[H|T],[[H,Key]|TT],[[Key,H]|TTT]):-
        K is Key+1,
        encodeTab2BstAddKeys(K,T,TT,TTT).
encodeTab2BstAddKeys(_,[],[],[]).

%------------------------------------------------------------------------------
%                               Tabulky
%------------------------------------------------------------------------------

encodeTable(latin2,    " ŸÔ‚Ø¡å¢ıçœ£…ì§µ¬Ò·ÖÕàüæ›éŞí¦").
encodeTable(win1250,   "áèïéìíòóøšúùıÁÈÏÉÌÍÒÓØŠÚÙİ").
encodeTable(iso8859_2, "áèïéìíòóø¹»úùı¾ÁÈÏÉÌÍÒÓØ©«ÚÙİ®").
encodeTable(txt,       "acdeeinorstuuyzACDEEINORSTUUYZ").
encodeTable(kam,       " ‡ƒ‚ˆ¡¤¢©¨Ÿ£–˜‘€…‰‹¥•›†——’").

%- EOF ------------------------------------------------------------------------
