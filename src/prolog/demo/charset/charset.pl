%------------------------------------------------------------------------------
%
%                      Demonstrace pouziti modu 'encode'
%                          Prekodovani znakovych sad
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module: 
Text:   Jednoduchá ukázka pou¾ití módu 'encode' na pøíkladu pøekódovávání
        znakových sad.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
% Komentar:
%       Priklad pracuje s pripravenymi predikaty a strukturami knihovny.
% Ukazuje, jak vyuzit uzivatelsky mod 'encode' (pc/library/adhoc.pl) pro
% prekodovavani znaku vstupniho textu, drive nez prejdou do vyssich vrstev, 
% napr. pred aplikaci semantickych akci. Muze se tak stat prvnim filtrem,
% podrobnosti o moznostech jeho kombinovani lze nalezt v pc/library/mode.pl.
%       Mod je pripraveny - zbyva tedy pouze pripravit predikat pro
% prekodovani.
%------------------------------------------------------------------------------
% Inicializace & knihovny

:- ['../../loadSWI'].          % SWI Prolog
%:- ['../../loadBP-2'].        % BinProlog

%------------------------------------------------------------------------------
%                                 Demo
%------------------------------------------------------------------------------

go:-
        loadFile('input.txt',Source),
        printString(Source),nl,
        encode(convertCzEnc(iso8859_2,txt),s(Source))+_
                :-> (item<*> <@ colonePrintString),
        encode(convertCzEnc(iso8859_2,latin2),s(Source))+_
                :-> (item<*> <@ colonePrintString).

%------------------------------------------------------------------------------
%                              Prekodovani
%------------------------------------------------------------------------------
/**     convertCzEnc(+InEnc,+OutEnc,+I,+O)
Text:   Predikát pro pøekódvání èe¹tin v módu 'encode'.
Arg:    InEnc
        Kódování vstupního textu - atomy 'latin2', 'win1250', 'iso8859_2'
        'txt', 'kam'. Pøípadnì dal¹í po doplnìní tabulek.
Arg:    OutEnc
        Kódování výstupního textu - viz parametr InEnc.
Example:
        ?- encode(convertCzEnc(latin2,iso8859_2),file(H))
        |       :-> SomeParser.
*/

convertCzEnc(I,O,C,CC):-
        % Nejdrive pro vstupni znak vyhleda klic a potom dle klice
        % odpovidajici znak vystupni. Tabulka obsahuje pouze znaky, ktere
        % se maji zmenit, ostatni, ktere maji zustat stejne se v tabulce
        % nevyskytuji. 
        encodeBstChar(I,BI),encodeBstKey(O,BO),
        (bstMember([C,Key],BI) -> bstMember([Key,CC],BO) ; C=CC).

/** encodeBstChar(-Name,-Bst)
Text:   Struktura obsahující tabulku v reprezentaci binárního vyhledávacího
        stromu urèená pro vyhledávání podle znaku.
*/

encodeBstChar(latin2, bst(bst(bst(bst(nil, [130, 4], bst(nil, [133, 13], nil)), [144, 19], bst(bst(nil, [155, 26], nil), [156, 11], bst(nil, [159, 2], nil))), [160, 1], bst(bst(bst(nil, [161, 6], nil), [162, 8], bst(nil, [163, 12], nil)), [166, 30], bst(bst(nil, [167, 15], nil), [172, 17], bst(nil, [181, 16], nil)))), [183, 20], bst(bst(bst(bst(nil, [210, 18], nil), [212, 3], bst(nil, [213, 22], nil)), [214, 21], bst(bst(nil, [216, 5], nil), [222, 28], bst(nil, [224, 23], nil))), [229, 7], bst(bst(bst(nil, [230, 25], nil), [231, 10], bst(nil, [233, 27], nil)), [236, 14], bst(bst(nil, [237, 29], nil), [252, 24], bst(nil, [253, 9], nil)))))).
encodeBstChar(win1250, bst(bst(bst(bst(nil, [138, 25], bst(nil, [141, 26], nil)), [142, 30], bst(bst(nil, [154, 10], nil), [157, 11], bst(nil, [158, 15], nil))), [193, 16], bst(bst(bst(nil, [200, 17], nil), [201, 19], bst(nil, [204, 20], nil)), [205, 21], bst(bst(nil, [207, 18], nil), [210, 22], bst(nil, [211, 23], nil)))), [216, 24], bst(bst(bst(bst(nil, [217, 28], nil), [218, 27], bst(nil, [221, 29], nil)), [225, 1], bst(bst(nil, [232, 2], nil), [233, 4], bst(nil, [236, 5], nil))), [237, 6], bst(bst(bst(nil, [239, 3], nil), [242, 7], bst(nil, [243, 8], nil)), [248, 9], bst(bst(nil, [249, 13], nil), [250, 12], bst(nil, [253, 14], nil)))))).
encodeBstChar(iso8859_2, bst(bst(bst(bst(nil, [169, 25], bst(nil, [171, 26], nil)), [174, 30], bst(bst(nil, [185, 10], nil), [187, 11], bst(nil, [190, 15], nil))), [193, 16], bst(bst(bst(nil, [200, 17], nil), [201, 19], bst(nil, [204, 20], nil)), [205, 21], bst(bst(nil, [207, 18], nil), [210, 22], bst(nil, [211, 23], nil)))), [216, 24], bst(bst(bst(bst(nil, [217, 28], nil), [218, 27], bst(nil, [221, 29], nil)), [225, 1], bst(bst(nil, [232, 2], nil), [233, 4], bst(nil, [236, 5], nil))), [237, 6], bst(bst(bst(nil, [239, 3], nil), [242, 7], bst(nil, [243, 8], nil)), [248, 9], bst(bst(nil, [249, 13], nil), [250, 12], bst(nil, [253, 14], nil)))))).
encodeBstChar(txt, bst(bst(bst(bst(nil, [65, 16], bst(nil, [67, 17], nil)), [68, 18], bst(bst(nil, [69, 19], nil), [69, 20], bst(nil, [73, 21], nil))), [78, 22], bst(bst(bst(nil, [79, 23], nil), [82, 24], bst(nil, [83, 25], nil)), [84, 26], bst(bst(nil, [85, 27], nil), [85, 28], bst(nil, [89, 29], nil)))), [90, 30], bst(bst(bst(bst(nil, [97, 1], nil), [99, 2], bst(nil, [100, 3], nil)), [101, 4], bst(bst(nil, [101, 5], nil), [105, 6], bst(nil, [110, 7], nil))), [111, 8], bst(bst(bst(nil, [114, 9], nil), [115, 10], bst(nil, [116, 11], nil)), [117, 12], bst(bst(nil, [117, 13], nil), [121, 14], bst(nil, [122, 15], nil)))))).
encodeBstChar(kam, bst(bst(bst(bst(nil, [128, 17], bst(nil, [130, 4], nil)), [131, 3], bst(bst(nil, [133, 18], nil), [134, 26], bst(nil, [135, 2], nil))), [136, 5], bst(bst(bst(nil, [137, 20], nil), [139, 21], bst(nil, [143, 16], nil)), [144, 19], bst(bst(nil, [145, 15], nil), [146, 30], bst(nil, [149, 23], nil)))), [150, 13], bst(bst(bst(bst(nil, [151, 27], nil), [151, 28], bst(nil, [152, 14], nil)), [155, 25], bst(bst(nil, [157, 29], nil), [158, 24], bst(nil, [159, 11], nil))), [160, 1], bst(bst(bst(nil, [161, 6], nil), [162, 8], bst(nil, [163, 12], nil)), [164, 7], bst(bst(nil, [165, 22], nil), [168, 10], bst(nil, [169, 9], nil)))))).

/** encodeBstKey(-Name,-Bst)
Text:   Struktura obsahující tabulku v reprezentaci binárního vyhledávacího
        stromu urèená pro vyhledávání podle klíèe.
*/

encodeBstKey(latin2, bst(bst(bst(bst(nil, [1, 160], bst(nil, [2, 159], nil)), [3, 212], bst(bst(nil, [4, 130], nil), [5, 216], bst(nil, [6, 161], nil))), [7, 229], bst(bst(bst(nil, [8, 162], nil), [9, 253], bst(nil, [10, 231], nil)), [11, 156], bst(bst(nil, [12, 163], nil), [13, 133], bst(nil, [14, 236], nil)))), [15, 167], bst(bst(bst(bst(nil, [16, 181], nil), [17, 172], bst(nil, [18, 210], nil)), [19, 144], bst(bst(nil, [20, 183], nil), [21, 214], bst(nil, [22, 213], nil))), [23, 224], bst(bst(bst(nil, [24, 252], nil), [25, 230], bst(nil, [26, 155], nil)), [27, 233], bst(bst(nil, [28, 222], nil), [29, 237], bst(nil, [30, 166], nil)))))).
encodeBstKey(win1250, bst(bst(bst(bst(nil, [1, 225], bst(nil, [2, 232], nil)), [3, 239], bst(bst(nil, [4, 233], nil), [5, 236], bst(nil, [6, 237], nil))), [7, 242], bst(bst(bst(nil, [8, 243], nil), [9, 248], bst(nil, [10, 154], nil)), [11, 157], bst(bst(nil, [12, 250], nil), [13, 249], bst(nil, [14, 253], nil)))), [15, 158], bst(bst(bst(bst(nil, [16, 193], nil), [17, 200], bst(nil, [18, 207], nil)), [19, 201], bst(bst(nil, [20, 204], nil), [21, 205], bst(nil, [22, 210], nil))), [23, 211], bst(bst(bst(nil, [24, 216], nil), [25, 138], bst(nil, [26, 141], nil)), [27, 218], bst(bst(nil, [28, 217], nil), [29, 221], bst(nil, [30, 142], nil)))))).
encodeBstKey(iso8859_2, bst(bst(bst(bst(nil, [1, 225], bst(nil, [2, 232], nil)), [3, 239], bst(bst(nil, [4, 233], nil), [5, 236], bst(nil, [6, 237], nil))), [7, 242], bst(bst(bst(nil, [8, 243], nil), [9, 248], bst(nil, [10, 185], nil)), [11, 187], bst(bst(nil, [12, 250], nil), [13, 249], bst(nil, [14, 253], nil)))), [15, 190], bst(bst(bst(bst(nil, [16, 193], nil), [17, 200], bst(nil, [18, 207], nil)), [19, 201], bst(bst(nil, [20, 204], nil), [21, 205], bst(nil, [22, 210], nil))), [23, 211], bst(bst(bst(nil, [24, 216], nil), [25, 169], bst(nil, [26, 171], nil)), [27, 218], bst(bst(nil, [28, 217], nil), [29, 221], bst(nil, [30, 174], nil)))))).
encodeBstKey(txt, bst(bst(bst(bst(nil, [1, 97], bst(nil, [2, 99], nil)), [3, 100], bst(bst(nil, [4, 101], nil), [5, 101], bst(nil, [6, 105], nil))), [7, 110], bst(bst(bst(nil, [8, 111], nil), [9, 114], bst(nil, [10, 115], nil)), [11, 116], bst(bst(nil, [12, 117], nil), [13, 117], bst(nil, [14, 121], nil)))), [15, 122], bst(bst(bst(bst(nil, [16, 65], nil), [17, 67], bst(nil, [18, 68], nil)), [19, 69], bst(bst(nil, [20, 69], nil), [21, 73], bst(nil, [22, 78], nil))), [23, 79], bst(bst(bst(nil, [24, 82], nil), [25, 83], bst(nil, [26, 84], nil)), [27, 85], bst(bst(nil, [28, 85], nil), [29, 89], bst(nil, [30, 90], nil)))))).
encodeBstKey(kam, bst(bst(bst(bst(nil, [1, 160], bst(nil, [2, 135], nil)), [3, 131], bst(bst(nil, [4, 130], nil), [5, 136], bst(nil, [6, 161], nil))), [7, 164], bst(bst(bst(nil, [8, 162], nil), [9, 169], bst(nil, [10, 168], nil)), [11, 159], bst(bst(nil, [12, 163], nil), [13, 150], bst(nil, [14, 152], nil)))), [15, 145], bst(bst(bst(bst(nil, [16, 143], nil), [17, 128], bst(nil, [18, 133], nil)), [19, 144], bst(bst(nil, [20, 137], nil), [21, 139], bst(nil, [22, 165], nil))), [23, 149], bst(bst(bst(nil, [24, 158], nil), [25, 155], bst(nil, [26, 134], nil)), [27, 151], bst(bst(nil, [28, 151], nil), [29, 157], bst(nil, [30, 146], nil)))))).

%- EOF ------------------------------------------------------------------------
