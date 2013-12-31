%------------------------------------------------------------------------------
%
%                                  Invoke
%
%                               Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/**     Module: invoke
Text:   Predikáty urèené k vyvolávání parserù v rùzných módech. Dal¹í
        podrobnosti viz 'mode.pl'.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     invokeString(+Parser, +String, -LOS)
Text:   Vyvolání parseru v módu quickS/1.
*/

invokeString(Parser,String,LOS):-
 quickS(String)+LOS
        :-> Parser.

%------------------------------------------------------------------------------
/**     invokeLoadString(+Parser, +InputFile, -LOS)
Text:   Vyvolání parseru v módu quickS/1. Vstupní øetìzec je zaveden
        ze souboru InputFile.
*/

invokeLoadString(Parser,File,LOS):-
        loadFile(File,Content),
          quickS(Content)+LOS
                :-> Parser.

%------------------------------------------------------------------------------
/**     invokeFile(+Parser, +InputFile, -LOS)
Text:   Vyvolání parseru v módu quickFile/1.
*/

invokeFile(Parser,File,LOS):-
        openStream(File,Handle,OldHandle,read),
         atStream(Handle,Pos),
          quickFile(Handle,Pos)+LOS
                :-> Parser,
          closeStream(OldHandle,read).

%------------------------------------------------------------------------------
/**     invokeFilter(+Parser, +InputFile, +OutputFile, -LOS)
Text:   Vyvolání parseru, který je schopen pracovat jako filtr.
        InputFile je soubor, ze kterého je vstup naèítán a OutputFile
        je soubor, do kterého je pøesmìrován standardní výstupní proud 
        parseru Parser. Atomy 'InputFile' a 'OutputFile' musejí být rùzné.
*/

invokeFilter(Parser,InputFile,OutputFile,LOS):-
        InputFile\==OutputFile,
        openFile(OutputFile,Old,write),
         openStream(InputFile,Handle,OldHandle,read),
          atStream(Handle,Pos),
           quickFile(Handle,Pos)+LOS
                                        :-> Parser,
         closeStream(OldHandle,read),
        closeFile(Old,write).

invokeFilter(_,_,_,_):-
        pcError(error,['Input file and output file are the same.']).

%------------------------------------------------------------------------------
/**     invokeOffset(+Parser, +Input, -LOS)
Text:   Vyvolání parseru v módu, kdy se udr¾uje informace o offsetu
        ve zpracovávaném textu. Vhodné pro oøezávání (<^>/2)
        a pro lokalizace chyb v módech ll1/4 a pseudoll1/4.
Arg:    Input
        Deskriptor vstupu.
*/

invokeOffset(Parser,Input,L):-
        off(0,Input)+L :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokeLine(+Parser, +Input, -LOS)
Text:   Vyvolání parseru v módu, kdy se udr¾uje informace o aktuálním
        øádku ve zpracovávaném textu. Vhodné pro oøezávání (<^>/2)
        a pro lokalizace chyb v módech ll1/4 a pseudoll1/4.
*/

invokeLine(Parser,Input,L):-
        line(1,Input)+L :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokeLineCol(+Parser, +Input, -LOS)
Text:   Vyvolání parseru v módu, kdy se udr¾uje informace o aktuálním
        øádku a sloupci ve zpracovávaném textu. Vhodné pro oøezávání (<^>/2)
        a pro lokalizace chyb v ll1/4 módech.
*/

invokeLineCol(Parser,Input,L):-
        lineCol(1,1,Input)+L :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokeEmpty(+Parser, -Emptyness)
Text:   Vyvolání parseru v módu empty/0. V promìnné Emptyness je vydán
        atom true právì kdy¾ Parser pøijímá prázdný vstup, jinak atom false.
*/

invokeEmpty(Parser,Emptyness):-
 empty+Emptyness :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokeFIRST(+Parser, -FirstSet)
Text:   Vyvolání parseru v módu first/0. Výsledkem je mna FIRST(Parser).
*/

invokeFIRST(Parser,FirstSetConds):-
 first+FirstSetConds :->
                        Parser.
        

%------------------------------------------------------------------------------
/**     invokeEmptyFIRST(+Parser, -Emptyness, -FirstSet)
Text:   Vyvolání parseru v módu eFirst/1. Obsah promìnné Emptyness informuje
        o schopnosti parseru pøijímat prázdný vstup, FirstSet obsahuje
        mnu FIRST(Parser).
*/

invokeEmptyFIRST(Parser,Empty,FirstSetConds):-
 eFirst(assert)+eFirst(Empty,FirstSetConds) :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokeLL1(+Alg, +Dbf, +Fst, +Pos, +P, +I, -Los)
Text:   Vyvolání parseru P v módu ll1/4. Rozklad je pro LL(1) jazyky
        provádìn algoritmem deterministické analýzy. Deterministiènost
        samotného parseru je ovìøována za bìhu. V pøípadì chyby je na
        standardní výstup vypsána informace o øádku a sloupci resp. offsetu,
        kde do¹lo k chybì.
Arg:    Alg
        Algoritmus výpoètu dynamické FOLLOW - atomy 'lazy' nebo 'early'.
Arg:    Dbf
        Vkládní do databáze - atomy 'assert' nebo 'off'.
Arg:    Fst
        Reprezentace mno¾in FIRST v okolí kombinátoru <::> - atomy
        'cond', 'enum', 'set' nebo 'bst'.
Arg:    Pos
        Atom 'offset' urèuje udr¾ování o pozici ve vstupu ve formì offsetu,
        atom 'lineCol' ve formì termu obsahujícího aktuální sloupec
        a øádek a 'line' øádek. Pou¾ívá se napøíklad pro lokalizaci chyb.
Arg:    I
        Vnoøený selektor.
*/
/**     invokeLL1(+Pos,+P,+I,-Los)
Text:   Viz invokeLL1/8.
Example:
        ?- openStream('input.dat',Handle,O,read),
        |   invokeLL1(offset, double, Handle, L).
        |  closeStream(O,read).
*/
/**     invokeLL1(+P,+I,-Los)
Text:   Viz invokeLL1/8.
*/
% Poznamka:
% - volby selektoru ll1/4 jsou podrobne rozebrany v mode.pl

% default
invokeLL1(Parser,I,L):-
        invokeLL1(lineCol,Parser,I,L).

% s formatem specifikace chyby
invokeLL1(Option,Parser,I,L):-                  % (line,lineCol,offset)
        % volby
        followAlgorithm modeOptionIs Algorithm, % (lazy,early)
        dbf             modeOptionIs Dbf,       % (assert, off)
        firstEnv        modeOptionIs First,     % (cond,enum,bst,set)
        % go!
        invokeLL1(Algorithm,Dbf,First,Option,Parser,I,L).

% vse pod kontrolou
invokeLL1(Algorithm,Dbf,First,offset,Parser,I,L):-
        (Algorithm=lazy -> FOLLOW=[followEofParser] ; FOLLOW=[==([pcEpsilon])]),
        getLookAhead(I,LA,Ii),          
        ll1(Algorithm^Dbf^First,LA,FOLLOW,off(0,Ii))+L :->
                        Parser.
invokeLL1(Algorithm,Dbf,First,lineCol,Parser,I,L):-
        (Algorithm=lazy -> FOLLOW=[followEofParser] ; FOLLOW=[==([pcEpsilon])]),
        getLookAhead(I,LA,Ii),          
        ll1(Algorithm^Dbf^First,LA,FOLLOW,lineCol(1,1,Ii))+L :->
                        Parser.
invokeLL1(Algorithm,Dbf,First,line,Parser,I,L):-
        (Algorithm=lazy -> FOLLOW=[followEofParser] ; FOLLOW=[==([pcEpsilon])]),
        getLookAhead(I,LA,Ii),          
        ll1(Algorithm^Dbf^First,LA,FOLLOW,line(1,Ii))+L :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokePseudoLL1(+Alg,+Opt,+Dbf,+Fst,+Pos,+P,+I,-Los)
Text:   Je provádìn "pseudodeterministický výpoèet". Pøi nìm je rozklad
        zrychlován tam, kde je to mo¾né, pou¾íváním výhledù. Ve zbylých
        pøípadech je chování parseru nedeterministické.
Arg:    Alg
        Algoritmus výpoètu dynamické FOLLOW - atomy 'lazy' nebo 'early'.
Arg:    Opt
        Pou¾ívání dynamické FOLLOW - atomy 'useFOLLOW' nebo 'noFOLLOW'.
Arg:    Dbf
        Vkládní do databáze - atomy 'assert' nebo 'off'.
Arg:    Fst
        Reprezentace mno¾in FIRST v okolí kombinátoru <::> - atomy
        'cond', 'enum', 'set' nebo 'bst'.
Arg:    Pos
        Atom 'offset' urèuje udr¾ování o pozici ve vstupu ve formì offsetu,
        atom 'lineCol' ve formì termu obsahujícího aktuální sloupec
        a øádek a 'line' øádek. Pou¾ívá se napøíklad pro lokalizaci chyb.
Arg:    P
        Parser.
Arg:    I
        Vnoøený selektor.
*/
/**     invokePseudoLL1(+Pos,+P,+I,-Los)
Text:   Viz invokePseudoLL1/8.
Example:
        ?- openStream('input.dat',Handle,O,read),
        |   invokePseudoLL1(offset, double, Handle, L).
        |  closeStream(O,read).
*/
/**     invokePseudoLL1(+P,+I,-Los)
Text:   Viz invokePseudoLL1/8.
*/
% Poznamky:
% - volby selektoru pseudoll1/4 jsou podrobne rozebrany v mode.pl

% default
invokePseudoLL1(Parser,I,L):-
        invokePseudoLL1(lineCol,Parser,I,L).

% s formatem specifikace chyby
invokePseudoLL1(Option,Parser,I,L):-            % (line,lineCol,offset)
        % volby
        followAlgorithm modeOptionIs Algorithm, % (lazy,early)
        optionalFollow  modeOptionIs OFollow,   % (useFOLLOW,noFOLLOW)
        dbf             modeOptionIs Dbf,       % (assert, off)
        firstEnv        modeOptionIs First,     % (cond,enum,bst,set)
        % go!
        invokePseudoLL1(Algorithm,OFollow,Dbf,First,Option,Parser,I,L).

% vse pod kontrolou
invokePseudoLL1(Algorithm,OFollow,Dbf,First,offset,Parser,I,L):-
        (Algorithm=lazy -> FOLLOW=[item] ; FOLLOW=[pcTrue]),
        getLookAhead(I,LA,Ii),
        pseudoll1(Algorithm^OFollow^Dbf^First,LA,FOLLOW,off(0,Ii))+L :->
                        Parser.
invokePseudoLL1(Algorithm,OFollow,Dbf,First,line,Parser,I,L):-
        (Algorithm=lazy -> FOLLOW=[item] ; FOLLOW=[pcTrue]),
        getLookAhead(I,LA,Ii),
        pseudoll1(Algorithm^OFollow^Dbf^First,LA,FOLLOW,line(1,Ii))+L :->
                        Parser.
invokePseudoLL1(Algorithm,OFollow,Dbf,First,lineCol,Parser,I,L):-
        (Algorithm=lazy -> FOLLOW=[item] ; FOLLOW=[pcTrue]),
        getLookAhead(I,LA,Ii),
        pseudoll1(Algorithm^OFollow^Dbf^First,LA,FOLLOW,lineCol(1,1,Ii))+L :->
                        Parser.

%- EOF ------------------------------------------------------------------------
