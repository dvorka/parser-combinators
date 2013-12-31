%------------------------------------------------------------------------------
%
%                                  Invoke
%
%                               Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/**     Module: invoke
Text:   Predik�ty ur�en� k vyvol�v�n� parser� v r�zn�ch m�dech. Dal��
        podrobnosti viz 'mode.pl'.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     invokeString(+Parser, +String, -LOS)
Text:   Vyvol�n� parseru v m�du quickS/1.
*/

invokeString(Parser,String,LOS):-
 quickS(String)+LOS
        :-> Parser.

%------------------------------------------------------------------------------
/**     invokeLoadString(+Parser, +InputFile, -LOS)
Text:   Vyvol�n� parseru v m�du quickS/1. Vstupn� �et�zec je zaveden
        ze souboru InputFile.
*/

invokeLoadString(Parser,File,LOS):-
        loadFile(File,Content),
          quickS(Content)+LOS
                :-> Parser.

%------------------------------------------------------------------------------
/**     invokeFile(+Parser, +InputFile, -LOS)
Text:   Vyvol�n� parseru v m�du quickFile/1.
*/

invokeFile(Parser,File,LOS):-
        openStream(File,Handle,OldHandle,read),
         atStream(Handle,Pos),
          quickFile(Handle,Pos)+LOS
                :-> Parser,
          closeStream(OldHandle,read).

%------------------------------------------------------------------------------
/**     invokeFilter(+Parser, +InputFile, +OutputFile, -LOS)
Text:   Vyvol�n� parseru, kter� je schopen pracovat jako filtr.
        InputFile je soubor, ze kter�ho je vstup na��t�n a OutputFile
        je soubor, do kter�ho je p�esm�rov�n standardn� v�stupn� proud 
        parseru Parser. Atomy 'InputFile' a 'OutputFile' musej� b�t r�zn�.
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
Text:   Vyvol�n� parseru v m�du, kdy se udr�uje informace o offsetu
        ve zpracov�van�m textu. Vhodn� pro o�ez�v�n� (<^>/2)
        a pro lokalizace chyb v m�dech ll1/4 a pseudoll1/4.
Arg:    Input
        Deskriptor vstupu.
*/

invokeOffset(Parser,Input,L):-
        off(0,Input)+L :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokeLine(+Parser, +Input, -LOS)
Text:   Vyvol�n� parseru v m�du, kdy se udr�uje informace o aktu�ln�m
        ��dku ve zpracov�van�m textu. Vhodn� pro o�ez�v�n� (<^>/2)
        a pro lokalizace chyb v m�dech ll1/4 a pseudoll1/4.
*/

invokeLine(Parser,Input,L):-
        line(1,Input)+L :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokeLineCol(+Parser, +Input, -LOS)
Text:   Vyvol�n� parseru v m�du, kdy se udr�uje informace o aktu�ln�m
        ��dku a sloupci ve zpracov�van�m textu. Vhodn� pro o�ez�v�n� (<^>/2)
        a pro lokalizace chyb v ll1/4 m�dech.
*/

invokeLineCol(Parser,Input,L):-
        lineCol(1,1,Input)+L :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokeEmpty(+Parser, -Emptyness)
Text:   Vyvol�n� parseru v m�du empty/0. V prom�nn� Emptyness je vyd�n
        atom true pr�v� kdy� Parser p�ij�m� pr�zdn� vstup, jinak atom false.
*/

invokeEmpty(Parser,Emptyness):-
 empty+Emptyness :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokeFIRST(+Parser, -FirstSet)
Text:   Vyvol�n� parseru v m�du first/0. V�sledkem je mna FIRST(Parser).
*/

invokeFIRST(Parser,FirstSetConds):-
 first+FirstSetConds :->
                        Parser.
        

%------------------------------------------------------------------------------
/**     invokeEmptyFIRST(+Parser, -Emptyness, -FirstSet)
Text:   Vyvol�n� parseru v m�du eFirst/1. Obsah prom�nn� Emptyness informuje
        o schopnosti parseru p�ij�mat pr�zdn� vstup, FirstSet obsahuje
        mnu FIRST(Parser).
*/

invokeEmptyFIRST(Parser,Empty,FirstSetConds):-
 eFirst(assert)+eFirst(Empty,FirstSetConds) :->
                        Parser.

%------------------------------------------------------------------------------
/**     invokeLL1(+Alg, +Dbf, +Fst, +Pos, +P, +I, -Los)
Text:   Vyvol�n� parseru P v m�du ll1/4. Rozklad je pro LL(1) jazyky
        prov�d�n algoritmem deterministick� anal�zy. Deterministi�nost
        samotn�ho parseru je ov��ov�na za b�hu. V p��pad� chyby je na
        standardn� v�stup vyps�na informace o ��dku a sloupci resp. offsetu,
        kde do�lo k chyb�.
Arg:    Alg
        Algoritmus v�po�tu dynamick� FOLLOW - atomy 'lazy' nebo 'early'.
Arg:    Dbf
        Vkl�dn� do datab�ze - atomy 'assert' nebo 'off'.
Arg:    Fst
        Reprezentace mno�in FIRST v okol� kombin�toru <::> - atomy
        'cond', 'enum', 'set' nebo 'bst'.
Arg:    Pos
        Atom 'offset' ur�uje udr�ov�n� o pozici ve vstupu ve form� offsetu,
        atom 'lineCol' ve form� termu obsahuj�c�ho aktu�ln� sloupec
        a ��dek a 'line' ��dek. Pou��v� se nap��klad pro lokalizaci chyb.
Arg:    I
        Vno�en� selektor.
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
Text:   Je prov�d�n "pseudodeterministick� v�po�et". P�i n�m je rozklad
        zrychlov�n tam, kde je to mo�n�, pou��v�n�m v�hled�. Ve zbyl�ch
        p��padech je chov�n� parseru nedeterministick�.
Arg:    Alg
        Algoritmus v�po�tu dynamick� FOLLOW - atomy 'lazy' nebo 'early'.
Arg:    Opt
        Pou��v�n� dynamick� FOLLOW - atomy 'useFOLLOW' nebo 'noFOLLOW'.
Arg:    Dbf
        Vkl�dn� do datab�ze - atomy 'assert' nebo 'off'.
Arg:    Fst
        Reprezentace mno�in FIRST v okol� kombin�toru <::> - atomy
        'cond', 'enum', 'set' nebo 'bst'.
Arg:    Pos
        Atom 'offset' ur�uje udr�ov�n� o pozici ve vstupu ve form� offsetu,
        atom 'lineCol' ve form� termu obsahuj�c�ho aktu�ln� sloupec
        a ��dek a 'line' ��dek. Pou��v� se nap��klad pro lokalizaci chyb.
Arg:    P
        Parser.
Arg:    I
        Vno�en� selektor.
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
