%------------------------------------------------------------------------------
%
%                                  Mode
%
%				Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/**     Module:
Text:   Parsery lze spou¹tìt v rùzných módech. Ke zvolení módu se
        pou¾ívají selektory. Pou¾ití selektorù umo¾òuje spou¹tìní
        jednoho parseru v rùzných re¾imech. V nich pak parser napøíklad
        pracuje výhradnì deterministicky, udr¾uje za bìhu informace o pozici
        ve vstupu nebo provádí svoji vlastní analýzu. Soubor obsahuje
        servisní predikáty módù.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
% Selektory rezimu:
% s(String)
%       ... vstupem parseru je prologovsky retezec String. Tento selektor
%           muze byt zanorovan do selektoru vyssi urovne - viz hierarchie.
%                       
% quickS(String)
%       ... zrychlena verze selektoru s/1. quickS *nesmi* byt vnorovan
%           do selektoru z vyhledem. Obecne
%           ho neni ani dobre vnorovat do jinych selektoru. Je navrzen
%           tak, aby byl schopen co nejrychleji prijimat prologovsky
%           retezec - pouziva "hooky" (napr. nezpracovava vstup po polozkach,
%           ale odebira cele casti, v nekterych pripadech nepouziva ani item).
%           Napr. po vnoreni do ll1/pseudoll1/lookAhead se stava token
%           nefunkcnim.
%                       
% file(Handle,Pos)
%       ... vstupem parseru je soubor. Handle na soubor je ziskan napriklad 
%           volanim openStream/4, Pos je pozice v souboru, kterou lze 
%           inicilizovat predikatem atStream/2.
%                       
% quickFile(Handle,Pos)
%       ... analogie selektoru quickS pro soubory. *Nesmi* byt vnorovan
%           do selektoru z vyhledem (pouziva "hooky" - token) 
%                       
% filter(Handle)
%       ... vstupem parseru je soubor. Handle na soubor je ziskan napriklad 
%           volanim openStream/4. Pozor, v tomto modu jsou podporovany pouze
%           nektere parsery a konstruktory a to se znacnymi omezenimi, to vse
%           navic v zavislosti na pouzitem interpretu. Pro praci se soubory 
%           je proto lepsi pouzit nektery z vyse uvedenych modu (Eof).
%
% line(Line,Sel)
%       ... selektor 1. urovne, ktery nese v Line informaci o aktualnim 
%           radku ve vstupu. Sel muze napriklad byt napriklad selektor
%           file - podrobne viz odstavec hierarchie.
%                       
% lineCol(Line,Col,Sel)
%       ... selektor 1. urovne, ktery udrzuje informaci o aktualnim radku 
%           a sloupci ve vstupu. Sel muze byt selektor dle hierarchie.
%           Projekci rovnez jako col.
%                       
% off(Offset,Sel)
%       ... selektor, ktery udrzuje informaci o aktualnim offsetu ve vstupu.
%           Sel dle hierarchie
%
% encode(TermTable,Sel)
%       ... mod ktery provadi prekodovavani na urovni polozek vstupniho textu
%           pomoci predikatu TermTable:
%                  termTable(Arg1, ..., ArgN, +Char, -EncChar)
%                       
% alt(Alt,Sel)
%       ... ovlivnuje globalne chovani konstruktoru alternativni kompozice.
%            Options:   Alt
%               Alt: <ndet,part>
%                       Pokud se Alt unifikuje s part, varianty konstruktoru
%               alternativni kompozice  <: a :> zustavaji beze zmeny.
%                       Pokud se Alt unifikuje s ndet, vsechny varianty
%               konstruktoru alternativni kompozice jsou nahrazeny <:>.
%               Priklad:
%                       eFirst(ndet,Sel)
%
% empty
%       ... selektor pro staticky vypocet schopnosti prijimat prazdny
%           retezec. Parser vyvolany s timto selektorem vyda misto
%           struktury LOS bud atom true nebo false, podle toho, zda prijima
%           resp. neprijima epsilon.
%                       
% first
%       ... selektor pro staticky vypocet mny FIRST parseru. Misto LOS
%           je vydan seznam predikatu [Cond1,...,CondN]. Prvkem mny
%           FIRST je potom znak, ktery splnuje alespon jednu z podminek
%           Condi.
%                       
% eFirst
%       ... vypocet schopnosti prijimat epsilon a mny FIRST. Vysledek je
%           vydan ve strukture eFirst(Empty,[Cond1,...,CondN]), kde
%           Empty E {true,false} a Condi je podminka.
%                       
% eFirst(Options)
%       ... konfigurovatelna varianta eFirst, volby maji nasledujici
%           format:
%                       
%            Options:   DBF
%               DBF: <assert>
%                       Pokud se DBF unifikuje s assert, uklada vypoctene 
%               vysledky do databaze. Ty lze pouzit pro zrychleni vypoctu. 
%               Ziskana data, lze ulozit do souboru pomoci eFirstSave/0
%               a nasledne vyuzivat jak pri dalsim vypoctu, tak pri behu
%               parseru.
%               Priklad:
%                       eFirst(assert)
%                       
% lookAhead(LookAhead,Sel)
%       ... selektor udrzujici v LookAhead vyhled ve vstupu. Inicializace
%           musi byt provedena explicitne - napriklad pomoci getLookAhead/3. 
%           Sel muze byt selektor dle hierarchie. Vyuziti pri 
%           nedeterministickem rozkladu s pouzitim vyhledu pro zrychleni.
%                       
% ll1(Options,LookAhead,FOLLOW,Sel)
%       ... selektor, ktery pro LL(1) gramatiky zajisti provadeni rozkladu 
%           algorimem deterministicke analyzy. Zasobnik FOLLOW je
%           inicializovan na [], LookAhead explicitne jako u selektoru
%           lookAhead, Sel muze byt selektor dle hierarchie. Vypocet mny 
%           FOLLOW je provaden "on demand" s pomoci zasobniku, ktery obsahuje
%           stopu vypoctu kombinatoru sekvencni kompozice.
%
%            Options:   Algorithm^Dbf^First
%               Algorithm: <lazy,early>
%                       Urcuje zpusob vypoctu mnoziny FOLLOW.
%               Dbf: <assert,off>
%                       Pokud se DBF unifikuje s assert, uklada vypoctene 
%               vysledky do databaze. Ty lze pouzit pro zrychleni vypoctu. 
%               Vytvorena data, lze ulozit do souboru a nasledne vyuzivat 
%               jak pri dalsim vypoctu, tak pri behu parseru.
%                       Jestlize je pouzit atom off, databaze se nevyuziva.
%               First: <cond,enum,bst,set>
%                       Urcuje zpusob reprezentace mnozin FIRST v environmentu
%               konstruktoru <::> cond znaci seznamem podminek (vstupem jsou 
%               termy), enum vycet ASCII kodu, bst binarni vyhledavaci 
%               strom ASCII kodu.
%               Priklad:
%                       ll1(lazy^assert^bst,LookAhead,Follow,Sel)
%                       
% pseudoll1(Options,LookAhead,FOLLOW,Sel)
%       ... selektor, ktery pro LL(1) gramatiky zajisti provadeni rozkladu 
%           algorimem deterministicke analyzy tam, kde je to mozne. V pripade
%           vyskytu kolize v mnozinach FIRST a s FOLLOW je schopen se pres 
%           tyto nejednoznacnosti navracet. Zasobnik FOLLOW je
%           inicializovan na [], LookAhead explicitne jako u selektoru
%           lookAhead, Sel muze byt selektor dle hierarchie. Vypocet mny 
%           FOLLOW je provaden "on demand" s pomoci zasobniku, ktery obsahuje
%           stopu vypoctu kombinatoru sekvencni kompozice.
%               Lazy varianta pouziva pro vypocet mnoziny Follow zasobnik
%           parseru, zatimco early varianta pracuje s prologovskym seznamem
%           podminek (follow stack je tedy nezapouzdrena cond reprezentace).
%           Stejnym zpusobem je reprezentovana i mnozina FOLLOW ve strukture
%           eFirst/3 ukladane do databaze.
%
%            Options:   Algorithm^Follow^Dbf^First
%               Algorithm: <lazy,early>
%                       Urcuje zpusob vypoctu mnoziny FOLLOW.
%               Follow: <useFOLLOW,noFOLLOW>
%                       noFollow zakazuje vypocet mny FOLLOW (rychlejsi, ale
%               vice nejednoznacnosti skryvajici vypocet), useFollow naopak 
%               provadi vypocet mny FOLLOW tak jako je tomu v ll1.
%               Dbf: <assert,off>
%                       Pokud se DBF unifikuje s assert, uklada vypoctene 
%               vysledky do databaze. Ty lze pouzit pro zrychleni vypoctu. 
%               Vytvorena data, lze ulozit do souboru a nasledne vyuzivat 
%               jak pri dalsim vypoctu, tak pri behu parseru.
%                       Jestlize je pouzit atom off, databaze se nevyuziva.
%               First: <cond,enum,bst,set>
%                       Urcuje zpusob reprezentace mnozin FIRST v envronmentu
%               konstruktoru <::>.
%               cond znaci seznamem podminek (vstupem jsou termy), enum
%               vycet ASCII kodu, bst binarni vyhledavaci strom ASCII kodu.
%               Priklad:
%                       pseudoll1(noFollow^lazy^assert^cond,LookAhead,Follow,Sel)
%

% Hierarchie selektoru:
% Relace ->:   X -> Y ... X muze byt vnoreno v Y
%
%       viz text prace
%
% Priklad:
%       off(Offset, colLine(Col, Line, lookAhead(A, s("this is string"))))
%
%------------------------------------------------------------------------------
% Poznamky:
% - aby pracovaly rezimy korektne, je zadouci, aby se seznamem uspesnych
%   rozkladu pracovaly vyhradne konstruktory k tomu urcene (tj. neni vhodne
%   do nich zasahovat "natvrdo"). Napriklad:
%
%    hlxStringA(I+[N>A]):-       -->    hlxStringA(W):-
%     hlxString(I+[N>S]).                W :-> hlxString <@ string2Atom.
%
%   Parser nalevo v modu empty,first,eFirst... selze, protoze nelze unifikovat
%   seznam uspesnych rozkladu se seznamem na prave strane. U parseru napravo
%   uz funguje vse korektne.
%       Pri hledani techto chyb je vhodne pouzivat debugger, ktery provadi
%   automatickou detekci selhani parseru (verbose pro lepsi lokalizaci):
%
%       ?- deBugger(eFirst+L :-> hlxStringA,[verbose,goals,display]).
%
%------------------------------------------------------------------------------
/**     modeOptionIs(?Option,?Value)
Text:   Globální volby pro módy parserù.
*/

% reprezentace mnoziny FIRST ve strukture eFirst:
% - nezapouzdreny cond

% reprezentace mnoziny FOLLOW:
% - lazy : seznam parseru
% - early: nezapouzdreny cond
    
% reprezentace mnozin FIRST ve environmentu konstruktoru <::>:
firstEnv modeOptionIs enum.             % (cond,enum,set,bst)

% zpusob vypoctu mnoziny FOLLOW (global):
followAlgorithm modeOptionIs lazy.      % (lazy,early)

% ll1/pseudoll1 pouziti databaze:
dbf modeOptionIs assert.                % (assert,off)

% mode pseudoll1 & FOLLOW (global):
optionalFollow modeOptionIs useFOLLOW.  % (useFOLLOW,noFOLLOW)

foo modeOptionIs nothing.               % E klauzule

%------------------------------------------------------------------------------
/**     showPcModeOptions
Text:   Globální nastavení módù, viz také pcStatistic/0.
*/

showPcModeOptions:-
        printf([nl,'Mode options: ',nl]),
        showPcModeOptions_,nl.

showPcModeOptions_:-
        modeOptionIs(X,Y), (X\=foo,printf([X,'=',Y,' ']) ; true),
        fail.
showPcModeOptions_.

%------------------------------------------------------------------------------
/**     pcSelector(?AllowedSelector)
Text:   Mechanismem navracení vydá prologovský seznam knihovních
        selektorù tj. selektorù, které nevyu¾ívají implicitní klauzule
        item - jsou explictinì implementovány v rámci této procedury.
*/

pcSelector(s).
pcSelector(file).
pcSelector(quickS).
pcSelector(quickFile).
pcSelector(off).
pcSelector(line).
pcSelector(lineCol).
pcSelector(alt).
pcSelector(filter).
pcSelector(lookAhead).
pcSelector(empty).
pcSelector(first).
pcSelector(eFirst).
pcSelector(ll1).
pcSelector(pseudoll1).

pcSelector(noname).             % E klauzule

%------------------------------------------------------------------------------
/**     pcAlienSelector(+Input)
Text:   Uspìje, pokud selektor Input je knihovní tj. nevyu¾ívá implicitní
        klauzule item, ale je o¹etøen explicitní klauzulí v rámci
        této procedury.
*/

pcAlienSelector(I):-
        functor(I,F,_),pcSelector(F) -> fail ; true.

%------------------------------------------------------------------------------
/**     pcSelectorError(+Parser, +Input)
Text:   V pøípadì, ¾e Input pou¾ívá selektor, který není knihovní, je vypsáno
        chybové hlá¹ení.
Arg:    Parser
        Funktor nebo jiná identifikace parseru, ve kterém do¹lo k chybì.
*/

pcSelectorError(P,I):-
        functor(I,F,_),pcSelector(Ds),member(F,Ds)
        -> deBug(warn,['(',P,') Warning: Mode selector ',I,' without treatment.',nl])
        ;  pcError(error,['Unknown mode selector ',I,' in (',P,').',nl]).

%------------------------------------------------------------------------------
/**     pcGetInput(+Descr, -Input)
Text:   Vybalí ze selektorù vstup tj. handle na soubour nebo øetìzec.
*/

pcGetInput(quickS(I),I).
pcGetInput(quickFile(I),I).
pcGetInput(s(I),I).
pcGetInput(file(I),I).
pcGetInput(ll1(_,_,_,I),II):-
        pcGetInput(I,II).
pcGetInput(pseudoll1(_,_,_,I),II):-
        pcGetInput(I,II).
pcGetInput(off(_,I),II):-
        pcGetInput(I,II).
pcGetInput(line(_,I),II):-
        pcGetInput(I,II).
pcGetInput(lineCol(_,_,I),II):-
        pcGetInput(I,II).
pcGetInput(S,s("")):-
        deBugAssert(fail,['Unknown selector in:',nl,S,nl,tab,' pcGetInput/2',nl,nl]).

%------------------------------------------------------------------------------
/**     pcShowPosition(+Stream, +Descr)
Text:   Vybalí ze selektorù módu v¹echny informace o pozici a vypí¹e
        je do výstupního proudu Stream.
*/

pcShowPosition(Stream,I):-
        pcGetPosition(I,Term),
        telling(Old),tell(Stream),
         printf(Term),
        told,tell(Old).

%------------------------------------------------------------------------------
/**     pcGetPosition(+Descr, -PosTerm)
Text:   Vybalí ze selektorù módu informace o pozici ve vstupu a
        vrátí je ve formì termu.
*/

pcGetPosition(ll1(_,_,_,I),P):-
        pcGetPosition(I,P).
pcGetPosition(pseudoll1(_,_,_,I),P):-
        pcGetPosition(I,P).
pcGetPosition(off(Offset,I),['(offset ',Offset,')'|P]):-
        pcGetPosition(I,P).
pcGetPosition(line(Line,I),['(line ',Line,')'|P]):-
        pcGetPosition(I,P).
pcGetPosition(lineCol(Line,Column,I),['(line:Column ',Line,':',Column,')'|P]):-
        pcGetPosition(I,P).
pcGetPosition(quickS(_),[]).
pcGetPosition(quickFile(_),[]).
pcGetPosition(s(_),[]).
pcGetPosition(file(_),[]).

%------------------------------------------------------------------------------
/**     getLookAhead(+Input,-LookAhead,-StartInput)
Text:   Predikát pro inicializaci výhledu pou¾ívaného pøi deterministickém 
        rozkladu.
*/

getLookAhead(I,LA,Ii):-
        item(I+L),
        (L=[Ii>LA] -> true ; LA=pcEpsilon,Ii=I).

%------------------------------------------------------------------------------
/**     endOfInput(+Input)
Text:   Uspìje, pokud se parser nachází na konci vstupního prologovského
        øetìzce.
*/

pcEndOfInput(DescrI):-
        pcGetInput(DescrI,I) -> (I=[];I=file(_);I=quickFile(_)).

%------------------------------------------------------------------------------
%                               FIRST set
%------------------------------------------------------------------------------
/** eFirst(?Parser, +Empty, +FIRST)
Text:   Struktura do ní¾ jsou ukládána data získaná v modech pracujících
        s epsilon a mno¾inou FIRST.
*/
% eFirst(-Foo, -Foo, -Foo)
% - foo zaznam dat, obsahujicich Empty a FIRST informace o parserech.
%   Zajistuje, aby eFirstGet/3 nezpusobil chybu volanim neexistujiciho
%   predikatu.
% - eFirst(Parser, Empty, FIRST)
%   - Empty E {true,false} ... dle schopnosti prijimat epsilon
%   - FIRST                ... prologovsky seznam podminek (cond) odpovidajici
%                              FIRST(Parser)

eFirst(followEofParser,true,[==([pcEpsilon])]).     % pro FOLLOW v ll1/pseudoll1
eFirst(*,fail,[]).

%------------------------------------------------------------------------------
% eFirstGet(+Parser, -Empty, -FIRST)
% - pokud jsou v databazi hodnoty Empty a FIRST, ktere byly pro Parser
%   vypocteny jiz drive, ziska je z ni. V opacnem pripade provede uplny
%   vypocet techto hodnot *bez* pridani do databaze.

eFirstGet(P,Empty,FIRST):-
         eFirst(P,Empty,FIRST),!
          ;
         eFirst+eFirst(Empty,FIRST) :-> P.

%------------------------------------------------------------------------------
% eFirstGetPut(+Parser, -Empty, -FIRST)
% - pokud jsou v databazi hodnoty Empty a FIRST, ktere byly pro Parser
%   vypocteny jiz drive, ziska je z ni. V opacnem pripade provede vypocet
%   techto hodnot s pomoci obsahu databaze. Vysledek prida do databaze.

eFirstGetPut(P,Empty,FIRST):-
        eFirst(P,Empty,FIRST),!
         ;
        eFirst(assert)+eFirst(Empty,FIRST) :-> P,
        eFirstAdd(P,Empty,FIRST),!.                    % pridani do databaze

%------------------------------------------------------------------------------
% eFirstAdd(+Parser, +Empty, +FIRST)
% - prida do databaze jiz vypoctenou informaci o Empty a FIRST parseru Parser

eFirstAdd(H,Empty,FIRST):-
        eFirst(H,Empty,FIRST),!
        %-> deBug(eFirst,[' (Already on place) ',nl])
        ;
        optimizeCondEnumBst(cond(FIRST),cond(OFIRST)),
        deBug(eFirst,['-f-> ',H,' -> ',OFIRST,nl]),
        assert(eFirst(H,Empty,OFIRST)).

%------------------------------------------------------------------------------
% eFirstGoal(Goal,Var,Var)
% - goal v optimalizavanych cond reprezentacich
% - Var je handle na promennou v tele cile

eFirstGoal(Goal,Var,Var):-
        Goal.

%------------------------------------------------------------------------------
/** eFirstShow
Text:   Vypí¹e vypoètené hodnoty Empty a FIRST parserù ulo¾ené ve struktuøe
        eFirst/3.
*/

eFirstShow:-
        nl,write('% eFirst data:'),
        eFirstShow_,
        nl.
eFirstShow_:-
        listing(eFirst),
        fail.
eFirstShow_.

%------------------------------------------------------------------------------
%                                ll1 <::>
%------------------------------------------------------------------------------
/** ll1Code(?ParserAlternative, -ParserTableAlternative)
Text:   Struktura do ní¾ jsou ukládána bìhová data módu ll1/4.
*/
% ll1Code(-Foo, -Foo)
% - foo zaznam dat zajistuje, aby ll1Code/3 nezpusobil chybu volanim 
%   neexistujiciho predikatu.

ll1Code(foo,foo).

%------------------------------------------------------------------------------
%                                pseudoll1 <::>
%------------------------------------------------------------------------------
/** pseudoll1Code(?ParserAlternative, -ParserTableAlternative)
Text:   Struktura do ní¾ jsou ukládána bìhová data módu pseudoll1/4.
*/
% pseudoll1Code(-Foo, -Foo)
% - foo zaznam dat zajistuje, aby pseudoll1Code/3 nezpusobil chybu volanim 
%   neexistujiciho predikatu.

pseudoll1Code(foo,foo).

%------------------------------------------------------------------------------
%                               FIRST set Aux
%------------------------------------------------------------------------------
% firstSetFitCond(+CondList, +Element)
% - reprezentace seznamem podminek (cond)
% - overi zda vyhled Element splnuje nekterou z podminek v seznamu
% - firstSetFitCond([],E) ... fail

firstSetFitCond([C|T],E):-
        :-@ [C,[E]] ; firstSetFitCond(T,E).

%------------------------------------------------------------------------------
% firstSetTrue(+Term)
% - predikat pouzivany ve FIRST reprezentovane mnou vyjimek, ktery
%   akceptuje libovolny znak

firstSetTrue(_).

%------------------------------------------------------------------------------
% genCompleteFirstSet(+Number, -NumberList)
% - vygeneruje uplnou mnu FIRST tj. mnu obsahujici znaky s ASCII kody
%   <1,Number>

% common
genCompleteFirstSet(256,[pcEpsilon, 255, 254, 253, 252, 251, 250, 249, 248, 247, 246, 245, 244, 243, 242, 241, 240, 239, 238, 237, 236, 235, 234, 233, 232, 231, 230, 229, 228, 227, 226, 225, 224, 223, 222, 221, 220, 219, 218, 217, 216, 215, 214, 213, 212, 211, 210, 209, 208, 207, 206, 205, 204, 203, 202, 201, 200, 199, 198, 197, 196, 195, 194, 193, 192, 191, 190, 189, 188, 187, 186, 185, 184, 183, 182, 181, 180, 179, 178, 177, 176, 175, 174, 173, 172, 171, 170, 169, 168, 167, 166, 165, 164, 163, 162, 161, 160, 159, 158, 157, 156, 155, 154, 153, 152, 151, 150, 149, 148, 147, 146, 145, 144, 143, 142, 141, 140, 139, 138, 137, 136, 135, 134, 133, 132, 131, 130, 129, 128, 127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 116, 115, 114, 113, 112, 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]).
genCompleteFirstSet(255,[pcEpsilon, 255, 254, 253, 252, 251, 250, 249, 248, 247, 246, 245, 244, 243, 242, 241, 240, 239, 238, 237, 236, 235, 234, 233, 232, 231, 230, 229, 228, 227, 226, 225, 224, 223, 222, 221, 220, 219, 218, 217, 216, 215, 214, 213, 212, 211, 210, 209, 208, 207, 206, 205, 204, 203, 202, 201, 200, 199, 198, 197, 196, 195, 194, 193, 192, 191, 190, 189, 188, 187, 186, 185, 184, 183, 182, 181, 180, 179, 178, 177, 176, 175, 174, 173, 172, 171, 170, 169, 168, 167, 166, 165, 164, 163, 162, 161, 160, 159, 158, 157, 156, 155, 154, 153, 152, 151, 150, 149, 148, 147, 146, 145, 144, 143, 142, 141, 140, 139, 138, 137, 136, 135, 134, 133, 132, 131, 130, 129, 128, 127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 116, 115, 114, 113, 112, 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]).
genCompleteFirstSet(128,[pcEpsilon, 127, 127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 116, 115, 114, 113, 112, 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0]).
genCompleteFirstSet(127,[pcEpsilon, 127, 127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 116, 115, 114, 113, 112, 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 100, 99, 98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88, 87, 86, 85, 84, 83, 82, 81, 80, 79, 78, 77, 76, 75, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]).
genCompleteFirstSet(1,  [pcEpsilon, 1]).

genCompleteFirstSet(I,[I|T]):-
        dec(I,Ii),genCompleteFirstSet(Ii,T).

%------------------------------------------------------------------------------
% genItemLangLos_(+Number, +NotParser, -LOS)
% - vytvori seznam uspesnych rozkladu obsahujici derivace tvaru
%   NotParsed>X, kde X je z <1,Number>. Vysledna struktura LOS tedy
%   obsahuje Number polozek

genItemLangLos_(I,N,[N>I|T]):-
        I>1 -> dec(I,Ii),genItemLangLos_(Ii,N,T).
genItemLangLos_(1,N,[N>1]).

%- EOF ------------------------------------------------------------------------
