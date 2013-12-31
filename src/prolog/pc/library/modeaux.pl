%------------------------------------------------------------------------------
%
%                                  Mode Aux
%
%				Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/**     Module:
Text:   Pomocné predikáty pro módy parserù.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                             Aux eFirst
%------------------------------------------------------------------------------
/**     convertCondEnumBst(+Rep, +In, -Out)
Text:   Konverze mezi reprezentacemi mno¾iny FIRST - cond/1, enum/1 a bst/1
        (ve struktuøe eFirst/3 se pou¾ívá výhradnì reprezentace cond/1).
Arg:    Repr
        Cílová reprezentace: cond/enum/bst.
*/
% Reprezentace:
%  cond( [C1, ... ,CN])     ... podminky jsou predikaty, ktere lze volat,
%                               testovany vyhled je *jednoprvkovy seznam*:
%                                   lElementOf([1,2,3],*[2]*)
%  enum( [7, 10, ..., 97])
%  bst ( bst(nil,10,nil))
% 3^2

% cond
convertCondEnumBst(enum,cond(I),enum(O)):-
        genCompleteFirstSet(255,L),                 % vsechny znaky
        filter(firstSetFitCond(I),L,O).             % filtrace
convertCondEnumBst(bst,cond(I),bst(O)):-
        genCompleteFirstSet(255,L),                 % vsechny znaky
        filter(firstSetFitCond(I),L,ListOfChars),   % filtrace
        list2Bst(ListOfChars,O).
% enum
convertCondEnumBst(cond,enum(A),cond(AA)):-
        (A=[X] -> AA=[==([X])] ; AA=[lElementOf(A)]).
convertCondEnumBst(bst,enum(I),bst(O)):-
        list2Bst(I,O).
% bst
convertCondEnumBst(cond,bst(I),cond(lElementOf(O))):-
        bst2List(infix,I,O).
convertCondEnumBst(enum,bst(I),enum(O)):-
        bst2List(infix,I,O).

% neutral
convertCondEnumBst(cond,cond(R),cond(R)).
convertCondEnumBst(enum,enum(R),enum(R)).
convertCondEnumBst(bst,bst(R),bst(R)).
convertCondEnumBst(bst,nil,nil).

%------------------------------------------------------------------------------
/**     findCondEnumBst(+FIRST, +LookAhead)
Text:   Hledání výhledu v reprezentacích mno¾iny FIRST - cond/1, enum/1 a bst/1.
        Pou¾ívá se v predikátu findInFOLLOW/2 pro hledání výhledu v mnì
        FOLLOW. Pokud není výhled nalezen, predikát selhává.
*/

findCondEnumBst(cond([C|T]),LA):-       % podminky porovnavaji proti [LA]
        :-@ [C,[LA]] ; findCondEnumBst(cond(T),LA).
findCondEnumBst(enum(X),LA):-
        member(LA,X).
findCondEnumBst(bst(X),LA):-
        bstMember(LA,X).

%------------------------------------------------------------------------------
/**     findInFOLLOW(+Algorithm,+Dbf,+LookAhead, +FOLLOW)
Text:   Hledání výhledu v mnì FOLLOW, predikát sel¾e nebo uspìje.
Arg:    Algorithm
        Zpùsob výpoètu mno¾iny FOLLOW - early/lazy.
Arg:    Dbf
        Pou¾ít databázi - assert/off.
*/

% early
% - stack je pri early vypoctu reprezentovan jako cond/1 - seznam podminek
findInFOLLOW(early,_,Stack,LA):-
        findCondEnumBst(cond(Stack),LA).

% lazy
% - Stack je seznam parseru -> je nutne ziskat mny FIRST a v nich existenci
%   overit. Nejrychlejsi je ziskavani v reprezentaci cond/1 z eFirst.
findInFOLLOW(lazy,D,Stack,LA):-
        findInLazyFOLLOW(D,Stack,LA).

findInLazyFOLLOW(D,[P|T],LA):- 
        (D=assert                           % vypocet FOLLOW pro TOS pomoci eFirst/3
         -> eFirstGetPut(P,Empty,CondList)
         ;  eFirstGet(P,Empty,CondList)),
        (findInFOLLOW(early,D,CondList,LA)  % pokus o overeni
          ;                                 % pokud je TOS pruhledny -> POP and again
          (Empty=true                       % transparent
           -> findInLazyFOLLOW(D,T,LA))).
                                            % jinak fail
%------------------------------------------------------------------------------
/**     optimizeCondEnumBst(+FIRST, -OptFIRST)
Text:   Optimalizace reprezentací mny FIRST - cond/1, enum/1 a bst/1.
*/
% Nejvice se osvedcil pruchod a posbirani podminek typu ==([]), lElementOf([])
% do jedineho rance a vytvoreni jedne lElementOf([]).

optimizeCondEnumBst(cond(C),cond(O)):-
        optimizeCond(C,O).

optimizeCondEnumBst(enum(B),enum(O)):-
        sort(B,O).                              % odstraneni duplicit

optimizeCondEnumBst(bst(B),bst(B)).

%------------------------------------------------------------------------------
% optimizeCond/2
% - viz komentar predchoziho predikatu

optimizeCond(C,CC):-
        optimizeCondMkList(C,A,R),
        (A=[] -> CC=R ; (A=[X] -> CC=[==([X])|R] ; sort(A,AA),CC=[lElementOf(AA)|R])).

% optimizeCondMkList(+CondList, -AsciiList, -CondListRest)
optimizeCondMkList([==([A])|T],[A|AA],TT):-
        optimizeCondMkList(T,AA,TT).
optimizeCondMkList([lElementOf(C)|T],AA,TT):-
        optimizeCondMkList(T,A,TT),
        append(C,A,AA).
optimizeCondMkList([H|T],A,[H|TT]):-
        optimizeCondMkList(T,A,TT).
optimizeCondMkList([],[],[]).

%------------------------------------------------------------------------------
%                               Aux <::>
%------------------------------------------------------------------------------
/**     convertCondEnumBstFIRST(+Repr, +In, -Out)
Text:   Konverze mezi reprezentacemi mno¾in pou¾ívaných v <::> pro výbìr
        alternativy dle výhledu - condFIRST/1, enumFIRST/1, setFIRST/1
        a bstFIRST/1.
Arg:    Repr
        Cílová reprezentace: cond/enum/bst pro condFIRST/enumFIRST/bstFIRST/1.
*/
% - Reprezentace:
%    condFIRST( [[[pcTrue,==(1)],P1]])
%    enumFIRST( [[1,P1],[2,P1],[3,P2]])
%    setFIRST(  [[[1,2],P1],[[3,5],P2],[[7,8],P2]])
%    bstFIRST/1(  bst(bst(nil,[1,P1],nil),[2,P1],bst(nil,[3,P2],nil)))
% - !Pozor! pouze slepe koverze (neprovadi se faktorizace a tedy ani
%   check resp. join jako v optFactorizeByFIRST), plati pro vsechny
%   reprezentace.

% enum ->
convertCondEnumBstFIRST(cond,enumFIRST(I),condFIRST(O)):-
        convertEnum2Set(I,S),
        convertCondEnumBstFIRST(cond,setFIRST(S),condFIRST(O)).

convertCondEnumBstFIRST(set, enumFIRST(I),setFIRST(O)):-
        convertEnum2Set(I,O).

convertCondEnumBstFIRST(bst, enumFIRST(I),bstFIRST(O)):-
        list2Bst(I,O).

% set ->
convertCondEnumBstFIRST(cond,setFIRST([[A,P]|T]),condFIRST([[AA,P]|TT])):-
        (A=[X] -> AA=[==([X])] ; AA=[lElementOf(A)]),
        convertCondEnumBstFIRST(cond,setFIRST(T),condFIRST(TT)).
convertCondEnumBstFIRST(cond,setFIRST([]),condFIRST([])).

convertCondEnumBstFIRST(enum,setFIRST(I),enumFIRST(O)):-
        convertSet2CrumbledEnum(I,O-[]).

convertCondEnumBstFIRST(bst,setFIRST(I),bstFIRST(O)):-
        convertSet2CrumbledEnum(I,M-[]),
        list2Bst(M,O).

% bst ->
convertCondEnumBstFIRST(cond,bstFIRST(I),condFIRST(O)):-
        bst2List(infix,I,L),
        convertCondEnumBstFIRST(cond,enumFIRST(L),condFIRST(O)).

convertCondEnumBstFIRST(enum,bstFIRST(I),enumFIRST(O)):-
        bst2List(infix,I,O).

convertCondEnumBstFIRST(set, bstFIRST(I),setFIRST(O)):-
        bst2List(infix,I,M),
        convertEnum2Set(M,O).

% cond ->
convertCondEnumBstFIRST(enum,condFIRST(C), enumFIRST(Crumbled)):-
        convertCond2CrumbledEnum(C,Crumbled-[]).

convertCondEnumBstFIRST(set,condFIRST([[C,P]|T]),setFIRST([[L,P]|TT])):-
        convertCondEnumBst(enum,cond(C),enum(L)),
        convertCondEnumBstFIRST(set,condFIRST(T),setFIRST(TT)).
convertCondEnumBstFIRST(set,condFIRST([]),setFIRST([])).

convertCondEnumBstFIRST(bst,condFIRST(C), bstFIRST(B)):-
        convertCond2CrumbledEnum(C,Crumbled-[]),
        list2Bst(Crumbled,B).

% neutral
convertCondEnumBstFIRST(cond,condFIRST(R),condFIRST(R)).
convertCondEnumBstFIRST(enum,enumFIRST(R),enumFIRST(R)).
convertCondEnumBstFIRST(set,setFIRST(R),setFIRST(R)).
convertCondEnumBstFIRST(bst,bstFIRST(R),bstFIRST(R)).
convertCondEnumBstFIRST(bst,nil,nil).

convertCondEnumBstFIRST(R,_,_):-
        deBugAssert(fail,['convertCondEnumBstFIRST: ',R,' unknown.']).

% ---

% auxiliary predicates
% - rozdrobi cond na enumFIRST
convertCond2CrumbledEnum([[C,P]|T],D-DDD):-
        % prevod seznamu podminek na vycet tj. seznam znaku
        convertCondEnumBst(enum,cond(C),enum(L)),
        convertCond2CrumbledEnum_(L,P,D-DD),
        convertCond2CrumbledEnum(T,DD-DDD).
convertCond2CrumbledEnum([],L-L).

% - jedna polozka seznamu
%   [[1,2,3],P1] -> [1,P1],[2,P1],[3,P1]
convertCond2CrumbledEnum_([H|T],P,[[H,P]|L]-D):-
        convertCond2CrumbledEnum_(T,P,L-D).
convertCond2CrumbledEnum_([],_,D-D).

% ---

% - rozdrobi set na enumFIRST
convertSet2CrumbledEnum([[L,P]|T],D-DDD):-
        convertCond2CrumbledEnum_(L,P,D-DD),
        convertSet2CrumbledEnum(T,DD-DDD).
convertSet2CrumbledEnum([],L-L).

% ---

% enum -> set
% - provadi se slouceni vsech polozek se stejnym parserem -> jsou sjednocovany
%   vyhledy
% - algoritmus:
%    1) [H|T] ... slouceni podle parseru v H -> ziska se:
%               HH ... hlava sloucena se vsemi se stejnym parserem
%               TT ... telo s vyrezanymi polozkami, ktere byly slouceny
%    2) [H|T]=[HH|TT] goto 1, dokud neni TT prazdny seznam

convertEnum2Set([[LA,P]|T],[Factor|Factors]):-
        convertEnum2SetFactorize([[LA],P],T,Factor,Rest),
        convertEnum2Set(Rest,Factors).
convertEnum2Set([],[]).

% convertEnum2SetFactorize(+Temp,+T,-TempFactor,-TRest)
% - Arg:
%       Temp  ... vzor podle ktereho se slucuje, vyhled je jiz v seznamu
%       T     ... zpracovavany seznam
%       TempFactor ... vzor slouceny s bratricky
%       TRest      ... T bez bratricku
% - pruchodem T jsou k Temp pripojeny vsechny vyhledy s nimiz ma T
%   shodny parser. Vysledny TempFactor je vydan ve tretim
%   argumentu, nepripojene polozky v argumentu poslednim.

convertEnum2SetFactorize([TLA,TP],[[LA,P]|T],Factor,Rest):-
        P=TP
         -> convertEnum2SetFactorize([[LA|TLA],P],T,Factor,Rest)
         ;  convertEnum2SetFactorize([TLA,TP],T,Factor,R),
            Rest=[[LA,P]|R].
convertEnum2SetFactorize(Factor,[],Factor,[]).

%------------------------------------------------------------------------------
% convertCondEnumBstFIRST(+Opt, +Joiner, +Repr, +In, -Out)
% - sofistikovanejsi verze, ktera provadi kondenzaci resp. konrolu dle voleb
% - condFIRST -> enumFIRST
% - Opt:
%    Atom 'join' pro slucovani (pseudoll1) nebo 'check' (ll1) pro kontroly
%    disjukce
% - Join:
%    Predikat pro slucovani jako <:^>, <:^ nebo :>^
convertCondEnumBstFIRST(Opt,Joiner,enum,condFIRST(C),enumFIRST(E)):-
        optFactorizeByFIRST(Opt,Joiner,condFIRST(C),enumFIRST(E)).

% convertCondEnumBstFIRST(+Opt, +Joiner, +Repr, +In, -Out)
% - condFIRST -> setFIRST
convertCondEnumBstFIRST(Opt,Joiner,set,condFIRST(C),setFIRST(S)):-
        optFactorizeByFIRST(Opt,Joiner,condFIRST(C),enumFIRST(E)),
        convertEnum2Set(E,S).

% convertCondEnumBstFIRST(+Opt, +Joiner, +Repr, +In, -Out)
% - condFIRST -> bstFIRST
convertCondEnumBstFIRST(Opt,Joiner,bst,condFIRST(C),bstFIRST(B)):-
        optFactorizeByFIRST(Opt,Joiner,condFIRST(C),enumFIRST(E)),
        list2Bst(E,B).

%------------------------------------------------------------------------------
/**     findCondEnumBstFIRST(+Env, +LookAhead, -Parser)
Text:   Vyhledá vhodný parser podle výhledu v okolí predikátu
        <::> ve strukturách condFIRST/1, enumFIRST/1, setFIRST/1 a bstFIRST/1. Pokud
        nelze pro výhled LookAhead vhodný parser, je vydán atom pcEmpty.
*/

findCondEnumBstFIRST(enumFIRST(FIRST),LA,Parser):-
        member([LA,Parser],FIRST) ; Parser=pcEmpty.
findCondEnumBstFIRST(bstFIRST(FIRST),LA,Parser):-
        findBstFIRST(FIRST,LA,Parser) ; Parser=pcEmpty.
findCondEnumBstFIRST(condFIRST([[C,P]|T]),LA,Parser):-
        findCondEnumBst(cond(C),LA)
         -> Parser=P
         ;  findCondEnumBstFIRST(condFIRST(T),LA,Parser).
findCondEnumBstFIRST(condFIRST([]),_,pcEmpty).
findCondEnumBstFIRST(setFIRST([[L,P]|T]),LA,Parser):-
        member(LA,L)
         -> Parser=P
         ;  findCondEnumBstFIRST(setFIRST(T),LA,Parser).
findCondEnumBstFIRST(setFIRST([]),_,pcEmpty).

findBstFIRST(bst(_,[LA,P],_),LA,P).
findBstFIRST(bst(L,[Val,_],R),LA,P):-
        LA@<Val -> findBstFIRST(L,LA,P) ; findBstFIRST(R,LA,P).

%------------------------------------------------------------------------------
/**     findAllCondEnumBstFIRST(+Env, +LookAhead, -Parser)
Text:   Vyhledá v¹echny vhodné parsery podle výhledu v okolí
        predikátu <::> ve strukturách condFIRST/1, enumFIRST/1, setFIRST/1
        a bstFIRST/1.
        Pokud nelze pro výhled nalézt ¾ádný parser, je vydán prázdný seznam.
*/

findAllCondEnumBstFIRST(condFIRST([[C,P]|T]),LA,Parsers):-
        (firstSetFitCond(C,LA) -> Parsers=[P|Ps] ; Parsers=Ps),
        findAllCondEnumBstFIRST(condFIRST(T),LA,Ps).
findAllCondEnumBstFIRST(condFIRST([]),_,[]).
findAllCondEnumBstFIRST(setFIRST([[L,P]|T]),LA,Parsers):-
        (member(LA,L) -> Parsers=[P|Ps] ; Parsers=Ps),
        findAllCondEnumBstFIRST(setFIRST(T),LA,Ps).
findAllCondEnumBstFIRST(setFIRST([]),_,[]).
findAllCondEnumBstFIRST(enumFIRST(E),LA,Ps):-
        findall(P,member([LA,P],E),Ps).
findAllCondEnumBstFIRST(bstFIRST(E),LA,Ps):-
        findAllBstFIRST(E,LA,Ps) ; Ps=[].

% Navic overit, zda ve sbirani nelze pokracovat...
findAllBstFIRST(bst(L,[LA,P],R),LA,Ps):-
        (findAllBstFIRST(L,LA,PL) -> P1=PL ; P1=[]),
        (findAllBstFIRST(R,LA,PR) -> append(P1,[P|PR],Ps) ; Ps=[P|P1]).
findAllBstFIRST(bst(L,[Val,_],R),LA,P):-
        LA@<Val -> findAllBstFIRST(L,LA,P) ; findAllBstFIRST(R,LA,P).

%------------------------------------------------------------------------------
/**     env2First(+FirstEnv, -First)
Text:   Vygeneruje z reprezentací enumFIRST, condFIRST/1, setFIRST/1, bstFIRST/1
        z okolí kombinátoru <::> mno¾inu FIRST a vydá ji ve
        struktuøe cond/1.
*/
% - vystupni reprezentace je *seznam* podminek (viz eFirst/3)

env2First(condFIRST(L),FIRST):-
        collectCondFirst(L,FIRST).           % za behu efektivnejsi
env2First(enumFIRST(L),[lElementOf(FIRST)]):-
        collectEnumFirst(L,FIRST).           % za behu efektivnejsi
env2First(setFIRST(L),[lElementOf(FIRST)]):-
        collectSetFirst(L,FIRST).           % za behu efektivnejsi
env2First(bstFIRST(L),[lElementOf(FIRST)]):-
        collectBstFirst(L,FIRST-[]).         % za behu efektivnejsi

collectCondFirst([[C,_]|T],CCC):-
        collectCondFirst(T,CC),
        append(C,CC,CCC).
collectCondFirst([],[]).

collectSetFirst([[L,_]|T],LLL):-
        collectSetFirst(T,LL),
        append(L,LL,LLL).
collectSetFirst([],[]).

collectEnumFirst([[C,_]|T],[C|Cs]):-
        collectEnumFirst(T,Cs).
collectEnumFirst([],[]).

collectBstFirst(nil,D-D).
collectBstFirst(bst(L,[C,_],R),LL-DR):-
        collectBstFirst(L,LL-[C|LR]),
        collectBstFirst(R,LR-DR).

%------------------------------------------------------------------------------
%                        Shallow backtrack <:> linker
%------------------------------------------------------------------------------
/**     shallowAltLinker(Repr,Mode,Parser,OptimizedParser)
Text:   Predikát pou¾ívaný za bìhu v módech ll1/4 a pseudoll1/4 pro
        mìlké zanoøení v rámci kombinátoru alternativní kompozice
        za úèelem vytvoøení rozkladové tabulky.
p       V pøípadì módu ll1/4 se provede zanoøení pøes v¹echny typy
        kombinátorù alternativní kompozice, pøi nìm je ovìøena
        disjunktnost mno¾in FIRST jednotlivých alternativ. V pøípadì
        výskytu chyby se výpoèet zastaví.
p       V módu pseudoll1/4 je mìlké zanoøení provádìno pouze v rámci
        jedné varianty kombinátoru alternativní kompozice. Navíc
        jsou v pøípadì výskytu kolize alternativy slouèeny (pomocí
        aktuální varianty kombinátoru).
Arg:    Repr
        Cílová reprezentace okolí konstruktoru <::>. Pøípustné
        jsou atomy 'enum', 'set', 'bst' nebo 'cond'.
Arg:    Mode
        Cílový mód pro který má optimalizace probìhnout - 'pseudoll1'
        nebo 'll1'.
*/
% shallowAltLinker(Mode,Representation,Parser,LinkedParser)
% - predikat, ktery se pouziva za behu v modech deterministicke analyzy
%   pro nejdulezitejsi vec - transformace a kontroly altertiv
% - slinkuje slozitejsi alternativni kompozice -> nerekurzivni optimalizace
%   (rekurze pouze pres kombinatory alternativni kompozice) a vytvori <::>
%   variantu
% - Parametry: 
%    Representation: <cond,set,enum,bst>
%       Zpusob reprezentace mnozin first ve vyhledavacim environmentu
%    Mode: <ll1,pseudoll1>
% - algoritmus:
%    viz optimize.pl
% - vkladani do databaze ve strukturach
%    ll1Code(KeyParser,<::>(Environment))
%    pseudoll1Code(KeyParser,<::>(Environment))

shallowAltLinker(Representation,Mode,Parser,OptimizedParser):-
        shallowRep(Representation,Mode,Parser,OptimizedParser)
         ;
        pcError(error,['shallowAltLinker: parser is not alternative: ',nl,Parser,nl,nl]),trace.

%------------------------------------------------------------------------------
% Na rozdil od optRep/3 jsou u melke optimalizace osetreny pouze alternativy
% kombinatoru <:, :> a <:> tj. neprovadi se rekurzivni optimalzace.

% ll1:
shallowRep(R,ll1,<:(LP,RP),Linked):-
        shallowRepLinker(R,ll1,<:(LP,RP),Linked).
shallowRep(R,ll1,:>(LP,RP),Linked):-
        shallowRepLinker(R,ll1,:>(LP,RP),Linked).
shallowRep(R,ll1,<:>(LP,RP),Linked):-
        shallowRepLinker(R,ll1,<:>(LP,RP),Linked).
% pseudoll1:
shallowRep(R,pseudoll1,<:(LP,RP),Linked):-
        shallowRepLinker(R,pseudoll1,<:(LP,RP),Linked).
shallowRep(R,pseudoll1,:>(LP,RP),Linked):-
        shallowRepLinker(R,pseudoll1,:>(LP,RP),Linked).
shallowRep(R,pseudoll1,<:>(LP,RP),Linked):-
        shallowRepLinker(R,pseudoll1,<:>(LP,RP),Linked).

% explicitne neosetrene parsery tvori listy
shallowRep(_,_,P,P).

%------------------------------------------------------------------------------
% shallowRepShallowLinker(R,ll1,AltComposition,<::>(Environ)):-

% ll1
shallowRepLinker(R,ll1,AltComposition,<::>(Environ)):-
        % rekurzivni zpracovani parseru pod urovni <:> v optGetAlts
        optGetAlts(ll1,shallowRep(R,ll1),AltComposition,ParserList),
         optAlts2FstAlts(ParserList,CondFIRST),   % zapouzdrena condFIRST
          % cond reprezentace neni slucovana      % joiner se nepouziva
          (R=cond -> optFactorizeCond(check,ghost,CondFIRST,Factorized)      % out condFIRST
                  ;  optFactorizeByFIRST(check,ghost,CondFIRST,Factorized)), % out enumFIRST
           convertCondEnumBstFIRST(R,Factorized,Environ). % -> do cilove reprezentace

% pseudoll1 - varianty dle jednotlivych linkeru (<:^>) & spol.)
shallowRepLinker(R,pseudoll1,AltComposition,<::>(Environ)):-
        optGetAlts(pseudoll1,shallowRep(R,pseudoll1),AltComposition,ParserList),
         optAlts2FstAlts(ParserList,CondFIRST),
          (AltComposition= <:>(LP,RP) -> Joiner= <:^> ;
           AltComposition= <:(LP,RP)  -> Joiner= <:^  ;
           AltComposition= :>(LP,RP)  -> Joiner= :>^  ),
          (R=cond -> optFactorizeCond(join,Joiner,CondFIRST,Factorized)      % out condFIRST
                  ;  optFactorizeByFIRST(join,Joiner,CondFIRST,Factorized)), % out enumFIRST
           convertCondEnumBstFIRST(R,Factorized,Environ).

%------------------------------------------------------------------------------
%                                   Debug
%------------------------------------------------------------------------------
% Motto:
%  ... environmenty - cteni stredecni noci ...
%------------------------------------------------------------------------------
% pseudoShowMan(<::>(Environment))

pseudoShowMan(<::>(condFIRST(Env))):-
        printf([nl,'VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV']),
        pseudoShowManEnv(Env),
        printf([   '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^',nl,nl]).
pseudoShowMan(_).

pseudoShowManEnv([[C,P]|T]):-
        printf([nl,'Cond:',nl,' ',C,nl]),
        printf([nl,'Parser:',nl,' ']),display(P),nl,
        pseudoShowManEnv(T).
pseudoShowManEnv([]).

%------------------------------------------------------------------------------
% ll1DeBug(+Parser,+ParserEpsilon,+L,+PE)
% - debug pro <::>/2

ll1DeBug(Parser,ParserEpsilon,L,PE):-
        pseudoShowMan(Parser),
        deBug(debug,[nl,
                '-Parser: ',nl,Parser,nl,
                '-ParserEpsilon: ',nl,ParserEpsilon,nl,
                '-LOS: ',nl,L,nl,
                '-PE: ',nl,PE,nl,nl
               ]).

%- EOF ------------------------------------------------------------------------
