%------------------------------------------------------------------------------
%
%                                  Colone
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Predikáty vhodné pro pou¾ití v kolonách.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     =>(+Predicate1, +Predicate2, +Input, -Output)
Text:   Tento operátor se pou¾ívá v knihovnì PC pro øetìzení sémantických
        akcí - tzv. "kolony". Je nutné, aby øetìzené predikáty mìly tvar:
v              p(?args1,...,?argN, +Input, -Output)
        viz implementace <@/3.
Example:
        ?- s("5")+L :->
        |     (natural <@ *(3) => / ->> 15 => *(30)),
        |        docShow(18,[L]).
        L= [s([])>30]
        Yes
*/
% pouzivani v kolonach viz komentar k predikatu ->>

=>(F1,F2,I,O):-
    :-@ [F1,I,Ot],
    :-@ [F2,Ot,O].

%------------------------------------------------------------------------------
/**     ->>(+Predicate, +Arg, -PredicateWithArg)
Text:   Haskell Curry: "Argumenty pøicházejí po jednom...". Tento
        predikát je pou¾íván pro pøidávání argumentù mechanismem
        curifikace v kolonách. Podrobný popis lze nalézt ve
        zdrojovém textu.
Example:
        ?- s("x")+L :->
        |   (item <@ ascii2Atom)
        |    <@ b ->> bb ->> bbb => c ->> cc ->> ccc.
        L = [s([])>c(b(x, bb, bbb), cc, ccc)]
        Yes
*/
% Pouzivani kolon muze parsery pomerne znacne zefektivnit, prace v sem.
% akcich s operatory => a ->> vsak neni na prvni pohled zrejma, proto
% nize uvedeny popis. Pouzivani techto obratu ma smysl predevsim na prikazove
% radce. Operace, ktere => a ->> umoznuji sebou samozrejme nesou urcitou rezii
% a tak je v kodu lepsi nahradit kolonu predikatem.

% Poznamky:
% - na to, aby byla prace kolon funkci, musite mit zvolenu
%   implementaci predikatu :-@/1 podporujici curifikaci.
%   Muzete tak ucinit v zavadeci knihovny pro danou
%   implementaci tj. v '/pc/srp/prolog/pc/pc*.pl'.
% - ->> ma vyssi precedenci nez =>

% Priklady:
%  * P <@ A
%   - je volani termu A (tj. :-@[A,+LOS,-Result])
%  * P <@ A ->> B
%   - je curryfikace argumentu B k termu A (tj. :-@[A,+LOS,B,-Result])
%  * P <@ A ->> B ->> C
%   - je postupna curryfikace argumentu B a C k termu A
%       *NEJLEVEJSI TERM V RETIZKU JE POVAZOVAN ZA CIL, ZBYLE ZA ARGUMENTY*
%     (tj. :-@[A,+LOS,B,C,-Result])
%  * P <@ A => B
%   - je volani A a vysledek se pripoji jako argument k B
%     (tj. :-@[A,+LOS,-R1],:-@[B,+R1,-Result])
%     je to ekvivalentni chovani (P <@ A) <@ B, ale mnohem efektivnejsi
%     misto n pruchodu LOSu (kde n je pocet <@), vse zvladne v pruchodu jednom
%  * P <@ A => B => C
%   - obaluje puvodni LOS z vnejsku, tj. postupem vpravo se zapouzdruje
%     je to postupne volani cilu A, B, C s predavanim vysledku.
%               *V KAZDEM KROKU POVAZUJE SVUJ LEVY ARGUMENT ZA CIL*
%     (tj. :-@[A,+LOS,-R1],:-@[B,+R1,-R2]),:-@[B,+R2,-Result])
%
%  * P <@ A ->> B ->> C => D                      tj. (A ->> B ->> C) => D
%   - postupna curryfikace argumentu B a C k A. Vysledek se vola jako
%     cil ... viz. vyse
%     (tj. :-@[A,+LOS,B,C,-R1],:-@[D,+R1,-Result])
%
%  * P <@ A ->> B ->> C => D ->> E ->> F
%                                       tj. (A->>B->>C) => (D->>E->>F)
%   - shrnuti: ->> pridavaji argumenty po jednom
%              =>  vola cile
%
% Zaverecne srovnani:
% retec =>
%     (tj. :-@[A,+LOS,-R1],:-@[B,+R1,-R2]),:-@[B,+R1,-Result])
% retec ->>
%     (tj. :-@[A,+LOS,-R1],:-@[+R1,B,-R2]),:-@[+R2,C,-Result])
%
%  X <@ b => bb => bbb          |       X <@ b ->> bb ->> bbb
%  bbb(bb(b(x)))                |       b(x,bb,bbb)
%
% Dodatek:
%       f => a(b1,b2,b3)               ... a(b1,b2,b3,+f(LOS),-Result)
%       f => a(b1,b2,b3) ->> b5 ->> b6 ... a(b1,b2,b3,+f(LOS),b5,b6,-Result)

->>(P,AB,PAB):-
        AB = ->>(A,B)
         -> :-@ [P,A,PA], ->>(PA,B,PAB)
         ;  :-@ [P,AB,PAB].
        
->>(P,AB,L,PAB):-
        AB = ->>(A,B)
         -> :-@ [P,L,A,PA], ->>(PA,B,PAB)
         ;  :-@ [P,L,AB,PAB].

%------------------------------------------------------------------------------
/**     sfx(+Functor,+Arg,-FunctorArg)
Text:   Pøidá k funktoru Functor argument Arg a vytvoøí term FunctorArg.
*/
% - suffix
% - in LPA it's possible directly (without =..)

sfx(F,A,T):-                    % common
        T=..[F,A].

/**     sfx2(+Functor, +Arg1, +Arg2, -FunctorArg)
Text:   Pøidá k funktoru Functor argumenty Arg1 a Arg2. Vznikne tak term
        FunctorArg.
*/

sfx2(F,A,B,T):-                 % usable in foldRs (see demo/ariexpr/ariexprII)
        T=..[F,A,B].

%------------------------------------------------------------------------------
/**     sfxFlip(+Arg, +Functor, -FunctorArg)
Text:   Pøidá k funktoru Functor argument Arg a vytvoøí term FunctorArg.
*/
% - suffix
% - in LPA it's possible directly (without =..)

sfxFlip(F,A,T):-                % usable in foldL
        T=..[A,F].

/** sfxFlip2(+Functor, +Arg2, +Arg1, -FunctorArg)
Text:   Pøidá k funktoru Functor nejdøíve argument Arg1 a pak argument Arg2.
        Tak vznikne term FunctorArg.
*/

sfxFlip2(F,A,B,T):-             % usable in foldLs (see demo/ariexpr/ariexprII)
        T=..[F,B,A].

%------------------------------------------------------------------------------
/**     addArgs(+Term, +ArgList, -TermArgs)
Text:   Pøidá k termu Term argumenty ArgList a vytvoøí nový term TermArgs.
Example:
        ?- addArgs(a(b),[c,d],T).
        T= a(b,c,d)
        Yes
*/

addArgs(T,Al,G):-
        T=..Tl,
        append(Tl,Al,Gl),
        G=..Gl.

%------------------------------------------------------------------------------
/**     addArg(+Term, +Arg, -TermArg)
Text:   Pøidá k termu Term argument Arg a vytvoøí nový term TermArg.
Example:
        ?- addArg(a(b),c,T).
        T= a(b,c)
        Yes
*/

addArg(T,Al,G):-
        T=..Tl,
        append(Tl,[Al],Gl),
        G=..Gl.

%------------------------------------------------------------------------------
%                               Colone contrib
%------------------------------------------------------------------------------
/**     id(+Input,-Input)
Text:   Identita - obsah vstupní promìnné pøedá do výstupní promìnné v
        nezmìnìném tvaru.
*/

id(I,I).   

%------------------------------------------------------------------------------
/**     const(+Constant, +_, -Constant)
Text:   Ignoruje obsah vstupní promìnné a v¾dy vydá ve výstupní promìnné
        term Constant.
*/
% - ignore input and always return given constant (use e.g. with <@ )

const(C,_,C).   

%------------------------------------------------------------------------------
/**     shownl(+Input, -Input)
Text:   Chová se jako identita, navíc vypí¹e do aktuálního výstupního
        proudu term Input následovaný oddìlovaèem nového øádku.
*/
% - useful for debug when testing <@, show does any action and prints input
% - finished with new line

shownl(X,X):-
    write(X),nl.

%------------------------------------------------------------------------------
/**     showAtom(+Input, -Input)
Text:   Chová se jako identita, navíc vypí¹e do aktuálního výstupního
        proudu atom resp. øetìzec Input.
*/
% - useful for debug when testing <@, show does any action and prints input

showAtom(X,X):-
    put(X).

%------------------------------------------------------------------------------
/**     showAtom(+Atom, +Input, -Input)
Text:   Chová se jako identita, navíc vypí¹e do aktuálního výstupního
        proudu Atom.
*/

showAtom(A,X,X):-
    put(A).

%------------------------------------------------------------------------------
/**     show(+Input, -Input)
Text:   Chová se jako identita, navíc vypí¹e do aktuálního výstupního
        proudu term Input.
*/
% - useful for debug when testing <@, show does any action and prints input

show(X,X):-
    write(X).

%------------------------------------------------------------------------------
/**     showConst(+Const, +Input, -Input)
Text:   Chová se jako identita, navíc vypí¹e do aktuálního výstupního
        proudu Const, co¾ je seznam termù ve formátu pro printf/1.
*/
% - useful for debug when testing <@, does any action and prints Const
%   CONST IS LIST OF CONSTANTS 

showConst(C,X,X):-
    printf(C).

%------------------------------------------------------------------------------
/**     showString(+Const, +Input, -Input)
Text:   Chová se jako identita, navíc vypí¹e do aktuálního výstupního
        proudu øetìzec Const.
*/
% - useful for debug when testing <@, does any action and prints Const
%   CONST IS LIST OF CONSTANTS 

showString(S,X,X):-
    name(A,S),
    write(A).

/**     showString(+Input, -Input)
Text:   Chová se jako identita, navíc vypí¹e do aktuálního výstupního
        proudu øetìzec ze struktury LOS.
*/

showString(X,X):-
    name(A,X),
    write(A).

%------------------------------------------------------------------------------
/**     flipList2(+InputList, -SwappedList)
Text:   Prohodí první a druhý prvek seznamu.
Example:
        ?- flipList2([a,b],[b,a]).
        Yes
*/

flipList2([A,B],[B,A]).

%------------------------------------------------------------------------------
/**     eval(+Expression, -Result)
Text:   Vyhodnotí pomocí standardního operátoru 'is' výraz Expression
        a výsledek vydá v Result.
*/

eval(Expr,Result):-
        Result is Expr.

%------------------------------------------------------------------------------
/**     alter(+InputList, +Wanted, +InputList, -Wanted)
Text:   V pøípadech, kdy seznam úspì¹ných rozkladù obsahuje jednu derivaci
        (napøíklad deterministické rozklady), transformuje výsledek
        v derivaci po¾adovaným zpùsobem. Pùvodní výsledek se unifikuje
        s InputList, nový výsledek je vytvoøen ve Wanted.
Example:
        % pseudocode
        W :-> ... <@ alter(Result,treeNode(Left,Result,Right))

        % pseudocode of tuple2List
        W :-> ... <@ alter(A>B,[A|B])
*/

alter(Input,Wanted,Input,Wanted).

%------------------------------------------------------------------------------
/**     colonePrintf(+Message, +Input, -Input)
Text:   Varianta printf/1 pro kolony - vypí¹e Message. Výsledek není
        modifikován.
*/

colonePrintf(Masage,Input,Input):-
        printf(Masage).

/**     colonePrintf(+InputList, +WantedOutput, +InputList, -InputList)
Text:   V pøípadech kdy seznam úspì¹ných rozkladù obsahuje jednu derivaci
        (napøíklad deterministické rozklady), lze pou¾ít tento predikát
        pro u¾ivatelský výpis výsledku.
Example:
        % pseudocode
        W :-> ... <@ colonePrintf(X>Y,['Precedence of ',X,' is ',Y])
*/

colonePrintf(Input,Wanted,Input,Input):-
        printf(Wanted).

%------------------------------------------------------------------------------
/**     elementOf(?ElementList, ?Element)
Text:   Uspìje, pokud Element je prvkem seznamu ElementList.
        Pou¾ití v mutátoru <?.
*/

elementOf(L,E):-
        member(E,L).

%------------------------------------------------------------------------------
/**     notElementOf(?ElementList,?Element)
Text:   Uspìje, pokud Element není prvkem seznamu ElementList.
        Pou¾ití v mutátoru <?.
*/

notElementOf(L,E):-
        member(E,L) -> fail ; true.

%------------------------------------------------------------------------------
/**     lElementOf(?ElementList,?Element)
Text:   Uspìje, pokud prvek jednoprvkového seznamu Element je také prvkem
        seznamu ElementList. Pou¾ití v mutátoru <?.
*/

lElementOf(L,[E]):-
        member(E,L).

%------------------------------------------------------------------------------
/**     lNotElementOf(?ElementList,?Element)
Text:   Uspìje, pokud prvek jednoprvkového  seznamu Element není prvkem 
        seznamu ElementList. Pou¾ití v mutátoru <?.
*/

lNotElementOf(L,[E]):-
        member(E,L) -> fail ; true.

%------------------------------------------------------------------------------
/**     coloneFlattenString(+String, -FlattenString)
Text:   Vyhladí øetìzec String do jedné úrovnì hloubky FlattenString.
*/

coloneFlattenString(I,O):-
        (I=[H|T] ->
         (atomic(H) -> coloneFlattenString(T,Tt),
                       O=[H|Tt]
                    ;  coloneFlattenString(H,OX),
                       coloneFlattenString(T,OT),
                       append(OX,OT,O)))
         ;
        (I=[H] ->           % jeden element
         (atomic(H) -> O=I
                    ;  coloneFlattenString(H,O)))
         ;
        I=[] -> O=[].

%------------------------------------------------------------------------------
/**     colonePrintString(+String, -String)
Text:   Vytiskne øetìzec, který nemusí být plochý. Výsledek
        zùstává nezmìnìn.
*/

colonePrintString(I,I):-
        (I=[H|T] ->
         (atomic(H) -> put(H);  colonePrintString(H,_)),
                       colonePrintString(T,_))
         ;
        (I=[H] ->           % jeden element
         (atomic(H) -> put(H)
                    ;  colonePrintString(H,_)))
         ;
        true.

%- EOF ------------------------------------------------------------------------
