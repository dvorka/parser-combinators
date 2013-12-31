%------------------------------------------------------------------------------
%
%              Zakladni primitiva a parsery terminalnich symbolu I
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Základní primitiva a parsery terminálních symbolù.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
% Poznamky:
% - konvence zdrojovych souboru viz pc/README.conventions
%------------------------------------------------------------------------------
/**     item(?Wrapper)
Text:   Primitivum pøijímající ze vstupu jednu polo¾ku.
Example:
        ?- item(s("gen:MdfDh")+L).
        L = [s("en:MdfDh") > O'g]
        Yes
*/

item(quickS([S|Is])+[quickS(Is)>S]).
item(quickS([])+[]).
item(quickFile(Sh,O)+L):-
        setStream(Sh,O),get0(B),
        (B=(-1) -> L=[] ; atStream(Sh,Oo),L=[quickFile(Sh,Oo)>B]).

item(ll1(Opt,LA,FOLLOW,I)+L):-
        LA=pcEpsilon -> pcError(eof,[I,'item/1']), % item *musi* prijmout alespon jeden znak (chyba)
                    L=[]
                 ;  item(I+Li),
                    (Li=[N>R] -> L=[ll1(Opt,R,FOLLOW,N)>LA]
                              ;  L=[ll1(Opt,pcEpsilon,FOLLOW,I)>LA]).

item(pseudoll1(_,pcEpsilon,_,I)+[]):-
        pcError(pseudoll1Eof,[I,'item/1']). % item *musi* prijmout alespon jeden znak (chyba)
item(pseudoll1(Opt,LA,FOLLOW,I)+L):-
        item(I+Li),
        (Li=[N>R]
         -> L=[pseudoll1(Opt,R,FOLLOW,N)>LA]
         ;  L=[pseudoll1(Opt,pcEpsilon,FOLLOW,I)>LA]).

item(s([S|Is])+[s(Is)>S]).
item(s([])+[]).
item(file(Sh,O)+L):-
        setStream(Sh,O),get0(B),
        (B=(-1) -> L=[] ; atStream(Sh,Oo),L=[file(Sh,Oo)>B]).
         
item(off(Off,I)+L):-
        item(I+Li) -> (Li=[N>R] -> New is Off+1,L=[off(New,N)>R]
                                ;  L=[]).
item(line(Line,I)+L):-
        item(I+Li)
         -> (Li=[N>R] -> (R=10 -> New is Line+1,L=[line(New,N)>10]
                               ;  L=[line(Line,N)>R])
                      ;  L=[]).
item(lineCol(Line,Col,I)+L):-
        item(I+Li)
         -> (Li=[N>R] -> (R=10 -> NewL is Line+1,L=[lineCol(NewL,1,N)>10]
                               ;  NewC is Col+1,L=[lineCol(Line,NewC,N)>R])
                      ; L=[]).
item(lookAhead(LA,I)+L):-
        LA=pcEpsilon -> L=[]
                 ;  item(I+Li),
                    (Li=[N>R] -> L=[lookAhead(R,N)>LA]
                              ;  L=[lookAhead(pcEpsilon,I)>LA]).
item(empty+false).
item(first+[pcTrue]).
item(eFirst+eFirst(Empty,First)):-
        item(empty+Empty),item(first+First).
item(eFirst(assert)+eFirst(Empty,First)):-
        eFirstGet(item,Empty,First).

item(alt(Alt,I)+L):-
        item(I+Li),
        (Li=[N>R] -> L=[alt(Alt,N)>R]
                  ;  L=Li).

item(filter(Sh)+L):-
        atStream(Sh,O),get0(B),
        (B=(-1) -> L=[],setStream(Sh,O) ; L=[filter(Sh)>B]).

item(prn(I)+L):-
        item(I+Li) -> (Li=[N>R] -> put(R),L=[prn(N)>R]
                                ;  L=[]).

% implicitni klauzule
item(I+L):-
        %deBug(modeWarn,['(item) Warning: implicit mode selector:',nl,I+L,nl]),
        % Jedna se uzivatelsky mod, zpracovani tohoto selektoru je
        % tedy ponechano na uzivatelske klauzuli, ktera musi mit stejne
        % jmeno a o 1 vetsi aritu nez ma struktura selektoru (pribaleni LOS).
        :-@ [I,L].

%------------------------------------------------------------------------------
/**     snift(?Wrapper)
Text:   Primitivum pøijímající ze vstupu jednu polo¾ku se zachováním
        pùvodního vstupu.
Example:
        ?- snift(s("gen:MdfDh")+L).
        L = [s("gen:MdfDh") > O'g]
        Yes
*/

snift(I+L):-                            % vedlejsi efekt pri vstupu ze
        item(I+Li),                     % souboru je osetren offsetem
        (Li=[_>R] -> L=[I>R] ; L=[]).

%------------------------------------------------------------------------------
/**     symbol(+Symbol, ?Wrapper)
Text:   Parser rozpoznávající jeden terminální symbol. Symbol je øetìzec
        obsahující rozpoznávaný symbol. Jedinou polo¾kou struktury LOS
        je dvojice v ní¾ je výsledek reprezentován jako ASCII kód symbolu.
Example:
        ?- symbol("-",s("-3")+L).
        L = [s("3") > O'-]
        Yes
*/
                       
symbol([S],quickS([S|Is])+[quickS(Is)>S]).
symbol(_,quickS([])+[]).
symbol([S],quickFile(Sh,O)+L):-
        setStream(Sh,O),get0(B),
        (S==B -> atStream(Sh,Oo),L=[quickFile(Sh,Oo)>S] ;  L=[]).
symbol([S],filter(Sh)+L):-
        atStream(Sh,O),get0(B),
        (S==B -> L=[filter(Sh)>B] ; L=[],setStream(Sh,O)).

symbol(S,I+L):-
        fulfil(==(S),I+L).

%------------------------------------------------------------------------------
/**     nonSymbol(+Symbol, ?Wrapper)
Text:   Parser pøijímající terminál rùzný od Symbol. Jedinou polo¾kou
        struktury LOS je dvojice v ní¾ je výsledek reprezentován jako
        ASCII kód symbolu.
Example:
        ?- nonSymbol("-",s("-3")+L).
        L = []
        Yes
*/

nonSymbol([S],quickS([I|Is])+L):-
        S=I -> L=[] ; L=[quickS(Is)>I].
nonSymbol(_,quickS([])+[]).
nonSymbol([S],quickFile(Sh,O)+L):-
        setStream(Sh,O), get0(B),
        (B=S -> L=[]
             ;  (B=(-1) -> L=[] ; atStream(Sh,Oo),L=[quickFile(Sh,Oo)>B])).
nonSymbol([S],filter(Sh)+L):-
        atStream(Sh,O), get0(B),
        (B=S -> L=[],setStream(Sh,O)
             ;  (B=(-1) -> L=[],setStream(Sh,O) ; L=[filter(Sh)>B])).

nonSymbol(C,first+[lNotElementOf([pcEpsilon|C])]).      % nonSymbol neprijima prazdny retezec

nonSymbol(S,I+L):-
        fulfil(\==(S),I+L).

%------------------------------------------------------------------------------
/**     fulfil(+Condition, ?Wrapper)
Text:   Parser fulfil je zobecnìním symbol. Rozpoznává symbol
        splòující podmínku Condition. Polo¾kou struktury LOS je dvojice
        obsahující jako výslednou hodnotu ASCII kód symbolu.
Example:
        ?- fulfil(==("1"),s("123")+L).
        L = [s("23") > O'1]
        Yes
*/

fulfil(C,quickS([I|Is])+L):-
        :-@ [C,[I]] -> L=[quickS(Is)>I] ; L=[].
fulfil(_,quickS([])+[]).
fulfil(C,quickFile(Sh,O)+L):-
        setStream(Sh,O), get0(B) 
        -> B\=(-1),(:-@[C,[B]] -> atStream(Sh,Oo),L=[quickFile(Sh,Oo)>B]
                               ;  L=[])
        ;  L=[].

fulfil(C,ll1(Opt,LA,FOLLOW,I)+L):-      % v ll1 modu musi vzdy uspet
        fulfil(C,lookAhead(LA,I)+LL),
        ( LL=[lookAhead(LLA,LLI)>R] -> L=[ll1(Opt,LLA,FOLLOW,LLI)>R]
           ;
          LL=[] -> L=[],
                   (LA=pcEpsilon
                    -> pcError(eof,[I,'fulfil/2'])
                    ;  pcError(ll1IllegalChar,[I,LA,'fulfil/2']))).
fulfil(C,pseudoll1(Opt,LA,FOLLOW,I)+L):-
        fulfil(C,lookAhead(LA,I)+LL),   % v pseudoll1 modu by mel vzdy uspet
        ( LL=[lookAhead(LLA,LLI)>R] -> L=[pseudoll1(Opt,LLA,FOLLOW,LLI)>R]
           ;
          LL=[] -> L=[],
                   (LA=pcEpsilon
                    -> pcError(pseudoll1Eof,[I,'fulfil/2'])
                    ;  pcError(pseudoll1WarnIllegalChar,[I,LA,'fulfil/2']))).

fulfil(_,empty+false).
fulfil(C,first+[C]).
fulfil(C,eFirst+eFirst(Empty,First)):-
        fulfil(C,empty+Empty),fulfil(C,first+First).
fulfil(C,eFirst(assert)+eFirst(Empty,First)):-
        eFirstGet(fulfil(C),Empty,First).

fulfil(C,filter(Sh)+L):-
        atStream(Sh,O), get0(B)
        -> B\=(-1),(:-@[C,[B]] -> L=[filter(Sh)>B]
                               ;  L=[],setStream(Sh,O))
        ;  L=[],setStream(Sh,O).

fulfil(C,I+L):-
        item(I+Li) -> (Li=[N>R],:-@ [C,[R]] -> return(R,N+L)
                                            ;  terminate(I+L)).

%------------------------------------------------------------------------------
/**     nonFulfil(+Condition, ?Wrapper)
Text:   Parser nonFulfil je zobecnìním nonSymbol. Rozpoznává symbol,
        který nesplòuje podmínku Condition. Polo¾kou struktury LOS je dvojice
        obsahující jako výslednou hodnotu ASCII kód symbolu.
Example:
        ?- nonFulfil(isDigit,s("x123")+L).
        L = [s("123") > O'x]
        Yes
*/

nonFulfil(C,quickS([I|Is])+L):-
    :-@ [C,[I]] -> L=[] ; L=[quickS(Is)>I].
nonFulfil(_,quickS([])+[]).
nonFulfil(C,quickFile(Sh,O)+L):-
        setStream(Sh,O), get0(B)
        -> B\=(-1),(:-@[C,[B]] -> L=[]
                               ;  atStream(Sh,Oo),L=[quickFile(Sh,Oo)>B])
         ;  L=[].

nonFulfil(C,ll1(Opt,LA,FOLLOW,I)+L):-      % v ll1 modu musi vzdy uspet
        nonFulfil(C,lookAhead(LA,I)+LL),
        ( LL=[lookAhead(LLA,LLI)>R] -> L=[ll1(Opt,LLA,FOLLOW,LLI)>R]
           ;
          LL=[] -> L=[],
                   (LA=pcEpsilon
                    -> pcError(eof,[I,'nonFulfil/2'])
                    ;  pcError(ll1IllegalChar,[I,LA,'nonFulfil/2']))).
nonFulfil(C,pseudoll1(Opt,LA,FOLLOW,I)+L):- % v pseudoll1 modu by mel vzdy uspet
        nonFulfil(C,lookAhead(LA,I)+LL),   
        ( LL=[lookAhead(LLA,LLI)>R] -> L=[pseudoll1(Opt,LLA,FOLLOW,LLI)>R]
           ;
          LL=[] -> L=[],
                   (LA=pcEpsilon
                    -> pcError(pseudoll1Eof,[I,'nonFulfil/2'])
                    ;  pcError(pseudoll1WarnIllegalChar,[I,LA,'nonFulfil/2']))).

nonFulfil(_,empty+false).
nonFulfil(C,first+[pcNot(C)]).
nonFulfil(C,eFirst+eFirst(Empty,First)):-
        nonFulfil(C,empty+Empty),nonFulfil(C,first+First).
nonFulfil(C,eFirst(assert)+eFirst(Empty,First)):-
        eFirstGet(nonFulfil(C),Empty,First).

nonFulfil(C,filter(Sh)+L):-
        atStream(Sh,O), get0(B)
        -> B\=(-1),(:-@[C,[B]] -> L=[],setStream(Sh,O)
                               ;  L=[filter(Sh)>B])
         ;  L=[],setStream(Sh,O).
        
nonFulfil(C,I+L):-
        item(I+Li) -> (Li=[N>R] -> (:-@ [C,[R]] -> terminate(I+L)
                                                ;  return(R,N+L))
                                ;  terminate(I+L)).

%------------------------------------------------------------------------------
/**     return(+Value, ?Wrapper)
Text:   return je primitivum, které nikdy nepøijme vstup a v¾dy vrací jako
        výsledek hodnotu Value.
Example:
        ?- return(never,s("vi sendmail.cf")+L).
        L = [s("vi sendmail.cf") > never]
        Yes
*/

return(_,empty+true).
return(_,first+[]).
return(_,eFirst+eFirst(Empty,First)):-
        return(_,empty+Empty),return(_,first+First).
return(_,eFirst(assert)+eFirst(Empty,First)):-
        eFirstGet(return(_),Empty,First).

return(V,ll1(Opt,LA,FOLLOW,I)+L):-              % v ll1 modu musi vzdy uspet
        LA=pcEpsilon
        -> L=[ll1(Opt,LA,FOLLOW,I)>V]
        ;  pcError(ll1IllegalChar,[I,LA,'return/2']),
           L=[].

% Epsilon prechod je v pseudoll1 modu pripustny, proto nebudeme zbytecne 
% rozklad zpomalovat vypisem warningu. Bez problemu ale lze kod odkomentovat
% a varovani vypisovat.
%
% return(V,pseudoll1(Opt,LA,FOLLOW,I)+L):-
%        LA=pcEpsilon
%        -> L=[pseudoll1(Opt,LA,FOLLOW,I)>V]
%        ;  pcError(pseudoll1WarnIllegalChar,[I,LA,'return/2']),
%           L=[pseudoll1(Opt,LA,FOLLOW,I)>V].                   % pripustne

return(V,I+[I>V]).

%------------------------------------------------------------------------------
/**     epsilon(?Wrapper)
Text:   Primitivum epsilon nikdy nepøijme vstup a v¾dy vrací strukturu
        LOS obsahující právì jednu derivaci: Input > [].
Example:
        ?- epsilon(s("stop")+L).
        L = [s("stop") > []]
        Yes
*/

epsilon(empty+true).
epsilon(first+[]).
epsilon(eFirst+eFirst(Empty,First)):-
        epsilon(empty+Empty),epsilon(first+First).
epsilon(eFirst(assert)+eFirst(Empty,First)):-
        eFirstGet(epsilon,Empty,First).

epsilon(ll1(Opt,LA,FOLLOW,I)+L):-                        % v ll1 modu musi vzdy uspet
        LA=pcEpsilon
        -> L=[ll1(Opt,LA,FOLLOW,I)>[]]
        ;  pcError(ll1IllegalChar,[I,LA,'epsilon/2']),
           L=[].

% Epsilon prechod je v pseudoll1 modu pripustny, proto nebudeme zbytecne 
% rozklad zpomalovat vypisem warningu. Bez problemu ale lze kod odkomentovat
% a varovani vypisovat.
%
% epsilon(pseudoll1(Opt,LA,FOLLOW,I)+L):-
%        LA=pcEpsilon
%        -> L=[pseudoll1(Opt,LA,FOLLOW,I)>[]]
%        ;  pcError(pseudoll1WarnIllegalChar,[I,LA,'epsilon/2']),
%           L=[pseudoll1(Opt,LA,FOLLOW,I)>[]].                  % pripustne

epsilon(I+[I>[]]).

%------------------------------------------------------------------------------
/**     terminate(?Wrapper)
Text:   V¾dy ignoruje vstup a signalizuje neúspì¹ný rozklad, tj. vrací prázdnou
        strukturu LOS.
Example:
        ?- terminate(s("EOF")+L).
        L = []
        Yes
*/

terminate(empty+false).
terminate(first+[]).
terminate(eFirst+eFirst(Empty,First)):-
        terminate(empty+Empty),terminate(first+First).
terminate(eFirst(assert)+eFirst(Empty,First)):-
        eFirstGet(terminate,Empty,First).

terminate(_+[]).

%------------------------------------------------------------------------------
/**     token(+Token, ?Wrapper)
Text:   Rozpoznává øetìzec symbolù Token. Polo¾kou struktury LOS je dvojice
        obsahující jako výslednou hodnotu øetìzec odpovídající tokenu.
Example:
        ?- token("Immu",s("Immu's paw")+L).
        L= [s("'s paw") > "Immu"]
        Yes
*/

token(T,quickS([I|Ii])+L):-
        append(T,Is,[I|Ii]) -> L=[quickS(Is)>T] ; L=[].
token(T,quickS([])+L):-
        T=[] -> L=[quickS([])>[]] ; L=[].
token(T,quickFile(Sh,O)+L):-
        setStream(Sh,O),length(T,Lng),loadBytes(Lng,Bytes,_),
        (T=Bytes -> atStream(Sh,Oo), L=[quickFile(Sh,Oo)>T] ; L=[]).

token([],ll1(Opt,LA,FOLLOW,I)+[ll1(Opt,LA,FOLLOW,I)>[]]).
        % v ll1 muze byt epsilon pouze na vstupu, proto zvlastni verze
token([],pseudoll1(Opt,LA,FOLLOW,I)+[pseudoll1(Opt,LA,FOLLOW,I)>[]]).
        % v pseudoll1 pripustne

token(T,filter(Sh)+L):-
        atStream(Sh,Pos),length(T,Lng),loadBytes(Lng,Bytes,_),
        (T=Bytes -> L=[filter(Sh)>T] ; L=[],setStream(Sh,Pos)).

token([H|T],I+L):-
        I+L :-> symbol([H]) <&> token(T).
token([],I+L):-
        I+L :-> return([]).

%------------------------------------------------------------------------------
%	      		        PATTERN VERSIONS
%------------------------------------------------------------------------------
/**     symbols(+Pattern, ?Wrapper)
Text:   Zobecnìní parseru symbol. Predikát rozeznává jeden z terminálních
        symbolù, které jsou uvedeny v øetìzci Pattern. Jedinou polo¾kou
        struktury LOS je dvojice, v ní¾ je výsledek reprezentován jako
        ASCII kód symbolu.
Example:
        ?- symbols("aBc",s("B52")+L).
        L = [s("52") > 66]
        Yes
*/

symbols(A,quickS([S|T])+L):-
        member(S,A) -> L=[quickS(T)>S] ; L=[].
symbols(_,quickS([])+[]).
symbols(A,quickFile(Sh,O)+L):-
        setStream(Sh,O),get0(S),
        (member(S,A) -> atStream(Sh,Oo),L=[quickFile(Sh,Oo)>S] ;  L=[]).

symbols(_,empty+false).
symbols(C,first+[lElementOf(C)]).
symbols(C,eFirst+eFirst(Empty,First)):-
        symbols(C,empty+Empty),symbols(C,first+First).
symbols(C,eFirst(assert)+eFirst(Empty,First)):-
        eFirstGet(symbols(C),Empty,First).

symbols(A,filter(Sh)+L):-
        atStream(Sh,O),get0(S),
        (member(S,A) -> L=[filter(Sh)>S] ;  L=[],setStream(Sh,O)).
                     
symbols(A,I+L):-
        item(I+Li) -> (Li=[N>R],member(R,A) -> return(R,N+L)
                                            ;  terminate(I+L)).

%------------------------------------------------------------------------------
/**     nonSymbols(+Pattern, ?Wrapper)
Text:   Negace predikátu symbols - rozeznává tedy právì ty terminální symboly,
        které nejsou uvedenu v øetìzci Pattern.
Example:
        ?- nonSymbols("aBc",s("B52")+L).
        L = []
        Yes
        ?- nonSymbols("ac",s("B52")+L).
        L = [s("52") > 66]
        Yes
*/

nonSymbols(A,quickS([S|T])+L):-
    member(S,A) -> L=[] ; L=[quickS(T)>S].
nonSymbols(_,quickS([])+[]).
nonSymbols(A,quickFile(Sh,O)+L):-
        setStream(Sh,O),get0(S),
        (member(S,A) -> L=[]
                     ; (S=(-1) -> L=[] ; atStream(Sh,Oo),L=[quickFile(Sh,Oo)>S])).

nonSymbols(_,empty+false).
nonSymbols(C,first+[lNotElementOf([pcEpsilon|C])]).     % nonSymbols neprijima prazdny retezec
nonSymbols(C,eFirst+eFirst(Empty,First)):-
        nonSymbols(C,empty+Empty),nonSymbols(C,first+First).
nonSymbols(C,eFirst(assert)+eFirst(Empty,First)):-
        eFirstGet(nonSymbols(C),Empty,First).

nonSymbols(A,filter(Sh)+L):-
        atStream(Sh,O),get0(S),
        (member(S,A) -> L=[],setStream(Sh,O)
                     ; (S=(-1) -> L=[],setStream(Sh,O) ; L=[filter(Sh)>S])).

nonSymbols(A,I+L):-
        item(I+Li) -> (Li=[N>R] -> (member(R,A) -> terminate(I+L)
                                                ;  return(R,N+L))
                                ;  terminate(I+L)).

%------------------------------------------------------------------------------
/**     tokens(+ListOfTokens, ?Wrapper)
Text:   Rozpoznává øetìzec symbolù, který je prvkem seznamu ListOfTokens. 
        Polo¾kou struktury LOS jsou dvojice obsahující jako výslednou hodnotu
        øetìzec odpovídající tokenu.
Example:
        ?- tokens(["left","right"],s("left(x)")+L).
        L= [s("(x)") > "left"]
        Yes
*/

tokens([H|T],empty+Empty):-
        token(H,empty+EmptyH), tokens(T,empty+EmptyT),
        pcOR(EmptyH,EmptyT,Empty).
tokens([],empty+false).
tokens([H|T],first+First):-
        token(H,first+FirstH), tokens(T,first+FirstT),
        append(FirstH,FirstT,First).
tokens([],first+[]).
tokens(T,eFirst+eFirst(Empty,First)):-
        tokens(T,empty+Empty),tokens(T,first+First).
tokens(T,eFirst(assert)+eFirst(Empty,First)):-
        eFirstGet(tokens(T),Empty,First).

tokens([H|T],I+L):-
        (token(H,I+Lt),Lt=[D] -> L=[D|Ls] ; L=Ls ),
        tokens(T,I+Ls).
tokens([],_+[]).

%------------------------------------------------------------------------------
/** tokensPart(+ListOfTokens, ?Wrapper)
Text:   Rozpoznává øetìzec symbolù, který je prvkem seznamu ListOfTokens. 
        Polo¾kou struktury LOS je nejvý¹e jedna dvojice obsahující jako
        výslednou hodnotu øetìzec odpovídající tokenu.
Example:
        ?- tokens(["left","leftHand"],s("leftHand")+L).
        L= [s("Hand") > "left",s("") > "leftHand"]
        Yes
        ?- tokensPart(["left","leftHand"],s("leftHand")+L).
        L= [s("Hand") > "left"]
        Yes
*/

tokensPart(T,empty+Empty):-
        tokens(T,empty+Empty).
tokensPart(T,first+First):-
        tokens(T,first+First).
tokensPart(T,eFirst+eFirst(Empty,First)):-
        tokens(T,eFirst+eFirst(Empty,First)).
tokensPart(T,eFirst(assert)+eFirst(Empty,First)):-
        tokens(T,eFirst(assert)+eFirst(Empty,First)).

tokensPart([H|T],I+L):-
        (token(H,I+Lt),Lt=[_] -> L=Lt ; tokensPart(T,I+L)).
tokensPart([],_+[]).

%------------------------------------------------------------------------------
%	      		        MUTANTS RETURNING ATOMS
%------------------------------------------------------------------------------
/**     itemA(?Wrapper)
Text:   Varianta primitiva item vydávající atom.
*/

itemA(W):-
 W :->
        item <@ ascii2Atom.

%------------------------------------------------------------------------------
/**     symbolA(+Symbol, ?Wrapper)
Text:   Varianta parseru symbol vydávající atom.
Example:
        ?- symbol("-","-3"+L).
        L= [s("3") > '-']
        Yes
*/

symbolA([S],quickS([I|Is])+L):-
        S=I -> name(A,[S]),L=[quickS(Is)>A] ; L=[].
symbolA(_,quickS([])+[]).
symbolA([S],quickFile(Sh,O)+L):-
        setStream(Sh,O),get0(B),
        (S==B -> atStream(Sh,Oo),name(A,[S]),L=[quickFile(Sh,Oo)>A]
              ;  L=[]).

symbolA(S,W):-
 W :->
	fulfil(==(S)) <@ ascii2Atom.

%------------------------------------------------------------------------------
/**     nonSymbolA(+Symbol, ?Wrapper)
Text:   Varianta parseru nonSymbolA vydávající atom.
Example:
        ?- nonSymbolA("+",s("-3")+L).
        L= [s("3") > '-']
        Yes
*/

nonSymbolA([S],quickS([I|Is])+L):-
        S=I -> L=[] ; name(A,[I]),L=[quickS(Is)>A].
nonSymbolA(_,quickS([])+[]).
nonSymbolA([S],quickFile(Sh,O)+L):-
        setStream(Sh,O),get0(B),
        (B=S -> L=[]
             ;  (B=(-1) -> L=[]
                        ;  atStream(Sh,Oo),name(A,[B]),L=[quickFile(Sh,Oo)>A])).

nonSymbolA(S,W):-
 W :->
	fulfil(\==(S)) <@ ascii2Atom.

%------------------------------------------------------------------------------
/**     fulfilA(+Condition, ?Wrapper)
Text:   Varianta parseru fulfil vydávající atom.
*/

fulfilA(C,quickS([I|Is])+L):-
        :-@ [C,[I]] -> name(A,[I]),L=[quickS(Is)>A] ; L=[].
fulfilA(_,quickS([])+[]).
fulfilA(C,quickFile(Sh,O)+L):-
        setStream(Sh,O),get0(B)
        -> B\=(-1),
           (:-@[C,[B]] -> atStream(Sh,Oo),name(A,[B]),L=[quickFile(Sh,Oo)>A] ; L=[])
        ;  L=[].

fulfilA(C,filter(Sh,O)+L):-
        atStream(Sh,O),get0(B)
        -> B\=(-1),
           (:-@[C,[B]] -> name(A,[B]),L=[filter(Sh)>A] ; L=[],setStream(Sh,Oo))
        ;  L=[],setStream(Sh,Oo).

fulfilA(C,I+L):-
        item(I+Li) -> (Li=[N>R],:-@ [C,[R]] -> name(A,[R]),return(A,N+L)
                                            ;  terminate(I+L)).

%------------------------------------------------------------------------------
/**     tokenA(+Token, ?Wrapper)
Text:   Varianta parseru token vydávající atom.
*/

tokenA(T,quickS([I|Ii])+L):-
        append(T,Is,[I|Ii]) -> name(A,T),L=[quickS(Is)>A] ; L=[].
tokenA(_,quickS([])+[]).
tokenA(T,quickFile(Sh,O)+L):-
        setStream(Sh,O),length(T,Lng),loadBytes(Lng,Bytes,_),
        (T=Bytes -> atStream(Sh,Oo),name(A,T),L=[quickFile(Sh,Oo)>A] ; L=[]).

tokenA(T,W):-
 W :->
        token(T) <@ string2Atom.

%------------------------------------------------------------------------------
/**     tokensA(+ListOfTokens, ?Wrapper)
Text:   Varianta parseru tokens vydávající atom.
*/

tokensA([H|T],I+L):-
        (tokenA(H,I+[Lt]) -> L=[Lt|Lts] ; L=Lts),
        tokensA(T,I+Lts).
tokensA([],_+[]).

%------------------------------------------------------------------------------
/**     tokensPartA(+ListOfTokens, ?Wrapper)
Text:   Varianta parseru tokensPartA vydávající atom.
*/

tokensPartA([H|T],I+L):-
        (tokenA(H,I+Lt),Lt=[_] -> L=Lt ; tokensPartA(T,I+L)).
tokensPartA([],_+[]).

%------------------------------------------------------------------------------
/**     symbolsA(+Pattern, ?Wrapper)
Text:   Varianta parseru symbols vydávající atom.
*/

symbolsA(A,quickS([S|T])+L):-
        member(S,A) -> name(SA,[S]),L=[quickS(T)>SA] ;  L=[].
symbolsA(_,quickS([])+[]).
symbolsA(A,quickFile(Sh,O)+L):-
        setStream(Sh,O),get0(S),
        (member(S,A) -> atStream(Sh,Oo),name(SA,[S]),L=[quickFile(Sh,Oo)>SA]
                     ;  L=[]).

symbolsA(T,W):-
 W :->
        symbols(T) <@ ascii2Atom.

%------------------------------------------------------------------------------
/**     nonSymbolsA(+Pattern, ?Wrapper)
Text:   Varianta parseru nonSymbols vydávající atom.
*/

nonSymbolsA(A,quickS([S|T])+L):-
    member(S,A) -> L=[] ; name(SA,[S]),L=[quickS(T)>SA].
nonSymbolsA(_,quickS([])+[]).
nonSymbolsA(A,quickFile(Sh,O)+L):-
        setStream(Sh,O),get0(S),
        (member(S,A)
         -> L=[]
         ;  (S=(-1) -> L=[] ; atStream(Sh,Oo),name(SA,[S]),L=[quickFile(Sh,Oo)>SA])).

nonSymbolsA(T,W):-
 W :->
        nonSymbols(T) <@ ascii2Atom.

%- EOF ------------------------------------------------------------------------
