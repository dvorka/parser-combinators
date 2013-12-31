%------------------------------------------------------------------------------
%
%                                 Ad hoc
%
%			       Martin Dvorak
%                                  2000
%------------------------------------------------------------------------------
/** Module:
Text:   V¹eobecnì pou¾itelné konstruktory parserù, utility,
        módy.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     wildCardFit(+WildCardString, +Input)
Text:   Predikát uspìje právì tehdy, kdy¾ prefix vstupu odpovídá výrazu
        zadanému ¾olíky.
p       Podrobnosti k syntaxi viz wildCard2Parser/2.
Arg:    Input
        Selektor vstupu.
Example:
         ?- wildCardFit("*[0-9]?.pl",s("source1a.pl")).
         Yes
         ?- wildCardFit("*[_A-Z]??.pl",s(".src_12.pl")).
         No
*/

wildCardFit(WC,Input):-
        wildCard2Parser(WC,P),
        Input+L :-> P
        ->
        L\==[].

%------------------------------------------------------------------------------
/**     wildCard2Parser(+WildCardString, -Parser)
Text:   Generátor parseru dle výrazu zadaného ¾olíky:
v       *     ... ¾ádný, jeden nebo více libovolných znakù. Pokud je na 
v       ......... zaèátku výrazu, nesmí tento zaèínat znakem '.'
v       ?     ... jeden libovolný znak
v       [abc] ... kterýkoli ze znakù uvedený v závorkách, intervaly napø.
v       ......... a-c jsou pøípustné
v       [!ab] ... kterýkoli ze znakù, který není uveden v závorkách
        Znak v pùvodním významu lze získat pou¾itím zpìtného lomítka.
        ®olíky jsou zadány ve formì prologovského øetìzce, vydán je
        parser ve formì termu.
Example:
         ?- wildCard2Parser("*.pl",P).
         P= (nonSymbol(".")<&>item<**>)<&>token(".pl")
         Yes
*/

wildCard2Parser(WC,Parser):-
        wcWCExpr(off(0,s(WC))+[off(_,_)>R|_]),
        (R=[] -> Parser=epsilon ; R=Parser).
        
% ----------------------------------------------------------------------------
% wcWCExpr(?Wrapper)
% vytvoreni parseru podle regularniho vyrazu ze zolikoveho vyrazu

% takhle udelat tecku ve spicce

wcWCExpr(W):-
 W :->
        wcSwitch<*> <@ wcConcat.

% wcChar(?Wrapper)
% - znak, ktery neni specialni tj. ruzny od \*?[]
wcChar(W):-
        nonSymbols([92|"[]"],W).

% wcBackChar(?Wrapper)
% - znak uvozeny zpetnym lomitkem tj. v puvodnim vyznamu
wcBackChar(W):-
 W :->
        symbol([92]) &> item.

% wcCharList(?Wrapper)
% - akceptuje vycet pripustnych znaku napr. [!ab\cf-g], [\\\(\)c]
wcCharList(W):-
 W :->
        brackets( symbol("!") <?@> switch-const(nonSwitch)
                   <&>
                  ((wcCharacter <&>> symbol("-") &> wcCharacter) % interval
                    <:
                   wcCharacter)<+>
                ).

% wcCharacter(?Wrapper)
wcCharacter(W):-
 W :->
        wcChar
         <:
        wcBackChar.

% wcSwitch(?Wrapper)
% - vytvari parser pro switch
wcSwitch(W):-
 W :->
       (wcChar         <@@ wcSwitchChar2Parser
         <:
        wcBackChar     <@ alter(X,[symbol([X])])
         <:
        wcCharList     <@ wcSwitchCharacterList2Parser).

% wcSwitchChar2Parser(+Character, -Parser)
% - "a" -> symbol("a")
wcSwitchChar2Parser(off(Off,I)>C,off(Off,I)>P):-        % znam mod
    % * na offsetu 0 nepripousti '.' jinde ano (off urcuje vyhled)
    C=63 -> P=[item]                                                 % ascii2Atom(63,'?')
     ; C=42 -> (Off=1 -> P=[nonSymbol(".")<&>item<**>] ; P=[item<**>]) % ascii2Atom(42,'*')
      ; P=[symbol([C])].                 

% wcSwitchCharList2P(+List, -Parser)
% - prevadi vystup parseru wcCharList/4 na parser seznamu pripustnych znaku
wcSwitchCharacterList2Parser([switch|CharList],[P]):-
    wcSwitchConcat(CharList,String,IntervalP,switch),
    (IntervalP=[] -> P=symbols(String)
                  ;  (String=[] -> P=IntervalP
                                ;  P= <:>(symbols(String),IntervalP))).
wcSwitchCharacterList2Parser([nonSwitch|CharList],[P]):-
    wcSwitchConcat(CharList,String,IntervalP,nonSwitch),
    (IntervalP=[] -> P=nonSymbols(String)
                  ;  (String=[] -> P=IntervalP
                                ;  P= <:>(nonSymbols(String),IntervalP))).

% wcSwitchConcat(+ListOfCharsAndIntervals, -String, -IntervalP, +Flag)
% - prevadi znaky v ListOfCharsAndIntervals na retezec String a
%   seznam intervalu podminek IntervalP
wcSwitchConcat([V|T],S,I,F):-
    wcSwitchConcat(T,Ss,Is,F),
    (V=(From>To)
      ->
     (
      S=Ss,
      ( F=switch
         ->
        (Is=[] -> I=fulfil(sIsBetween([From],[To]))
               ;  I= <:(fulfil(sIsBetween([From],[To])),Is) )
         ;
        (Is=[] -> I=fulfil(sIsNotBetween([From],[To]))
               ; I= <:(fulfil(sIsNotBetween([From],[To])),Is) )
      )
     )
      ;
     (S=[V|Ss], I=Is)
    ).
wcSwitchConcat([],[],[],_).

% wcConcat(+ListOfParsers, Parser)
% - spoji parsery v seznamu ListOfParsers kombinatorem sekvencni kompozice
wcConcat(I,P):-
    ((I=[[symbol(_)]|_];I=[[token(_)]|_])
      -> wcLinkSymbol(I,T,S),(S=[_] -> HP=symbol(S) ; HP=token(S))
      ;  I=[[HP]|T]),
    wcConcat(T,TP),
    (TP=[] -> P=HP ; P= <&>(HP,TP)).
wcConcat([],[]).
    
wcLinkSymbol(I,T,S):-
     I=[[symbol([C])]|Tt] -> wcLinkSymbol(Tt,T,R), S=[C|R]
      ;
     I=[[token(Cs)]|Tt] -> wcLinkSymbol(Tt,T,R), append(Cs,R,S)
      ;
     S=[], I=T.
wcLinkSymbol([],[],[]).

%------------------------------------------------------------------------------
/**     wc(+Input,-WcStruct)
Text:   Výsledek obsahuje strukturu wc/2 s poètem øádkù a znakù souboru:
v               wc(Lines,Bytes)
        Pro tento úèel je vytvoøen u¾ivatelský mód  wc/3.
Example:
        ?- wc(s("abc"),Wc).
        Wc=wc(1,3)
        Yes
*/
% Nejprve definujeme klauzuli uzivatelskeho modu s aritou o 1 vetsi nez
% ma struktura selektoru - je volana z implicitni klauzule primitiva item/1:

% wc(+Lines,+Bytes,+Input,-Los)
wc(Lines,Bytes,Input,L):-
        item(Input+Li)
        -> (Li=[N>R]
            -> NewBytes is Bytes+1,
               (R=10 -> NewLines is Lines+1 ; NewLines=Lines),
               L=[wc(NewLines,NewBytes,N)>R]
            ;  L=[]).

% Predikat samotny:
wc(Input,WC):-
        wc(1,1,Input)+L :-> item<*>,
        (L=[] -> WC=wc(0,0)
              ;  L=[wc(Ls,Bs,_)>_|_], Bytes is Bs-1, WC=wc(Ls,Bytes)).

%------------------------------------------------------------------------------
/**     encode(+TermTable,+Input,-LOS)
Text:   Klauzule u¾ivatelského módu 'encode'. Tento mód není realizován
        pøímo, ale externím zpùsobem bez modifikace primitiv a konstruktorù
        jádra knihovny.
Arg:    TermTable
        Predikát provádìjící pøekódování termù.
Arg:    Input
        Dekriptor vstupu.
Example:
        ?- encode(ascii2Atom,file(Handle))+L
        |      :-> SomeParser.

        ?- encode(convertCzEnc(iso8859_2,win1250),file(H))
        |       :-> SomeParser.
*/

encode(TermTable,Input,L):-
        item(Input+Li)
        -> (Li=[N>R]
            -> :-@[TermTable,R,RR],
               L=[encode(TermTable,N)>RR]
            ;  L=[]).

% Predikaty pro ruzna prekodovani:
% - lze pouzivat predikaty z 'colone.pl', ktere maji pozadovanou signaturu
% - priklad pouziti modu 'encode' pro prekodovani znakovych sad lze nalezt
%   v demo/charset/charset.pl

%- EOF ------------------------------------------------------------------------
