%------------------------------------------------------------------------------
%
%                       Parsery terminalnich symbolu I
%
%				Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Parsery terminálních symbolù I.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     lower(?Wrapper)
Text:   Malá písmena.
*/

lower(W):-
       fulfil(isLwrChar,W).

%------------------------------------------------------------------------------
/**     upper(?Wrapper)
Text:   Velká písmena.
*/

upper(W):-
       fulfil(isUprChar,W).

%------------------------------------------------------------------------------
/**     letter(?Wrapper)
Text:   Písmena.
*/

letter(W):-
       fulfil(isChar,W).

%------------------------------------------------------------------------------
/**     whiteSpace(?Wrapper)
Text:   Prázdné znaky: mezera, tabulátor, oddìlovaè øádku.
*/

whiteSpace(W):-
        symbols([32,9,13,10],W).

%------------------------------------------------------------------------------
/**     digit(?Wrapper)
Text:   Èíslice.
*/

digit(W):-
        symbolsA("0123456789",W).

%------------------------------------------------------------------------------
/**     numAlpha(?Wrapper)
Text:   Viz isDigitChar.
*/

numAlpha(W):-
        fulfil(isDigitChar,W).

%------------------------------------------------------------------------------
/**     symbolInterval(+Low, +High, ?Wrapper)
Text:   Pøijímá znak, jeho¾ ASCII kód je mezi znaky Low a High.
Example:
        ?- s("unary")+L :-> symbolInterval("d","w").
        L = [s("nary") > O'u]
        Yes
*/

symbolInterval(L,H,W):-
        fulfil(sIsBetween(L,H),W).

%------------------------------------------------------------------------------
/**     symbolIntervalA(+Low, +High, ?Wrapper)
Text:   Varianta symbolInterval/3 vydávající atom.
Example:
        ?- s("5AD")+L :-> symbolIntervalA("1","9").
        L = [s("AD") > 5]
        Yes
*/

symbolIntervalA(L,H,W):-
 W :->
        fulfil(sIsBetween(L,H)) <@ ascii2Atom.

%------------------------------------------------------------------------------
/**     symboli(+Symbol, ?Wrapper)
Text:   Varianta parseru symbol, která není 'case sensitive'.
Example:
        ?- s("AD")+L :-> symboli("a").
        L = [s("D") > O'A]
        Yes
*/

symboli(S,W):-
        (isUprChar(S) -> upr2Lwr(S,R) ; lwr2Upr(S,R)),
 W :->
        symbol(S) <: symbol(R).

%------------------------------------------------------------------------------
/**     symboliA(+Symbol, ?Wrapper)
Text:   Varianta parseru symboli, vydávající atom.
Example:
        ?- s("AD")+L :-> symboliA("a").
        L = [s("D") > 'A']
        Yes
*/

symboliA(S,W):-
 W :->
        symboli(S) <@ ascii2Atom.

%------------------------------------------------------------------------------
/**     symbolHex(+HexadecimalNumber, ?Wrapper)
Text:   Varianta parseru symbol, která oèekává specifikaci pøijímaného
        znaku ve formì hexadecimálního èísla.
Example:
        ?- s("AD")+L :-> symbolHex("0x41").
        L = [s("D") > 65]
        Yes
*/

symbolHex(S,I+L):-
 S+LS :->
        (symbol("0") <&> tokeni("x")) &> hexadecimal,
        ( LS=[[]>Number] -> symbol([Number],I+L) ; L=[]).

%------------------------------------------------------------------------------
/**     symbolOctal(+OctalNumber, ?Wrapper)
Text:   Varianta parseru symbol, která oèekává specifikaci pøijímaného
        znaku ve formì èísla v oktalovém zápisu.
Example:
        ?- s("AD")+L :-> symbolOctal("101").
        L = [s("D") > 65]
        Yes
*/

symbolOctal(S,I+L):-
        octal(S+LS),
        ( LS=[[]>Number] -> symbol([Number],I+L) ; L=[]).

%------------------------------------------------------------------------------
/**     tokeni(+String, ?Wrapper)
Text:   Varianta parseru token, která není 'case sensitive'.
Example:
        ?- s("OpenWindow")+L :-> tokeni("openwindow").
        L = [s("") > "OpenWindow"]
        Yes
*/

tokeni([H|T],W):-
 W :->
       (symboli([H]) <&>> tokeni(T) <@ tuple2List).
tokeni([],I+[I>[]]).

%------------------------------------------------------------------------------
/**     tokeniA(+Symbol, ?Wrapper)
Text:   Varianta parseru tokeni, vydávající atom.
Example:
        ?- s("OpenWindow")+L :-> tokeniA("openwindow").
        L = [s("") > 'OpenWindow']
        Yes
*/

tokeniA(S,W):-
 W :->
        tokeni(S) <@ string2Atom.

%------------------------------------------------------------------------------
/**     eSymbol(+Symbol, ?Wrapper)
Text:   Parser rozpoznávající jeden terminální symbol. Symbol je øetìzec
        obsahující rozpoznávaný symbol, který mù¾e navíc obsahovat ní¾e
        uvedené metaznaky. Jedinou polo¾kou struktury LOS
        je dvojice v ní¾ je výsledek reprezentován jako ASCII kód symbolu.

        Speciální sekvence:
v       \n ... new line (0x0a)
v       \t ... tabulátor
v       \r ... CR (0x0d)
v       \e ... ESC (0x1B)
v       \s ... white space (CR,LF,TAB,SPACE)
v       \S ... znak rùzny od white space
v       \l ... lower case a-z
v       \u ... upper case A-Z
v       \w ... \l a \u
v       \d ... digit
v       \D ... znak, který není èíslicí
v       \a ... digit+w
v       \x00;  ... daná hexadecimální hodnota (viz ';')
v       \i000; ... daná decimální hodnota (viz ';')
v       \o000; ... daná octa hodnota (viz ';')
Example:
        ?- s("\xAA;")+L :-> eSymbol("\x").
        L = [s("") > 170]
        Yes
*/

% - akceptuje znak, jemuz predchazi zpetne lomitko
% - priklad:
%    \a ... a
%    \\ ... backslash
% - specialni sekvence:
%    \n ... new line (0x0a)
%    \t ... tabulator
%    \r ... CR (0x0d)
%    \e ... ESC (0x1B)
%    \s ... white space (CR,LF,TAB,SPACE)
%    \S ... znak ruzny od white space
%    \l ... lower case a-z
%    \u ... upper case A-Z
%    \w ... \l a \u
%    \d ... digit
%    \D ... znak, ktery neni cislici
%    \a ... digit+w
%    \x00;  ... dana hexadecimalni hodnota (viz ';')
%    \i000; ... dana decimalni hodnota (viz ';')
%    \o000; ... dana octa hodnota (viz ';')

eSymbol([92|"n"],W):-
 W :->
        symbol([10]).
eSymbol([92|"t"],W):-
 W :->
        symbol([9]).
eSymbol([92|"r"],W):-
 W :->
        symbol([13]).
eSymbol([92|"e"],W):-
 W :->
        symbol([27]).
eSymbol([92|"s"],W):-
 W :->
        whiteSpace.
eSymbol([92|"S"],W):-
 W :->
        nonFulfil(isWhiteSpace).
eSymbol([92|"l"],W):-
 W :->
        lower.
eSymbol([92|"u"],W):-
 W :->
        upper.
eSymbol([92|"w"],W):-
 W :->
        letter.
eSymbol([92|"d"],W):-
 W :->
        digit<@ascii.
eSymbol([92|"D"],W):-
 W :->
        nonFulfil(isDigit).
eSymbol([92|"a"],W):-
 W :->
        numAlpha.
eSymbol([92|"x"],W):-
 W :->
         (token([92|"x"]) &> hexadecimal) <& symbol(";").
eSymbol([92|"i"],W):-
 W :->
         (token([92|"i"]) &> natural) <& symbol(";").
eSymbol([92|"o"],W):-
 W :->
         (token([92|"o"]) &> octal) <& symbol(";").
eSymbol([92|S],W):-                     % [O'\,S]
 W :->
         symbol(S).
eSymbol(S,W):-                          
 W :->
         symbol(S).

%------------------------------------------------------------------------------
/**     eSymbolA(+Symbol, ?Wrapper)
Text:   Varianta parseru eSymbolA vydávající atom.
Example:
        ?- s(" ")+L :-> eSymbolA("\s").
        L= [s("") > ' ']
        Yes
*/

eSymbolA(S,W):-
 W :->
	eSymbol(S) <@ ascii2Atom.

%------------------------------------------------------------------------------
/**     rSymbols(+RegSwitch, ?Wrapper)
Text:   Parser rozpoznávající jeden terminální symbol. Prvním argumentem
        je specifikace terminálù tak, jak je tomu v regulárních výrazech.
        Napøíklad:
v       "a-z"
        nebo
v       "^a-bX-Z\\"
        Uvnitø øetìzce mohou být
        pou¾ity metaznaky, které jsou uvedeny v dokumentaci predikátu
        eSymbol.
Example:
        ?- s("A;")+L :-> rSymbols("^a-z").
        L = [s(";") > 97]
        Yes
*/

% Popis:
%       Nejdrive je dle parametru RegExpr vytvoren parser, ktery je nasledne
% aplikovan na vstupni retezec.

rSymbols(R,I+L):-
 quickS(R)+[_>[P]] :->
        rSymCharList <@ rSymSwitch_CharList2P,
 deBug(verbose,[P]),
 I+L :->
        P.
rSymbols(_,_+[]).

%- Auxiliary predicates --------------

% rSymBackChar(?Wrapper)
% - akceptuje znak, jemuz predchazi zpetne lomitko
rSymBackChar(W):-
 W :->
        symbol([92])                    % ascii2Atom(92,'\')
        &>
        (
         symbol("n") <@ const(symbol([10])) % korektni jak pod DOS: 0xD,*0xA*
          <:                                % tak UNIX: *0xA*
         symbol("t") <@ const(symbol([9]))
          <:
         symbol("r") <@ const(symbol([13]))
          <:
         symbol("e") <@ const(symbol([27]))
          <:
         symbol("s") <@ const(whiteSpace)
          <:
         symbol("S") <@ const(nonFulfil(isWhiteSpace))
          <:
         symbol("l") <@ const(lower)
          <:
         symbol("u") <@ const(upper)
          <:
         symbol("w") <@ const(letter)
          <:
         symbol("d") <@ const(digit<@ascii)
          <:
         symbol("D") <@ const(nonFulfil(isDigit))
          <:
         symbol("a") <@ const(numAlpha)
          <:
         (symbol("x") &> hexadecimal) <& symbol(";") <@ alter(X,symbol([X]))
          <:
         (symbol("i") &> natural) <& symbol(";") <@ alter(X,symbol([X]))
          <:
         (symbol("o") &> octal) <& symbol(";") <@ alter(X,symbol([X]))
          <:
         item <@ alter(X,symbol([X]))
        ).

% rSymCharList(?Wrapper)
% - akceptuje seznam pripustnych znaku v regularnim vyrazu

rSymCharList(W):-
 W :->
        (symbol("^") <?@> switch-const(nonSwitch))
         <&>
        (rSymCharInterval <: rSymCharacter)<+> .

% rSymCharInterval(?Wrapper)
rSymCharInterval(W):-
 W :->
        rSymCharacter <&>> symbol("-") &> rSymCharacter.

% rSymCharacter(?Wrapper)
rSymCharacter(W):-
 W :->
        nonSymbols([92|"*+?[]|()"])
         <:
        rSymListBackChar.

% rSymListBackChar(?Wrapper)
rSymListBackChar(W):-
 W :->
        symbol([92])                    % ascii2Atom(92,'\')
        &>
        (
         nonSymbols("nt")
          <:
         symbol("n") <@ const(10)       % korektni jak pod OS DOS: 0xD,*0xA*
          <:                            % tak UNIX: *0xA*
         symbol("t") <@ const(9)
        ).

% rSymSwitch_CharList2P(+List, -Parser)
% - transformuje vystup parseru rSymCharList/4 na parser seznamu pripustnych
%   znaku
rSymSwitch_CharList2P([switch|CharList],[P]):-
    rSymSwitch_Concat(CharList,String,IntervalP,switch),
    (IntervalP=[] -> P=symbols(String)
                  ;  (String=[] -> P=IntervalP
                                ;  P= <:(symbols(String),IntervalP))).
rSymSwitch_CharList2P([nonSwitch|CharList],[P]):-
    rSymSwitch_Concat(CharList,String,IntervalP,nonSwitch),
    (IntervalP=[] -> P=nonSymbols(String)
                  ;  (String=[] -> P=IntervalP
                                ;  P= <:(nonSymbols(String),IntervalP))).

% rSymSwitch_Concat(+ListOfCharsAndIntervals, -String, -IntervalP, +Flag)
% - transformuje znaky v ListOfCharsAndIntervals na retezec String a
%   seznam intervalu podminek IntervalP
% - Flag: switch v nonSwitch
rSymSwitch_Concat([],[],[],_).
rSymSwitch_Concat([V|T],S,I,F):-
    rSymSwitch_Concat(T,Ss,Is,F),
    (
     V=(From>To)
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

%- EOF ------------------------------------------------------------------------
