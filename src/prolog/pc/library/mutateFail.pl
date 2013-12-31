%------------------------------------------------------------------------------
%
%                            Repeat-fail mutator
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/** Module:
Text:   Iterátory vyu¾ívající navracení.
*/
%------------------------------------------------------------------------------
/** <\/>(+Parser, ?Wrapper)
Text:   Iterátor nezpracovává vstup klasickým zpùsobem, ale pou¾ívá
        internì cyklus repeat-fail, proto má výraznì men¹í po¾adavky
        na prostor i èas. Lze ho vyu¾ít pouze v módu, jen¾ udr¾uje 
        pernamentní informaci o pozici ve vstupním textu, která 
        pøetrvá i navracení - pro tento úèel je pøipraven mód filter/1.
p       Rozklad je øízen obsahem seznamu úspì¹ných rozkladù po ka¾dé iteraci
        - daný parser se aplikuje dokud je seznam úspì¹ných rozkladù
        neprázdný (rozklad lze tedy explicitnì ukonèit pou¾itím
        primitiva terminate a naopak prodlou¾it mutátorem <@@/3).
Example:
        unix2DosFilterRepeat(W):-
        | W :->
        |    (
        |     nonSymbol([10]) <@ showAtom
        |      <:
        |     symbol([10])
        |      <@ const([13,10]) => string2Atom => show
        |    )<\/> .
*/

<\/>(P,empty+L):-
        (empty+L :-> P)<*> .
<\/>(P,first+FIRST):-
        (first+FIRST :-> P)<*> .
<\/>(P,eFirst+eFirst(Empty,FIRST)):-
        (eFirst+eFirst(Empty,FIRST) :-> P)<*> .
<\/>(P,eFirst(assert)+eFirst(Empty,FIRST)):-
        (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*> .

<\/>(P,I+L):-
        repeat,
         invokeParserOnce(P,I+L),
        L=[],           % until .... pokracovani je rizeno obsahem seznamu
                        % uspesnych rozkladu -> volajici muze snadno
                        % ovlivnit ukonceni.
        !.              % konec cyklu.

%------------------------------------------------------------------------------
/**     </\>(+Parser, ?Wrapper)
Text:   Iterátor je variantou
v       <\/>/2
        která má stejné vlastnosti,
        pouze iteruje dokud je seznam úspì¹ných rozkladù prázdný. Lze
        ho tedy pou¾ívat ve smyslu parserù non* napø. pro pøijímání
        proudu tokenù zakonèeného nìjakým speciálních terminátorem.
        Opìt je urèen pro mód filter/1.
Example:
        % while not the end of line
        ?- filter(Sh)+L :->
        |       (itemA <? ==('\n'))</\> .
*/

</\>(P,I+L):-
        repeat,
         invokeParserOnce(P,I+L),
        L\=[], 
        !.     

%------------------------------------------------------------------------------
% invokeParserOnce(Parser,I,L)
% - predikat nutny vzhledem ke zpusobu implementace knihovnich parseru
%   a vyuzivani mechanismu navraceni v <\/>.

invokeParserOnce(P,W):-
        W :-> P,!.

%- EOF ------------------------------------------------------------------------
