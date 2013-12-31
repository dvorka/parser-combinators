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
Text:   Iter�tory vyu��vaj�c� navracen�.
*/
%------------------------------------------------------------------------------
/** <\/>(+Parser, ?Wrapper)
Text:   Iter�tor nezpracov�v� vstup klasick�m zp�sobem, ale pou��v�
        intern� cyklus repeat-fail, proto m� v�razn� men�� po�adavky
        na prostor i �as. Lze ho vyu��t pouze v m�du, jen� udr�uje 
        pernamentn� informaci o pozici ve vstupn�m textu, kter� 
        p�etrv� i navracen� - pro tento ��el je p�ipraven m�d filter/1.
p       Rozklad je ��zen obsahem seznamu �sp�n�ch rozklad� po ka�d� iteraci
        - dan� parser se aplikuje dokud je seznam �sp�n�ch rozklad�
        nepr�zdn� (rozklad lze tedy explicitn� ukon�it pou�it�m
        primitiva terminate a naopak prodlou�it mut�torem <@@/3).
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
Text:   Iter�tor je variantou
v       <\/>/2
        kter� m� stejn� vlastnosti,
        pouze iteruje dokud je seznam �sp�n�ch rozklad� pr�zdn�. Lze
        ho tedy pou��vat ve smyslu parser� non* nap�. pro p�ij�m�n�
        proudu token� zakon�en�ho n�jak�m speci�ln�ch termin�torem.
        Op�t je ur�en pro m�d filter/1.
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
