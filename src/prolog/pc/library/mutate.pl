%------------------------------------------------------------------------------
%
%                              Parser mutators
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Mutátor vytváøí z ji¾ existujícího parseru, který je jeho parametrem,
        parser nový, jen¾ je nìjakým zpùsobem modifikován - vytváøí tedy
        jeho mutaci.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     <@(+Parser, +Fun, ?Wrapper)
Text:   Mutátor vyváøející parser, který modifikuje svùj výsledek aplikací
        dané sémantické akce na ka¾dou polo¾ku struktury LOS. Zbytek vstupu,
        který se nepodaøilo pøijmout, zùstává nedotèen.
Example:
        ?- s("Linus Torvalds")+L
        |       :-> token("Linus") <@ append("Mr ").
        L = [s(" Torvalds") > "Mr Linus"]
        Yes
*/

<@(P,_,empty+L):-                                       
        empty+L :-> P.
<@(P,_,eFirst+eFirst(Empty,FIRST)):-
        eFirst+eFirst(Empty,FIRST) :-> P.                       % no redirect
<@(P,_,first+FIRST):-
        first+FIRST :-> P.
<@(P,_,eFirst(assert)+eFirst(Empty,FIRST)):-
        eFirst(<@(P,_),Empty,FIRST)
         ;
        (eFirst(assert)+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<@(P,_),Empty,FIRST).

% <@(+Parser, +Fun, ?Wrapper)
<@(P,F,I+FL):-
    I+L :-> P,
    <@^(F,L,FL).

<@^(F,[N>R|Ls],[N>FR|FLs]):-
    :-@ [F,R,FR],               
    <@^(F,Ls,FLs).
<@^(_,[],[]).

%------------------------------------------------------------------------------
/**     <?(+Parser, +Condition, ?Wrapper)
Text:   Mutátor nejdøíve aplikuje na vstup Parser. Potom jsou z takto
        získaného seznamu úspì¹ných rozkladù odfiltrovány polo¾ky, 
        které nevyhovují podmínce Condition.        
*/

<?(P,_,empty+L):-
        empty+L :-> P.
<?(P,_,eFirst+eFirst(Empty,FIRST)):-
        eFirst+eFirst(Empty,FIRST) :-> P.                       % no redirect
<?(P,_,first+FIRST):-
        first+FIRST :-> P.
<?(P,_,eFirst(assert)+eFirst(Empty,FIRST)):-
        eFirst(<?(P,_),Empty,FIRST)
         ;
        (eFirst(assert)+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<?(P,_),Empty,FIRST).

<?(P,Cond,I+FL):-
        I+L :-> P,
        filter(sieve(Cond),L,FL).

sieve(Cond,_>R):- :-@ [Cond,R].

%------------------------------------------------------------------------------
/**     <?-(+Parser, +Condition, ?Wrapper)
Text:   Mutátor nejdøíve aplikuje na vstup Parser. Potom jsou z takto
        získaného seznamu úspì¹ných rozkladù odfiltrovány polo¾ky, 
        které vyhovují podmínce Condition.
*/

<?-(P,_,empty+L):-
        empty+L :-> P.
<?-(P,_,eFirst+eFirst(Empty,FIRST)):-
        eFirst+eFirst(Empty,FIRST) :-> P.
<?-(P,_,first+FIRST):-
        first+FIRST :-> P.
<?-(P,_,eFirst(assert)+eFirst(Empty,FIRST)):-
        eFirst(<?-(P,_),Empty,FIRST)
         ;
        (eFirst(assert)+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<?-(P,_),Empty,FIRST).

<?-(P,Cond,I+FL):-
        I+L :-> P,
        nonFilter(sieve(Cond),L,FL).

%------------------------------------------------------------------------------
/**     <@@(+Parser, +Fun, ?Wrapper)
Text:   Mutátor vyváøející parser, který modifikuje svùj výsledek aplikací
        dané sémantické akce na ka¾dou polo¾ku struktury LOS. Zbytek vstupu,
        který se nepodaøilo pøijmout, zùstává nedotèen. Narozdíl od mutátoru
        <@/3, který pracuje pouze s výsledkem, tento konstruktor transformuje
        pomocí predikátu Fun celou derivaci. Umo¾òuje tak pracovat i se
        selektory módù.
*/

<@@(P,_,empty+L):-
        empty+L :-> P.
<@@(P,_,eFirst+eFirst(Empty,FIRST)):-
        eFirst+eFirst(Empty,FIRST) :-> P.                       % no redirect                               
<@@(P,_,first+FIRST):-
        first+FIRST :-> P.
<@@(P,_,eFirst(assert)+eFirst(Empty,FIRST)):-
        eFirst(<@@(P,_),Empty,FIRST)
         ;
        (eFirst(assert)+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<@@(P,_),Empty,FIRST).

% <@@(+Parser, +Fun, ?Wrapper)
<@@(P,F,I+FL):-
    I+L :-> P,
    <@@^(F,L,FL).

<@@^(F,[D|Ls],[DR|FLs]):-
    :-@ [F,D,DR],
    <@@^(F,Ls,FLs).
<@@^(_,[],[]).

%------------------------------------------------------------------------------
/**     <*@*>(+Parser, +Actions, ?Wrapper)
Text:   Mutátor, který vytvoøí parser akceptující 0 nebo více iterací
        pùvodního parseru. Vnitønì je pro reprezentaci výsledku pou¾ita
        n-tice, kde n je poèet iterací.
        Toto je obecná verze. Bottom je terminátor (parser) vytváøející
        základ struktury LOS. Fun je predikát transformující strukturu LOS po
        ka¾dé iteraci.
Arg:    Actions
        +Bottom - +Fun.
*/
% Poznamky:
% - ekvivalentne lze:
%      P <*@*> return(*)-id       ... vystupem je seznam dvojic <**>>
%      P <*@*> epsilon-tuple2List ... P<**>

<*@*>(P,F,empty+L):-                                            % <*@>/2
         (empty+L :-> P)<*@>F.
<*@*>(P,F,first+FIRST):-
         (first+FIRST :-> P)<*@>F.
<*@*>(P,F,eFirst+eFirst(Empty,FIRST)):-
         (eFirst+eFirst(Empty,FIRST) :-> P)<*@>F.
<*@*>(P,F,eFirst(assert)+eFirst(Empty,FIRST)):-
         (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*@>F.

% <*@*>(+Parser, +Bottom - +Fun, ?Wrapper)
<*@*>(P,B-F,W):-
 W :->
        (P <&>> P<*@*>B-F <@ F)
         <:>
        B.

%------------------------------------------------------------------------------
/**     <**>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <*@*>/2. Vytváøí parser akceptující
        0 nebo více iterací parseru pùvodního. Výsledkem je seznam.
Example:
        ?- s("125.6E-23")+L :-> digit<**>.
        L = [s(".6E-23") > [1,2,5]|_]
        Yes
*/

<**>(P,empty+L):-                                               %<*>/1
      (empty+L :-> P)<*> .
<**>(P,first+FIRST):-
      (first+FIRST :-> P)<*> .
<**>(P,eFirst+eFirst(Empty,FIRST)):-
      (eFirst+eFirst(Empty,FIRST) :-> P)<*> .
<**>(P,eFirst(assert)+eFirst(Empty,FIRST)):-
      (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*> .

% <**>(+Parser, ?Wrapper)
<**>(P,W):-
 W :->
        (P <&> P<**>)
         <:>
        epsilon.

%------------------------------------------------------------------------------
/**     <**>>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <*@*>/2. Vytváøí parser akceptující
        0 nebo více iterací parseru pùvodního. Výsledkem je n-tice.
Example:
        ?- s("125.6E-23")+L :-> digit<**>>.
        L = [s(".6E-23")> (1> (2> (5> *)))|_ ]
        Yes
*/

<**>>(P,empty+L):-                                               %<*>/1
      (empty+L :-> P)<*> .
<**>>(P,first+FIRST):-
      (first+FIRST :-> P)<*> .
<**>>(P,eFirst+eFirst(Empty,FIRST)):-
      (eFirst+eFirst(Empty,FIRST) :-> P)<*> .
<**>>(P,eFirst(assert)+eFirst(Empty,FIRST)):-
      (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*> .

% <**>>(+Parser, ?Wrapper)
<**>>(P,W):-
 W :->       
        P <*@*> return(*)-id.

%------------------------------------------------------------------------------
/**     <+@+>(+Parser, +Actions, ?Wrapper)
Text:   Mutátor, který vytvoøí parser akceptující 1 nebo více iterací
        pùvodního parseru.
Arg:    Actions
        +Bottom - +Fun. Bottom je terminátor vytváøející základ struktury
        LOS a Fun predikát transformující strukturu LOS po ka¾dé iteraci.
*/
% Poznamky:
% - ekvivalentne lze:
%       P <+@+> epsilon-tuple2List   ...   P<++>
% - eFirst pomoci jiz drive definovanych konstruktoru

% <+@+>(+Parser, +Bottom - +Fun, ?Wrapper)
<+@+>(P,B-F,W):-
 W :->
        (P <&>> P<*@*>B-F) <@ F.

%------------------------------------------------------------------------------
/**     <++>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <+@+>/2. Vytváøí parser akceptující
        1 nebo více iterací parseru pùvodního . Výsledkem je seznam.
Example:
        ?- s("125.6E-23")+L :-> digit<++>.
        L = [s(".6E-23") > [1,2,5]|_]
        Yes
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <++>(+Parser, ?Wrapper)
<++>(P,W):-
 W :->
        P <&> P<**> .

%------------------------------------------------------------------------------
/**     <++>>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <+@+>/2. Vytváøí parser akceptující
        1 nebo více iterací parseru pùvodního . Výsledkem je n-tice.
Example:
        ?- s("125.6E-23")+L :-> digit<++>>.
        L = [s(".6E-23") > (1>(2>(5>*)))|_]
        Yes
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <++>>(+Parser, ?Wrapper)
<++>>(P,W):-
 W :->
        P <&>> P<**>> .

%------------------------------------------------------------------------------
/**     <?@?>(+Parser, +Actions, ?Wrapper)
Text:   Mutátor vytváøející alternativu pùvodního parseru. Akceptuje
        jednu nebo ¾ádnou iteraci. Pokud iterace uspìje, je výsledek
        vytvoøen voláním Yes, v opaèném pøípadì je výsledkem konstanta No.
Arg:    Actions
        +Yes - +No. 
Example:
        ?- s("-125.6E-23")+L
        |       :-> symbol("-") <?@?> []-ascii2Atom.
        L = [s("125.6E-23") > '-']
        Yes
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <?@?>(+Parser, +No - +Yes, ?Wrapper)
<?@?>(P,No-Yes,W):-
 W :->
        (P <@ Yes)
         <:>
        return(No).

%------------------------------------------------------------------------------
/**     <??>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <?@?>/2. Vytváøí parser akceptující
        ¾ádnou nebo jednu iteraci parseru pùvodního. Tato varianta je vhodná,
        pokud je výstupem seznam.
Example:
        ?- s("-125.6E-23")+L :-> symbolA("-")<??>.
        L = [s("125.6E-23") > '-'|_]
        Yes
        ?- s("-125.6E-23")+L :-> symbolA("+")<??>.
        L = [s("-125.6E-23") > []]
        Yes
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <??>(+Parser, ?Wrapper)
<??>(P,W):-
 W :->
        P
         <:>
        epsilon.

%------------------------------------------------------------------------------
% Poznamky:
% - staticky vypocet empty a FIRST:
%   vzajemne volani predikatu v techto modech je realizovano tak, ze je vse,
%   az na vyjimky, presmerovano na 'smart' verze zkraceneho vyhodnoceni:
%    <@, <@@, <?, <?-, <^>      -> na miste
%    <*@*>/3,<*@*>/2            -> <*@>/2
%    <*@>/3,                    -> <*@>/2
%    <+@+>/3,<+@+>/2,<+@>/3     -> implicit
%    <?@?>/3,<?@?>/2,<?@>/3     -> implicit
%    <**>/2,<**>/1,<*>/2        -> <*>/1
%    <++>/2,<++>/1,<+>/2        -> implicit
%    <??>/2,<??>/1,<?>/2        -> implicit
%    <^@>/2,<^@@>/2,<^>/2       -> ./2
%    <\/>/2,</\>/2              -> <*>/1
%    <>/2                       -> <>/1
%    <<>> verze                 -> <*@>/2,<*>/1
%- EOF ------------------------------------------------------------------------
