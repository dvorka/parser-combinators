%------------------------------------------------------------------------------
%
%                          Partial parser mutators
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Mutátory parserù vyu¾ívající zkráceného vyhodnocování v
        kombinátorech <: a :>.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     <*@>(+Parser, +BottomFun, ?Wrapper)
Text:   Mutátor, který vytvoøí parser akceptující 0 nebo více iterací
        parseru pùvodního. Mutant provádí vnitønì èásteèné vyhodnocení tak,
        ¾e výsledná struktura LOS je z èásti oøíznuta - obsahuje pouze
        maximální øe¹ení.
Arg:    BottomFun
        Argument má formát +Bottom - +Fun.
        Bottom je terminátor, který vytváøí základ struktury LOS.
        Fun je predikát transformující strukturu LOS po ka¾dé iteraci.
*/

<*@>(P,BF,empty+Empty):-
          (empty+Empty :-> P)<*@>BF.
<*@>(P,BF,first+FIRST):-
        (first+FIRST :-> P)<*@>BF.
<*@>(P,BF,eFirst+eFirst(Empty,FIRST)):-
         (eFirst+eFirst(Empty,FIRST) :-> P)<*@>BF.
<*@>(P,BF,eFirst(assert)+eFirst(Empty,FIRST)):-
         (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*@>BF.

% <*@>(+Parser, +Bottom - +Fun, ?Wrapper)
<*@>(P,B-F,W):-
 W :->
        (P <&>> P<*@>B-F <@ F)
         <:
        B.

%------------------------------------------------------------------------------
/**     <*>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <*@>/2. Vytváøí parser akceptující
        ¾ádnou nebo více iterací parseru pùvodního. Nový parser rovnì¾
        provádí vnitønì èásteèné vyhodnocení. Výstupem je LOS obsahující
        pouze maximální øe¹ení (kterých v¹ak mù¾e být nìkolik). Výsledky
        pùvodního parseru se ukládají do seznamu.
Example:
        ?- s("125.6E-23")+L :-> digit<*>.
        L = [s(".6E-23") > [1,2,5]]
        Yes
*/

<*>(P,empty+L):-
     (empty+L :-> P)<*> .
<*>(P,first+FIRST):-
     (first+FIRST :-> P)<*> .
<*>(P,eFirst+eFirst(Empty,FIRST)):-
     (eFirst+eFirst(Empty,FIRST) :-> P)<*> .
<*>(P,eFirst(assert)+eFirst(Empty,FIRST)):-
     (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*> .

% <*>(+Parser, ?Wrapper)
<*>(P,W):-
 W :->
        (P <&> P<*>)
         <:
        epsilon.

%------------------------------------------------------------------------------
/**     <+@>(+Parser, +BottomFun, ?Wrapper)
Text:   Mutátor, který vytvoøí parser akceptující 1 nebo více iterací
        pùvodního parseru. Mutant provádí èásteèné vyhodnocení za bìhu
        tak, ¾e výsledná struktura LOS je z èásti oøíznuta.
Arg:    BottomFun
        Argument má formát +Bottom - +Fun.
        Bottom je terminátor, který vytváøí základ struktury LOS.
        Fun je predikát transformující strukturu LOS po ka¾dé iteraci.
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <+@>(+Parser, +Bottom - +Fun, ?Wrapper)
<+@>(P,B-F,W):-
 W :->
        (P <&>> P<*@>B-F) <@ F.

%------------------------------------------------------------------------------
/**     <+>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <+@>/2. Vytváøí parser akceptující
        jednu nebo více iterací parseru pùvodního. Nový parser rovnì¾
        oøezává strukturu LOS za bìhu. Výsledky pùvodního parseru jsou
        ukládány do seznamu.
Example:
        ?- s("125.6E-23")+L :-> digit<+>.
        L = [s(".6E-23") > [1,2,5]]
        Yes
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <+>(+Parser, ?Wrapper)
<+>(P,W):-
 W :->
        P
         <&>
        P<*> .

%------------------------------------------------------------------------------
/**     <?@>(+Parser, +NoYes, ?Wrapper)
Text:   Mutátor vytváøející alternativu pùvodního parseru. Akceptuje
        jednu nebo ¾ádnou iteraci. Pokud iterace uspìje je výsledek
        vytvoøen voláním Yes a ukonèen, v opaèném pøípadì je výsledkem
        konstantní hodnota No.
Arg:    NoYes
        Argument má formát +No - +Yes.
Arg: Yes
        Predikát pro provedení transformace v pøípadì úspì¹né
        aplikace parseru.
Arg: No
        Konstanta vydaná v pøípadì, ¾e se Parser nepodaøilo aplikovat.
Example:
        ?- s("-125.6E-23")+L :-> symbol("-") <?@> []-ascii2Atom.
        L = [s("125.6E-23") > '-']
        Yes
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <?@>(+Parser, +No - +Yes, ?Wrapper)
<?@>(P,No-Yes,W):-
 W :->
        (P <@ Yes)
         <:
        return(No).

%------------------------------------------------------------------------------
/**     <?>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <?@>/2. Vytváøí parser akceptující
        ¾ádnou nebo jednu iteraci parseru pùvodního . Tato varianta je
        vhodná pokud je výstupem seznam.
Example:
        ?- s("-125.6E-23")+L :-> symbolA("-")<?>.
        L = [s("125.6E-23") > '-']
        Yes
        ?- s("-125.6E-23")+L :-> symbolA("-")<??>.
        L = [s("125.6E-23") > '-',_]
        Yes
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <?>(+Parser, ?Wrapper)
<?>(P,W):-
 W :->
        P
         <:
        epsilon.

%- EOF ------------------------------------------------------------------------
