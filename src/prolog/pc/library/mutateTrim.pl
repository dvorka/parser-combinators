%------------------------------------------------------------------------------
%
%                           Trim parser mutators
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Iterátory oøezávající seznam úspì¹ných rozkladù mutátorem <>.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     <>(+Parser, ?Wrapper)
Text:   Mutátor provádìjící oøíznutí struktury LOS, která je výstupem
        libovolného parseru. Ze struktury LOS  je vybrána první polo¾ka,
        která se tím stane výstupem mutovaného parseru.
Example:
        ?- s("125.6")+L :-> digit<**> <>.
        L = [s(".6") > [1, 2, 5]]
        Yes
*/

<>(P,empty+Empty):-
     empty+Empty :-> P.
<>(P,first+FIRST):-
     first+FIRST :-> P.
<>(P,eFirst+eFirst(Empty,FIRST)):-
     eFirst+eFirst(Empty,FIRST) :-> P .                 % no redirection
<>(P,eFirst(assert)+eFirst(Empty,FIRST)):-
     (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<> .

% <>(+Parser, ?Wrapper)
<>(P,I+L):-
        I+L_ :-> P,
        (L_=[L__|_] -> L=[L__] ; L=[]).

%------------------------------------------------------------------------------
/**     <<*@>>(+Parser, +BottomFun, ?Wrapper)
Text:   Mutátor, který vytvoøí parser akceptující 0 nebo více iterací
        parseru pùvodního. Nový parser provádí oøezávání za bìhu tak, ¾e
        výsledná struktura LOS obsahuje nejvý¹e jednu polo¾ku.
p       Tato varianta mutátoru <*@*> je prostorovì nejvýhodnìj¹í. Díky
        zpùsobu oøezávání je v¹ak z hlediska èasové slo¾itosti <*@>/2
        efektivnìj¹í.
Arg: BottomFun
        Bottom - +Fun
Arg: Bottom
        Terminátor, který vytváøí základ struktury LOS.
Arg: Fun
        Predikát transformující strukturu LOS po ka¾dé iteraci.
*/

<<*@>>(P,F,empty+L):-
          (empty+L :-> P)<*@>F.
<<*@>>(P,F,first+FIRST):-
          (first+FIRST :-> P)<*@>F.
<<*@>>(P,F,eFirst+eFirst(Empty,FIRST)):-
          (eFirst+eFirst(Empty,FIRST) :-> P)<*@>F.
<<*@>>(P,F,eFirst(assert)+eFirst(Empty,FIRST)):-
          (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*@>F.

% <<*@>>(+Parser, +Bottom - +Fun, ?Wrapper)
<<*@>>(P,B-F,W):-
 W :->
       (P <&>> P<<*@>>B-F <@ F
         <:
        B) <> .

%------------------------------------------------------------------------------
/**     <<*>>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <*@>/2. Vytváøí parser akceptující
        jednu nebo více iterací parseru pùvodního, který oøezává strukturu
        LOS za bìhu. Výsledky pùvodního parseru jsou ukládány do seznamu.
        Struktura LOS obsahuje díky oøezávání v¾dy nejvý¹e jednu polo¾ku.
Example:
        ?- s("125.6E-23")+L :-> digit<<*>>.
        L = [s(".6E-23") > [1,2,5]]
        Yes
*/

<<*>>(P,empty+L):-
       (empty+L :-> P)<*> .
<<*>>(P,first+FIRST):-
       (first+FIRST :-> P)<*> .
<<*>>(P,eFirst+eFirst(Empty,FIRST)):-
       (eFirst+eFirst(Empty,FIRST) :-> P)<*> .
<<*>>(P,eFirst(assert)+eFirst(Empty,FIRST)):-
       (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*> .

% <<*>>(+Parser, ?Wrapper)
<<*>>(P,W):-
 W :->
       (P <&> P<<*>>
         <:
        epsilon) <> .

%------------------------------------------------------------------------------
/**     <<+@>>(+Parser, +BottomFun, ?Wrapper)
Text:   Mutátor, který vytvoøí parser akceptující 1 nebo více iterací
        pùvodního parseru. Mutant provádí oøezávání za bìhu tak, ¾e
        výsledná struktura LOS obsahuje nejvý¹e jednu polo¾ku.
p       Tato varianta mutátoru <+@+> je prostorovì nejvýhodnìj¹í. Díky
        zpùsobu oøezávání je v¹ak z hlediska èasové slo¾itosti
        <+@>/2 efektivnìj¹í.
Arg: BottomFun
        Bottom - +Fun
Arg: Bottom
        Terminátor, který vytváøí základ struktury LOS.
Arg: Fun
        Predikát transformující strukturu LOS po ka¾dé iteraci.
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <+@>(+Parser, +Bottom - +Fun, ?Wrapper)
<<+@>>(P,B-F,W):-
 W :->
        (P <&>> P<<*@>>B-F <@ F) <> .

%------------------------------------------------------------------------------
/**     <<+>>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <+@>/2. Vytváøí parser akceptující
        jednu nebo více iterací parseru pùvodního. Nový parser rovnì¾
        oøezává strukturu LOS za bìhu. Výsledky pùvodního parseru jsou
        ukládány do seznamu. Struktura LOS obsahuje v dùsledku oøezávání
        v¾dy nejvý¹e jednu polo¾ku.
Example:
      ?- s("125.6E-23")+L :-> digit<<+>>.
      L = [s(".6E-23") > [1,2,5]]
      Yes
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <<+>>(+Parser, ?Wrapper)
<<+>>(P,W):-
 W :->
       (P
         <&>
        P<<*>>) <> .

%------------------------------------------------------------------------------
/**     <<?@>>(+Parser, +NoYes, ?Wrapper)
Text:   Mutátor vytváøející alternativu pùvodního parseru. Akceptuje
        jednu nebo ¾ádnou iteraci. Pokud iterace uspìje, je výsledek
        vytvoøen voláním Yes a oøíznut, v opaèném pøípadì je výsledkem
        konstantní hodnota No. Struktura LOS tedy v¾dy obsahuje nejvý¹e
        jednu polo¾ku.
Arg: NoYes
        +No - +Yes
Arg: Yes
        Predikát pro provedení transformace v pøípadì úspì¹né
        aplikace parseru.
Arg: No
        Konstanta vydaná v pøípadì, ¾e se Parser nepodaøilo aplikovat.
Example:
        ?- s("-125.6E-23")+L :-> symbol("-") <<?@>> []-ascii2Atom.
        L = [s("125.6E-23") > '-']
        Yes
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <<?@>>(+Parser, +No - +Yes, ?Wrapper)
<<?@>>(P,No-Yes,W):-
 W :->
       (P <@ Yes
         <:
        return(No)) <> .

%------------------------------------------------------------------------------
/**     <<?>>(+Parser, ?Wrapper)
Text:   Tento mutátor je specializací <<?@>>/2. Vytváøí parser akceptující
        ¾ádnou nebo jednu iteraci parseru pùvodního, který oøezává strukturu
        LOS tak, ¾e obsahuje nejvý¹e jednu polo¾ku. Tato varianta je vhodná
        pokud je výsledkem seznam.
Example:
        ?- s("-125.6E-23")+L :-> symbolA("-")<<?>>.
        L = [s("125.6E-23") > '-']
        Yes
        ?- s("-125.6E-23")+L :-> symbolA("-")<??>.
        L = [s("125.6E-23") > '-',_]
        Yes
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <<?>>(+Parser, ?Wrapper)
<<?>>(P,W):-
 W :->
       (P
         <:
        epsilon) <> .

%- EOF ------------------------------------------------------------------------
