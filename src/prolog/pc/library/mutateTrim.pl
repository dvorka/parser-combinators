%------------------------------------------------------------------------------
%
%                           Trim parser mutators
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Iter�tory o�ez�vaj�c� seznam �sp�n�ch rozklad� mut�torem <>.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     <>(+Parser, ?Wrapper)
Text:   Mut�tor prov�d�j�c� o��znut� struktury LOS, kter� je v�stupem
        libovoln�ho parseru. Ze struktury LOS  je vybr�na prvn� polo�ka,
        kter� se t�m stane v�stupem mutovan�ho parseru.
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
Text:   Mut�tor, kter� vytvo�� parser akceptuj�c� 0 nebo v�ce iterac�
        parseru p�vodn�ho. Nov� parser prov�d� o�ez�v�n� za b�hu tak, �e
        v�sledn� struktura LOS obsahuje nejv��e jednu polo�ku.
p       Tato varianta mut�toru <*@*> je prostorov� nejv�hodn�j��. D�ky
        zp�sobu o�ez�v�n� je v�ak z hlediska �asov� slo�itosti <*@>/2
        efektivn�j��.
Arg: BottomFun
        Bottom - +Fun
Arg: Bottom
        Termin�tor, kter� vytv��� z�klad struktury LOS.
Arg: Fun
        Predik�t transformuj�c� strukturu LOS po ka�d� iteraci.
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
Text:   Tento mut�tor je specializac� <*@>/2. Vytv��� parser akceptuj�c�
        jednu nebo v�ce iterac� parseru p�vodn�ho, kter� o�ez�v� strukturu
        LOS za b�hu. V�sledky p�vodn�ho parseru jsou ukl�d�ny do seznamu.
        Struktura LOS obsahuje d�ky o�ez�v�n� v�dy nejv��e jednu polo�ku.
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
Text:   Mut�tor, kter� vytvo�� parser akceptuj�c� 1 nebo v�ce iterac�
        p�vodn�ho parseru. Mutant prov�d� o�ez�v�n� za b�hu tak, �e
        v�sledn� struktura LOS obsahuje nejv��e jednu polo�ku.
p       Tato varianta mut�toru <+@+> je prostorov� nejv�hodn�j��. D�ky
        zp�sobu o�ez�v�n� je v�ak z hlediska �asov� slo�itosti
        <+@>/2 efektivn�j��.
Arg: BottomFun
        Bottom - +Fun
Arg: Bottom
        Termin�tor, kter� vytv��� z�klad struktury LOS.
Arg: Fun
        Predik�t transformuj�c� strukturu LOS po ka�d� iteraci.
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <+@>(+Parser, +Bottom - +Fun, ?Wrapper)
<<+@>>(P,B-F,W):-
 W :->
        (P <&>> P<<*@>>B-F <@ F) <> .

%------------------------------------------------------------------------------
/**     <<+>>(+Parser, ?Wrapper)
Text:   Tento mut�tor je specializac� <+@>/2. Vytv��� parser akceptuj�c�
        jednu nebo v�ce iterac� parseru p�vodn�ho. Nov� parser rovn�
        o�ez�v� strukturu LOS za b�hu. V�sledky p�vodn�ho parseru jsou
        ukl�d�ny do seznamu. Struktura LOS obsahuje v d�sledku o�ez�v�n�
        v�dy nejv��e jednu polo�ku.
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
Text:   Mut�tor vytv��ej�c� alternativu p�vodn�ho parseru. Akceptuje
        jednu nebo ��dnou iteraci. Pokud iterace usp�je, je v�sledek
        vytvo�en vol�n�m Yes a o��znut, v opa�n�m p��pad� je v�sledkem
        konstantn� hodnota No. Struktura LOS tedy v�dy obsahuje nejv��e
        jednu polo�ku.
Arg: NoYes
        +No - +Yes
Arg: Yes
        Predik�t pro proveden� transformace v p��pad� �sp�n�
        aplikace parseru.
Arg: No
        Konstanta vydan� v p��pad�, �e se Parser nepoda�ilo aplikovat.
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
Text:   Tento mut�tor je specializac� <<?@>>/2. Vytv��� parser akceptuj�c�
        ��dnou nebo jednu iteraci parseru p�vodn�ho, kter� o�ez�v� strukturu
        LOS tak, �e obsahuje nejv��e jednu polo�ku. Tato varianta je vhodn�
        pokud je v�sledkem seznam.
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
