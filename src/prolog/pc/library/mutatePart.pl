%------------------------------------------------------------------------------
%
%                          Partial parser mutators
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Mut�tory parser� vyu��vaj�c� zkr�cen�ho vyhodnocov�n� v
        kombin�torech <: a :>.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     <*@>(+Parser, +BottomFun, ?Wrapper)
Text:   Mut�tor, kter� vytvo�� parser akceptuj�c� 0 nebo v�ce iterac�
        parseru p�vodn�ho. Mutant prov�d� vnit�n� ��ste�n� vyhodnocen� tak,
        �e v�sledn� struktura LOS je z ��sti o��znuta - obsahuje pouze
        maxim�ln� �e�en�.
Arg:    BottomFun
        Argument m� form�t +Bottom - +Fun.
        Bottom je termin�tor, kter� vytv��� z�klad struktury LOS.
        Fun je predik�t transformuj�c� strukturu LOS po ka�d� iteraci.
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
Text:   Tento mut�tor je specializac� <*@>/2. Vytv��� parser akceptuj�c�
        ��dnou nebo v�ce iterac� parseru p�vodn�ho. Nov� parser rovn�
        prov�d� vnit�n� ��ste�n� vyhodnocen�. V�stupem je LOS obsahuj�c�
        pouze maxim�ln� �e�en� (kter�ch v�ak m��e b�t n�kolik). V�sledky
        p�vodn�ho parseru se ukl�daj� do seznamu.
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
Text:   Mut�tor, kter� vytvo�� parser akceptuj�c� 1 nebo v�ce iterac�
        p�vodn�ho parseru. Mutant prov�d� ��ste�n� vyhodnocen� za b�hu
        tak, �e v�sledn� struktura LOS je z ��sti o��znuta.
Arg:    BottomFun
        Argument m� form�t +Bottom - +Fun.
        Bottom je termin�tor, kter� vytv��� z�klad struktury LOS.
        Fun je predik�t transformuj�c� strukturu LOS po ka�d� iteraci.
*/
% - eFirst pomoci jiz drive definovanych konstruktoru

% <+@>(+Parser, +Bottom - +Fun, ?Wrapper)
<+@>(P,B-F,W):-
 W :->
        (P <&>> P<*@>B-F) <@ F.

%------------------------------------------------------------------------------
/**     <+>(+Parser, ?Wrapper)
Text:   Tento mut�tor je specializac� <+@>/2. Vytv��� parser akceptuj�c�
        jednu nebo v�ce iterac� parseru p�vodn�ho. Nov� parser rovn�
        o�ez�v� strukturu LOS za b�hu. V�sledky p�vodn�ho parseru jsou
        ukl�d�ny do seznamu.
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
Text:   Mut�tor vytv��ej�c� alternativu p�vodn�ho parseru. Akceptuje
        jednu nebo ��dnou iteraci. Pokud iterace usp�je je v�sledek
        vytvo�en vol�n�m Yes a ukon�en, v opa�n�m p��pad� je v�sledkem
        konstantn� hodnota No.
Arg:    NoYes
        Argument m� form�t +No - +Yes.
Arg: Yes
        Predik�t pro proveden� transformace v p��pad� �sp�n�
        aplikace parseru.
Arg: No
        Konstanta vydan� v p��pad�, �e se Parser nepoda�ilo aplikovat.
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
Text:   Tento mut�tor je specializac� <?@>/2. Vytv��� parser akceptuj�c�
        ��dnou nebo jednu iteraci parseru p�vodn�ho . Tato varianta je
        vhodn� pokud je v�stupem seznam.
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
