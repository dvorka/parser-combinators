%------------------------------------------------------------------------------
%
%                              Parser mutators
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Mut�tor vytv��� z ji� existuj�c�ho parseru, kter� je jeho parametrem,
        parser nov�, jen� je n�jak�m zp�sobem modifikov�n - vytv��� tedy
        jeho mutaci.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     <@(+Parser, +Fun, ?Wrapper)
Text:   Mut�tor vyv��ej�c� parser, kter� modifikuje sv�j v�sledek aplikac�
        dan� s�mantick� akce na ka�dou polo�ku struktury LOS. Zbytek vstupu,
        kter� se nepoda�ilo p�ijmout, z�st�v� nedot�en.
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
Text:   Mut�tor nejd��ve aplikuje na vstup Parser. Potom jsou z takto
        z�skan�ho seznamu �sp�n�ch rozklad� odfiltrov�ny polo�ky, 
        kter� nevyhovuj� podm�nce Condition.        
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
Text:   Mut�tor nejd��ve aplikuje na vstup Parser. Potom jsou z takto
        z�skan�ho seznamu �sp�n�ch rozklad� odfiltrov�ny polo�ky, 
        kter� vyhovuj� podm�nce Condition.
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
Text:   Mut�tor vyv��ej�c� parser, kter� modifikuje sv�j v�sledek aplikac�
        dan� s�mantick� akce na ka�dou polo�ku struktury LOS. Zbytek vstupu,
        kter� se nepoda�ilo p�ijmout, z�st�v� nedot�en. Narozd�l od mut�toru
        <@/3, kter� pracuje pouze s v�sledkem, tento konstruktor transformuje
        pomoc� predik�tu Fun celou derivaci. Umo��uje tak pracovat i se
        selektory m�d�.
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
Text:   Mut�tor, kter� vytvo�� parser akceptuj�c� 0 nebo v�ce iterac�
        p�vodn�ho parseru. Vnit�n� je pro reprezentaci v�sledku pou�ita
        n-tice, kde n je po�et iterac�.
        Toto je obecn� verze. Bottom je termin�tor (parser) vytv��ej�c�
        z�klad struktury LOS. Fun je predik�t transformuj�c� strukturu LOS po
        ka�d� iteraci.
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
Text:   Tento mut�tor je specializac� <*@*>/2. Vytv��� parser akceptuj�c�
        0 nebo v�ce iterac� parseru p�vodn�ho. V�sledkem je seznam.
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
Text:   Tento mut�tor je specializac� <*@*>/2. Vytv��� parser akceptuj�c�
        0 nebo v�ce iterac� parseru p�vodn�ho. V�sledkem je n-tice.
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
Text:   Mut�tor, kter� vytvo�� parser akceptuj�c� 1 nebo v�ce iterac�
        p�vodn�ho parseru.
Arg:    Actions
        +Bottom - +Fun. Bottom je termin�tor vytv��ej�c� z�klad struktury
        LOS a Fun predik�t transformuj�c� strukturu LOS po ka�d� iteraci.
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
Text:   Tento mut�tor je specializac� <+@+>/2. Vytv��� parser akceptuj�c�
        1 nebo v�ce iterac� parseru p�vodn�ho . V�sledkem je seznam.
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
Text:   Tento mut�tor je specializac� <+@+>/2. Vytv��� parser akceptuj�c�
        1 nebo v�ce iterac� parseru p�vodn�ho . V�sledkem je n-tice.
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
Text:   Mut�tor vytv��ej�c� alternativu p�vodn�ho parseru. Akceptuje
        jednu nebo ��dnou iteraci. Pokud iterace usp�je, je v�sledek
        vytvo�en vol�n�m Yes, v opa�n�m p��pad� je v�sledkem konstanta No.
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
Text:   Tento mut�tor je specializac� <?@?>/2. Vytv��� parser akceptuj�c�
        ��dnou nebo jednu iteraci parseru p�vodn�ho. Tato varianta je vhodn�,
        pokud je v�stupem seznam.
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
