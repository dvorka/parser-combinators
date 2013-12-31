%------------------------------------------------------------------------------
%
%                              Parser mutators
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Mut�tory parser� s v�en�m derivac�.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     <^@>(+ParserFun, ?Wrapper)
Text:   O�ez�v� seznam �sp�n�ch rozklad� tak, �e obsahuje nejv��e jednu
        derivaci. Jako krit�riu se pou��v� predik�t Fun, kter� je "p�ibalen"
        k prvn�mu parametru a mus� m�t n�sleduj�c� signaturu:
v               fun(Arg1, ..., ArgN, +Result, -Weigth)
        V�hy jednotliv�ch v�sledk� jsou porovn�ny a je vybr�n "nejt쾹�"
        z nich. V p��pad� rovnosti vah rozhoduje po�ad�.
Arg:    ParserFun
        Parser - Fun
*/

<^@>(P,empty+Empty):-
      (empty+Empty :-> P)<^@> .
<^@>(P,first+FIRST):-
      (first+FIRST :-> P)<^@> .
<^@>(P,eFirst+eFirst(Empty,FIRST)):-
      (eFirst+eFirst(Empty,FIRST) :-> P)<^@> .
<^@>(P,eFirst(assert)+eFirst(Empty,FIRST)):-
      (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<^@> .

<^@>(P-F,I+L) :-
 I+L_ :->P,
    ( L_==[]
       -> L=[]
       ;  <^@>^(F,L_,_,L)).

% <^@>^(+Fun,+LOS,-Weight,-WinnerDeriv)
<^@>^(F,[N>R|T],Wo,Do):-
        <^@>^(F,T,Wi,Di),
        :-@[F,R,W],
        (Wi=<W -> Wo=W,Do=[N>R] ; Wo=Wi,Do=Di).
<^@>^(F,[N>R],W,[N>R]):-
        :-@[F,R,W].

%------------------------------------------------------------------------------
/**     <^@@>(+ParserFun, ?Wrapper)
Text:   Varianta mut�toru ur�en� pro m�dy, kter� nepracuje pouze s
        v�sledkem, ale s celou derivac�.
*/

<^@@>(PF,empty+Empty):-
      (empty+Empty :-> PF)<^@@> .
<^@@>(PF,first+FIRST):-
      (first+FIRST :-> PF)<^@@> .
<^@@>(PF,eFirst+eFirst(Empty,FIRST)):-
      (eFirst+eFirst(Empty,FIRST) :-> PF)<^@@> .
<^@@>(PF,eFirst(assert)+eFirst(Empty,FIRST)):-
      (eFirst(assert)+eFirst(Empty,FIRST) :-> PF)<^@@> .

<^@@>(P-F,I+L) :-
 I+L_ :->P,
    ( L_==[]
       -> L=[]
       ;  <^@@>^(F,L_,_,L)).

% <^@@>^(+Fun,+LOS,-Weight,-WinnerDeriv)
<^@@>^(F,[N>R|T],Wo,Do):-
        <^@@>^(F,T,Wi,Di),
        :-@[F,N>R,W],
        (Wi=<W -> Wo=W,Do=[N>R] ; Wo=Wi,Do=Di).
<^@@>^(F,[N>R],W,[N>R]):-
        :-@[F,N>R,W].

%------------------------------------------------------------------------------
/**     <^>(+Parser, ?Wrapper)
Text:   Mut�tor prov�d�j�c� o��znut� struktury LOS, kter� je v�stupem
        libovoln�ho parseru. Ze struktury LOS je vybr�na polo�ka,
        kter� m� dle n�e uveden�ho krit�ria nejv�t�� v�hu. V�ha je
        typicky d�na d�lkou vstupu (nap�. nejv�t�� dosa�en� offset),
        ale p�i zaveden� u�ivatelsk�ho m�du m��e b�t v�ha definov�na
        prakticky libovoln�. Z�sk�n� v�hy se prov�d� vybalen�m
        nejvn�j�n�j��ho deskriptoru (pcGetWeight/2), kter� je jej�m
        nositelem. V p��pad� rovnosti vah rozhoduje mezi derivacemi
        jejich po�ad� v seznamu �sp�n�ch rozklad�.
p       Program�tor m��e roz���it predik�t pcGetWeight/2 tak, aby
        pracoval i v jeho vlastn�ch m�dech.
Example:
        ?- off(0,s("Parser, ?Wrapper"))+L :->
        |       item<**> <^>.
        L=[off(17,s(""))>"Parser, ?Wrapper"]
        Yes
*/

<^>(P,empty+Empty):-
      (empty+Empty :-> P)<^> .
<^>(P,first+FIRST):-
      (first+FIRST :-> P)<^> .
<^>(P,eFirst+eFirst(Empty,FIRST)):-
      (eFirst+eFirst(Empty,FIRST) :-> P)<^> .
<^>(P,eFirst(assert)+eFirst(Empty,FIRST)):-
      (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<^> .

<^>(P,I+TrimL):-
        I+L :-> P,
        <^>^(L,_,TrimL).

% <^>^(+InputLOS,-Val,-OutputLOS)
% - vyhleda vysledek s nejvetsi vahou, napr.  dosazenym offsetem/radkou apod.
% - OutputLOS - LOS s jedinou derivaci, ktera ma nejvetsi vahu
<^>^([I>R|T],VV,LL):-
    <^>^(T,V,L),
    pcGetWeight(I,IV),
    (IV>=V -> LL=[I>R],VV=IV ; LL=L,VV=V).
<^>^([I>R],V,[I>R]):-                              % inicialni hodnota
    pcGetWeight(I,V).
<^>^([],-1,[]).                                    % inicialni hodnota

%------------------------------------------------------------------------------
%                               Aux
%------------------------------------------------------------------------------
% pcGetWeight(quickS(I),Value)
% - vybali vahu tj. meritko zpracovane casti vstupu:
%    off     -> Value=offset
%    line    -> Value=line
%    lineCol -> Value=line*1000+col
% - pokud si programator zavede vlastni mod, mel by byt tento predikat
%   rozsiren, tak aby byl schopen vybalovat vahy i z jeho selektoru

pcGetWeight(off(Off,_),Off).
pcGetWeight(line(Line,_),Line).
pcGetWeight(lineCol(L,C,_),Val):-
        Val is L*1000+C.
pcGetWeight(pseudoll1(_,_,_,I),II):-
        pcGetWeight(I,II).
pcGetWeight(ll1(_,_,_,I),II):-
        pcGetWeight(I,II).
pcGetWeight(quickS(I),I).
pcGetWeight(quickFile(I),I).
pcGetWeight(s(I),I).
pcGetWeight(file(I),I).
pcGetWeight(S,-1):-
        deBugAssert(fail,['Unknown selector:',nl,S,nl,tab,' in pcGetWeight/2']).

%- EOF ------------------------------------------------------------------------
