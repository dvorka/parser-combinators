%------------------------------------------------------------------------------
%
%                              Parser mutators
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Mutátory parserù s vá¾ením derivací.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     <^@>(+ParserFun, ?Wrapper)
Text:   Oøezává seznam úspì¹ných rozkladù tak, ¾e obsahuje nejvý¹e jednu
        derivaci. Jako kritériu se pou¾ívá predikát Fun, který je "pøibalen"
        k prvnímu parametru a musí mít následující signaturu:
v               fun(Arg1, ..., ArgN, +Result, -Weigth)
        Váhy jednotlivých výsledkù jsou porovnány a je vybrán "nejtì¾¹í"
        z nich. V pøípadì rovnosti vah rozhoduje poøadí.
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
Text:   Varianta mutátoru urèená pro módy, která nepracuje pouze s
        výsledkem, ale s celou derivací.
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
Text:   Mutátor provádìjící oøíznutí struktury LOS, která je výstupem
        libovolného parseru. Ze struktury LOS je vybrána polo¾ka,
        která má dle ní¾e uvedeného kritéria nejvìt¹í váhu. Váha je
        typicky dána délkou vstupu (napø. nejvìt¹í dosa¾ený offset),
        ale pøi zavedení u¾ivatelského módu mù¾e být váha definována
        prakticky libovolnì. Získání váhy se provádí vybalením
        nejvnìj¹nìj¹ího deskriptoru (pcGetWeight/2), který je jejím
        nositelem. V pøípadì rovnosti vah rozhoduje mezi derivacemi
        jejich poøadí v seznamu úspì¹ných rozkladù.
p       Programátor mù¾e roz¹íøit predikát pcGetWeight/2 tak, aby
        pracoval i v jeho vlastních módech.
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
