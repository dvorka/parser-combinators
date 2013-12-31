%------------------------------------------------------------------------------
%
%                      "Smart" kombinatory a mutatory
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/**     Module:
Text:   Verze kombinátorù a mutátorù umo¾òující vyu¾ití operátorové notace
        pro zefektivnìní výpoètu.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                               Kombinatory
%------------------------------------------------------------------------------
% <&@>(+Parser1, +Parser2Fun, ?Wrapper)
<&@>(ll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2-Fun):-
        seqCompAux(Fun,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
<&@>(pseudoll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2-Fun):-
        seqCompAux(Fun,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).
<&@>(empty+L:->P1,P2-_):-
        <&>>(P1,P2,empty+L).
<&@>(first+FIRST:->P1,P2-_):-                      % vyuziva empty
        <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
<&@>(eFirst+eFirst(Empty,FIRST):->P1,P2-_):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<&@>(eFirst(assert)+eFirst(Empty,FIRST):->P1,P2-_):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

<&@>(I+L:->P1,P2-F):-
    I+L1 :-> P1,
    <&@>^(P2-F,L1,L-[]).

%------------------------------------------------------------------------------
% <&>>(Wrapper :-> Parser1, Parser2)
<&>>(ll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2):-
        seqCompAux(<&>>^^^ ,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
<&>>(pseudoll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2):-
        seqCompAux(<&>>^^^ ,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).
<&>>(empty+L:->P1,P2):-
        <&>>(P1,P2,empty+L).
<&>>(eFirst+eFirst(Empty,FIRST):->P1,P2):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<&>>(first+FIRST:->P1,P2):-
        <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
<&>>(eFirst(assert)+eFirst(Empty,FIRST):->P1,P2):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

I+L :-> P1<&>>P2 :-
    I+L1 :-> P1,
    <&>>^(P2,L1,L-[]).
    
%------------------------------------------------------------------------------
% <&(Wrapper :-> Parser1, Parser2)
<&(ll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2):-
        seqCompAux(<&^^^ ,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
<&(pseudoll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2):-
        seqCompAux(<&^^^ ,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).
<&(empty+L:->P1,P2):-
        <&>>(P1,P2,empty+L).
<&(eFirst+eFirst(Empty,FIRST):->P1,P2):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<&(first+FIRST:->P1,P2):-
        <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
<&(eFirst(assert)+eFirst(Empty,FIRST):->P1,P2):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

I+L :-> P1<&P2 :-
    I+L1 :-> P1,
    <&^(P2,L1,L-[]).

%------------------------------------------------------------------------------
% &>(Wrapper :-> Parser1, Parser2)
&>(ll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2):-
        seqCompAux(&>^^^ ,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
&>(pseudoll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2):-
        seqCompAux(&>^^^ ,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).
&>(empty+L:->P1,P2):-
        <&>>(P1,P2,empty+L).
&>(eFirst+eFirst(Empty,FIRST):->P1,P2):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
&>(first+FIRST:->P1,P2):-
        <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
&>(eFirst(assert)+eFirst(Empty,FIRST):->P1,P2):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

I+L :-> P1&>P2 :-
    I+L1 :-> P1,
    &>^(P2,L1,L).

%------------------------------------------------------------------------------
% <&>(Wrapper :-> Parser1, Parser2)
<&>(ll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2):-
        seqCompAux(<&>^^^ ,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
<&>(pseudoll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2):-
        seqCompAux(<&>^^^ ,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).
<&>(empty+L:->P1,P2):-
        <&>>(P1,P2,empty+L).
<&>(eFirst+eFirst(Empty,FIRST):->P1,P2):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<&>(first+FIRST:->P1,P2):-
        <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
<&>(eFirst(assert)+eFirst(Empty,FIRST):->P1,P2):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

I+L :-> P1<&>P2 :-
    I+L1 :-> P1,
    <&>^(P2,L1,L-[]).

%------------------------------------------------------------------------------
% <&&>(Wrapper :-> Parser1, Parser2)
<&&>(ll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2):-
        seqCompAux(<&&>^^^ ,P1,P2,ll1(Options,LookAhead,FOLLOW,I)+L).
<&&>(pseudoll1(Options,LookAhead,FOLLOW,I)+L:->P1,P2):-
        seqCompAux(<&&>^^^ ,P1,P2,pseudoll1(Options,LookAhead,FOLLOW,I)+L).
<&&>(empty+L:->P1,P2):-
        <&>>(P1,P2,empty+L).
<&&>(eFirst+eFirst(Empty,FIRST):->P1,P2):-
        <&>>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<&&>(first+FIRST:->P1,P2):-
        <&>>(P1,P2,eFirst+eFirst(_,FIRST)).
<&&>(eFirst(assert)+eFirst(Empty,FIRST):->P1,P2):-
        <&>>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

I+L :-> P1<&&>P2 :-
    I+L1 :-> P1,
    <&&>^(P2,L1,L-[]).

%------------------------------------------------------------------------------
% <:>(Wrapper :-> Parser1, Parser2)
<:>(ll1(Opt,LA,FOLLOW,I)+L:->P1,P2):-
        <:>(P1,P2,ll1(Opt,LA,FOLLOW,I)+L).
<:>(pseudoll1(Opt,LA,FOLLOW,I)+L:->P1,P2):-
        <:>(P1,P2,pseudoll1(Opt,LA,FOLLOW,I)+L).

<:>(empty+L:->P1,P2):-
        <:>(P1,P2,empty+L).
<:>(eFirst+eFirst(Empty,FIRST):->P1,P2):-
        <:>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<:>(first+FIRST:->P1,P2):-
        <:>(P1,P2,eFirst+eFirst(_,FIRST)).
<:>(eFirst(assert)+eFirst(Empty,FIRST):->P1,P2):-
        <:>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

I+L :-> P1<:>P2 :-
    I+L1 :-> P1,
    I+L2 :-> P2,
    append(L1,L2,L).

%------------------------------------------------------------------------------
% <:(Wrapper :-> Parser1, Parser2)
<:(ll1(Opt,LA,FOLLOW,I)+L:->P1,P2):-
        <:(P1,P2,ll1(Opt,LA,FOLLOW,I)+L).
<:(pseudoll1(Opt,LA,FOLLOW,I)+L:->P1,P2):-
        <:(P1,P2,pseudoll1(Opt,LA,FOLLOW,I)+L).

<:(empty+L:->P1,P2):-
        <:>(P1,P2,empty+L).
<:(eFirst+eFirst(Empty,FIRST):->P1,P2):-
        <:>(P1,P2,eFirst+eFirst(Empty,FIRST)).
<:(first+FIRST:->P1,P2):-
        <:>(P1,P2,eFirst+eFirst(_,FIRST)).
<:(eFirst(assert)+eFirst(Empty,FIRST):->P1,P2):-
        <:>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

I+L :-> P1<:P2 :-
    I+L_ :-> P1,
    ( L_=[] -> I+L:->P2 ; L=L_ ).

%------------------------------------------------------------------------------
% :>(Wrapper :-> Parser1, Parser2)
:>(ll1(Opt,LA,FOLLOW,I)+L:->P1,P2):-
        :>(P1,P2,ll1(Opt,LA,FOLLOW,I)+L).
:>(pseudoll1(Opt,LA,FOLLOW,I)+L:->P1,P2):-
        :>(P1,P2,pseudoll1(Opt,LA,FOLLOW,I)+L).

:>(empty+L:->P1,P2):-
        <:>(P1,P2,empty+L).
:>(eFirst+eFirst(Empty,FIRST):->P1,P2):-
        <:>(P1,P2,eFirst+eFirst(Empty,FIRST)).
:>(first+FIRST:->P1,P2):-
        <:>(P1,P2,eFirst+eFirst(_,FIRST)).
:>(eFirst(assert)+eFirst(Empty,FIRST):->P1,P2):-
        <:>(P1,P2,eFirst(assert)+eFirst(Empty,FIRST)).

I+L :-> P1:>P2 :-
    I+L_ :-> P2,
    ( L_=[] -> I+L:->P1 ; L=L_ ).

%------------------------------------------------------------------------------
% <::>(?Wrapper :-> +Environment)
<::>(W :-> PE):-                        % rozbaleni tele by zefektivnilo
        <::>(PE,W).                     % (-) synchronizace kodu

%------------------------------------------------------------------------------
% <:^>(?Wrapper :-> +Parser1, +Parser2)
<:^>(I+L:->P1,P2):-
        I+L1 :-> P1,
        I+L2 :-> P2,
        append(L1,L2,L).

%------------------------------------------------------------------------------
% <:^(?Wrapper :-> +Parser1, +Parser2)
<:^(I+L:->P1,P2):-
        I+L_ :-> P1,
        ( L_=[] -> I+L :-> P2 ; L=L_ ).

%------------------------------------------------------------------------------
% :>^(?Wrapper :-> +Parser1, +Parser2)
:>^(I+L:->P1,P2):-
        I+L_ :-> P2,
        ( L_=[] -> I+L :-> P1 ; L=L_ ).

%------------------------------------------------------------------------------
%                            Tradicni mutatory
%------------------------------------------------------------------------------
% <@(?Wrapper :-> +Parser, +Fun)
<@(empty+L:->P,_):-
        empty+L :-> P.
<@(eFirst+eFirst(Empty,FIRST):->P,_):-
        eFirst+eFirst(Empty,FIRST) :-> P.               % no redirect
<@(first+FIRST:->P,_):-
        first+FIRST :-> P.
<@(eFirst(assert)+eFirst(Empty,FIRST):->P,_):-
        eFirst(<@(P,_),Empty,FIRST)
         ;
        (eFirst(assert)+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<@(P,_),Empty,FIRST).

<@(I+FL :-> P,F):-
    I+L :-> P,
    <@^(F,L,FL).

%------------------------------------------------------------------------------
% <?(?Wrapper :-> +Parser, +Cond)
<?(empty+L:->P,_):-
        empty+L :-> P.
<?(eFirst+eFirst(Empty,FIRST):->P,_):-
        eFirst+eFirst(Empty,FIRST) :-> P.
<?(first+FIRST:->P,_):-
        first+FIRST :-> P.                              % no redirect
<?(eFirst(assert)+eFirst(Empty,FIRST):->P,_):-
        eFirst(<?(P,_),Empty,FIRST)
         ;
        (eFirst(assert)+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<?(P,_),Empty,FIRST).

<?(I+FL :-> P,Cond):-
        I+L :-> P,
        filter(sieve(Cond),L,FL).

%------------------------------------------------------------------------------
% <?-(?Wrapper :-> +Parser, +Cond)
<?-(empty+L:->P,_):-
        empty+L :-> P.
<?-(eFirst+eFirst(Empty,FIRST):->P,_):-
        eFirst+eFirst(Empty,FIRST) :-> P.               % no redirect
<?-(first+FIRST:->P,_):-
        first+FIRST :-> P.
<?-(eFirst(assert)+eFirst(Empty,FIRST):->P,_):-
        eFirst(<?-(P,_),Empty,FIRST)
         ;
        (eFirst(assert)+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<?-(P,_),Empty,FIRST).

<?-(I+FL :-> P,Cond):-
        I+L :-> P,
        nonFilter(sieve(Cond),L,FL).

%------------------------------------------------------------------------------
% <@@(?Wrapper :-> +Parser, +Fun)
<@@(empty+L:->P,_):-
        empty+L :-> P.
<@@(eFirst+eFirst(Empty,FIRST):->P,_):-
        eFirst+eFirst(Empty,FIRST) :-> P.               % no redirect                       
<@@(first+FIRST:->P,_):-
        first+FIRST :-> P.
<@@(eFirst(assert)+eFirst(Empty,FIRST):->P,_):-
        eFirst(<@@(P,_),Empty,FIRST)
         ;
        (eFirst(assert)+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<@@(P,_),Empty,FIRST).

<@@(I+FL :-> P,F):-
        I+L :-> P,
        <@@^(F,L,FL).

%------------------------------------------------------------------------------
% <*@*>(?Wrapper :-> +Parser, +Bottom - +Fun )
<*@*>(empty+L:->P,F):-
        (empty+L :-> P)<*@>F.
<*@*>(first+FIRST:->P,F):-
        (first+FIRST :-> P)<*@>F.
<*@*>(eFirst+eFirst(Empty,FIRST):->P,F):-
        (eFirst+eFirst(Empty,FIRST) :-> P)<*@>F.
<*@*>(eFirst(assert)+eFirst(Empty,FIRST):->P,F):-
        (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*@>F.

<*@*>(W :-> P,B-F):-
 W :->
        (P <&>> P<*@*>B-F <@ F)
         <:>
        B.

%------------------------------------------------------------------------------
% <**>(?Wrapper :-> +Parser)
<**>(empty+L:->P):-
    (empty+L :-> P)<*> .
<**>(first+FIRST:->P):-
    (first+FIRST :-> P)<*> .
<**>(eFirst+eFirst(Empty,FIRST):->P):-
    (eFirst+eFirst(Empty,FIRST) :-> P)<*> .
<**>(eFirst(assert)+eFirst(Empty,FIRST):->P):-
    (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*> .

<**>(W :-> P):-
 W :->
        P <&> P<**>
         <:>
        epsilon.

%------------------------------------------------------------------------------
% <**>>(+Parser, ?Wrapper)
<**>>(empty+L:->P):-
    (empty+L :-> P)<*> .
<**>>(first+FIRST:->P):-
    (first+FIRST :-> P)<*> .
<**>>(eFirst+eFirst(Empty,FIRST):->P):-
    (eFirst+eFirst(Empty,FIRST) :-> P)<*> .
<**>>(eFirst(assert)+eFirst(Empty,FIRST):->P):-
    (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*> .

<**>>(W :-> P):-
 W :->       
        P <*@*> return(*)-id.

%------------------------------------------------------------------------------
% <+@+>(?Wrapper :-> +Parser, +Bottom - +Fun )
<+@+>(W :-> P,B-F):-
 W :->
        (P <&>> P<*@*>B-F) <@ F.

%------------------------------------------------------------------------------
% <++>(?Wrapper :-> +Parser)
<++>(W :-> P):-
 W :->
        P <&> P<**> .

%------------------------------------------------------------------------------
% <++>>(?Wrapper :-> +Parser)
<++>>(W :-> P):-
 W :->
        P <&> P<**>> .

%----+--------------------------------------------------------------------------
%H <?@?>(?Wrapper :-> +Parser, +No - +Yes )
<?@?>(W :-> P,No-Yes):-
 W :->
        (P <@ Yes)
         <:>
        return(No).

%------------------------------------------------------------------------------
% <??>(?Wrapper :-> +Parser)
<??>(W :-> P):-
 W :->
        P
         <:>
        epsilon.

%------------------------------------------------------------------------------
%                          Diamantove mutatory
%------------------------------------------------------------------------------
% <>(?Wrapper :-> +Parser )
<>(empty+Empty:->P):-
   empty+Empty :-> P.
<>(first+FIRST:->P):-
   first+FIRST :-> P.
<>(eFirst+eFirst(Empty,FIRST):->P):-
   eFirst+eFirst(Empty,FIRST) :-> P.                    % no redirect
<>(eFirst(assert)+eFirst(Empty,FIRST):->P):-
        eFirst(<>(P),Empty,FIRST)
         ;
        (eFirst+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<>(P),Empty,FIRST).

<>(I+L :-> P):-
        I+L_ :-> P,
        (L_=[L__|_] -> L=[L__] ; L=[]).

%------------------------------------------------------------------------------
% <<*@>>(?Wrapper :-> +Parser, +Bottom - +Fun )
<<*@>>(empty+L:->P,F):-
      (empty+L :-> P)<*@>F.
<<*@>>(first+FIRST:->P,F):-
      (first+FIRST :-> P)<*@>F.
<<*@>>(eFirst+eFirst(Empty,FIRST):->P,F):-
      (eFirst+eFirst(Empty,FIRST) :-> P)<*@>F.
<<*@>>(eFirst(assert)+eFirst(Empty,FIRST):->P,F):-
      (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*@>F.

<<*@>>(W :-> P,B-F):-
 W :->
       (P <&>> P<<*@>>B-F <@ F
         <:
        B) <> .

%------------------------------------------------------------------------------
% <<*>>(?Wrapper :-> +Parser)
<<*>>(empty+L:->P):-
     (empty+L :-> P)<*> .
<<*>>(first+FIRST:->P):-
     (first+FIRST :-> P)<*> .
<<*>>(eFirst+eFirst(Empty,FIRST):->P):-
     (eFirst+eFirst(Empty,FIRST) :-> P)<*> .
<<*>>(eFirst(assert)+eFirst(Empty,FIRST):->P):-
     (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*> .

<<*>>(W :-> P):-
 W :->
        (P <&> P<<*>>
         <:
        epsilon) <> .

%------------------------------------------------------------------------------
% <<+@>>(?Wrapper :-> +Parser, +Bottom - +Fun )
<<+@>>(W :-> P,B-F):-
 W :->
        (P <&>> P<<*@>>B-F <@ F) <> .

%------------------------------------------------------------------------------
% <<+>>(?Wrapper :-> +Parser)
<<+>>(W :-> P):-
 W :->
       (P
         <&>
        P<<*>>) <> .

%------------------------------------------------------------------------------
% <<?@>>(?Wrapper :-> +Parser, +No - +Yes )
<<?@>>(W :-> P,No-Yes):-
 W :->
       (P <@ Yes
         <:
        succeed(No)) <> .

%------------------------------------------------------------------------------
% <<?>>(?Wrapper :-> +Parser)
<<?>>(W :-> P):-
 W :->
       (P
         <:
        epsilon) <> .

%------------------------------------------------------------------------------
%                       Mutatory se zkracenym vyhodnocenim
%------------------------------------------------------------------------------
% <*@>(?Wrapper :-> +Parser, +Bottom - +Fun )
<*@>(empty+Empty:->_,B-_):-
        % parser Bottom se aplikuje vzdy, zalezi pouze na nem
        empty+Empty :-> B.
<*@>(first+FIRST:->P,B-_):-
        first+FIRSTB :-> B, first+FIRSTP :-> P,
        append(FIRSTB,FIRSTP,FIRST).
<*@>(eFirst+eFirst(Empty,FIRST):->P,BF):-
        (empty+Empty :-> P)<*@>BF,
        (first+FIRST :-> P)<*@>BF.
<*@>(eFirst(assert)+eFirst(Empty,FIRST):->P,BF):-
        % vsechny varianty jsou presmerovany sem -> bezpecnejsi + uspora pameti
        eFirst(<*@>(P,BF),Empty,FIRST)
         ;
        (eFirst+eFirst(Empty,FIRST) :-> P)<*@>BF,
        eFirstAdd(<*@>(P,BF),Empty,FIRST).

<*@>(W :-> P,B-F):-
 W :->
        (P <&>> P<*@>B-F <@ F)
         <:
        B.

%------------------------------------------------------------------------------
% <*>(?Wrapper :-> +Parser)
<*>(empty+true:->_).            % parser Bottom je v tomto pripade epsilon/1
<*>(first+FIRST:->P):-
        first+FIRST :-> P.
<*>(eFirst+eFirst(true,FIRST):->P):-
        first+FIRST :-> P.
<*>(eFirst(assert)+eFirst(Empty,FIRST):->P):-
        eFirst(<*>(P),Empty,FIRST)
         ;
        (eFirst+eFirst(Empty,FIRST) :-> P)<*>,
        eFirstAdd(<*>(P),Empty,FIRST).

<*>(W :-> P):-
 W :->
        (P <&> P<*>)
         <:
        epsilon.

%------------------------------------------------------------------------------
% <+@>(?Wrapper :-> +Parser, +Bottom - +Fun )
% - eFirst pomoci jiz drive definovanych konstruktoru

<+@>(W :-> P,B-F):-
 W :->
        (P <&>> P<*@>B-F) <@ F.

%------------------------------------------------------------------------------
% <+>(?Wrapper :-> +Parser)
% - eFirst pomoci jiz drive definovanych konstruktoru

<+>(W :-> P):-
 W :->
        P
         <&>
        P<*> .

%------------------------------------------------------------------------------
% <?@>(?Wrapper :-> +Parser, +No - +Yes )
% - eFirst pomoci jiz drive definovanych konstruktoru

<?@>(W :-> P,No-Yes):-
 W :->
        (P <@ Yes)
         <:
        succeed(No).

%------------------------------------------------------------------------------
% <?>(?Wrapper :-> +Parser)
% - eFirst pomoci jiz drive definovanych konstruktoru

<?>(W :-> P):-
 W :->
        P
         <:
        epsilon.

%------------------------------------------------------------------------------
% <^>(?Wrapper :-> +Parser)
<^>(empty+Empty:->P):-
        empty+Empty :-> P.
<^>(first+FIRST:->P):-
        first+FIRST :-> P.
<^>(eFirst+eFirst(Empty,FIRST):->P):-
        empty+Empty :-> P,first+FIRST :-> P.
<^>(eFirst(assert)+eFirst(Empty,FIRST):->P):-
        eFirst(<^>(P),Empty,FIRST)
         ;
        (eFirst+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<^>(P),Empty,FIRST).

<^>(I+TrimL :-> P):-
        I+L :-> P,
        <^>^(L,_,TrimL).

%------------------------------------------------------------------------------
% <^@>(?Wrapper :-> +Parser)
<^@>(empty+Empty:->P-_):-
        empty+Empty :-> P.
<^@>(first+FIRST:->P-_):-
        first+FIRST :-> P.
<^@>(eFirst+eFirst(Empty,FIRST):->P-_):-
        empty+Empty :-> P,first+FIRST :-> P.
<^@>(eFirst(assert)+eFirst(Empty,FIRST):->P-_):-
        eFirst(<^@>(P-_),Empty,FIRST)
         ;
        (eFirst+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<^@>(P-_),Empty,FIRST).

<^@>(I+L :-> P-F) :-
 I+L_ :->P,
    ( L_==[]
       -> L=[]
       ;  <^@>^(F,L_,_,L)).

%------------------------------------------------------------------------------
% <^@@>(?Wrapper :-> +Parser)
<^@@>(empty+Empty:->P-_):-
        empty+Empty :-> P.
<^@@>(first+FIRST:->P-_):-
        first+FIRST :-> P.
<^@@>(eFirst+eFirst(Empty,FIRST):->P-_):-
        empty+Empty :-> P,first+FIRST :-> P.
<^@@>(eFirst(assert)+eFirst(Empty,FIRST):->P-_):-
        eFirst(<^@@>(P-_),Empty,FIRST)
         ;
        (eFirst+eFirst(Empty,FIRST) :-> P),
        eFirstAdd(<^@@>(P-_),Empty,FIRST).

<^@@>(I+L :-> P-F) :-
 I+L_ :->P,
    ( L_==[]
       -> L=[]
       ;  <^@@>^(F,L_,_,L)).

%------------------------------------------------------------------------------
%                           Repeat-fail mutatory
%------------------------------------------------------------------------------
% <\/>(?Wrapper :-> +Parser)
<\/>(empty+L:->P):-
    (empty+L :-> P)<*> .
<\/>(first+FIRST:->P):-
    (first+FIRST :-> P)<*> .
<\/>(eFirst+eFirst(Empty,FIRST):->P):-
    (eFirst+eFirst(Empty,FIRST) :-> P)<*> .
<\/>(eFirst(assert)+eFirst(Empty,FIRST):->P):-
    (eFirst(assert)+eFirst(Empty,FIRST) :-> P)<*> .

<\/>(I+L :-> P):-
        repeat,
         invokeParserOnce(P,I+L),
        L=[],           % until .... pokracovani je rizeno obsahem seznamu
                        % uspesnych rozkladu -> volajici muze snadno
                        % ovlivnot ukoncei.
        !.              % konec cyklu.

%------------------------------------------------------------------------------
% </\>(?Wrapper :-> +Parser)
</\>(I+L:->P):-
        repeat,
         invokeParserOnce(P,I+L),
        L\=[], 
        !.     

%- EOF ------------------------------------------------------------------------
