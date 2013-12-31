%------------------------------------------------------------------------------
%
%                                  Vlnka
%
%                               Martin Dvorak
%                                    2000
%------------------------------------------------------------------------------
/** Module: Vlnka
Text:   Program pro vkládání znakù tilda, jen¾ zabraòují ne¾ádoucímu
        zalamování øádkù v LaTeXových dokumentech.
*/
%------------------------------------------------------------------------------
% Popis:
%       Vstup je delen na tokeny (program se chova jako scanner). Vyuziva
% se vlastni mod, ktery urcuje zda, tildy maji byt vkladany (napriklad
% v prostredi verbatim se ~ nevkladaji).
%       Mezi tokeny se identifikuji prikazy, ktere prepinaji mod. Dale
% jednoznakove tokeny, za ktere se vklada ~.
%       Je to pouze demostracni priklad, takze nema osetreny vsechny
% vyjimky.
%       Priklad je vsak funkcni do te miry, ze s nim mohly byt bezproblemu
% osetreny *vsechny* LaTeXove soubory textu diplomove prace.
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
% Inicializace & knihovny

 :-op(500,xfy,vlnkaOptionIs).

 debug       vlnkaOptionIs no.         % (yes,no), rovnez pc/library/debug.pl

:- ['../../loadSWI'].          % SWI Prolog
%:- ['../../loadBP-2'].        % BinProlog

%-----------------------------------------------------------------------------
%                                    Demo
%------------------------------------------------------------------------------

go1:-
        %I=s("a aa \\begin{quote}\\begin{verbatim} b b \\end{verbatim}\\end{quote} d dd"),
        I=s("a aa \\verb|d d d| b bb \\begin{verbatim}a a\\end{verbatim} c cc"),

        vlnka(I,_).

go2:-
        tildiFile('in.tex','out.tex').

go:-
        printf([nl,'Type go1 or go2.',nl]).
%------------------------------------------------------------------------------
/**     tildiFile(+InputFile, -OutputFile)
Text:   Vlnkování souboru.
*/

tildiFile(InputFile,OutputFile):-
        InputFile\==OutputFile,
	printf(['TildiFile: ',InputFile,' -> ',OutputFile]),
        openFile(OutputFile,Old,write),
         openStream(InputFile,Handle,OldHandle,read),
          atStream(Handle,Pos),
                vlnka(quickFile(Handle,Pos),_),
         closeStream(OldHandle,read),
        closeFile(Old,write),
	printf([' Done!',nl]).

% varianta pro mod pseudoll1
tildiFile(pseudoll1,InputFile,OutputFile):-
        InputFile\==OutputFile,
	printf(['TildiFile: ',InputFile,' -> ',OutputFile]),
        (debug vlnkaOptionIs yes,assert(pcDeBugName(debug)) ; (retractall(pcDeBugName(_)),assert(pcDeBugName(noname)))),

        Parser   =vlnkaToken<*> ,
        I        =vlnka(enabled,x,quickFile(Handle,Pos)),
        Algorithm=lazy,
        OFollow  =useFOLLOW,
        Dbf      =assert,
        First    =enum,

        InputFile\==OutputFile,
        openFile(OutputFile,Old,write),
         openStream(InputFile,Handle,OldHandle,read),
          atStream(Handle,Pos),
          (Algorithm=lazy -> FOLLOW=[item] ; FOLLOW=[pcTrue]),
          getLookAhead(I,LA,Ii),
           pseudoll1(Algorithm^OFollow^Dbf^First,LA,FOLLOW,Ii)+_ :->
                        Parser,
         closeStream(OldHandle,read),
        closeFile(Old,write),
	printf([' Done!',nl]).

%------------------------------------------------------------------------------
/**     vlnka(+Input, -Los)
Text:   Vlnkování vstupu do standardního výstupního proudu.
*/

vlnka(I,L):-
        vlnka(enabled,x,I)+L :->
                vlnkaToken<*> .

%------------------------------------------------------------------------------
/**     vlnka(+Switch, +VerbChar +Selector, -LOS)
Text:   Predikát, jím¾ je zaveden u¾ivatelský mód 'vlnka'.
Arg:    Switch
        Atom 'enabled', pokud lze tildovat resp. 'disabled' v pøípadì opaèném.
Arg:    VerbChar
        V otevøeném módu verbatim obsahuje token, který má prostøedí zavøít.
        Programátor jej mù¾e inicializovat libovolnou hodnotou (mimo
        verbatim toti¾ není hodnota definována).
*/

vlnka(Switch,V,S,LOS):-
        item(S+L),
        (L=[N>R] -> LOS=[vlnka(Switch,V,N)>R] ; LOS=[]).

%------------------------------------------------------------------------------
% vlnkaToken(?Wrapper)
% - jeden token v modu vlnka
vlnkaToken(W):-
 W :->
       (
        % switch vlnkovanciho modu
        % vypnuti
        (token("\\")            % left factorization
         <&&>
          (token("begin{verbatim}")
           <:
          token("verb") <&&> (token("+") <: token("|"))))
                                        <@@ vlnkaTokenSwitchOff
         <:
        % zapnuti
        (token("\\end{verbatim}")
         <:
        (token("+") <: token("|")))     <@@ vlnkaTokenSwitchOn
         <:
        token("\\")
         <:
        % komentar nevlnkuj
        token("%") <&&> nonSymbol([10])<*>
         <:
        % tildovat - lisi se podle toho, zda jsem ve verb nebo ne
        letter <&>> whiteSpace<+>       <@@ vlnkaTokenTildify
         <:
        % bezny token
        nonSymbols([32,10,13,9,92,43,124])<+>
         <:
        % whitespace
        whiteSpace<+>
       ) <@ showString => const(v).

%------------------------------------------------------------------------------
% vlnkovani
% - vlnka
vlnkaTokenTildify(vlnka(enabled,V,S)>(Letter>_),vlnka(enabled,V,S)>[Letter,126]).
vlnkaTokenTildify(vlnka(disabled,V,S)>(Letter>Wsp),vlnka(disabled,V,S)>[Letter|Wsp]).
% - pseudoll1 (<@@ -> vnejsi vrstva)
vlnkaTokenTildify(pseudoll1(O,LA,FOLLOW,vlnka(enabled,V,S))>(Letter>_),pseudoll1(O,LA,FOLLOW,vlnka(enabled,V,S))>[Letter,126]).
vlnkaTokenTildify(pseudoll1(O,LA,FOLLOW,vlnka(disabled,V,S))>(Letter>Wsp),pseudoll1(O,LA,FOLLOW,vlnka(disabled,V,S))>[Letter|Wsp]).

%------------------------------------------------------------------------------
% vypnuti vlnkovani
% - vlnka
vlnkaTokenSwitchOff(vlnka(disabled,V,S)>R,vlnka(disabled,V,S)>R).
vlnkaTokenSwitchOff(vlnka(enabled,_,S)>R,vlnka(disabled,VV,S)>R):-
        R="\\verb+" -> VV=43
         ;
        R="\\verb|" -> VV=124
         ;
        VV=env.                         % \begin{verbatim}
% - pseudoll1
vlnkaTokenSwitchOff(pseudoll1(O,LA,FOLLOW,vlnka(disabled,V,S))>R,pseudoll1(O,LA,FOLLOW,vlnka(disabled,V,S))>R).
vlnkaTokenSwitchOff(pseudoll1(O,LA,FOLLOW,vlnka(enabled,_,S))>R,pseudoll1(O,LA,FOLLOW,vlnka(disabled,VV,S))>R):-
        R="\\verb+" -> VV=43
         ;
        R="\\verb|" -> VV=124
         ;
        VV=env.                         % \begin{verbatim}
vlnkaTokenSwitchOff(X,X).
        %deBugAssert(fail,['vlnkaTokenSwitchOff - unable to switch it off ',nl,X,nl,nl]).

%------------------------------------------------------------------------------
% zapnuti vlnkovani
% - vlnka
vlnkaTokenSwitchOn(vlnka(enabled,V,S)>R,vlnka(enabled,V,S)>R).
vlnkaTokenSwitchOn(vlnka(disabled,V,S)>[V],vlnka(enabled,V,S)>[V]). % \verb| |, \verb+ +
vlnkaTokenSwitchOn(vlnka(disabled,env,S)>[124],vlnka(disabled,env,S)>[124]). % \begin{verbatim}
vlnkaTokenSwitchOn(vlnka(disabled,env,S)>[43],vlnka(disabled,env,S)>[43]). % \begin{verbatim}
vlnkaTokenSwitchOn(vlnka(disabled,env,S)>R,vlnka(enabled,env,S)>R). % \begin{verbatim}

% - pseudoll1
vlnkaTokenSwitchOn(pseudoll1(O,LA,FOLLOW,vlnka(enabled,V,S))>R,pseudoll1(O,LA,FOLLOW,vlnka(enabled,V,S))>R).
vlnkaTokenSwitchOn(pseudoll1(O,LA,FOLLOW,vlnka(disabled,env,S))>R,pseudoll1(O,LA,FOLLOW,vlnka(enabled,env,S))>R). % \begin{verbatim}
vlnkaTokenSwitchOn(pseudoll1(O,LA,FOLLOW,vlnka(disabled,V,S))>[V],pseudoll1(O,LA,FOLLOW,vlnka(enabled,V,S))>[V]). % \verb| |, \verb+ +
vlnkaTokenSwitchOn(X,X). % napriklad 124 - [43]
        %deBugAssert(fail,['vlnkaTokenSwitchOn - unable to switch it on: ',nl,X,nl,nl]).

%------------------------------------------------------------------------------
/**     stripVlnka(+InputFile, +OutputFile)
Text:   Odstraní vlnkování ze vstupního souboru
*/

% pouziti uzivatelskeho modu encode
stripVlnka(InputFile,OutputFile):-
        InputFile\==OutputFile,
	printf(['TrimTildiFile: ',InputFile,' -> ',OutputFile]),
        openFile(OutputFile,Old,write),
         openStream(InputFile,Handle,OldHandle,read),
          atStream(Handle,Pos),
                prn(encode(vlnkaKillTilda,quickFile(Handle,Pos)))+_
                :->     item<*>,
         closeStream(OldHandle,read),
        closeFile(Old,write),
	printf([' Done!',nl]).

vlnkaKillTilda(126,32).         % ~ -> space
vlnkaKillTilda(X,X).

%- EOF ------------------------------------------------------------------------
