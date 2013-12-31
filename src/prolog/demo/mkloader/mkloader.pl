%------------------------------------------------------------------------------
%
%                                  MkLoader
%
%                               Martin Dvorak
%                                    2000
%------------------------------------------------------------------------------
/** Module: MkLoader
Text:   Program generuj�c� zavad��e knihovny kombin�tor� parser� pro
        r�znou hloubku v adres��ov� struktu�e.
*/
%------------------------------------------------------------------------------
% Inicializace & knihovny

 :- ['../../loadSWI'].          % SWI Prolog

 %:- ['../../loadBP-2'].        % BinProlog

%-----------------------------------------------------------------------------
%                                    Demo
%------------------------------------------------------------------------------
go:-
        mkLoader('loadBP-2.pl','x.pl',0).

%------------------------------------------------------------------------------
% Popis:
%       Ze zavadece pro hloubku druhe urovne zanoreni jsou vygenerovany
% zavadece pro hloubku 1 a aktualni adresar. Je rozpoznavano nekolik druhu
% direktiv, komentare jsou ponechany.
%------------------------------------------------------------------------------
/** mkLoader(+InputFile, +OutputFile, +Depth)
Text:   Vygeneruje ze souboru InputFile do souboru OutputFile zavade� 
        pro hloubku Depth.
Arg:    Depth
        P��pustn� hodnoty jsou 0 a 1.
Arg:    InputFile
        Atom se jm�nem vstupn�ho souboru.
Arg:    OutputFile
        Atom se jm�nem v�stupn�ho souboru.
*/

mkLoader(IFile,OFile,Depth):-
        (Depth=0 ; Depth=1)
                -> printf(['MkLoader: ',IFile,' -> ',OFile,nl,' Creating loader for depth ',Depth,'.']),
                   invokeFilter(mklLine(Depth)<*>,IFile,OFile,_)
                ; pcError(error,['Depth can be 0 or 1.']).
        

%------------------------------------------------------------------------------
% mklLine(?Wrapper)
% - analyze line by line
mklLine(Depth,W):-
 W :->
        (mklDirective(Depth)
          <:
         getLine)       <@ string2Atom => shownl => const(x).

%------------------------------------------------------------------------------
mklDirective(Depth,W):-
 W :->
        ( #>token(":-")<#
         <&> (token("consult(") <: symbol("["))
                <&> (getAtomized <@ mklTransform(Depth))
         <&> symbols(")]") <&> symbol(".") <&> getLine) <@ coloneFlattenString.

%------------------------------------------------------------------------------
% mklTransform(+Depth, +Input, -Output)

% - cut out "../"
mklTransform(1,I,[39|O]):-
        (append("../",Oo,I) ; O="Error"),
        append(Oo,"'",O).

% - cut out "../../"
mklTransform(0,I,[39|O]):-
        (append("../../",Oo,I) ; O="Error"),
        append(Oo,"'",O).

%- EOF ------------------------------------------------------------------------
