%------------------------------------------------------------------------------
%
%                         Ukazka pouziti mutatoru <\/>
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module: 
Text:   Ukázka pou¾ití repeat-fail mutátoru pøi vytváøení filtrù
        v knihovnì kombinátorù parserù.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
% Komentar:
%       Ukazka pouziti mutatoru <\/>, ktery pracuje s vedlejsim efektem
% pri vstupu ze souboru. Mutator je pouzit pro jednoduchou konverzi
% Unixovych souboru do DOS konvence (0xA -> 0xD 0xA) a vice versa.
% Protoze konstruktor <\/> iteruje v repeat-fail cyklu, lze ho pouzit
% pouze pro vytvareni programu, ktere nejakym zpusobem filtruji vstup.
% Vysledek je vypisovan do aktualniho vystupniho proudu - v tomto pripade
% do souboru.
%       Motivaci pro zavedeni tohoto mutatoru jsou nesrovnatelne mensi
% naroky na pamet, nez je tomu u tradicniho zpusobu analyzy a z toho
% plynouci zryhleni. V cyklu je totiz mechanismem navraceni uvolnovana 
% jiz za behu pouzita pamet zpet systemu a otevira se tim cesta ke 
% zpracovavani radove vetsich vstupnich textu. Vse lze porovnat na 
% nize uvedenych prikladech.
%       Pro srovnani jsou zde vsechny tri moznosti, jak filtry implemetovat.
% Objektivni pro srovnavani je az druhe spusteni cile.
%       Konstruktor <\/> vsak pouziva mod filter a v tomto modu jsou
% podporovany pouze nektere parsery a konstruktory a to se znacnymi 
% omezenimi, to vse navic v zavislosti na pouzitem interpretu! Na miste
% je tedy opatrnost (Eof)
%------------------------------------------------------------------------------
% Inicializace & knihovny

:- ['../../loadSWI'].          % SWI Prolog
%:- ['../../loadBP-2'].        % BinProlog

%------------------------------------------------------------------------------
%                                 Demo
%------------------------------------------------------------------------------

go:- gomd, gomu, gosd, gosu, gofd, gofu.

% Zavedeni vstupu do pameti
gomd:-  unix2Dos('data/unix.in','data/dos.out').
gomu:-  dos2Unix('data/dos.in','data/unix.out').

% Seekovani po souboru
gosd:-  unix2DosFile('data/unix.in','data/sdos.out').
gosu:-  dos2UnixFile('data/dos.in','data/sunix.out').

% Pouziti mutatoru <\/>
gofd:-   unix2DosRepeat('data/unix.in','data/fdos.out').
gofu:-   dos2UnixRepeat('data/dos.in','data/funix.out').

%------------------------------------------------------------------------------
%                       Zavedeni vstupu do pameti
%------------------------------------------------------------------------------
% unix2Dos(+UnixFileName, -DosFileName)
unix2Dos(UnixFile,DosFile):-
	printf(['Unix2Dos: ',UnixFile,' -> ',DosFile]),
         openFile(DosFile,Old,write),           % redirect output to file
          invokeLoadString(unix2DosFilter,UnixFile,_),
         closeFile(Old,write),                  % close output file
	printf(['... Bye!',nl]).

%------------------------------------------------------------------------------
% dos2Unix(+DosFileName, +UnixFileName)
dos2Unix(DosFile,UnixFile):-
	printf(['Dos2Unix: ',UnixFile,' -> ',DosFile]),
         openFile(UnixFile,Old,write),          % redirect output to file
          invokeLoadString(dos2UnixFilter,DosFile,_),
         closeFile(Old,write),                  % close output file
	printf(['... Bye!',nl]).

%------------------------------------------------------------------------------
%                         Seekovani po souboru
%------------------------------------------------------------------------------
% unix2DosFile(+UnixFileName, -DosFileName)
unix2DosFile(UnixFile,DosFile):-
	printf(['Unix2Dos: ',UnixFile,' -> ',DosFile]),
         % filtr s pouhym presmerovanim vystupniho proudu do souboru
         invokeFilter(unix2DosFilter,UnixFile,DosFile,_),
	printf(['... Bye!',nl]).

% dos2UnixFile(+DosFileName, +UnixFileName)
dos2UnixFile(DosFile,UnixFile):-
	printf(['Dos2Unix: ',DosFile,' -> ',UnixFile]),
         % filtr s pouhym presmerovanim vystupniho proudu do souboru
         invokeFilter(dos2UnixFilter,DosFile,UnixFile,_),
	printf(['... Bye!',nl]).

%------------------------------------------------------------------------------
%                         Repeat fail verze
%------------------------------------------------------------------------------
% unix2DosRepeat(+UnixFileName, +DosFileName)

unix2DosRepeat(UnixFileName, DosFileName):-
	printf(['Unix2Dos stream: ',UnixFileName,' -> ',DosFileName]),
         openStream(UnixFileName,Handle,OS,read), % open input stream
          openFile(DosFileName,OF,write),         % output redirect to file
           unix2DosFilterRepeat(filter(Handle)+_),
          closeFile(OF,write),                    % close output file
         closeStream(OS,read),                    % close input stream
	printf(['... Bye!',nl]).

unix2DosFilterRepeat(W):-
 W :->
        (nonSymbol([10]) <@ showAtom
          <:
         symbol([10]) <@ const([13,10]) => string2Atom => show
        )<\/> .         % repeat-fail cyklus

%------------------------------------------------------------------------------
% dos2UnixRepeat(+DosFileName, +UnixFileName)

dos2UnixRepeat(DosFile, UnixFile):-
	printf(['Dos2Unix stream: ',DosFile,' -> ',UnixFile]),
         openStream(DosFile,Handle,OS,read),      % open input stream
          openFile(UnixFile,OF,write),            % output redirect to file
           dos2UnixFilterRepeat(filter(Handle)+_),
          closeFile(OF,write),                    % close output file
         closeStream(OS,read),                    % close input stream
	printf(['... Bye!',nl]).

dos2UnixFilterRepeat(W):-
 W :->
        (nonSymbol([13]) <@ showAtom            
          <:
         symbol([13])
        )<\/> .

%------------------------------------------------------------------------------
%                               Sdileny kod
%------------------------------------------------------------------------------
% dos2UnixFilter(?Wrapper)
% - konverze: 0xD 0xA -> 0xA ... odstraneni vsech 0xD

dos2UnixFilter(W):-
 W :->
       (nonSymbol([13]) <@ showAtom     % zobraz zbyle
         <:
        symbol([13]))<*> .              % ignoruj 0xD

%------------------------------------------------------------------------------
% unix2DosFilter(?Wrapper)
% - koverze: 0xA -> 0xD 0xA

unix2DosFilter(W):-
 W :->
        (nonSymbol([10]) <@ showAtom
          <:
         symbol([10]) <@ const([13,10]) => string2Atom => show
        )<*> .

%- EOF ------------------------------------------------------------------------
