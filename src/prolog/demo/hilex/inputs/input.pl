%------------------------------------------------------------------------------
%
%                               Benchmark I:
%
%                               List version
%
%           Convert Unix files to DOS (0xA -> 0xD 0xA) a vice versa
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Init & libraries

% SWI Prolog
 :- ['../../loadSWI'].
% BinProlog
 %:- ['../../loadBP-2'].

%------------------------------------------------------------------------------

go:-
        unix2Dos('input1','output11').
goo:-
        dos2Unix('output11','output12').

gol:-
        unix2Dos('input2','output21').
gool:-
        dos2Unix('output21','output22').

%- code -----------------------------------------------------------------------
% unix2dos(+UnixFileName, -DosFileName)
% must be:
%       ?- UnixFileName \= DosFileName.
%       Yes

unix2Dos(UnixFileName, DosFileName):-
                printf(['Unix2Dos: ',UnixFileName,' -> ',DosFileName]),
        loadFile(UnixFileName, UnixBuffer),    % input file into buffer
        openFile(DosFileName,Old,write),       % redirect output to file
         unix2DosFilter(UnixBuffer+_),
        closeFile(Old,write),                  % close output file
                printf(['... Bye!',nl]).

%------------------------------------------------------------------------------
% unix2DosFilter(?Wrapper)
% - conversion: 0xA -> 0xD 0xA

unix2DosFilter(W):-
 W :->
       (
        nonSymbol([10]) <@ showAtom
         <:
        symbol([10]) <@ const([13,10]) => string2Atom => show
       )<*> .

%------------------------------------------------------------------------------
% dos2Unix(+DosFileName, -UnixFileName)
% must be:
%       ?- UnixFileName == DosFileName.
%       No

dos2Unix(DosFileName, UnixFileName):-
                printf(['Dos2Unix: ',UnixFileName,' -> ',DosFileName]),
        loadFile(DosFileName, DosBuffer),       % input file into buffer
        openFile(UnixFileName,Old,write),       % redirect output to file
         dos2UnixFilter(DosBuffer+_),
        closeFile(Old,write),                   % close output file
                printf(['... Bye!',nl]).

%------------------------------------------------------------------------------
% dos2UnixFilter(?Wrapper)
% - conversion: 0xD 0xA -> 0xA ... remove all 0xDs

dos2UnixFilter(W):-
 W :->
       (
        nonSymbol([13]) <@ showAtom     % show the others
         <:
        symbol([13])                    % ignore 0xD
       )<*> .

%- EOF ------------------------------------------------------------------------
