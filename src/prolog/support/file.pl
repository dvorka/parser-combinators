%------------------------------------------------------------------------------
%
%                                File & I/O
%
%                               Martin Dvorak
%				    1998
%------------------------------------------------------------------------------
/** Module: File
Text:   Predikáty pro vstupnì/výstupní operace.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                                  File
%------------------------------------------------------------------------------
/**     openFile(+FileName, -OldStream, +Mode)
Text:   Otevøení souboru FileName.
Arg: Mode
        Pokud se Mode unifikuje s 'read', je soubor otevøen pro ètení, pokud s
        'write', je otevøen pro zápis.
Example:
        ?- openFile('parser.pl',S,read).
        Yes
*/

openFile(Name,Old,read):-
        seeing(Old),                    % get actuall stream
        see(Name).                	% open file

openFile(Name,Old,write):-
        telling(Old),                   % get actuall stream
        tell(Name).                	% open file

%------------------------------------------------------------------------------
/**     closeFile(+OldStream, +Mode)
Text:   Uzavøení souboru FileName.
Arg: Mode
        Pokud se Mode unifikuje s 'read', je soubor uzavøen po ètení, pokud s
        'write', je uzavøen po zápisu.
Example:
        ?- closeFile(S,write).
        Yes
*/

closeFile(Old,read):-
        seen,                           % close file
        see(Old).                       % open old stream

closeFile(Old,write):-
        told,                           % close file
        tell(Old).                      % open old stream
    
%------------------------------------------------------------------------------
/**     loadByByte(-FileContent)
Text:   Naèítá obsah aktuálního vstupního proudu do øetìzce FileContent,
        dokud nenarazí na jeho konec.
*/
% Rem:
% - get0() returns:
%       -1 ... when name/2 not used in predicate and end_of_file or some error
%       36 ... if end_of_file and name/2 is used in predicate

loadByByte(FileContent):-
         get0(H),                       % read one byte from file
         H\==(-1)                       % nl,write('-> '),write(H),
          ->                            % name(A, [H]), write(A),
         FileContent=[H|T],
         loadByByte(T).
loadByByte([]).

%------------------------------------------------------------------------------
/**     loadBytes(+Number, -Bytes, -ReadBytes)
Text:   Naèítá Number bytù z aktuálního vstupního proudu do promìnné
        Bytes. V ReadBytes je poèet nenaètených bytù oproti po¾adovanému
        poètu.
*/

loadBytes(N,Bytes,NotRead):-
        N\==0,
        get0(B),                        % read one byte from file
        B\==(-1)
         ->
        N_ is N-1,
        Bytes = [B|T],
        loadBytes(N_,T,NotRead).
loadBytes(NotRead,[],NotRead).

%------------------------------------------------------------------------------
/**     loadFile(+FileName, -FileContent)
Text:   Naète obsah souboru FileName do øetìzce FileContent.
*/

loadFile(FileName,FileContent):-
        seeing(Old),                    % get actuall stream
        see(FileName),                  % open file
         loadByByte(FileContent),
        seen,                           % close file
        see(Old).                       % open old stream

%------------------------------------------------------------------------------
/**     saveFile(+FileName, +FileContent)
Text:   Ulo¾í obsah promìnné FileContent do souboru FileName.
*/

saveFile(FileName,FileContent):-
    telling(Old),                       % get actuall stream
    tell(FileName),                     % open file
     write(FileContent),
    told,                               % close file
    tell( Old ).                        % open old stream

%------------------------------------------------------------------------------
/**     saveFile(+FileName, +FileContent, +Type)
Text:   Ulo¾í obsah FileContent zpùsobem specifikovaným v promìnné Type.
Arg: Type
        Pokud je promìnná Type vázána na atom 'term', ulo¾í se term
        FileContent do souboru FileName. Jestli¾e je vázána na atom
        'binary', ulo¾í se seznam FileContent do souboru FileName binárnì.
*/

saveFile(FileName,FileContent,term):-
    telling(Old),                       % get actuall stream
    tell(FileName),                     % open file
     write(FileContent),
     write('.'),nl,		        % term terminator
    told,                               % close file
    tell( Old ).                        % open old stream


% Example: FileContent="cat" -> save 'cat'
saveFile(FileName,FileContent,binary):-
    telling(Old),                       % get actuall stream
    tell(FileName),                     % open file
     name(Atom,FileContent),
     write(Atom),
    told,                               % close file
    tell( Old ).                        % open old stream

%------------------------------------------------------------------------------
/**     seekFile(+Stream, +Offset, +Method, -NewLocation)
Text:   Nastavení pozice v aktuálním vstupním proudu na Offset.
Arg: Method
        Mù¾e obsahovat atomy:
v        'bof'     ... poèítá se offset od poèátku souboru,
v        'current' ... od aktuální pozice
v        'eof'     ... od konce souboru.
*/
% - Method can be:      bof     ...     beginning of file
%                       current ...     current positon
%                       eof     ...     end of file
% - see specific/${MY_PROLOG} for implementation

% seekFile(Stream,Off,Method,NewLocation):-
%        seek(Stream,Off,Method,NewLocation).

%------------------------------------------------------------------------------
/**     fileCopy(+InputFile, -OutputFile)
Text:   Kopie souboru.
*/

fileCopy(I,O):-
        openFile(I,OldI,read),
        openFile(O,OldO,write),
         fileCopy_,
        closeFile(OldI,read),
        closeFile(OldO,write).

fileCopy_:-
         get0(H),
         H\==(-1)
          ->     
         put(H),
         fileCopy_.
fileCopy_.
        
%------------------------------------------------------------------------------
%                                   I/O
%------------------------------------------------------------------------------
/**     printString(+String)
Text:   Bezpeèný výpis stringu do aktuálního výstupního proudu.
*/
% - prints string by byte because there is problem with '~'. Some Prolog
%   implementations (LPA) think that it's escape sequence and name/2 is thus
%   unusable.

printString([X|R]):-
        put(X),                         % name(A,[X]),write(A),
        printString(R).
printString([]).

%------------------------------------------------------------------------------
/**     readLine(-Line)
Text:   Naètení øádku ukonèeného znakem 0x10.
Arg:    Line
        Prologovský seznam.
*/

readLine(L):-
        get0(H),
        (H=10
          -> L=[]
          ;  L=[H|T],readLine(T)).

%------------------------------------------------------------------------------
% beep/0
beep:-
        put(7).

%- EOF ------------------------------------------------------------------------
