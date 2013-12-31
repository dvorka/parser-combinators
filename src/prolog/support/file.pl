%------------------------------------------------------------------------------
%
%                                File & I/O
%
%                               Martin Dvorak
%				    1998
%------------------------------------------------------------------------------
/** Module: File
Text:   Predik�ty pro vstupn�/v�stupn� operace.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                                  File
%------------------------------------------------------------------------------
/**     openFile(+FileName, -OldStream, +Mode)
Text:   Otev�en� souboru FileName.
Arg: Mode
        Pokud se Mode unifikuje s 'read', je soubor otev�en pro �ten�, pokud s
        'write', je otev�en pro z�pis.
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
Text:   Uzav�en� souboru FileName.
Arg: Mode
        Pokud se Mode unifikuje s 'read', je soubor uzav�en po �ten�, pokud s
        'write', je uzav�en po z�pisu.
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
Text:   Na��t� obsah aktu�ln�ho vstupn�ho proudu do �et�zce FileContent,
        dokud nenaraz� na jeho konec.
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
Text:   Na��t� Number byt� z aktu�ln�ho vstupn�ho proudu do prom�nn�
        Bytes. V ReadBytes je po�et nena�ten�ch byt� oproti po�adovan�mu
        po�tu.
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
Text:   Na�te obsah souboru FileName do �et�zce FileContent.
*/

loadFile(FileName,FileContent):-
        seeing(Old),                    % get actuall stream
        see(FileName),                  % open file
         loadByByte(FileContent),
        seen,                           % close file
        see(Old).                       % open old stream

%------------------------------------------------------------------------------
/**     saveFile(+FileName, +FileContent)
Text:   Ulo�� obsah prom�nn� FileContent do souboru FileName.
*/

saveFile(FileName,FileContent):-
    telling(Old),                       % get actuall stream
    tell(FileName),                     % open file
     write(FileContent),
    told,                               % close file
    tell( Old ).                        % open old stream

%------------------------------------------------------------------------------
/**     saveFile(+FileName, +FileContent, +Type)
Text:   Ulo�� obsah FileContent zp�sobem specifikovan�m v prom�nn� Type.
Arg: Type
        Pokud je prom�nn� Type v�z�na na atom 'term', ulo�� se term
        FileContent do souboru FileName. Jestli�e je v�z�na na atom
        'binary', ulo�� se seznam FileContent do souboru FileName bin�rn�.
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
Text:   Nastaven� pozice v aktu�ln�m vstupn�m proudu na Offset.
Arg: Method
        M��e obsahovat atomy:
v        'bof'     ... po��t� se offset od po��tku souboru,
v        'current' ... od aktu�ln� pozice
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
Text:   Bezpe�n� v�pis stringu do aktu�ln�ho v�stupn�ho proudu.
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
Text:   Na�ten� ��dku ukon�en�ho znakem 0x10.
Arg:    Line
        Prologovsk� seznam.
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
