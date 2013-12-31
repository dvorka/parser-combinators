%------------------------------------------------------------------------------
%
%                       BinProlog specific predicates
%
%                               Martin Dvorak
%				    1999
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
% Argument legend:
%  Stream  ... Edimburg streams
%  EStream ... explicit streams (non Edimburg)
%
%       * for BinProlog is used: Stream = EStream = Edimburg *
%       (explicit Streams are emulated using Edimburg streams)
%------------------------------------------------------------------------------
% BinProlog support:
%
%  implemented but no documentation for explicit streams:
%   prolog:current_stream(IO,'$stream'(EStream,IO,Stream))      (don't work)
%    IO can be: marksee
%               marktell
%   iso_lseek(EStream,Offset,Method,NewLocation)                (OK)
%
%  sufficient for out purpose (seek in Edimburg):
%   seeing_at(-Pos)     ... offset from the beginning of the input file
%   see_at(+Pos)        ... set file desctriptor pointer to Pos (from beg.)
%
%------------------------------------------------------------------------------
% seekFile(+EStream, +Offset, +Method, -NewLocation)
% - seek for Edimburg streams
% - Method can be:      bof     ...     beginning of file
%                       current ...     current positon

seekFile(see,Offset,bof,Offset):-
        see_at(Offset).

seekFile(see,Offset,current,NewPos):-
        seeing_at(CurrentPos),
        NewPos is CurrentPos + Offset,
        see_at(NewPos).

seekFile(tell,Offset,bof,Offset):-
        tell_at(Offset).

seekFile(tell,Offset,current,NewPos):-
        telling_at(CurrentPos),
        NewPos is CurrentPos + Offset,
        tell_at(NewPos).
        
%------------------------------------------------------------------------------
%                               Streams
%------------------------------------------------------------------------------
% openStream(+FileName, -EStreamHandle, -OldStream, +Mode)
% - Mode can be: read, write
% - BinProlog built-in explicit streams are not used -> we will emulate
%   them using implicit ones. 'see' and 'tell' atoms are our stream IDs.

openStream(FileName,see,OldStream,read):-
        seeing(OldStream),              % get actuall stream
        see(FileName).                  % open file

openStream(FileName,tell,OldStream,write):-
        telling(OldStream),              % get actuall stream
        tell(FileName).                  % open file

%------------------------------------------------------------------------------
% closeStream(+OldStream, +Mode)
% - close explicit stream
% - BinProlog built-in explicit streams are not used -> we will emulate
%   them using implicit ones. 'see' and 'tell' atoms are our stream IDs.

closeStream(Stream,read):-
        seen,                           % close file
        see(Stream).                    % open old stream

closeStream(Stream,write):-
        told,                           % close file
        tell(Stream).                   % open old stream

%------------------------------------------------------------------------------
% atStream(+EStreamHandle, -Positon)
% - get the position in the stream
% - BinProlog built-in explicit streams are not used -> we will emulate
%   them using implicit ones. 'see' and 'tell' atoms are our stream IDs.

atStream(see,Position):-
        seeing_at(Position).

atStream(tell,Position):-
        telling_at(Position).

%------------------------------------------------------------------------------
% setStream(+EStreamHandle, -Positon)
% - set the position in the stream from it's start
% - BinProlog built-in explicit streams are not used -> we will emulate
%   them using implicit ones. 'see' and 'tell' atoms are our stream IDs.

setStream(see,Position):-
        see_at(Position).

setStream(tell,Position):-
        tell_at(Position).

%------------------------------------------------------------------------------
% seekStream(+EStreamHandle, +Offset, +Method, -NewLocation)
% - seeks in the stream specified by StreamHandle
% - Method can be:      bof     ...     from the beginning of the file
%                       current ...     from the current positon
% - BinProlog built-in explicit streams are not used -> we will emulate
%   them using implicit ones. 'see' and 'tell' atoms are our stream IDs.

seekStream(see,Offset,current,NewPos):-
        seeing_at(CurrentPos),
        NewPos is CurrentPos + Offset,
        see_at(NewPos).

seekStream(see,NewPos,bof,NewPos):-
        see_at(NewPos).



seekStream(tell,Offset,current,NewPos):-
        telling_at(CurrentPos),
        NewPos is CurrentPos + Offset,
        tell_at(NewPos).

seekStream(tell,NewPos,bof,NewPos):-
        tell_at(NewPos).

%------------------------------------------------------------------------------
% shiftStream(+EStreamHandle, +Offset)
% - seeks in the stream specified by EStreamHandle from the curr. pos.
% - BinProlog built-in explicit streams are not used -> we will emulate
%   them using implicit ones. 'see' and 'tell' atoms are our stream IDs.

shiftStream(see,Offset):-
        seeing_at(CurrentPos),
        NewPos is CurrentPos + Offset,
        see_at(NewPos).

shiftStream(tell,Offset):-
        telling_at(CurrentPos),
        NewPos is CurrentPos + Offset,
        tell_at(NewPos).

%------------------------------------------------------------------------------
%                               Arithmetic
%------------------------------------------------------------------------------

% left asociativity
^^(X,Y,R):-
        pow(X,Y,R).
^(X,Y,R):-
        pow(X,Y,R).

%------------------------------------------------------------------------------
%                                 Sets
%------------------------------------------------------------------------------
% subset(+Subset, +Set)
subset([E|T],S):-
        member(E,S),subset(T,S).
subset([],_).

%- EOF ------------------------------------------------------------------------
