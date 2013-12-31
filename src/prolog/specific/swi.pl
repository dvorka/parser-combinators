%------------------------------------------------------------------------------
%
%                       SWI Prolog specific predicates
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
%------------------------------------------------------------------------------
% SWI support:
%  current_stream(FileName,Mode,StreamHandle)
%  seek(StreamHandle,Offset,Method,NewLocation)
%
%------------------------------------------------------------------------------
%                               Files
%------------------------------------------------------------------------------
% seekFile(+Stream, +Offset, +Method, -NewLocation)
% - seek for Edimburg streams
% - Method can be:      bof     ...     beginning of file
%                       current ...     current positon
%                       eof     ...     end of file

seekFile(Stream,Off,Method,NewLocation):-
        current_stream(Stream,_,StreamHandle),
        seek(StreamHandle,Off,Method,NewLocation).

%------------------------------------------------------------------------------
%                               Streams
%------------------------------------------------------------------------------
% openStream(+FileName, -EStreamHandle, -OldStream, +Mode)
% - get explicit handle to implicit stream

openStream(FileName,EStreamHandle,OldStream,read):-
        seeing(OldStream),              % get actuall stream
        see(FileName),                  % open file
        current_stream(FileName,read,EStreamHandle).

openStream(FileName,EStreamHandle,OldStream,write):-
        telling(OldStream),              % get actuall stream
        tell(FileName),                  % open file
        current_stream(FileName,write,EStreamHandle).

%------------------------------------------------------------------------------
% closeStream(+OldStream, +Mode)
% - set implicit stream

closeStream(Stream,read):-
        seen,                           % close file
        see(Stream).                    % open old stream

closeStream(Stream,write):-
        told,                           % close file
        tell(Stream).                   % open old stream

%------------------------------------------------------------------------------
% atStream(+EStreamHandle, -Positon)
% - get the position in the stream

atStream(EStreamHandle,Position):-
        seek(EStreamHandle,0,current,Position).

%------------------------------------------------------------------------------
% setStream(+EStreamHandle, -Positon)
% - set the position in the stream from it's start

setStream(EStreamHandle,Position):-
        seek(EStreamHandle,Position,bof,Position).

%------------------------------------------------------------------------------
% seekStream(+EStreamHandle, +Offset, +Method, -NewLocation)
% - seeks in the stream specified by StreamHandle
% - Method can be:      bof     ...     beginning of file
%                       current ...     current positon
%                       eof     ...     end of file

seekStream(EStreamHandle,Offset,Method,NewLocation):-
        seek(EStreamHandle,Offset,Method,NewLocation).

%------------------------------------------------------------------------------
% shiftStream(+StreamHandle, +Offset)
% - seeks in the stream specified by EStreamHandle from the curr. pos.

shiftStream(EStreamHandle,Offset):-
        seek(EStreamHandle,Offset,current,_).

%------------------------------------------------------------------------------
%                               Arithmetic
%------------------------------------------------------------------------------

% right asociativity

+(X,Y,R):-
        R is X+Y.

-(X,Y,R):-
        R is X-Y.

*(X,Y,R):-
        R is X*Y.

/(X,Y,R):-
        R is X/Y.

mod(X,Y,R):-
        R is X mod Y.

//(X,Y,R):-
        R is X//Y.

% left asociativity

^^(X,Y,R):-
        R is X^Y.

pow(X,Y,R):-
        R is X^Y.

%- EOF ------------------------------------------------------------------------
