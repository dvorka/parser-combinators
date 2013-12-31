%------------------------------------------------------------------------------
%
%                                  Stream
%
%                              Martin Dvorak
%				    1999
%------------------------------------------------------------------------------
% Remark:
%     Definitions of predicates mentioned here are in ../specific/${MY_PROLOG}
%     because it's Prolog implementation dependent thing.
%------------------------------------------------------------------------------
% Argument legend:
%  Stream  ... Edimburg streams
%  EStream ... explicit streams (non Edimburg)
%
%       * for BinProlog is used: Stream = EStream = Edimburg *
%       (explicit Streams are emulated using Edimburg streams)
%------------------------------------------------------------------------------
% openStream(+FileName, -EStreamHandle, -OldStream, +Mode)
% - Mode can be: read, write

%------------------------------------------------------------------------------
% closeStream(+OldStream, +Mode)
% - close explicit stream

%------------------------------------------------------------------------------
% atStream(+EStreamHandle, -Positon)
% - get the position in the stream

%------------------------------------------------------------------------------
% setStream(+EStreamHandle, -Positon)
% - set the position in the stream from it's start

%------------------------------------------------------------------------------
% seekStream(+EStreamHandle, +Offset, +Method, -NewLocation)
% - seeks in the stream specified by StreamHandle
% - Method can be:      bof     ...     from the beginning of the file
%                       current ...     from the current positon

%------------------------------------------------------------------------------
% shiftStream(+EStreamHandle, +Offset)
% - seeks in the stream specified by EStreamHandle from the curr. pos.

%- EOF ------------------------------------------------------------------------
