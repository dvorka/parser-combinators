%------------------------------------------------------------------------------
%
%                      Create BinProlog pseudo application
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------

:- makeBinPrologApplications, halt.

makeBinPrologApplications:-
        make_appl('dos2unix.pl').

%- EOF ------------------------------------------------------------------------
