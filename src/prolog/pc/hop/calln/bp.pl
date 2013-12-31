%------------------------------------------------------------------------------
%
%                     Optimized :-@ version for BinProlog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/**     :-@ [+Term | +ListOfAdditionalArguments]
Text:   Implementace predik�tu vy���ho ��du call/n pro BinProlog.
Arg: Term
        Z�klad termu.
Arg: ListOfAdditionalArguments
        Dal�� argumenty, kter� jsou p�ipojeny k z�rodku. Je vytvo�en c�l,
        kter� je zavol�n.
Example:
        ?- :-@ [append("Robert "), "Kovalski", Name].
        Name = "Robert Kovalski"
        Yes
*/
% - currying possible
% - this ad hoc implementation is better than =../2 (it wastes global stack).

:-@ [Struct|[A]]        :- call(Struct,A).
:-@ [Struct|[A,B]]      :- call(Struct,A,B).
:-@ [Struct|[A,B,C]]    :- call(Struct,A,B,C).
:-@ [Struct|[A,B,C,D]]  :- call(Struct,A,B,C,D).
:-@ [Struct|[A,B,C,D,E]]:- call(Struct,A,B,C,D,E).

% Another possible implementation is:
%
% :-@ CallInput:-
%       Goal =.. [call|CallInput],
%       Goal.
%
% Unfortunately =../2 seriously wastes heap
%- EOF ------------------------------------------------------------------------
