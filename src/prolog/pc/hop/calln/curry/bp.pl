%------------------------------------------------------------------------------
%
%                   Optimized curry :-@ version for BinProlog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% Code generation:
%       1. run application(s)
%       2. call saveCurry/0 to save code into file
%          (you can only list it using showCurry/0)
%       3. use generated file instead of this one
%------------------------------------------------------------------------------
/** :-@ [+Term | +ListOfAdditionalArguments]
*/

% :-@/2, direct call - it is not CURRYING
:-@ [First,Result]:-
        call(First,Result).

% ----------------------------------------------------------------------------

% :-@/4 "curry"
:-@ [Term,A,B,Result]:-
        :-@ [Term,A,At],
        :-@ [At,  B,Result].

% :-@/5
:-@ [Term,A,B,C,Result]:-
        :-@ [Term,A,At],
        :-@ [At,  B,Bt],
        :-@ [Bt,  C,Result].

% :-@/6
:-@ [Term,A,B,C,D,Result]:-
        :-@ [Term,A,At],
        :-@ [At,  B,Bt],
        :-@ [Bt,  C,Ct],
        :-@ [Ct,  D,Result].

% :-@/7
:-@ [Term,A,B,C,D,E,Result]:-
        :-@ [Term,A,At],
        :-@ [At,  B,Bt],
        :-@ [Bt,  C,Ct],
        :-@ [Ct,  D,Dt],
        :-@ [Dt,  E,Result].

% ----------------------------------------------------------------------------
% :-@/3 "curry" or call
% - in BinProlog clause/3 fails -> test
/*
:-@ [Term,A,Result]:-
        Term =.. TermL,
        (
         append(TermL,[A,Result],GoalL),
         Goal =.. GoalL,
         clause(Goal,_)
          ->
         Goal
          ;
         append(TermL,[A],StepGoalL),
         Result =.. StepGoalL
        ).
*/

% version "assert on-line" for speed up -> code is generated
:-@ [Term,A,Result]:-
        Term =.. [Func|TermL],
        append([Func|TermL],[A,Result],GoalL),
        Goal =.. GoalL,
        ( % consulted      built-in
         (clause(Goal,_) ; (functor(Goal,FF,Ar),bp_info(/(FF,Ar),_)))
          ->
         Goal
          ;
         :-@^(TermL,GResultL,GGoalL,GResult),           % goal generalization
         GResult =.. [Func|GResultL],
         GGoal =.. [Func|GGoalL], asserta(GGoal),
         assert(pcCurryCode(GGoal)),                    % data to save
         Goal                                           % call asserted goal
        ).

% 
% :-@^ (TemplateL, ResultL, GoalL, ResultTerm)
% - goal generalization
%    arity(Template)+1 = arity(Result)
%    arity(Result)+1 = arity(Goal)
% - example:
%     :-@^ ([p,1,2],[p,A1,A2],[p,A1,A2,p(A1,A2)],ResultTerm)
:-@^([],[V],[V,Result],Result).        % arity+1 ([A])
:-@^([_|T],[V|R],[V|G],Result):-
        :-@^(T,R,G,Result).

% ----------------------------------------------------------------------------

showCurry:-
        listing(pcCurryCode(_)).

% ----------------------------------------------------------------------------
% save curry code to file -> this file can be then replaced.
saveCurry:-
        write('Enter file name to save curry code: '), read(FileName),
        openFile(FileName,Old,write),
         write('% Machine generated file'),nl,nl,
          forall(pcCurryCode(Goal),(write(Goal),write('.'),nl)),
          nl,
          listing(:-@(_)),
         nl,write('% - EOF -'),nl,
        closeFile(Old,write),
        printf(['File ',FileName,' was saved into actuall directory.',nl]).

%- EOF ------------------------------------------------------------------------
