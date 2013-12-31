%------------------------------------------------------------------------------
%
%                             :-> specializer
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% ToDo:
% - predicate save(std/swi/bp) -> vygeneruje specializovany soubor
%------------------------------------------------------------------------------
% Code generation:
%   This simple specializer is Prolog implementation independent -> is very 
%   slow and ineffective. But it's OK - you should run it *once* to generate
%   specialized and implementation dependent code. From this time you can 
%   use it instead of standard.
%
% Description:
%      1. Consult this file instead of pc/core/hop/standard.pl.
%      2. Run your application(s). Specializer will generate source code
%         of specialized :-> predicate. Source code is stored
%         in file ./special.pl. Once you run the application, source
%         code is generated.
%      3. From this time you can always use special.pl instead of
%         pc/core/hop/*.pl.
%------------------------------------------------------------------------------
/** :->(?Wrapper, +Parser)
*/

Wrapper :-> Parser:-
	 Parser=..[Functor|As]
          ->
          makeVarList(GW,As,HeadL,BodyL),
          Head=..[Functor|HeadL],
          Body=..[Functor|BodyL],
          Clause=(:->(GW,Head):-Body), asserta(Clause),
          asserta(pcBinderCode(Clause)),
          Wrapper :-> Parser.           % call specialized version


% makeVarList(Wrapper,ParserVars,GHeadVars,GBodyVars)
makeVarList(Wrapper,[],[],[Wrapper]).
makeVarList(W,[_|T],[V|A],[V|B]):-
        makeVarList(W,T,A,B).

% -----------------------------------------------------------------------------

showBinder:-
          listing(:->).

% -----------------------------------------------------------------------------

saveBinder:-
        write('Enter file name to save curry code: '), read(FileName),
        repeat,
                write('Choose interpret std/bin/swi: '), read(I),
                (I==std;I==bin;I==swi),
                !,
        openFile(FileName,Old,write),
         write('% Machine generated file'),nl,
          forall(pcBinderCode(Goal),(write(Goal),write('.'),nl)),
          addGeneralBinder(I),
         nl,write('% - EOF -'),nl,
        closeFile(Old,write),
        printf(['File ',FileName,' was saved into actuall directory.',nl]).

addGeneralBinder(std):-
        write('W:->P:-P=..PL,append(PL,[W],GL),G=..GL,G.').
addGeneralBinder(swi):-
        write('W:->P:-apply(P,[W]).').
addGeneralBinder(bp):-
        write('W:->P:-call(P,W).').

%- EOF ------------------------------------------------------------------------
