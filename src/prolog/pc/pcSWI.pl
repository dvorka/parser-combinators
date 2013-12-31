%------------------------------------------------------------------------------
%
%               Zavadec knihovny kombinatoru parseru pro SWI-Prolog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------

:-nl,
  write('+----------------------------------------+'),nl,
  write('| Loading PC library for *SWI Prolog*... |'),nl,
  write('+----------------------------------------+'),
  nl.

%------------------------------------------------------------------------------
%                                 Config
%------------------------------------------------------------------------------

pcProlog(swi).                  % aktualni implementace jazyka Prolog
pcStdIn(user).                  % stdin
pcStdOut(user).                 % stdout
pcStdErr(user).                 % proud pro smerovani chybovych vypisu

% Ladici jmena jsou definovana ve strukture pcDeBugName/1 v library/debug.pl.

% Volby modu jsou definovany ve strukture modeOptionIs/1 v library/mode.pl.

% Volby pro programovani vyssiho radu (:->,:-@):
 % :-> ... zvolena muze byt prave jedna z nize uvedenych voleb
 pcHopBinder(optimized).        % verze optimalizovana pro SWI Prolog
 %pcHopBinder(specializer).     % verze vytvarejici specializovany kod pro :->
                                % viz hop/binder/spec/specializer.pl
 %pcHopBinder(spec).            % verze vyuzivajici specializovany kod
                                % pripraveny napriklad prave pomoci
                                % specializer.pl. Tento kod se pokusi zavest
                                % ze souboru hop/binder/spec/swi.pl

 % :-@ ... zvolena muze byt prave jedna z nize uvedenych voleb
 pcHopCalln(optimized).         % verze optimalizovana pro SWI Prolog
 %pcHopCalln(curry).            % verze umoznujici currying volanych
                                % predikatu s moznosti ulozeni
                                % specializovaneho kodu viz
                                % hop/calln/curry/swi.pl
 %pcHopBinder(spec).            % verze vyuzivajici specializovany kod
                                % pripraveny napriklad pomoci predchozi
                                % volby. Tento kod se pokusi zavest ze souboru 
                                % hop/calln/curry/swispec.pl

%------------------------------------------------------------------------------
%                            Zavedeni knihovny
%------------------------------------------------------------------------------

:- ['library/operator'].        % deklarace operatoru

% Programovani vyssiho radu:
% :->
:- pcHopBinder(Binder)
   -> ( Binder=optimized -> ['hop/binder/swi']
         ;
        Binder=specializer -> ['hop/binder/spec/specializer']
         ;
        Binder=spec -> ['hop/binder/spec/swi']
         ;
        write('Error: unknown :-> option, see pcSWI.pl'),nl)
    ;
    write('Error: :-> loader error - no option defined, see pcSWI.pl'),nl.
% :-@
:- pcHopCalln(Calln)
   -> ( Calln=optimized -> ['hop/calln/swi']
         ;
        Calln=curry -> ['hop/calln/curry/swi']
         ;
        Calln=spec -> ['hop/calln/curry/swispec']
         ;
        write('Error: unknown :-@ option, see pcSWI.pl'),nl)
    ;
    write('Error: :-@ loader error - no option defined, see pcSWI.pl'),nl.



% PC: jadro knihovny
:- ['library/terminal'].    	% primitiva a parsery terminalu
:- ['library/combine'].    	% kombinatory parseru
:- ['library/mutate'].    	% mutatory parseru
:- ['library/mutatePart'].    	% mutatory parseru - partial evaluation versions
:- ['library/mutateTrim'].    	% mutatory parseru - trimming versions
:- ['library/mutateFail'].    	% repeat fail mutator
:- ['library/mutateOff'].    	% weight
 :- ['library/smart'].    	% smart versions ...

% PC: contrib
:- ['library/is'].		% classification
:- ['library/to'].		% conversions
:- ['library/fun'].             % functional goodies
:- ['library/colone'].          % colones of <@
:- ['library/tuple'].           % tuples
:- ['library/terminalI'].       % more terminal symbol parsers
:- ['library/combineI'].        % more combinators
:- ['library/combineII'].       
:- ['library/mutateI'].         % more mutators
:- ['library/generator'].       % generators
:- ['library/idf'].             % identifiers
:- ['library/impera'].          % function and procedure heads
:- ['library/datatype'].        % int, fixed, real parsers
:- ['library/expr'].            % expressions
:- ['library/mode'].            % modes
:- ['library/modeaux'].         % modes - auxiliary
:- ['library/modeio'].          % modes - io
:- ['library/invoke'].          % mode invokes
:- ['library/optimize'].        % optimalizations
:- ['library/auxiliary'].       % auxiliary predicates
:- ['library/error'].           % error treatment
:- ['library/adhoc'].           % utility
:- ['library/tester'].          % testovaci a ukazkove predikaty

% Tools:

% DeBug tools:
:- ['library/debug'].             % deBug, debugAssert, printf
:- ['library/debugger/bench'].    % benchmark
:- ['library/debugger/debugger']. % debugger

%- EOF ------------------------------------------------------------------------
