%------------------------------------------------------------------------------
%
%               Zavadec knihovny kombinatoru parseru pro LPA Prolog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------

:-nl,
  write('+---------------------+'),nl,
  write('| PC library for LPA  |'),nl,
  write('+---------------------+'),nl,
  nl.

%------------------------------------------------------------------------------
%                                 Config
%------------------------------------------------------------------------------

pcProlog(lpa).                  % aktualni implementace jazyka Prolog
pcStdIn(user).                  % stdin
pcStdOut(user).                 % stdout
pcStdErr(user).                 % proud pro smerovani chybovych vypisu

% Ladici jmena jsou definovana ve strukture pcDeBugName/1 v library/debug.pl.

% Volby modu jsou definovany ve strukture modeOptionIs/1 v library/mode.pl.

%------------------------------------------------------------------------------
%                            Zavedeni knihovny
%------------------------------------------------------------------------------
:-consult('pc/library/operator').      % deklarace operatoru

% Programovani vyssiho radu (:->,:-@):
% :->
:-consult('pc/hop/binder/standard.pl').
% :-@ 
:-consult('pc/hop/calln/curry/standard.pl').



% PC: jadro knihovny
:-consult('pc/library/terminal').      % fundamental parsers, list and stream input
:-consult('pc/library/combine').       % parser combinators
:-consult('pc/library/mutate').        % parser mutators
:-consult('pc/library/mutate~126~49'). % partial evaluation mutators (mutate~1)
:-consult('pc/library/mutate~126~50'). % parser mutators - trimming versions
:-consult('pc/library/mutate~126~51'). % repeat fail mutator         (mutate~3)
:-consult('pc/library/mutate~126~52'). % weight
:-consult('pc/library/smart').        % smart versions ...

% PC: contrib
:-consult('pc/library/is').		% classification
:-consult('pc/library/to').		% conversions
:-consult('pc/library/fun').             % functional goodies
:-consult('pc/library/colone').          % colones of <@
:-consult('pc/library/tuple').           % tuples
:-consult('pc/library/termin~126~49').   % more terminal symbol parsers
:-consult('pc/library/combineI').        % more combinators
:-consult('pc/library/combin~126~49').
:-consult('pc/library/mutateI').         % more mutators
:-consult('pc/library/genera~126~49').   % generators
:-consult('pc/library/idf').             % identifiers
:-consult('pc/library/impera').          % function and procedure heads
:-consult('pc/library/datatype').        % int, fixed, real parsers
:-consult('pc/library/expr').            % expressions
:-consult('pc/library/mode').	        % modes
:-consult('pc/library/modeaux').	        % modes - auxiliary
:-consult('pc/library/modeio').	        % modes - io
:-consult('pc/library/invoke').	        % mode invokes
:-consult('pc/library/optimize').        % optimalizations
:-consult('pc/library/auxili~126~49').   % auxiliary predicates
:-consult('pc/library/error').           % error treatment
:-consult('pc/library/adhoc').           % utility
:-consult('pc/library/tester').          % testovaci a ukazkove predikaty

% Tools

% Debug tools
:-consult('pc/library/debug').             % deBug, debugAssert, printf
:-consult('pc/library/debugger/bench').    % benchmarks
:-consult('pc/library/debugger/debugger'). % metainterpret

%- EOF ------------------------------------------------------------------------
