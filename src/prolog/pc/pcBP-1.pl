%------------------------------------------------------------------------------
%
%               Zavadec knihovny kombinatoru parseru pro BinProlog
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
% BinProlog loading algorithm:
%  directive :-[file] ... is the same as preprocessor include (#define<file>)
%                         so all files are included into the root file.
%                         Therefore paths must be specied from the root dir.

:-write('+----------------------------------------+'),nl,
  write('| Loading PC library for *BinProlog*...  |'),nl,
  write('+----------------------------------------+'),
  nl.

%------------------------------------------------------------------------------
%                                 Config
%------------------------------------------------------------------------------

pcProlog(bp).                   % aktualni implementace jazyka Prolog
pcStdIn(user).                  % stdin
pcStdOut(user).                 % stdout
pcStdErr(user).                 % proud pro smerovani chybovych vypisu

% Ladici jmena jsou definovana ve strukture pcDeBugName/1 v library/debug.pl.

% Volby modu jsou definovany ve strukture modeOptionIs/1 v library/mode.pl.

%------------------------------------------------------------------------------
%                            Zavedeni knihovny
%------------------------------------------------------------------------------
:-['../pc/library/operator'].  % deklarace operatoru

% Programovani vyssiho radu (:->,:-@):
% :->
:-['../pc/hop/binder/bp.pl']. % verze optimalizovana pro BinProlog
% :-@  ... zvolena muze byt prave jedna z nize uvedenych voleb
:-['../pc/hop/calln/bp.pl'].        % verze optimalizovana pro BinProlog
 %:- ['../../pc/hop/calln/curry/bp.pl']. % verze umoznujici currying volanych
                                         % predikatu



% PC: jadro knihovny
:-['../pc/library/terminal'].       % primitiva a parsery terminalu
:-['../pc/library/combine'].        % kombinatory parseru
:-['../pc/library/mutate'].         % mutatory parseru
:-['../pc/library/mutatePart'].     % mutatory parseru - partial evaluation ver.
:-['../pc/library/mutateTrim'].     % mutatory parseru - trimming ver.
:-['../pc/library/mutateFail'].     % repeat fail mutator
:-['../pc/library/mutateOff'].      % weight
:-['../pc/library/smart'].         % smart versions

% PC: contrib
:-['../pc/library/is'].	        % classification
:-['../pc/library/to'].	        % conversions
:-['../pc/library/fun'].            % functional goodies
:-['../pc/library/colone'].         % colone in <@
:-['../pc/library/tuple'].          % tuples
:-['../pc/library/terminalI'].      % more terminal symbol parsers
:-['../pc/library/combineI'].       % more combinators
:-['../pc/library/combineII'].       
:-['../pc/library/mutateI'].        % more mutators
:-['../pc/library/generator'].      % generators
:-['../pc/library/idf'].            % identifiers
:-['../pc/library/impera'].         % function and procedure heads
:-['../pc/library/datatype'].       % int, fixed, real parsers
:-['../pc/library/expr'].           % expressions
:-['../pc/library/mode'].	        % modes
:-['../pc/library/modeaux'].	% mode - auxiliary
:-['../pc/library/modeio'].	        % mode - io
:-['../pc/library/invoke'].	        % mode invokes
:-['../pc/library/optimize'].       % optimalizations
:-['../pc/library/auxiliary'].      % auxiliary predicates
:-['../pc/library/error'].          % error treatment
:-['../pc/library/adhoc'].          % utility
:-['../pc/library/tester'].         % testovaci a ukazkove predikaty

% Tools

% Debug tools
:-['../pc/library/debug'].             % deBug, deBugAssert, printf
:-['../pc/library/debugger/bench'].    % benchmark
:-['../pc/library/debugger/debugger']. % debugger

%- EOF ------------------------------------------------------------------------
