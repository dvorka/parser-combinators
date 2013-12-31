%------------------------------------------------------------------------------
%
%                           Operator definitions
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/**     Module:
Text:   Tento soubor obsahuje definice operátorù pou¾itých v knihovnì
        konstruktorù parserù.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
% Operator definition:
%  op(+Precedence, +Type, +Name)
%
% Precedence
%  High precedence -> low priority
%
%  Simple object or object in braces    ... precedence 0
%  Compound object                      ... precedence of its functor
%
% Operator                                               +
%  f ... operator                                       / \
%  x ... operand has priority >  than f                +   c
%  y ... operand has priority =< than f               / \
%                                                    a   b
% Examples                                                       
%  yfx ... (a+b)+c ... +(+(a,b), c)   ->   asoc. left (see figure)
%  xfy ... a+(b+c) ... +(a, +(b,c))   ->   asoc. right 
%  xfx ... means that these operators cann't be neighbours (e.g. :- or =)
%  fy  ... not not p ... allowed
%  fx  ...           ... forbidden
%  yf  ... p not not ... allowed
%  xf  ...           ... forbidden
%------------------------------------------------------------------------------

:- op(499,xf,  <#     ).  % whitespace mutator <#/2
:- op(500,fx,   #>    ).  % whitespace mutator #>/2
                          % (operator is defined in BinProlog with the same
                          %  precedence, fortunately as #>/1)

:- op(510,fx,  commaListOf ).
:- op(510,fx,  semicolonListOf ).

:- op(515,xfy, separatedBy ).
:- op(515,xfy, separated0By ).

:- op(515,xfy, lchainedBy ).
:- op(515,xfy, rchainedBy ).

:- op(550,xfy, and        ). % combinator item binder
:- op(555,xfy, enclosedIn ). 
:- op(555,xfy, nestedIn ).

:- op(650,fx,  :-@    ).  % call/n

:- op(651,xfy, ->>    ).  % uncurry: a(b) ->> c === a(b,c)

:- op(652,xfy, =->    ).  % sequential application of predicates (as funs)
:- op(652,xfy, =>     ).  % colone: sequential application of predicates

:- op(653,xfy, *>*    ).  % tuple constructor

:- op(665,yf,  <**>   ).  % repeater 0...       (returns list)
:- op(665,xfy, <*@*>  ).  % general <*>         (use sem for customization)
:- op(665,yf,  <++>   ).  % repeater 1...       (returns list)
:- op(665,xfy, <+@+>  ).  % general <*>
:- op(665,yf,  <??>   ).  % repeater 0 or 1     (returns list)
:- op(665,xfy, <?@?>  ).  % general <?>

:- op(665,yf,  <**>>  ).  % repeater 0...       (returns tuple)
:- op(665,yf,  <++>>  ).  % repeater 1...       (returns tuple)

:- op(665,yf,  <*>    ).  % partial <*>
:- op(665,xfy, <*@>   ).  % general <*@>
:- op(665,yf,  <+>    ).  % partial <+>
:- op(665,xfy, <+@>   ).  % general <+@>
:- op(665,yf,  <?>    ).  % partial <?>
:- op(665,xfy, <?@>   ).  % general <?@>

:- op(665,yf,  <<*>>  ).  % trim <*>
:- op(665,xfy, <<*@>> ).  % general <*@>
:- op(665,yf,  <<+>>  ).  % trim <+>
:- op(665,xfy, <<+@>> ).  % general <+@>
:- op(665,yf,  <<?>>  ).  % trim <?>
:- op(665,xfy, <<?@>> ).  % general <?@>

:- op(665,yf,  <\/>   ).  % repeat while LOS\==[]
:- op(665,yf,  </\>   ).  % repeat while LOS==[]

:- op(667,xfy, chainPassage ).  % step by step
               
:- op(670,xfy, :->    ).  % call/2 with advanced syntax for parsers

:- op(675,xfy, <&>>   ).  % sequential composition (returns tuple)
:- op(675,xfy, <&>    ).  % sequential composition (returns list)
:- op(675,xfy, <&@>   ).  % general <&>
:- op(675,xfy, <&     ).  % sequential composition discarting right
:- op(675,xfy,  &>    ).  % sequential composition discarting left
:- op(675,xfy, <&&>   ).  % sequential comp. (always concatenates strings)

:- op(685,xfy, <@     ).  % apply semantic function
                          % priority lower than <&> because usage
                          %         (P0 <&> P1 <&> P2) <@ S
                          %                     ==
                          %          P0 <&> P1 <&> P2 <@ S
                          % is very frequent

:- op(685,xfy, <@@    ).  % apply semantic function (whole derivation)

:- op(685,xfy, <?     ).  % filter LOS using condition
:- op(685,xfy, <?-    ).  % filter LOS using noncondition

:- op(687,fy,  kill   ).  % make list of successes empty

:- op(690,xfy, <::>   ).  % environment alternative composition
:- op(690,xfy, <:^>   ).  % agent of the alternative composition after opt.
:- op(690,xfy, <:^    ).  % agent of the alternative composition after opt.
:- op(690,xfy,  :>^   ).  % agent of the alternative composition after opt.

:- op(690,xfy, <:>    ).  % alternative composition
:- op(690,xfy, <:     ).  % a.c. left hand partial evaluation
:- op(690,xfy,  :>    ).  % a.c. right hand partial evaluation

:- op(695,yf,  <>     ).  % takes the first item from LOS
:- op(695,fy,  whole  ).  % mutate parser to trim the LOS
:- op(695,yf,  <^>    ).  % takes the haviest (mode)
:- op(695,yf,  <^@>   ).  % takes the haviest and applies sem (result)
:- op(695,yf,  <^@@>  ).  % takes the haviest and applies sem (derivation)

:- op(699,xfy, <::=>  ).  % Pison tool: grammar rule


:- op(600,xfy, modeOptionIs ).

%------------------------------------------------------------------------------
% Prepared:
% <$ $> <$>
% <#>             counter repeaters
% <^ ^> 
% <+ +>
% <* *>
% </ />
% </>             it's 's' -> use it for something special such as stream
% <\>             it's '?'
% <@> <@@>
% <~>
% :-@
% :-?
% :^>
% <->
% <<&>>
% feeder

% Unusable (some implementations have problems with parsing operators
% if some of following characters occured):
% ! ~ | . , ; % " ' [ ] ( ) { }

% Usable:
% @ $ ^ & + - / * ? ` = \ : < > # /

%:- op(655,yf,  <@>    ).  % (it is trim, isn't it)
%:- op(655,yf,  <@@>   ).  % non-greedy take as little as possible

%- EOF ------------------------------------------------------------------------
