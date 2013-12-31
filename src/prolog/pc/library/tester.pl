%------------------------------------------------------------------------------
%
%                           Testovaci predikaty
%
%				Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/** Module:
Text:   Pomocné predikáty pro testování a profiling èástí knihovny.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
%                               Demo
%------------------------------------------------------------------------------
% d/0
% - predikat pro spousteni *d*emo prikladu

d:-     
        % konverze reprezentaci mnoziny first
         %pcTestTrigger(tester(firstconvert)),
        % konverze environmentu kostruktoru <::>
         %pcTestTrigger(tester(envconvertsearch)),
        % test optimalizatoru parseru na urovni zakladnich konstruktoru
         %pcTestTrigger(tester(optimize)),
        % ll1 mod
         %pcTestTrigger(tester(ll1)),
        % pseudoll1 mod
         pcTestTrigger(tester(pseudoll1)),
        true.

%------------------------------------------------------------------------------
%                               Trigger
%------------------------------------------------------------------------------
/**     pcTestTrigger(+Goal)
Text:   Predikát pro spou¹tìní testovacích pøíkladù detekující selhání.
*/

pcTestTrigger(Goal):-
        printf([nl,'*Goal: ',Goal,nl,nl]),
        (Goal -> printf([nl,nl,'*Goal ',Goal,' OK.'])
          ;
         printf([nl,nl,'*Goal ',Goal,' failed!'])).

%------------------------------------------------------------------------------
%                                Goals
%------------------------------------------------------------------------------
% firstcovert
% - konverze mezi reprezentacemi mny first (cond,enum,first)

tester(firstconvert):-
        Parser=double,

        eFirst+eFirst(_,FIRST) :-> Parser,

        printf(['c  : ',nl,FIRST,nl]),
        convertCondEnumBst(enum,cond(FIRST),enum(O1)), printf(['c2e: ',nl,O1,nl]),
        convertCondEnumBst(bst, cond(FIRST),bst(O2)),  printf(['c2b: ',nl,O2,nl]), % bstShow(O2),
        convertCondEnumBst(cond,enum(O1),   cond(O3)), printf(['e2c: ',nl,O3,nl]),
        convertCondEnumBst(bst, enum(O1),   bst(O4)),  printf(['e2b: ',nl,O4,nl]), % bstShow(O4),
        convertCondEnumBst(cond,bst(O2),    cond(O5)), printf(['b2c: ',nl,O5,nl]),
        convertCondEnumBst(enum,bst(O2),    enum(O6)), printf(['b2e: ',nl,O6,nl]),
        printf([nl,'Search:',nl]),
        (time(findCondEnumBst(cond(FIRST),55)),write(ok) ; write(nok)),nl,
        (time(findCondEnumBst(enum(O1),55)),write(ok)    ; write(nok)),nl,
        (time(findCondEnumBst(bst(O2),55)),write(ok)    ; write(nok)),nl,
        (time(findCondEnumBst(cond(FIRST),0)),write(ok) ; write(nok)),nl,
        (time(findCondEnumBst(enum(O1),0)),write(ok)    ; write(nok)),nl,
        (time(findCondEnumBst(bst(O2),0)),write(ok)    ; write(nok)),nl,
        printf([nl,'Optimize:',nl]),
        optimizeCondEnumBst(cond(FIRST),X1), printf(['X1: ',X1,nl]),
        optimizeCondEnumBst(enum(O1),X2),    printf(['X2: ',X2,nl]),
        optimizeCondEnumBst(bst(O2),X3),     printf(['X3: ',X3,nl]).

%------------------------------------------------------------------------------
% <::> environment converting and searching
% - konverze mezi reprezentacemi vyhledavaciho env (condFIRST,enumFIRST,
%   firstFIRST) a hleddani

tester(envconvertsearch):-
        % condFIRST
        Env=condFIRST([[[==([pcEpsilon]), ==([97])], symbol([97])<*>],
                       [[==([98])], symbol([98])],
                       [[==([98])], token([98, 98])],
                       [[==([45]), lElementOf([48, 49, 50, 51, 52, 53, 54, 55, 56, 57])], double]]),
        printf([nl,'condFIRST: ',Env,nl]),

        convertCondEnumBstFIRST(enum,Env,             enumFIRST(O1)), printf(['c2e: ',nl,O1,nl]),
        convertCondEnumBstFIRST(bst, Env,             bstFIRST(O2)),  printf(['c2b: ',nl,O2,nl]),
        convertCondEnumBstFIRST(cond,enumFIRST(O1),   condFIRST(O3)), printf(['e2c: ',nl,O3,nl]),
        convertCondEnumBstFIRST(bst, enumFIRST(O1),   bstFIRST(O4)),  printf(['e2b: ',nl,O4,nl]),
        convertCondEnumBstFIRST(cond,bstFIRST(O2),    condFIRST(O5)), printf(['b2c: ',nl,O5,nl]),
        convertCondEnumBstFIRST(enum,bstFIRST(O2),    enumFIRST(O6)), printf(['b2e: ',nl,O6,nl]),
        convertCondEnumBstFIRST(set ,Env,             setFIRST(O7)),  printf(['c2s: ',nl,O7,nl]),
        convertCondEnumBstFIRST(set ,enumFIRST(O1),   setFIRST(O8)),  printf(['e2s: ',nl,O8,nl]),
        convertCondEnumBstFIRST(set ,bstFIRST(O2),    setFIRST(O9)),  printf(['b2s: ',nl,O9,nl]),
        convertCondEnumBstFIRST(cond,setFIRST(O7),    condFIRST(OA)), printf(['s2c: ',nl,OA,nl]),
        convertCondEnumBstFIRST(enum,setFIRST(O7),    enumFIRST(OB)), printf(['s2e: ',nl,OB,nl]),
        convertCondEnumBstFIRST(bst ,setFIRST(O7),    bstFIRST(OC)),  printf(['s2b: ',nl,OC,nl]),
        printf([nl,'FIRST reverse:',nl]),
        env2First(Env,F1),          printf(['F1: ',F1,nl]),
        env2First(enumFIRST(O1),F2),printf(['F2: ',F2,nl]),
        env2First(bstFIRST(O2),F3) ,printf(['F3: ',F3,nl]),
        env2First(setFIRST(O7),F4) ,printf(['F4: ',F4,nl]),
        printf([nl,'Search:',nl]),
        time(findCondEnumBstFIRST(condFIRST(O3),55,P1)),printf(['P1: ',P1,nl]),
        time(findCondEnumBstFIRST(enumFIRST(O1),55,P2)),printf(['P2: ',P2,nl]),
        time(findCondEnumBstFIRST(bstFIRST(O2),55,P3)), printf(['P3: ',P3,nl]),
        time(findCondEnumBstFIRST(setFIRST(O7),55,PA)), printf(['PA: ',PA,nl]),
        time(findCondEnumBstFIRST(condFIRST(O3),0,P4)), printf(['P4: ',P4,nl]),
        time(findCondEnumBstFIRST(enumFIRST(O1),0,P5)), printf(['P5: ',P5,nl]),
        time(findCondEnumBstFIRST(bstFIRST(O2),0,P6)),  printf(['P6: ',P6,nl]),
        time(findCondEnumBstFIRST(setFIRST(O7),0,PB)),  printf(['PB: ',PB,nl]),
        printf([nl,'SearchAll:',nl]),
        time(findAllCondEnumBstFIRST(condFIRST(O3),55,A1)),printf(['A1: ',A1,nl]),
        time(findAllCondEnumBstFIRST(enumFIRST(O1),55,A2)),printf(['A2: ',A2,nl]),
        time(findAllCondEnumBstFIRST(bstFIRST(O2),55,A3)), printf(['A3: ',A3,nl]),
        time(findAllCondEnumBstFIRST(setFIRST(O7),55,AA)), printf(['AA: ',AA,nl]),
        time(findAllCondEnumBstFIRST(condFIRST(O3),0,A4)), printf(['A4: ',A4,nl]),
        time(findAllCondEnumBstFIRST(enumFIRST(O1),0,A5)), printf(['A5: ',A5,nl]),
        time(findAllCondEnumBstFIRST(bstFIRST(O2),0,A6)),  printf(['A6: ',A6,nl]),
        time(findAllCondEnumBstFIRST(setFIRST(O7),0,AB)),  printf(['AB: ',AB,nl]).

%------------------------------------------------------------------------------
% optimizeRegExprParser
% - optimalizace regularniho vyrazu parseru

tester(optimize):-
        % Volby:
        Mode=ll1,               % mod (ll1, pseudoll1)
        Repr=cond,                    % reprezentace (enum,bst,cond,set)

        %Parser= (symbol("a")<*> <:> epsilon),
        %Parser= (symbol("a")<*> <:> epsilon <:> symbol("b")<*>),
        %Parser= (symbol("a")<*> <:> symbol("b")) :> symbol("c"),
        %Parser= (symbol("a")<*> <:> symbol("b") <:> double),
        %Parser= (symbol("a")<*> <:> symbol("b") <:> symbol("1") <:> double),
        %Parser= (symbol("a")<*> <:> symbol("b") <: double) :> symbol("c"),
        %Parser= (symbol("1")<*> <:> symbol("1") <: double) :> symbol("1"),
        %Parser= (symbol("1")<*> <:> token("12")) <: symbols("13") <: symbol("1"),
        %Parser=  token("12") <: symbols("13") <: symbol("1"),
             % ll1 parser - testy
        Parser=( (token([130,56])<:>token([62,76])
                 )
                  <@hlxAddRaw([1])
                <:
                 (symbol([61])<:>symbol([123])<:>symbol([125])<:>symbol([40])<:>symbol([41])<:>symbol([59])
                 )
                  <@hlxAddRaw([2])
                <:
                 symbol([35])<&>nonSymbols([10])<*> <&>symbol([10])
                  <@hlxAddRaw([3])
                <:
                 (token([69, 108, 101, 109, 101, 110, 116])<:>token([83, 101, 116, 117, 112])
                 )
                  <@hlxAddRaw([4])
                <:
                 symbol([34])<&> (token([34])<:nonSymbols([34]))<*> <&>symbol([34])
                 <@hlxAddRaw([5])
                <^>
              )
              <@hlxShowWinner
               <: fulfil(isWhiteSpace)<+> <@hlxPrintWhiteSpace,


         optimizeRegExprParser(Repr,Mode,Parser,OptimizedParser),

        printf(['Optimized: ',nl,OptimizedParser,nl,nl]).

%------------------------------------------------------------------------------
% ll1 mode

tester(ll1):-
        First=set,        % reprezentace FIRST v <::> (enum,cond,bst,set)

        %Parser=token("Bertin"),
        %Parser=nonFulfil(isDigit),
        %Parser=symbol("a")<*>,
        %Parser=symbol("a") <&> symbol("c") <&> symbol("d") <&> epsilon,
        %Parser=symbol("a") <&>> symbol("b"),
        %Parser=(symbol("c")<:>symbol("b")) <&> symbol("a")<*>,
        Parser=symbol("a") <&> (symbol("z") <:> token("bcd")) <&&> token("ef") <&&> epsilon,

        % input
        I=s("abcdef"),
        
        invokeLL1(lazy,off,First,offset, Parser, I, L1),
        printf(['LOS1: ',nl,L1,nl,nl]),
        invokeLL1(early,off,First,offset, Parser, I, L2),
        printf(['LOS2: ',nl,L2,nl,nl]),
        invokeLL1(lazy,assert,First,offset, Parser, I, L3),
        printf(['LOS3: ',nl,L3,nl,nl]),
        invokeLL1(early,assert,First,offset, Parser, I, L4),
        printf(['LOS4: ',nl,L4,nl,nl]).


%------------------------------------------------------------------------------
% pseudoll1 mode

tester(pseudoll1):-
        First=cond,            % reprezentace FIRST v <::> (enum,cond,bst,set)
        OFOLLOW=noFOLLOW,      % (useFOLLOW,noFOLLOW)

        %Parser=symbol("a"),
        %Parser=(symbol("c")<:>symbol("b")),
        %Parser=token("Bertin"),
        %Parser=nonFulfil(isDigit),
        %Parser=symbol("a")<*>,
        %Parser=symbol("a") <&> symbol("c") <&> symbol("d") <&> epsilon,
        %Parser=symbol("a") <&> (token("cd") <:> token("cd")) <&&> token("ef") <&> epsilon,
        %Parser= token("ab") <&&> symbol("c") <&> epsilon,
        %Parser=symbol("a") <&>> symbol("b"),
        %Parser=(symbol("c")<:>symbol("b")) <&> symbol("a")<*>,
        %Parser=epsilon,

        Parser = <::>(condFIRST([
                                [[==([47])],
                                 (token([47, 42])
                                   <&>
                                  <::>(condFIRST([
                                                  [[==([42])],
                                                    symbol([42])<+> <&>nonSymbols([47])
                                                  ]
                                                  ,
                                                  [[lNotElementOf([pcEpsilon,42])],
                                                    nonSymbols([42])
                                                  ]]))
                                   <*>
                                  <&>
                                   symbol([42])<+>
                                  <&>
                                   symbol([47]))
                                 <@ show
                                ],
                                [[isWhiteSpace],
                                 fulfil(isWhiteSpace)<+>
                                 <@ shownl
                                ]
                               ])),

/*
        % innards
        Parser =                  <::>(condFIRST([
                                                  [[==([42])],
                                                    symbol([42])<+> <&>nonSymbols([47])
                                                  ],
                                                  [[lNotElementOf([pcEpsilon,42])],
                                                    nonSymbols([42])
                                                  ]]))
                                   <*> ,
*/ /*
        %Parser = (symbol([42])<+> <&>nonSymbols([47])
        Parser = (symbol([42]) <&>nonSymbols([47])
                   <:>
                  nonSymbols([42]))
                 <*> ,
*/ /*
        % innards
        Parser = <::>(condFIRST([
                                 [[lNotElementOf([pcEpsilon,42])],
                                  nonSymbols([42])
                                 ]
                                ]))
                                 <*> ,
        % input
*/



        I=s("/* * */"),

        invokePseudoLL1(lazy,OFOLLOW,off,First,offset, Parser, I, L1),
        printf([nl,'LOS1: ',nl,L1,nl,nl]),
        invokePseudoLL1(early,OFOLLOW,off,First,offset, Parser, I, L2),
        printf([nl,'LOS2: ',nl,L2,nl,nl]),
        invokePseudoLL1(lazy,OFOLLOW,assert,First,offset, Parser, I, L3),
        printf([nl,'LOS3: ',nl,L3,nl,nl]),
        invokePseudoLL1(early,OFOLLOW,assert,First,offset, Parser, I, L4),
        printf([nl,'LOS4: ',nl,L4,nl,nl]),

        true.

%- EOF ------------------------------------------------------------------------
