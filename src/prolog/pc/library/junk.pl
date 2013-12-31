%------------------------------------------------------------------------------
%
%                                  Junk
%
%			       Martin Dvorak
%                                  2000
%------------------------------------------------------------------------------
% Komentar:
%       Zastarale mody deterministickeho vypoctu, bez ukladani mezivysledku
% do databaze. V techto pripadech neni provadeno narovnavani disjunkci,
% ale v kazdem konstruktoru, na kazde urovni je provaden opetovny rozbor
% pripadu.

<:>(P1,P2,det(FOLLOW,I)+L):-
        endOfInput(I)
           ->                                 % test na konec vstupu
            ((eFirstGet(P1,true,_)            % OK P1 prijima eps.
               -> det(FOLLOW,I)+L :-> P1)
               ;
             (eFirstGet(P2,true,_)            % OK P2 prijima eps.
               -> det(FOLLOW,I)+L :-> P2)
               ;
              L=[],
              deBug(error,['(<:>) Error: Unexpected end of file']))
           ;                                    % vstup je neprazdny
             item(I+Li),
             ( Li=[_>R],
              ((eFirstGet(P1,_,FIRST1),firstSetFitCond(FIRST1,R)
                 ->
                det(FOLLOW,I)+L :-> P1)
               ;
                (eFirstGet(P2,_,FIRST2),firstSetFitCond(FIRST2,R)
                 ->
                det(FOLLOW,I)+L :-> P2)
               ;
                (eFirstGet(P1,true,_),
                 (followSetIsElement(FOLLOW,R);deBug(warn,['(<:>) Warning: Inconsistent FOLLOW={',FOLLOW,'}, char=',R]))
                 ->
                det(FOLLOW,I)+L :-> P1)
               ;
                (eFirstGet(P2,true,_),
                 (followSetIsElement(FOLLOW,R);deBug(warn,['(<:>) Warning: Inconsistent FOLLOW={',FOLLOW,'}, char=',R]))
                 ->
                det(FOLLOW,I)+L :-> P2)
               ;
              L=[],
              deBug(error,['(<:>) Error: Illegal input symbol: ',R]))).

<:>(P1,P2,obsoletepseudoll1(Opt,LA,FOLLOW,I)+L):-
        LA=pcEof                                % test na konec vstupu
        ->
           % EOF -> P1 prijima epsilon   v   P2 prijima epsilon
           ((eFirstGetPut(P1,true,_)            % P1 prijima eps.
               -> obsoletepseudoll1(Opt,LA,FOLLOW,I)+L :-> P1)
               ;
            (eFirstGetPut(P2,true,_)            % P2 prijima eps.
               -> obsoletepseudoll1(Opt,LA,FOLLOW,I)+L :-> P2)
               ;
            pcError(err_LL1_EOF,[I]),L=[])
        ;
           % vstup je neprazdny
           ((eFirstGetPut(P1,_,First1),firstSetFitCond(First1,LA)
               ->                               % vyhled nalezi FIRST(P1)
              obsoletepseudoll1(Opt,LA,FOLLOW,I)+L :-> P1)
            ; % backtrack pres nejednoznacnosti
            (eFirstGetPut(P2,_,First2),firstSetFitCond(First2,LA)
               ->                               % vyhled nalezi FIRST(P2)
              obsoletepseudoll1(Opt,LA,FOLLOW,I)+L :-> P2)
            ; % backtrack pres nejednoznacnosti
            (eFirstGetPut(P1,true,_),           % empty(P1) -> test FOLLOW(P1)
              % pouziti FOLLOW
              (Opt=_^useFOLLOW^_
               -> (followSetIsElement(FOLLOW,LA)
                   -> obsoletepseudoll1(Opt,LA,FOLLOW,I)+L :-> P1 % enumerovana FOLLOW
                   ;  pcError(err_LL1_Follow,[I,LA,FOLLOW]),L=[])
               ;  obsoletepseudoll1(Opt,LA,FOLLOW,I)+L :-> P1,
                  L\==[]                        % selhani -> backtrack p. n.
              ))
            ; % backtrack pres nejednoznacnosti
            (eFirstGetPut(P2,true,_),
              % pouziti FOLLOW
              (Opt=_^useFOLLOW^_
               -> (followSetIsElement(FOLLOW,LA)
                   -> obsoletepseudoll1(Opt,LA,FOLLOW,I)+L :-> P2 % enumerovana FOLLOW
                   ;  pcError(err_LL1_Follow,[I,LA,FOLLOW]),L=[])
               ;  obsoletepseudoll1(Opt,LA,FOLLOW,I)+L :-> P2,
                  L\==[]                        % selhani -> backtrack p. n.
              ))
            ; % ani jedna z variant nebyla uspesna
            pcError(err_LL1_IllegalChar,[I,LA]),L=[]).

%- EOF ------------------------------------------------------------------------
