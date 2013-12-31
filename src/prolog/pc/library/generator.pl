%------------------------------------------------------------------------------
%
%                            Parser generators I
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Gener�tory jsou predik�ty vy���ho ��du umo��uj�c�
        automatizovan� vytv��en� parser�, jen� narozd�l od
        kombin�tor� a mut�tor� v r�mci sv� pr�ce zpravidla neprov�d�j�
        aplikov�n� parser�, n�br� pou��vaj� parsery a jejich
        konstruktory pouze jako stavebn� materi�l.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
% Poznamky:
% - nize uvedeny prehled obsahuje nektere vlastnosti zakladnich kostruktoru
%   parseru - casto jsou vyuzivany prave v generatorech:
%
%    a) terminate <&> p         -> terminate
%    b) p <&> terminate         -> terminate
%    c) terminate <:> p         -> p
%    d) p <:> terminate         -> p
%    e) p <:> q                 -> q <:> p
%    f) p <:> (q <:> r)         -> (p <:> q) <:> r
%    g) (p <:> q) <&> r         -> (p <&> r) <:> (q <&> r)
%    h) p <&> (q <:> r)         -> (p <&> q) <:> (p <&> r)
%    i) terminate <@ f          -> terminate
%    j) p <:> q <@ f            -> (p <@ f) <:> (q <@ f)
%    k) p <: terminate          -> p
%    l) terminate <: p          -> p
%
%  komentar:
%    a) terminate je levy nulovy prvek sekvencni kompozice 
%    b) terminate je pravy nulovy prvek sekvencni kompozice 
%    c) terminate je levy jednotkovy prvek alternativni kompozice
%    d) terminate je levy jednotkovy prvek alternativni kompozice
%    e) kumulativnost alternativni kompozice
%    f) asociativita alternativni kompozice
%    g) distributivnost sekvencni kompozice zleva
%    h) distributivnost sekvencni kompozice zprava
%    i) terminate je levy nulovy prvek mutatoru aplikace semanticke operace
%    j) distributivnost mutatoru aplikace semanticke operace zleva
%
%------------------------------------------------------------------------------
/**     convoy(+ChainOperator, +Terminator, +ListOfParsers, -Convoy )
Text:   Gener�tor convoy vytv��� nov� parsery �et�zen�m pomoc� dan�ho
        konstruktoru ChainOperator s pravou asociativitou. Je parametrizov�n
        jednak kombin�torem, kter� m� b�t pro �et�zen� pou�it a seznamem
        samotn�ch parser� ListOfParsers. Je mo�no tak� spefikovat
        Terminator - parser uzav�raj�c� �et�zec.
Example:
        ?- convoy(<&@>,epsilon,[symbol("a"),symbol("b")],O).
        O = symbol([97]) <&@> symbol([98]) <&@> epsilon
        Yes
*/

% convoy/4 with terminator
convoy(B,T,Ps,C):-
        foldR(convoy_(B),T,Ps,C).

convoy_(B,P1,P2,P):- P=..[B,P1,P2].

/**     convoy(+ChainOperator, +ListOfParsers, -Convoy )
Text:   Viz convoy/4 bez termin�toru.
*/
% convoy/3 with no terminator
convoy(_,[P],P).
convoy(B,[H|T],Convoy):-
	Convoy=..[B,H,TConvoy],
	convoy(B,T,TConvoy).
convoy(_,[],terminate).                 % prazdny seznam -> terminator

%------------------------------------------------------------------------------
/**     sequence(+ListOfParsers, -Convoy)
Text:   Specializovan� verze gener�toru convoy prov�d�j�c� �et�zen�
        pomoc� kombin�toru sekven�n� kompozice.
*/

% - <&> convoy of ListOfParsers
sequence(Ps,C):-
    convoy(<&>,Ps,C).

% sequence/3 with terminator
sequence(Ps,T,C):-
    convoy(<&>,T,Ps,C).

%------------------------------------------------------------------------------
/**     selection(+ListOfParsers, -Convoy)
Text:   Specializovan� verze gener�toru convoy prov�d�j�c� �et�zen�
        pomoc� kombin�toru alternativn� kompozice.
*/

% - <:> convoy of ListOfParsers
selection(Ps,C):-
    convoy(<:>,Ps,C).

% selection/3 with terminator
selection(Ps,T,C):-
    convoy(<:>,T,Ps,C).

%------------------------------------------------------------------------------
/**     choice(+ListOfParsers, -Convoy)
Text:   Viz selection (zp�tn� kompatibilita).
*/

choice(Ps,C):-
    selection(Ps,C).

choice(Ps,T,C):-
    selection(Ps,T,C).

%------------------------------------------------------------------------------
/**     option(+ListOfParsers, -Convoy)
Text:   Specializovan� verze gener�toru convoy prov�d�j�c� �et�zen�
        pomoc� kombin�toru alternativn� kompozice se zkr�cen�m
        vyhodnocen�m zleva.
*/

% - L(P1) & L(P1) = 0
option(Ps,C):-
    convoy(<:,Ps,C).

% option/3 with terminator
option(Ps,T,C):-
    convoy(<:,T,Ps,C).

%------------------------------------------------------------------------------
/**     multiPass(+ListOfParsers, -Chain)
Text:   Ze seznamu ListOfParsers zkonstruuje  parser pro v�cepr�chodov�
        rozklad.
Example:
        ?- multiPass([prelex,lexer,synt],P).
        P= prelex chainPassage lexer chainPassage synt
        Yes
*/

multiPass(Ps,C):-
        convoy(chainPassage,Ps,C).

%- EOF ------------------------------------------------------------------------
