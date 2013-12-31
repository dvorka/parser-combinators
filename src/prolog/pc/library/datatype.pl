%------------------------------------------------------------------------------
%
%                    Parsers for miscellaneous data types         
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Parsery b�n�ch datov�ch typ�.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     natural(?Wrapper)
Text:   Parser p�irozen�ho ��sla. V�sledkem parseru je jeho hodnota.
Example:
        ?- s("36")+L :-> natural.
        L= [s([]) > 36]
        Yes
*/

natural(W):-
 W :->
        digit<+> <@ foldL(evalNatural,0).

evalNatural(Acc,I,Result):-
        Result is Acc*10 + I.

%------------------------------------------------------------------------------
/**     int(?Wrapper)
Text:   Parser p�ij�maj�c� cel� ��sla. V�sledkem parseru je hodnota 
        akceptovan�ho ��sla.        
Example:
        ?- s("-127")+L :-> int.
        L= [s([]) > -127]
        Yes
*/

int(W):-
 W :->
        ( symbol("-")<?@>1-const(-1)
           <&>>
          natural <@ alter(Sig>Val,Sig*Val) => eval).

%------------------------------------------------------------------------------
/** double(?Wrapper)
Text:   Parser ��sla s plovouc� ��dovou ��rkou. V�sledkem parseru je hodnota
        akceptovan�ho ��sla.        
Example:
        ?- s("-1.2e-3")+L :-> double.
        L= [s([])> -0.0012]
        Yes
*/

double(W):-
 W :->
     ((int
        <&>>
       (symbol(".") &> fractionalPart)<?@>0.0-id
        <&>>
       (symbols("eE") &> int)<?@>0.0-id) <@ evalDouble).

evalDouble(I>(F>E),R):-
        I<0 -> R is (I+F*(-1))*10^E ; R is (I+F)*10^E.

% fractionalPart(?Wrapper)
% - parser desetine casti cisla s plovouci radovou carkou
fractionalPart(W):-
 W :->
        ( digit<+> <@ foldR(evalFract,0.0) ).

evalFract(I,Acc,Result):-
        Result is (I+Acc)/10.

%------------------------------------------------------------------------------
/**     bin(?Wrapper)
Text:   Parser bin�rn�ho ��sla. V�sledkem parseru je jeho hodnota.
Example:
        ?- s("010110")+L :-> bin.
        L= [s([])>22]
        Yes
*/

bin(W):-
 W :->
        (symbolA("0") <: symbolA("1"))<*>
                <@ foldR(evalBin,0>0) => alter(Val>_,Val).

evalBin(I,Val>Pow,Value>Power):-
        Power is Pow+1, Value is Val+(2^Pow)*I.

%------------------------------------------------------------------------------
/**     natBase(+Base, ?Wrapper)
Text:   Parser p�irozen�ho ��sla v soustav� Base, kde Base je libovoln�
        p�irozen� ��slo.
Example:
        ?- s("010110")+L :-> natBase(2).   % bin
        L= [s([])>22]
        Yes
        ?- s("B1A")+L :-> natBase(12).
        L= [s([])>1606]
        Yes
*/

natBase(B,W):-
        B=<10
         ->
        (
         X is B-1+48,            % ascii2Atom(48,0)
         W :->
                symbolIntervalA("0",[X])<*>
                        <@ foldR(evalNatBase(B),0>0) => alter(Val>_,Val)
        );
        (
         X is B-11+65,           % ascii2Atom(65,'A')
         Y is B-11+97,           % ascii2Atom(97,'a')
         W :->
                ( digit <: (symbolInterval("A",[X]) <@ sub ->> 55)
                        <: (symbolInterval("a",[Y]) <@ sub ->> 87))<*>
                   <@ foldR(evalNatBase(B),0>0) => alter(Val>_,Val)
        ).

evalNatBase(B,I,Val>Pow,Value>Power):-
        Power is Pow+1, Value is Val+(B^Pow)*I.

%------------------------------------------------------------------------------
/**     octal(?Wrapper)
Text:   Parser ��sla v oktalov�m z�pisu.
Example:
        ?- s("111")+L :-> octal.
        L= [s([])>73]
        Yes
*/

octal(W):-
        natBase(8,W).

%------------------------------------------------------------------------------
/**     hexadecimal(?Wrapper)
Text:   Parser p�irozen�ho ��sla v hexadecim�ln�m z�pisu.
Example:
        ?- s("2CD")+L :-> hexadecimal.
        L= [s([])>717]
        Yes
*/

hexadecimal(W):-
        natBase(16,W).

%------------------------------------------------------------------------------
/**     determinant(?Wrapper)
Text:   Parser determinantu matice, kter� je posloupnost posloupnost� desetinn�ch
        ��sel odd�len�ch ��rkami, jen� je odd�len� dv�ma svisl�tky, mezi nimi�
        mohou b�t pr�zdn� znaky. Cel� determinant je uzav�en op�t do svisl�tek.
Example:
        ?- s("| 1, -5, 7e-3 |
        |     | 13, 6,    0 |
        |     | -9, 3,  1.5 |")+L :-> determinant.

        L= [s([])>[[1, -5, 0.007], [13, 6, 0], [-9, 3, 1.5]]]
        Yes
*/

determinant(W):-
 W :->
        (   commaListOf #>double<#
           separatedBy
            (symbol("|") <&> #>symbol("|"))
          enclosedIn
           symbol("|") and symbol("|")).

%- EOF ------------------------------------------------------------------------
