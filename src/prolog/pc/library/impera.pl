%------------------------------------------------------------------------------
%
%                 Some procedure/function declaration/definition
%
%                              Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   B�n� obraty strukturovan�ho programov�n� imperativn�ch
        a funkcion�ln�ch programovac�ch jazyk�.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     cmdAri(+LeftOperand, +Delimiter, +Body, ?Wrapper)
Text:   Imperativn� konstrukt.
Example:
        ?- s("a:=b")+L :-> cmdAri(symbol("a"),token(":="),symbol("b")).
        L= [s([]) > (97>98)]
        Yes
*/
% - used for parsing of arithmetic commands such as
%   LeftOperand <Delimiter> Body
cmdAri(LOp,D,Body,W):-
 W :->
        (LOp <& D) <&>> Body .

%------------------------------------------------------------------------------
/**     procHead(+Name, +ArgBlock, ?Wrapper)
Text:   Parser signatury  procedury,  nap��klad:
v               clone(a,b,c)
        V�sledkem parseru je dvojice obsahuj�c� funktor a struktury
        parametr�.
Arg:    Name
        Parser funktoru.
Arg:    ArgBlock
        Parser ��sti pro popis parametr�.
Example:
        ?- W :->
        |   procHead(poorIdf,
        |            parentheses(commaList(cmdAri(.)))).
*/

procHead(Name,ParaBlock,W):-
 W :->
        Name <&>> ParaBlock.

%------------------------------------------------------------------------------
/**     funHead(+Prefix, +Name, +ArgBlock, ?Wrapper)
Text:   Parser signatury funkce. Oproti parseru
        procHead p�ibyl nav�c parametr Prefix pro specifikaci
        funk�n� hodnoty. V�sledkem je seznam se t�emi polo�kami.
Example:
        ?- W :->
        |   funHead( cType
        |            poorIdf,
        |            parentheses(commaList(cmdAri(.)))).
*/

% no postfix
funHead(Prefix,Name,ParaBlock,W):-
 W :->
        Prefix <&> procHead(Name,ParaBlock).

/**     funHead(+Prefix, +Name, +ArgBlock, +Postfix, ?Wrapper)
Text:   Parser signatury funkce. Oproti parseru
        funHead p�ibyl nav�c parametr Postfix pro specifikaci
        informace typu vyhazovan�ch v�jimek (Java). V�sledkem
        je seznam se �ty�mi polo�kami.
Example:
        ?- W :->
        |   funHead( cType
        |            poorIdf,
        |            parentheses(commaList(cmdAri(.)))
        |            throws).
        
*/

% with postfix
funHead(Prefix,Name,ParaBlock,Postfix,W):-
 W :->
        Prefix <&> procHead(Name,ParaBlock) <&> Postfix.

%------------------------------------------------------------------------------
/**     funProcDecl(+Head, +Finisher, ?Wrapper)
Text:   Parser deklarace procedury resp. funkce.
Example:
        ?- W :->
        |       funProcDecl(procHead(.), symbol(";")).
*/

funProcDecl(H,F,W):-
 W :->
        H <& F.
    
%------------------------------------------------------------------------------
/**     funProcDef(+Head, +Body, ?Wrapper)
Text:   Parser deklarace procedury resp. funkce. Lze j�m vytvo�it
        parser pro konstrukci typu:
v       bool clone(Ghost x, Ghost y)
v       {
v        globalClones++;
v        ...
v       }
        Syntaktick� stromy parseru singnatury a t�la jsou spojeny
        do dvojice.
Example:
        ?- W :->
        |       funProcDef(funHead(.),
        |                  brace(semicolonList(cCommand))).
*/

funProcDef(H,B,W):-
 W :->
        H <&>> B.
    
%- EOF ------------------------------------------------------------------------
