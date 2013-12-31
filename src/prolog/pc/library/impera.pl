%------------------------------------------------------------------------------
%
%                 Some procedure/function declaration/definition
%
%                              Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Bì¾né obraty strukturovaného programování imperativních
        a funkcionálních programovacích jazykù.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/**     cmdAri(+LeftOperand, +Delimiter, +Body, ?Wrapper)
Text:   Imperativní konstrukt.
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
Text:   Parser signatury  procedury,  napøíklad:
v               clone(a,b,c)
        Výsledkem parseru je dvojice obsahující funktor a struktury
        parametrù.
Arg:    Name
        Parser funktoru.
Arg:    ArgBlock
        Parser èásti pro popis parametrù.
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
        procHead pøibyl navíc parametr Prefix pro specifikaci
        funkèní hodnoty. Výsledkem je seznam se tøemi polo¾kami.
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
        funHead pøibyl navíc parametr Postfix pro specifikaci
        informace typu vyhazovaných výjimek (Java). Výsledkem
        je seznam se ètyømi polo¾kami.
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
Text:   Parser deklarace procedury resp. funkce. Lze jím vytvoøit
        parser pro konstrukci typu:
v       bool clone(Ghost x, Ghost y)
v       {
v        globalClones++;
v        ...
v       }
        Syntaktické stromy parseru singnatury a tìla jsou spojeny
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
