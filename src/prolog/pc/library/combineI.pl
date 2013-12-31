%------------------------------------------------------------------------------
%
%                          More parser combinators I
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Kombinátory parserù I.
*/
%------------------------------------------------------------------------------
% Todo:
%------------------------------------------------------------------------------
/**     enclosedIn(+Innards, +Openning and +Closing, ?Wrapper)
Text:   Vydá øetìzec znakù akceptovaný parserem Innards uzavøený mezi
        otevíracím tokenem pøijímaným parserem Openning a uzavíracím
        tokenem pøijímaným Closing.
Example:
        ?- s("<li>")+L :->
        |       token("li") enclosedIn symbol("<") and symbol(">").
        L= [s("")>"li"]
        Yes
*/
/**     enclosedIn(+Innards, +OpenningAndClosing, ?Wrapper)
Text:   Vydá øetìzec znakù akceptovaný parserem Innards uzavøený mezi
        otevíracím a uzavíracím tokenem, jen¾ jsou pøijímany
        parserem OpenningAndClosing.
Example:
        ?- s("'Hi!'")+L :->
        |       token("Hi!") enclosedIn symbol("'").
        L= [s("")>"Hi!"]
        Yes
*/

enclosedIn(P,S,W):-
 S = (SO and SC)
  ->
 W :->
        (SO &> P <& SC)
  ;
 W :->
        (S &> P <& S).

%------------------------------------------------------------------------------
/**     parentheses(+Innards, ?Wrapper)
Text:   Vydá øetìzec znakù uzávorkovaný mezi ( a ), jen¾ je pøijímán
        parserem Innards.
Example:
        ?- s("(-1)")+L :-> parentheses(int).
        L= [s("")> -1]
        Yes
*/

parentheses(P,W):-
        W :-> P enclosedIn (#>symbol("(")) and (#>symbol(")")).

%------------------------------------------------------------------------------
/**     brackets(+Innards, ?Wrapper)
Text:   Vydá øetìzec znakù uzávorkovaný mezi [ a ], jen¾ je pøijímán
        parserem Innards.
Example:
        ?- s("[2]")+L :-> brackets(natural).
        L= [s("")> 2]
        Yes
*/

brackets(P,W):-
        W :-> P enclosedIn (#>symbol("[")) and (#>symbol("]")).        

%------------------------------------------------------------------------------
/**     brace(+Innards, ?Wrapper)
Text:   Vydá øetìzec znakù uzavøený ve slo¾ených závorkách, jen¾ je pøijímán
        parserem Innards.
Example:
        ?- s("{111}")+L :-> brace(natural).
        L= [s("")> 111]
        Yes
*/

brace(P,W):-
        W :-> P enclosedIn (#>symbol("{")) and (#>symbol("}")).

%------------------------------------------------------------------------------
/**     angled(+Innards, ?Wrapper)
Text:   Vydá øetìzec znakù uzavøený mezi znaky > a <, jen¾ je pøijímán
        parserem Innards.
Example:
        ?- s("<li>")+L :-> angled(letter<*>).
        L= [s("")>"li"]
        Yes
*/

angled(P,W):-
        W :-> P enclosedIn (#>symbol("<")) and (#>symbol(">")).

%------------------------------------------------------------------------------
/**     quoted(+Innards, ?Wrapper)
Text:   Vydá øetìzec znakù v uvozovkách, jen¾ je pøijímán
        parserem Innards.
*/

quoted(P,W):-
        W :-> P enclosedIn #>symbol([34]).           % ascii(",34)

%------------------------------------------------------------------------------
/**     stropped(+Innards, ?Wrapper)
Text:   Vydá øetìzec znakù uzavøený mezi zpìtnými lomítky, jen¾ je pøijímán
        parserem Innards.
*/

stropped(P,W):-
        W :-> P enclosedIn #>symbol([92]).           % ascii(\,92)

%------------------------------------------------------------------------------
/**     atomized(+Innards, ?Wrapper)
Text:   Vydá øetìzec znakù v apostrofech, jen¾ je pøijímán
        parserem Innards.
Example:
        ?- s("'Hi'")+L :-> atomized(letter<*>).
        L= [s("")>"Hi"]
        Yes
*/

atomized(P,W):-
        W :-> P enclosedIn #>symbol("'").           

%------------------------------------------------------------------------------
/**     pascalCompound(+Innards, ?Wrapper)
Text:   Parser bloku jazyka Pascal tj. bloku ohranièeného tokeny
        "begin" a "end".
*/

pascalCompound(P,W):-
        W :-> P enclosedIn (#>token("begin")) and (#>token("end")).

%------------------------------------------------------------------------------
/**     getAngled(?Wrapper)
Text:   Vydá øetìzec znakù uzavøený mezi znaky > a <. Pokud uzavíracímu
        znaku > pøedchází zpìtné lomítko, je akceptován jako znak obyèejný.
Example:
        ?- s("<li>")+L :-> getAngled.
        L= [s("")>"li"]
        Yes
*/

getAngled(W):-                  % token("\>")
        W :-> ((token([92,62]) <: nonSymbol(">"))<*>)
                enclosedIn (#>symbol("<")) and (#>symbol(">")).

%------------------------------------------------------------------------------
/**     getAtomized(?Wrapper)
Text:   Vydá øetìzec znakù uzavøený v apostrofech. Pokud apostrofu
        pøedchází zpìtné lomítko, je akceptován jako znak obyèejný.
Example:
        ?- s("'Hi!'")+L :-> getAtomized.
        L= [s("")>"Hi!"]
        Yes
*/

getAtomized(W):-                  % token("\'")
        W :-> ((token([92,39]) <: nonSymbol("'"))<*>)
                enclosedIn (#>symbol("'")).

%------------------------------------------------------------------------------
/**     getQuoted(?Wrapper)
Text:   Vydá øetìzec znakù uzavøený v uvozovkách. Pokud tomuto znaku
        pøedchází zpìtné lomítko, je akceptován jako znak obyèejný.
*/

getQuoted(W):-                  % token("\"")
        W :-> ((token([92,34]) <: nonSymbol([34]))<*>)
                enclosedIn (#>symbol([34])).

%------------------------------------------------------------------------------
/**     separatedBy(+Element, +Separator, ?Wrapper)
Text:   Parser obecné posloupnosti jednoho nebo více tokenù pøijímaných
        parserem Element. Èleny posloupnosti jsou oddìleny sentencemi,
        které akceptuje parser Separator. Výsledek je vydáván
        ve formì seznamu elementù.
Arg:    Element
        Parser èlena posloupnosti.
Arg:    Separator
        Parser oddìlovaèe èlenù posloupnosti.
Example:
        ?- s("a;b;c")+L :-> letter separatedBy (#>symbol(";")).
        L= [s("")>[O'a,O'b,O'c]]
        Yes
*/

separatedBy(P,S,W):-
 W :->
        (P <&> ( S &> P )<*>).

%------------------------------------------------------------------------------
/** commaListOf(+Element, ?Wrapper)
Text:   Parser èárkami oddìlené posloupnosti jednoho a více tokenù,
        které jsou pøijímány parserem Element. Výsledek je vydáván
        ve formì seznamu elementù.
Arg:    Element
        Parser prvku posloupnosti.
Example:
        ?- s("a,b,c")+L :-> commaListOf letter.
        L= [s("")>[O'a,O'b,O'c]]
        Yes
*/

commaListOf(P,W):-
 W :->
        P separatedBy (#>symbol(",")).

%------------------------------------------------------------------------------
/** semicolonListOf(+Element, ?Wrapper)
Text:   Parser støedníky oddìlené posloupnosti jednoho a více tokenù, 
        které jsou pøijímány parserem Element. Výsledek je vydáván
        ve formì seznamu elementù.
Arg:    Element
        Parser prvku posloupnosti.
Example:
        ?- s("a;b;c")+L :-> semicolonListOf letter.
        L= [s("")>[O'a,O'b,O'c]]
        Yes
*/

semicolonListOf(P,W):-
 W :->
        P separatedBy (#>symbol(";")).

%------------------------------------------------------------------------------
/** separated0By(+Element, +Separator, ?Wrapper)
Text:   Parser obecné posloupnosti ¾ádného nebo více tokenù,
        které jsou pøijímány parserem Element, oddìlené sentencemi
        pøijímanými parserem Separator. Výsledek je vydáván
        ve formì seznamu elementù.
Arg:    Element
        Parser èlena posloupnosti.
Arg:    Separator
        Parser oddìlovaèe èlenù posloupnosti.
Example:
        ?- s("a;b;c")+L :-> letter separated0By (#>symbol(";")).
        L= [s("")>[O'a,O'b,O'c]]
        Yes
*/

separated0By(P,S,W):-
 W :->
        (P separatedBy S
          <:>
         epsilon).

%------------------------------------------------------------------------------
/** comma0ListOf(+Element, ?Wrapper)
Text:   Parser èárkami oddìlené posloupnosti ¾ádného nebo více tokenù,
        které jsou pøijímány parserem Element. Výsledek je vydáván
        ve formì seznamu elementù.
Arg:    Element
        Parser prvku posloupnosti.
Example:
        ?- s("a,b,c")+L :-> comma0ListOf letter.
        L= [s("")>[O'a,O'b,O'c]]
        Yes
*/

comma0ListOf(P,W):-
 W :->
        P separated0By (#>symbol(",")).

%------------------------------------------------------------------------------
/** semicolon0ListOf(+Element, ?Wrapper)
Text:   Parser støedníky oddìlené posloupnosti ¾ádného nebo více tokenù,
        které jsou pøijímány parserem Element. Výsledek je vydáván
        ve formì seznamu elementù.
Arg:    Element
        Parser prvku posloupnosti.
Example:
        ?- s("a;b;c")+L :-> semicolon0ListOf letter.
        L= [s("")>[O'a,O'b,O'c]]
        Yes
*/

semicolon0ListOf(P,W):-
 W :->
        P separated0By (#>symbol(";")).

%------------------------------------------------------------------------------
/** lineComment(+String, ?Wrapper)
Text:   Parser obecného jednoøádkového komentáøe.
Arg:    String
        Tento argument je tokenen, který je prefixem komentáøe.
        Pro jazyk Prolog je to tedy øetìzec "%".
Example:
        prologLComment(W):-
                lineComment("%",W).
*/

lineComment(S,W):-
 W :->
        (token(S)
          &>
           nonSymbol([10])<<*@>>return(comment)-sndTuple).

%------------------------------------------------------------------------------
/** prologLComment(+String, ?Wrapper)
Text:   Parser jednoøádkového komentáøe jazyka Prolog  (jeho souèástí
        je rovnì¾ oddìlovaè nového øádku).
Example:
        % this is comment
*/

prologLComment(W):-
        lineComment("%",W).

%------------------------------------------------------------------------------
/** haskellLComment(?Wrapper)
Text:   Parser jednoøádkového komentáøe jazyka Haskell (jeho souèástí
        je rovnì¾ oddìlovaè nového øádku).
Example:
        -- this is comment
*/

haskellLComment(W):-
        lineComment("--",W).

%------------------------------------------------------------------------------
/** prologMLComment(?Wrapper)
Text:   Parser víceøádkového (MultiLine) komentáøe jazyka Prolog, C, Java,
        ML, ...
Example:
        / *
            this is multiline
                        comment         
        * /
*/

prologMLComment(W):-
 W :->
        token("/*")
         <&
        (symbol("*")<+> &> nonSymbol("/") <: nonSymbol("*"))<*>
         <&
        (symbol("*")<+> &> symbol("/")).

%------------------------------------------------------------------------------
/** pascalMLComment(?Wrapper)
Text:   Parser víceøádkového (MultiLine) komentáøe jazyka Pascal.
Example:
        {
         this is multiline
         comment
        }
*/

pascalMLComment(W):-
 W :->
        nonSymbol("}") enclosedIn symbol("{") and symbol("}").

%- EOF ------------------------------------------------------------------------
