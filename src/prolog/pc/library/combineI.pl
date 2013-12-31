%------------------------------------------------------------------------------
%
%                          More parser combinators I
%
%                               Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Kombin�tory parser� I.
*/
%------------------------------------------------------------------------------
% Todo:
%------------------------------------------------------------------------------
/**     enclosedIn(+Innards, +Openning and +Closing, ?Wrapper)
Text:   Vyd� �et�zec znak� akceptovan� parserem Innards uzav�en� mezi
        otev�rac�m tokenem p�ij�man�m parserem Openning a uzav�rac�m
        tokenem p�ij�man�m Closing.
Example:
        ?- s("<li>")+L :->
        |       token("li") enclosedIn symbol("<") and symbol(">").
        L= [s("")>"li"]
        Yes
*/
/**     enclosedIn(+Innards, +OpenningAndClosing, ?Wrapper)
Text:   Vyd� �et�zec znak� akceptovan� parserem Innards uzav�en� mezi
        otev�rac�m a uzav�rac�m tokenem, jen� jsou p�ij�many
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
Text:   Vyd� �et�zec znak� uz�vorkovan� mezi ( a ), jen� je p�ij�m�n
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
Text:   Vyd� �et�zec znak� uz�vorkovan� mezi [ a ], jen� je p�ij�m�n
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
Text:   Vyd� �et�zec znak� uzav�en� ve slo�en�ch z�vork�ch, jen� je p�ij�m�n
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
Text:   Vyd� �et�zec znak� uzav�en� mezi znaky > a <, jen� je p�ij�m�n
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
Text:   Vyd� �et�zec znak� v uvozovk�ch, jen� je p�ij�m�n
        parserem Innards.
*/

quoted(P,W):-
        W :-> P enclosedIn #>symbol([34]).           % ascii(",34)

%------------------------------------------------------------------------------
/**     stropped(+Innards, ?Wrapper)
Text:   Vyd� �et�zec znak� uzav�en� mezi zp�tn�mi lom�tky, jen� je p�ij�m�n
        parserem Innards.
*/

stropped(P,W):-
        W :-> P enclosedIn #>symbol([92]).           % ascii(\,92)

%------------------------------------------------------------------------------
/**     atomized(+Innards, ?Wrapper)
Text:   Vyd� �et�zec znak� v apostrofech, jen� je p�ij�m�n
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
Text:   Parser bloku jazyka Pascal tj. bloku ohrani�en�ho tokeny
        "begin" a "end".
*/

pascalCompound(P,W):-
        W :-> P enclosedIn (#>token("begin")) and (#>token("end")).

%------------------------------------------------------------------------------
/**     getAngled(?Wrapper)
Text:   Vyd� �et�zec znak� uzav�en� mezi znaky > a <. Pokud uzav�rac�mu
        znaku > p�edch�z� zp�tn� lom�tko, je akceptov�n jako znak oby�ejn�.
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
Text:   Vyd� �et�zec znak� uzav�en� v apostrofech. Pokud apostrofu
        p�edch�z� zp�tn� lom�tko, je akceptov�n jako znak oby�ejn�.
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
Text:   Vyd� �et�zec znak� uzav�en� v uvozovk�ch. Pokud tomuto znaku
        p�edch�z� zp�tn� lom�tko, je akceptov�n jako znak oby�ejn�.
*/

getQuoted(W):-                  % token("\"")
        W :-> ((token([92,34]) <: nonSymbol([34]))<*>)
                enclosedIn (#>symbol([34])).

%------------------------------------------------------------------------------
/**     separatedBy(+Element, +Separator, ?Wrapper)
Text:   Parser obecn� posloupnosti jednoho nebo v�ce token� p�ij�man�ch
        parserem Element. �leny posloupnosti jsou odd�leny sentencemi,
        kter� akceptuje parser Separator. V�sledek je vyd�v�n
        ve form� seznamu element�.
Arg:    Element
        Parser �lena posloupnosti.
Arg:    Separator
        Parser odd�lova�e �len� posloupnosti.
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
Text:   Parser ��rkami odd�len� posloupnosti jednoho a v�ce token�,
        kter� jsou p�ij�m�ny parserem Element. V�sledek je vyd�v�n
        ve form� seznamu element�.
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
Text:   Parser st�edn�ky odd�len� posloupnosti jednoho a v�ce token�, 
        kter� jsou p�ij�m�ny parserem Element. V�sledek je vyd�v�n
        ve form� seznamu element�.
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
Text:   Parser obecn� posloupnosti ��dn�ho nebo v�ce token�,
        kter� jsou p�ij�m�ny parserem Element, odd�len� sentencemi
        p�ij�man�mi parserem Separator. V�sledek je vyd�v�n
        ve form� seznamu element�.
Arg:    Element
        Parser �lena posloupnosti.
Arg:    Separator
        Parser odd�lova�e �len� posloupnosti.
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
Text:   Parser ��rkami odd�len� posloupnosti ��dn�ho nebo v�ce token�,
        kter� jsou p�ij�m�ny parserem Element. V�sledek je vyd�v�n
        ve form� seznamu element�.
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
Text:   Parser st�edn�ky odd�len� posloupnosti ��dn�ho nebo v�ce token�,
        kter� jsou p�ij�m�ny parserem Element. V�sledek je vyd�v�n
        ve form� seznamu element�.
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
Text:   Parser obecn�ho jedno��dkov�ho koment��e.
Arg:    String
        Tento argument je tokenen, kter� je prefixem koment��e.
        Pro jazyk Prolog je to tedy �et�zec "%".
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
Text:   Parser jedno��dkov�ho koment��e jazyka Prolog  (jeho sou��st�
        je rovn� odd�lova� nov�ho ��dku).
Example:
        % this is comment
*/

prologLComment(W):-
        lineComment("%",W).

%------------------------------------------------------------------------------
/** haskellLComment(?Wrapper)
Text:   Parser jedno��dkov�ho koment��e jazyka Haskell (jeho sou��st�
        je rovn� odd�lova� nov�ho ��dku).
Example:
        -- this is comment
*/

haskellLComment(W):-
        lineComment("--",W).

%------------------------------------------------------------------------------
/** prologMLComment(?Wrapper)
Text:   Parser v�ce��dkov�ho (MultiLine) koment��e jazyka Prolog, C, Java,
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
Text:   Parser v�ce��dkov�ho (MultiLine) koment��e jazyka Pascal.
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
