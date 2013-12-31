%
% exception(ExceptionType,Sel)
%       .... mod zavadejici mechanismus vyjimek do konstruktoru parseru.
%        Viz predikaty raiser, pro vyhozeni vyjimky a kacer, pro jeji
%        odchyceni - error.pl.
%         Options:
%               ExceptionType:
%                       Libovolna uzivatelska struktura. Funktor spolecne
%               s aritou urcuje 'typ' vyjimky. Vyjimka je vyhazovana
%               parserm raiser a lze ji odchytit parserem kacer prave
%               tehdy, kdyz se 'typ' vyjimky unifikuje s parametrem
%               predikatu kacer.
%       

%------------------------------------------------------------------------------
%                               Exceptions
%------------------------------------------------------------------------------
% Popis:
%       System vyjimek knihovny je analogii obdobnych mechanismu,
% ktere zname napriklad z jazyka Java. Zde je realizovan pomoci uzivatelskeho
% modu.
%       Vyjimka se vyhazuje prechodem do modu exception (popis selektoru
% viz mode.pl), kterou lze provest pomoci parseru raiser (podle throw,raise),
% jehoz parametrem je struktura s vyjimkou. Funktor struktury lze povazovat
% za typ vyjimky.
%       Obdobne lze provest odchyceni vyjimky pomoci predikatu kacer (catch),
% ktery provede vybaleni puvodniho deskriptoru a navraceni do modu, v
% nemz parser bezel, nez vyjimka nastala.
%       Atypicka jmena predikatu byla zvolena vzhledem ke kolizim s predity
% jiz existujicimi v ruznych intrepretech.
%       Je-li vyhozena vyjimka, znamena to prechod do modu exception/2.
% V tomto modu se ignoruje vstup (primitivum item se chova netecne)
% dokud neni vyjimka odchycene kacerem, ktery opet vybali selektor a umozni
% tak dalsi beh. Vybublani vyjimky az na toplevel odpovida vraceni
% struktury  s vyjimkou - z vyse uvedeneho plyne, ze z druheho parametru 
% selektoru vyjimky lze zjistit, kde nastala.
%------------------------------------------------------------------------------
/**     raiser(+Exception,?Wrapper)
Text:   Parser pro vyhozen� v�jimky. Viz tak� m�d exception/2 a kacer.
Arg:    Exception
        Libovoln� u�ivatelsk� struktura. Funktor spole�n� s aritou
        struktury ur�uje 'typ' v�jimky. Chov�n� p�i odchycen� v�jimky
        lze definovat v predik�tu kacer.
*/

raiser(ExcInfo,I+[exception(ExcInfo,I)>eXi]).  % eXi je nahrazku vysledku

%------------------------------------------------------------------------------
/**     kacer(+Exception,?Wrapper)
Text:   Parser pro odchycen� v�jimky.  Viz tak� m�d exception/2 a raiser.
        V kacer/2 je pro jednotliv� 'typy' v�jimek definov�na
        chov�n� v p��pad� jejich odchycen�.
v
Arg:    Exception
        Libovoln� u�ivatelsk� struktura. Funktor spole�n� s aritou
        struktury ur�uje 'typ' v�jimky. Aby byla v�jimka odchycena,
        mus� se Exception unifikovat se selektor ve vstupn�m
        termu vstupn� v�stupn�ho termu Wrapper.
*/

% nezname exception info
kacer(_,exception(_,I)+[I]).


