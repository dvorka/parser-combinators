%------------------------------------------------------------------------------
%
%                       Operator environment generator
%
%			        Martin Dvorak
%                                   1999
%------------------------------------------------------------------------------
/** Module:
Text:   Generátor bìhového environmentu parseru výrazù.
*/
%------------------------------------------------------------------------------
% Inicializace & knihovny

:- ['../../loadSWI'].          % SWI Prolog
%:- ['../../loadBP-2'].        % BinProlog

%------------------------------------------------------------------------------
%                                 Demo
%------------------------------------------------------------------------------

go:-
        op2Env(
                % lowest priority
                [ [fy("!",flip), fx("++",inc), fx("--",dec),xf("++",inc), xf("--",dec)],
                [fx("~",neg), xf("?",que)]],
                % highest priority
                E),
        deBug(expr,[nl,'Environment: ',nl,E,nl]).

%------------------------------------------------------------------------------
/**     op2Env(+Operators, -Environment)
Text:   Generuje z u¾ivatelského výètu operátorù, tak jak byl popsán
        v expression/3 environment, který je následnì pou¾íván k rozkladu.
*/

op2Env([],[]).
op2Env([PrecLevel|T],[EnvLevel|ET]):-
        op2EnvLev(PrecLevel,            % jedna prioritni uroven
                  % fx        xf        fy        yf
                  [terminate,terminate,terminate,terminate,
                  % xfx       yxf       xfy       yfx
                   terminate,terminate,terminate,terminate],
                  EnvLevel),  
        op2Env(T,ET).

op2EnvLev([],E,E).
% operatory: fx xf fy yf xfx yfy xfy yfx
op2EnvLev([fx(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(FX,O,V,FXo), op2EnvLev(T,[FXo,XF,FY,YF,XFX,YFY,XFY,YFX],E).
op2EnvLev([xf(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(XF,O,V,XFo), op2EnvLev(T,[FX,XFo,FY,YF,XFX,YFY,XFY,YFX],E).
op2EnvLev([fy(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(FY,O,V,FYo), op2EnvLev(T,[FX,XF,FYo,YF,XFX,YFY,XFY,YFX],E).
op2EnvLev([yf(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(YF,O,V,YFo), op2EnvLev(T,[FX,XF,FY,YFo,XFX,YFY,XFY,YFX],E).
op2EnvLev([xfx(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(XFX,O,V,XFXo), op2EnvLev(T,[FX,XF,FY,YF,XFXo,YFY,XFY,YFX],E).
op2EnvLev([yfy(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(YFY,O,V,YFYo), op2EnvLev(T,[FX,XF,FY,YF,XFX,YFYo,XFY,YFX],E).
op2EnvLev([xfy(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(XFY,O,V,XFYo), op2EnvLev(T,[FX,XF,FY,YF,XFX,YFY,XFYo,YFX],E).
op2EnvLev([yfx(O,V)|T],[FX,XF,FY,YF,XFX,YFY,XFY,YFX],E):-
        op2EnvP(YFX,O,V,YFXo), op2EnvLev(T,[FX,XF,FY,YF,XFX,YFY,XFY,YFXo],E).

op2EnvP(P,S,V,Po):-
        P=terminate -> Po= (token(S)<@const(V))
                     ; Po= (<:>(token(S)<@const(V),P)).

%- EOF ------------------------------------------------------------------------
