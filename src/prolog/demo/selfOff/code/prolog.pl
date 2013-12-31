% Machine generated file!

% Swi Prolog loader
:- ['../../../loadSWI'].

% Example goal:
% ?- nINTEGER(s("357")+L).

nPROGRAM(_G229804):-_G229804:-> (nCOMMAND<&>nPROGRAM<:>nCOMMAND).
nCOMMAND(_G229848):-_G229848:-> (nCLAUSE<&>token([46])<:>nQUERY<&>token([46])<:>nOPDECL<&>token([46])).
nCLAUSE(_G229865):-_G229865:-> (nFACT<:>nRULE).
nQUERY(_G229887):-_G229887:-> (token([63, 45])<&>nQUERYBODY).
nOPDECL(_G229955):-_G229955:-> (token([58, 45, 111, 112])<&>token([40])<&>nPRIORITY<&>token([44])<&>nASOCIATIVITY<&>token([44])<&>nOPNAME<&>token([41])<&>token([46])).
nFACT(_G229969):-_G229969:->nHEAD.
nRULE(_G229994):-_G229994:-> (nHEAD<&>token([58, 45])<&>nRULEBODY).
nHEAD(_G230011):-_G230011:-> (nATOM<:>nSTRUCTURE).
nQUERYBODY(_G230025):-_G230025:->nALTERNATIVES.
nALTERNATIVEBODY(_G230039):-_G230039:->nALTERNATIVES.
nALTERNATIVES(_G230067):-_G230067:-> (nHEAD<&>token([59])<&>nALTERNATIVES<:>nALTERNATIVE).
nPRIORITY(_G230081):-_G230081:->nINTEGER.
nASOCIATIVITY(_G230107):-_G230107:-> (nPRFASOC<:>nPOFASOC<:>nINFASOC<:>nLEFASOC<:>nRIFASOC).
nPRFASOC(_G230134):-_G230134:-> (token([102, 120])<:>token([102, 121])).
nPOFASOC(_G230161):-_G230161:-> (token([120, 102])<:>token([121, 102])).
nINFASOC(_G230188):-_G230188:-> (token([120, 102, 120])<:>token([121, 102, 121])).
nLEFASOC(_G230207):-_G230207:->token([121, 102, 120]).
nRIFASOC(_G230226):-_G230226:->token([120, 102, 121]).
nOPNAME(_G230240):-_G230240:->nATOM.
nATOM(_G230282):-_G230282:-> (nLWRCHAR<:>nLWRCHAR<&>nNONSPECIALCHARS<:>nSPECIALCHARS<:>token([39])<&>nCHARS<&>token([39])).
nSTRUCTURE(_G230321):-_G230321:-> (nFUNCTOR<&>token([40])<&>nARGUMENTS<&>token([41])<:>nSTRUCTWITHOP<:>nLIST).
nSTRUCTWITHOP(_G230353):-_G230353:-> (nPRFOP<&>nTERM<:>nTERM<&>nPOFOP<:>nTERM<&>nINFOP<&>nTERM).
nFUNCTOR(_G230375):-_G230375:-> (nATOM<:>token([46])).
nARGUMENTS(_G230403):-_G230403:-> (nARGUMENT<&>token([44])<&>nARGUMENTS<:>nARGUMENT).
nARGUMENT(_G230417):-_G230417:->nTERM.
nCHAR(_G230445):-_G230445:-> (nALPHACHAR<:>nNUMBERCHAR<:>nSPECIALCHAR<:>token([95])).
nCHARS(_G230465):-_G230465:-> (nCHAR<&>nCHARS<:>nCHAR).
nNONSPECIALCHAR(_G230490):-_G230490:-> (nALPHACHAR<:>nNUMBERCHAR<:>token([95])).
nNONSPECIALCHARS(_G230510):-_G230510:-> (nNONSPECIALCHAR<&>nNONSPECIALCHARS<:>nNONSPECIALCHAR).
nALPHACHAR(_G230527):-_G230527:-> (nLWRCHAR<:>nUPRCHAR).
nNUMBERCHAR(_G230618):-_G230618:-> (token([48])<:>token([49])<:>token([50])<:>token([51])<:>token([52])<:>token([53])<:>token([54])<:>token([55])<:>token([56])<:>token([57])).
nLWRCHAR(_G230837):-_G230837:-> (token([97])<:>token([98])<:>token([99])<:>token([100])<:>token([101])<:>token([102])<:>token([103])<:>token([104])<:>token([105])<:>token([106])<:>token([107])<:>token([108])<:>token([109])<:>token([110])<:>token([111])<:>token([112])<:>token([113])<:>token([114])<:>token([115])<:>token([116])<:>token([117])<:>token([118])<:>token([119])<:>token([120])<:>token([121])<:>token([122])).
nUPRCHAR(_G231056):-_G231056:-> (token([65])<:>token([66])<:>token([67])<:>token([68])<:>token([69])<:>token([70])<:>token([71])<:>token([72])<:>token([73])<:>token([74])<:>token([75])<:>token([76])<:>token([77])<:>token([78])<:>token([79])<:>token([80])<:>token([81])<:>token([82])<:>token([83])<:>token([84])<:>token([85])<:>token([86])<:>token([87])<:>token([88])<:>token([89])<:>token([90])).
nSPECIALCHAR(_G231139):-_G231139:-> (token([64])<:>token([35])<:>token([36])<:>token([38])<:>token([61])<:>token([45])<:>token([43])<:>token([42])<:>token([47])).
nSPECIALCHARS(_G231159):-_G231159:-> (nSPECIALCHAR<&>nSPECIALCHARS<:>nSPECIALCHAR).
nPRFOP(_G231197):-_G231197:-> (token([63, 45])<:>token([110, 111, 116])<:>token([58, 45])<:>nATOM).
nPOFOP(_G231211):-_G231211:->nATOM.
nINFOP(_G231276):-_G231276:-> (token([58, 45])<:>nARIOP<:>token([59])<:>token([44])<:>nRELOP<:>token([105, 115])<:>token([61, 46, 46])<:>token([46])).
nARIOP(_G231335):-_G231335:-> (token([43])<:>token([45])<:>token([42])<:>token([47])<:>token([109, 111, 100])<:>token([94])).
nRELOP(_G231426):-_G231426:-> (token([61])<:>token([92, 61])<:>token([60])<:>token([61, 60])<:>token([62])<:>token([62, 61])<:>token([61, 58, 61])<:>token([61, 92, 61])<:>token([61, 61])<:>token([92, 61, 61])).
nLIST(_G231465):-_G231465:-> (token([91, 93])<:>nEASYLIST<:>nDIVIDEDLIST<:>nTERM<&>token([46])<&>nLIST).
nEASYLIST(_G231495):-_G231495:-> (token([91])<&>nTERMS<&>token([93])).
nDIVIDEDLIST(_G231536):-_G231536:-> (token([91])<&>nTERMS<&>token([124])<&>nTERMS<&>token([93])).
nTERMS(_G231564):-_G231564:-> (nTERM<&>token([44])<&>nTERMS<:>nTERM).
nTERM(_G231590):-_G231590:-> (nATOM<:>nINTEGER<:>nIDENTIFIER<:>nSTRUCTURE<:>nLIST).
nINTEGER(_G231610):-_G231610:-> (nDIGIT<&>nINTEGER<:>nDIGIT).
nDIGIT(_G231624):-_G231624:->nNUMBERCHAR.
nIDENTIFIER(_G231638):-_G231638:->nATOM.

% EOF
