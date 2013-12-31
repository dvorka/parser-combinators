%------------------------------------------------------------------------------
%
%                  	         Benchmark
%
%				Martin Dvorak
%                                   2000
%------------------------------------------------------------------------------
/** Module:
Text:   Predik�ty ur�en� pro profiling.
*/
%------------------------------------------------------------------------------
% ToDo:
%------------------------------------------------------------------------------
/** pcBenchmark(+Goal)
Text:   Predik�t ur�en� pro profiling.
        Implementaci jazyka Prolog ur�uje struktura pcProlog/1 v souboru
        prolog/pc/pc*.pl, tj. v zavad��i pro danou implementaci. V z�vislosti
        na n� jsou vyps�ny informace o b�hu jako �as, po�et inferenc� apod.
Arg:    Goal
        M��en� c�l.
*/

pcBenchmark(Goal):-             % Ve SWI Prolog: ?- time(Goal).
     pcProlog(swi),             %           nebo ?- profile(Goal,plain,20)
     time(Goal).               
     
pcBenchmark(Goal):-             % BinProlog
     pcProlog(bp),
     ctime(X1), X is X1/1000,
     printf([nl,' Start: ',X,nl]),
      Goal,
     ctime(Y1), Y is Y1/1000,
     printf([nl,' Stop: ',Y,nl]).

pcBenchmark(Goal):-             % LPA Prolog
    pcProlog(lpa),
    timeStart(H,M,S,SS),
     Goal,
    timeStop(H,M,S,SS).

%------------------------------------------------------------------------------
% timeStart(H,M,S,SS) & timeStop(H,M,S,SS)
% - uklada casove znamky do souboru time
% - specificke pro implementaci LPA Prolog
timeStart(H,M,S,SS):-
    time(H,M,S,SS).
timeStop(H,M,S,SS):-
    time(H2,M2,S2,SS2),
    D  is (H2*3600 + M2*60 + S2) - (H*3600 + M*60 + S),
    DM is D // 60,
    DS is D mod 60,
    openFile('time',Oldtime,write),
     printf([nl,'Start ',H,':',M,':',S,':',SS,nl,'Stop  ',H2,':',M2,':',S2,':',SS2,' -> ',DM,':',DS]),
    closeFile(Oldtime,write).

%- EOF ------------------------------------------------------------------------
