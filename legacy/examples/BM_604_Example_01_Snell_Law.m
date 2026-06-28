(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_01";
(* ============================================== *)
useParallelTbl = False;
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];
(* ============================================== *)
opts =
    {
      BDPlotFigures -> False,
      UseEulerAngles -> False,
      PrintCommonDebugInfo -> True,
      PrintCommonDebugInfoLevel -> PCDILEVELALL
    };
(* ============================================== *)
FuncList =
    {
    (*
    StokesVectorR[1],
    StokesVectorR[2],
    StokesVectorR[3],
    StokesVectorR[4],
    *)
      RFull (*,
      TFull,
      StokesGammaR,
      StokesGammaDegreeR,
      StokesChiR,
      StokesChiDegreeR,
      StokesPolarizedR,
      XirDegree,
      Elr,
      PsiPPDegree,
      DeltaPPDegree,
      {Rx, Ry},
      {Tx, Ty} *)
    };
(* ============================================== *)
systemDescription = "Snell Law: Two semi-infinite isotropic media, NO film.";
(* ============================================== *)
Print["\:041f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:044b \:043f\:0430\:0434\:0430\:044e\:0449\:0435\:0433\:043e \:0441\:0432\:0435\:0442\:0430..."];
nUpper = 1;

lambda = {600, 600, 1, "\[Lambda]", nm};
fita = {0, 85, 85, "\[Phi]", Degree};
beta = {0, 0, 30, "\[Beta]", Degree};
gamma = {0, 0, 30, "\[Gamma]", Degree};
ellipt = {-1, 1, 2, "e"};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(*
(* ============================================== *)
Print["\:041e\:043f\:0442\:0438\:0447\:0435\:0441\:043a\:0438\:0435 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:044b \:043d\:0438\:0436\:043d\:0435\:0439 \:0441\:0440\:0435\:0434\:044b..."];
nLower = 1.5;
lowerMedia = CreateSemiInfiniteMediaFromN[nLower];
(* ============================================== *)
Print["\:0421\:043e\:0437\:0434\:0430\:0435\:043c \:043e\:043f\:0442\:0438\:0447\:0435\:0441\:043a\:0443\:044e \:0441\:0438\:0441\:0442\:0435\:043c\:0443..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["\:041f\:0440\:043e\:0438\:0437\:0432\:043e\:0434\:0438\:043c \:0432\:044b\:0447\:0438\:0441\:043b\:0435\:043d\:0438\:044f \:0434\:043b\:044f \:0440\:0430\:0437\:043b\:0438\:0447\:043d\:044b\:0445 \:0437\:043d\:0430\:0447\:0435\:043d\:0438\:0439 \:043f\:0430\:0440\:0430\:043c\:0435\:0442\:0440\:043e\:0432...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
*)
(* ============================================== *)
