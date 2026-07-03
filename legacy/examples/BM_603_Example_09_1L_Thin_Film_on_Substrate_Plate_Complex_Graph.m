(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_09";
(* ============================================== *)
useParallelTbl = False;
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];
(* ============================================== *)
opts =
    {
      BDPlotFigures -> True,
      UseEulerAngles -> False,
      NoOfAveragingPoints -> 3
    };
(* ============================================== *)

FuncList =
    {
      EpsComponent[1, 1, 1, 1],
      {EpsComponent[1, 1, 2, 2], EpsComponent[1, 1, 3, 3]},
      StokesVectorR[1],
      StokesVectorR[2],
      StokesVectorR[3],
      StokesVectorR[4],
      RFull,
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
      {Tx, Ty}
    };
(* ============================================== *)
systemDescription = "One Layer biaxial thin film on slightly absorbing thick substrate plate - dispersion calculations.";
Print["!!! For absorbing plate I > R + T !!!"];
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper = 1;

lambda = {400, 800, 100, "λ", nm};
fita = {60, 60, 5, "ϕ", Degree};
beta = {0, 0, 45, "β", Degree};
gamma = {0, 0, 30, "γ", Degree};
ellipt = {0, 1, 0.25, "e"};
fi = {0, 0, 1, "φ", Degree};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры первого тонкого слоя."];
thicknessLayer1 = {75, 75, 10, "h", nm};

kCoeff11 = 2.5;
lambdaNullCoeff11 = 100 nm;

kCoeff12 = 2.4;
lambdaNullCoeff12 = 120 nm;

kCoeff13 = 2.3;
lambdaNullCoeff13 = 140 nm;

epsFunc1[lamd_] := Module[{nVal1, nVal2, nVal3, epsRet},
  nVal1 = Sqrt[1 + kCoeff11 * lamd^2 / (lamd^2 - lambdaNullCoeff11^2)];
  nVal2 = Sqrt[1 + kCoeff12 * lamd^2 / (lamd^2 - lambdaNullCoeff12^2)];
  nVal3 = Sqrt[1 + kCoeff13 * lamd^2 / (lamd^2 - lambdaNullCoeff13^2)];
  epsRet = EpsilonFromN[nVal1, nVal2, nVal3];
  Return[N[epsRet]];
];

fiLayer1 = {0, 0, 30, Subscript["φ", "1"], Degree};
thetaLayer1 = {0, 0, 30, Subscript["θ", "1"], Degree};
psiLayer1 = {0, 0, 30, Subscript["ψ", "1"], Degree};
rotationAnglesLayer1 = {fiLayer1, thetaLayer1, psiLayer1};

layer1 = CreateFilm[thicknessLayer1, rotationAnglesLayer1, epsFunc1];
(* ============================================== *)
Print["Оптические параметры толстой пластинки"];
Print["Для расчетов для различных толщин пластинки нужно поменять значение thickness"];
thickness = 1 mm;

kCoeffT1 = 2.5;
lambdaNullCoeffT1 = 100 nm;

kCoeffT2 = 2.6;
lambdaNullCoeffT2 = 90 nm;

kCoeffT3 = 2.7;
lambdaNullCoeffT3 = 70 nm;

epsFuncT[lamd_] := Module[{nVal1, nVal2, nVal3, epsRet},
  nVal1 = Sqrt[1 + kCoeffT1 * lamd^2 / (lamd^2 - lambdaNullCoeffT1^2)];
  nVal2 = Sqrt[1 + kCoeffT2 * lamd^2 / (lamd^2 - lambdaNullCoeffT2^2)];
  nVal3 = Sqrt[1 + kCoeffT3 * lamd^2 / (lamd^2 - lambdaNullCoeffT3^2)];
  epsRet = EpsilonFromN[nVal1, nVal2, nVal3];
  Return[N[epsRet]];
];

fiThickPlate = {0, 0, 30, Subscript["φ", "t"], Degree};
thetaThickPlate = {0, 0, 30, Subscript["θ", "t"], Degree};
psiThickPlate = {0, 0, 30, Subscript["ψ", "t"], Degree};
rotationAnglesThickPlate = {fiThickPlate, thetaThickPlate, psiThickPlate};

thickPlate = CreateThickPlate[thickness, rotationAnglesThickPlate, epsFuncT];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
nLower = 1;
lowerMedia = CreateSemiInfiniteMediaFromN[nLower];
(* ============================================== *)
If[useParallelTbl == True,
  Print["Distributing definitions for parallel calculations..."];
  DistributeDefinitions[epsFunc1, epsFuncT];
];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, layer1, thickPlate, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)
