(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_05";
(* ============================================== *)
useParallelTbl = False;
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];
(* ============================================== *)
opts =
    {
      BDPlotFigures -> True,
      UseEulerAngles -> False
    };
(* ============================================== *)
FuncList =
    {
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
systemDescription = "Snell Law: Active, anisotropic, absorbing, etc... thick plate between air, NO film.";
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper = 1;

lambda = {600, 600, 1, "λ", nm};
fita = {0, 85, 85, "ϕ", Degree};
beta = {0, 0, 30, "β", Degree};
gamma = {0, 0, 30, "γ", Degree};
ellipt = {0, 1, 0.5, "e"};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Для расчетов для различных толщин пластинки нужно поменять значение thickness."];
thickness = 10 mkm;

fiSubstr = {0, 0, 30, Subscript["φ", "1"], Degree};
thetaSubstr = {0, 0, 30, Subscript["θ", "1"], Degree};
psiSubstr = {0, 0, 30, Subscript["ψ", "1"], Degree};
rotationAnglesSubstr = {fiSubstr, thetaSubstr, psiSubstr};

epsSubstr =
    {
      {2.471 + I * 0.0432, -0.000357 + I * 0.002945, 0},
      {-0.000357 + I * 0.002945, 2.465 + I * 0.0425, 0},
      {0, 0, 2.51 + I * 0.048}
    };

thickPlate = CreateThickPlate[thickness, rotationAnglesSubstr, epsSubstr];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
nLower = 1;
lowerMedia = CreateSemiInfiniteMediaFromN[nLower];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, thickPlate, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)
