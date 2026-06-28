(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_09b";
(* ============================================== *)
useParallelTbl = False;
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];
(* ============================================== *)
opts =
    {
      BDPlotFigures -> True,
      UseEulerAngles -> False,
      NoOfAveragingPoints -> 3 (* ,
      PrintCommonDebugInfo -> True,
      PrintCommonDebugInfoLevel -> PCDILEVELALL *)
    };
(* ============================================== *)
FuncList =
    {
      {Rx, Ry},
      {Tx, Ty},
      XirDegree,
      PsiPPDegree,
      DeltaPPDegree,
      Elr
    };
(* ============================================== *)
systemDescription = "Uniaxial slightly absorbing thick substrate plate (La3Ga5SiO14) on Si substrate - dispersion calculations.";
Print["!!! For absorbing plate I > R + T !!!"];
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper = 1;

lambda = {200, 800, 100, "λ", nm};
fita = {0, 0, 5, "ϕ", Degree};
beta = {0, 90, 45, "β", Degree};
gamma = {0, 0, 30, "γ", Degree};
ellipt = {0, 0, 0.25, "e"};
fi = {0, 0, 1, "φ", Degree};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры толстой пластинки: La3Ga5SiO14."];
Print["Для расчетов для различных толщин пластинки нужно поменять значение thickness."];
thickness = 1 mm;

fiThickPlate = {0, 0, 30, Subscript["φ", "t"], Degree};
thetaThickPlate = {0, 0, 30, Subscript["θ", "t"], Degree};
psiThickPlate = {0, 0, 30, Subscript["ψ", "t"], Degree};
rotationAnglesThickPlate = {fiThickPlate, thetaThickPlate, psiThickPlate};

thickPlate = CreateThickPlate[thickness, rotationAnglesThickPlate, eps$La3Ga5SiO14, muMstandard, rho$La3Ga5SiO14];
(* ============================================== *)
Print["Оптические параметры нижней среды: Si."];
lowerMedia = CreateSemiInfiniteMedia[eps$Si];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, thickPlate, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)
