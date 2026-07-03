(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_09a";
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
      {Rx, Ry},
      {Tx, Ty},
      XirDegree,
      PsiPPDegree,
      DeltaPPDegree,
      Elr
    };
(* ============================================== *)
systemDescription = "Uniaxial slightly absorbing thin substrate plate (La3Ga5SiO14) - dispersion calculations.";
Print["!!! For absorbing plate I > R + T !!!"];
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper = 1;

lambda = {250, 350, 100, "λ", nm};
fita = {0, 0, 5, "ϕ", Degree};
beta = {0, 90, 45, "β", Degree};
gamma = {0, 0, 30, "γ", Degree};
ellipt = {0, 0, 0.25, "e"};
fi = {0, 0, 1, "φ", Degree};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры тонкой пластинки: La3Ga5SiO14."];
Print["Для расчетов для различных толщин пластинки нужно поменять значение thickness."];
thickness = {0.1, 0.1, 1, "h", mm};

fiThickPlate = {0, 0, 30, Subscript["φ", "t"], Degree};
thetaThickPlate = {0, 0, 30, Subscript["θ", "t"], Degree};
psiThickPlate = {0, 0, 30, Subscript["ψ", "t"], Degree};
rotationAnglesThickPlate = {fiThickPlate, thetaThickPlate, psiThickPlate};

layer1 = CreateFilm[thickness, rotationAnglesThickPlate, eps$La3Ga5SiO14, muMstandard, rho$La3Ga5SiO14];
(* ============================================== *)
Print["Оптические параметры нижней среды: vacuum."];
lowerMedia = CreateSemiInfiniteMedia[eps$Vacuum];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, layer1, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)
