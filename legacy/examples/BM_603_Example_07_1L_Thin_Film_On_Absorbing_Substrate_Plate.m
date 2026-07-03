(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_07";
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
systemDescription = "One Layer biaxial thin film on slightly absorbing thick substrate plate.";
Print["!!! For absorbing plate I > R + T !!!"];
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper = 1;

lambda = {600, 600, 1, "λ", nm};
fita = {0, 85, 5, "ϕ", Degree};
beta = {0, 0, 45, "β", Degree};
gamma = {0, 0, 30, "γ", Degree};
ellipt = {-1, 1, 0.5, "e"};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры первого тонкого слоя."];
thicknessLayer1 = {75, 75, 10, "h", nm};

fiLayer1 = {0, 0, 30, Subscript["φ", "1"], Degree};
thetaLayer1 = {0, 0, 30, Subscript["θ", "1"], Degree};
psiLayer1 = {0, 0, 30, Subscript["ψ", "1"], Degree};
rotationAnglesLayer1 = {fiLayer1, thetaLayer1, psiLayer1};

epsLayer1 = EpsilonFromN[1.50, 2.00, 1.75];
Print["epsLayer1 = ", epsLayer1 // MatrixForm];

layer1 = CreateFilm[thicknessLayer1, rotationAnglesLayer1, epsLayer1];
(* ============================================== *)
Print["Оптические параметры толстой пластинки"];
Print["Для расчетов для различных толщин пластинки нужно поменять значение thickness"];
nSubstr = 1.5;
kSubstr = 3 * 10^-6;
thickness = 1 mm;
thickPlate = CreateThickPlateFromN[thickness, nSubstr + I * kSubstr];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
nLower = 1.5;
lowerMedia = CreateSemiInfiniteMediaFromN[nLower];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, layer1, thickPlate, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)
