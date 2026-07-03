(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "TCINV_";
OutFileName = "TEST_300_1.CSV";
(* ============================================== *)
useParallelTbl = False;
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];
(* ============================================== *)
opts =
    {
      BDPlotFigures -> False,
      UseEulerAngles -> False,
      RotateAll -> True
    };
(*==============================================*)
FuncList =
    {
      Rx,
      Ry,
      Tx,
      Ty
    };
(*==============================================*)
systemDescription = "One Layer biaxial thin film on slightly absorbing thick substrate plate.";
(*==============================================*)
Print["Параметры падающего света..."];
nUpper = 1;

lambda = {600, 600, 1, "λ", nm};
fita = {0, 75, 15, "ϕ", Degree};
beta = {0, 90, 90, "β", Degree};
gamma = {-90, 90, 30, "γ", Degree};
ellipt = {0, 0, 1, "e"};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры первого тонкого слоя."];
thicknessLayer1 = {75, 75, 10, "h", nm};

fiLayer1 = {7, 7, 1, Subscript["φ", "1"], Degree};
thetaLayer1 = {-11, -11, 1, Subscript["θ", "1"], Degree};
psiLayer1 = {17, 17, 1, Subscript["ψ", "1"], Degree};
rotationAnglesLayer1 = {fiLayer1, thetaLayer1, psiLayer1};

epsLayer1 = EpsilonFromN[1.50, 1.75, 1.65];
Print["epsLayer1 = ", epsLayer1 // MatrixForm];

layer1 = CreateFilm[thicknessLayer1, rotationAnglesLayer1, epsLayer1];

rotn = RotationNew[fiLayer1[[1]] Degree, thetaLayer1[[1]] Degree, psiLayer1[[1]] Degree, opts];
eeExact1 = N[Transform[epsLayer1, rotn]];
Print["eeExact1 = ", eeExact1 // MatrixForm];
(* ============================================== *)
Print["Оптические параметры толстой пластинки"];
Print["Для расчетов для различных толщин пластинки нужно поменять значение thickness"];
thickness = 1 mm;
nSubstr = 1.52;
kSubstr = 3 * 10^-6;
thickPlate = CreateThickPlateFromN[thickness, nSubstr + I * kSubstr];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
nLower = 1;
lowerMedia = CreateSemiInfiniteMediaFromN[nLower];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, layer1, thickPlate, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)
Print["allCalc = "];
allCalc