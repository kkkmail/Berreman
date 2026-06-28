(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_08";
(* ============================================== *)
useParallelTbl = False;
Get["BerremanInit.m", Path -> PathList];
InitializeBM[PathList, useParallelTbl];
(* ============================================== *)
opts =
    {
      BDPlotFigures -> False,
      UseEulerAngles -> False
    };
(* ============================================== *)
FuncList =
    {
      (*
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
      *)
      {Rx, Ry},
      {Tx, Ty}
    };
(* ============================================== *)
systemDescription = "One Layer biaxial thin film between two semi-infinite media.";
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper = 1;

lambda = {600, 600, 1, "λ", nm};
fita = {35, 35, 85, "ϕ", Degree};
beta = {0, 0, 30, "β", Degree};
gamma = {0, 0, 30, "γ", Degree};
ellipt = {0, 0, 0.5, "e"};

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
Print["epsLayer1 = ", epsLayer1 // toFSharpMatrix];

layer1 = CreateFilm[thicknessLayer1, rotationAnglesLayer1, epsLayer1];
(* ============================================== *)
Print["Оптические параметры второго тонкого слоя."];
thicknessLayer2 = {100, 100, 10, "h", nm};

fiLayer2 = {0, 0, 30, Subscript["φ", "2"], Degree};
thetaLayer2 = {0, 0, 30, Subscript["θ", "2"], Degree};
psiLayer2 = {0, 0, 30, Subscript["ψ", "2"], Degree};
rotationAnglesLayer2 = {fiLayer2, thetaLayer2, psiLayer2};

epsLayer2 = EpsilonFromN[1.75, 1.50, 2.00];
muLayer2 = DiagonalMatrix[{1, 1, 1}];
rhoLayer2 = I * {{1.9 * 10^-3, -3.5 * 10^-3, 0}, {3.5 * 10^-3, 1.9 * 10^-3, 0}, {0, 0, -5.7 * 10^-3}};
Print["epsLayer2 = ", epsLayer2 // toFSharpMatrix];
Print["muLayer2 = ", muLayer2 // toFSharpMatrix];
Print["rhoLayer2 = ", rhoLayer2 // toFSharpMatrix];

layer2 = CreateFilm[thicknessLayer2, rotationAnglesLayer2, epsLayer2, muLayer2, rhoLayer2];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
nLower = 1.52;
lowerMedia = CreateSemiInfiniteMediaFromN[nLower];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, layer1, layer2, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)

