(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_04";
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
(*
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
*)
FuncList =
    {
      StokesVectorI[1],
      StokesVectorI[2],
      StokesVectorI[3],
      StokesVectorI[4],
      StokesVectorR[1],
      StokesVectorR[2],
      StokesVectorR[3],
      StokesVectorR[4],
      StokesVectorT[1],
      StokesVectorT[2],
      StokesVectorT[3],
      StokesVectorT[4],
      {Rx, Ry},
      {Tx, Ty}
    };
(* ============================================== *)
systemDescription = "Snell Law: Random media - for tests, NO film.";
Print["!!! For absorbing plate I > R + T !!!"];
(* ============================================== *)
Print["Параметры падающего света..."];
nUpper = 1;

lmb = RandomInteger[{200, 800}];
lambda = {lmb, lmb, 1, "λ", nm};

ft = RandomInteger[{0, 85}];
fita = {ft, ft, 90, "ϕ", Degree};

bt = RandomInteger[{0, 85}];
beta = {bt, bt, 90, "β", Degree};

gamma = {0, 0, 90, "γ", Degree};

el = RandomReal[{0, 1}];
ellipt = {el, el, 1, "e"};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];

fiLower = {0, 0, 30, Subscript["φ", "1"], Degree};
thetaLower = {0, 0, 30, Subscript["θ", "1"], Degree};
psiLower = {0, 0, 30, Subscript["ψ", "1"], Degree};
rotationAnglesLower = {fiLower, thetaLower, psiLower};

epsReMin = 1;
epsReMax = 5;

epsImMin = 0;
epsImMax = 10^-2;

muMin = 0.9;
muMax = 1.1;

rhoMin = -0.1;
rhoMax = 0.1;

rotationEpsRe = RotationNew[RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, opts];
Print["rotationEpsRe = ", N[rotationEpsRe] // MatrixForm];

rotationEpsIm = RotationNew[RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, opts];
Print["rotationEpsIm = ", N[rotationEpsIm] // MatrixForm];

rotationMu = RotationNew[RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, opts];
Print["rotationMu = ", N[rotationMu] // MatrixForm];

rotationRho = RotationNew[RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, RandomInteger[{0, 90}] Degree, opts];
Print["rotationRho = ", N[rotationRho] // MatrixForm];

useIm = False;

epsLower = Transform[DiagonalMatrix[{RandomReal[{epsReMin, epsReMax}], RandomReal[{epsReMin, epsReMax}], RandomReal[{epsReMin, epsReMax}]}], rotationEpsRe] + I * Transform[DiagonalMatrix[{RandomReal[{epsImMin, epsImMax}], RandomReal[{epsImMin, epsImMax}], RandomReal[{epsImMin, epsImMax}]}], rotationEpsIm];

muLower = Transform[DiagonalMatrix[{RandomReal[{muMin, muMax}], RandomReal[{muMin, muMax}], RandomReal[{muMin, muMax}]}], rotationMu];

rhoLower = I * Transform[DiagonalMatrix[{RandomReal[{rhoMin, rhoMax}], RandomReal[{rhoMin, rhoMax}], RandomReal[{rhoMin, rhoMax}]}], rotationRho];

Print["epsLower = ", N[epsLower] // MatrixForm];
Print["muLower = ", muLower // MatrixForm];
Print["rhoLower = ", rhoLower // MatrixForm];

lowerMedia = CreateSemiInfiniteMedia[rotationAnglesLower, epsLower, muLower, rhoLower];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)
