(* ============================================== *)
ClearAll["Global`*"];
(* ============================================== *)
BaseDir = "C:\\GitHub\\BM\\";
PathList = {BaseDir <> "Kernel\\"};
OutDir = BaseDir <> "Calc\\";
(* ============================================== *)
BaseFileName = "Example_06";
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
systemDescription = "One Layer biaxial thin film between two semi-infinite media.";
(* ============================================== *)
Print["Параметры падающего света..."];
seedLight = RandomInteger[10^12];
Print["seedLight = ", seedLight];
nUpper = 1;

{lmb, ft, bt, el} = randomLightInfo[seedLight];
lambda = {lmb, lmb, 1, "λ", nm};
fita = {ft, ft, 85, "ϕ", Degree};
beta = {bt, bt, 30, "β", Degree};
ellipt = {el, el, 0.5, "e"};
gamma = {0, 0, 30, "γ", Degree};

incidentLight = CreateIncidentRay[nUpper, lambda, fita, beta, ellipt];
OutputIncidentRayInfo[incidentLight];
(* ============================================== *)
Print["Оптические параметры первого тонкого слоя."];
seedMedia = RandomInteger[10^12];
Print["seedMedia = ", seedMedia];

fiLayer1 = {0, 0, 30, Subscript["φ", "1"], Degree};
thetaLayer1 = {0, 0, 30, Subscript["θ", "1"], Degree};
psiLayer1 = {0, 0, 30, Subscript["ψ", "1"], Degree};
rotationAnglesLayer1 = {fiLayer1, thetaLayer1, psiLayer1};
thk = RandomInteger[{50, 250}];
Print["thk = ", thk // InputForm];
thicknessLayer1 = {thk, thk, 10, "h", nm};

useIm = True;
useMu = True;
useRho = True;
{epsLayer1, muLayer1, rhoLayer1} = randomMedia[seedMedia, useIm, useMu, useRho];

Print["epsLayer1 = ", epsLayer1 // MatrixForm];
Print["muLayer1 = ", muLayer1 // MatrixForm];
Print["rhoLayer1 = ", rhoLayer1 // MatrixForm];

layer1 = CreateFilm[thicknessLayer1, rotationAnglesLayer1, epsLayer1, muLayer1, rhoLayer1];
(* ============================================== *)
Print["Оптические параметры нижней среды..."];
nLower = 1.52;
lowerMedia = CreateSemiInfiniteMediaFromN[nLower];
(* ============================================== *)
Print["Создаем оптическую систему..."];
layeredSystem = CreateLayeredSystem[incidentLight, gamma, layer1, lowerMedia];
OutputLayeredSystem[layeredSystem];
(* ============================================== *)
Print["Производим вычисления для различных значений параметров...."];
allCalc = PerformAllCalculations[layeredSystem, FuncList, systemDescription, opts];
(* ============================================== *)

