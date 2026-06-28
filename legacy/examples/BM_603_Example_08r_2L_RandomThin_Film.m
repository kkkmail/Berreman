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
systemDescription = "Two Layer random thin film between two semi-infinite media.";
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
seedMedia1 = RandomInteger[10^12];
Print["seedMedia1 = ", seedMedia1];

fiLayer1 = {0, 0, 30, Subscript["φ", "1"], Degree};
thetaLayer1 = {0, 0, 30, Subscript["θ", "1"], Degree};
psiLayer1 = {0, 0, 30, Subscript["ψ", "1"], Degree};
rotationAnglesLayer1 = {fiLayer1, thetaLayer1, psiLayer1};
thk1 = RandomInteger[{50, 250}];
Print["thk1 = ", thk1 // InputForm];
thicknessLayer1 = {thk1, thk1, 10, "h1", nm};

useIm1 = True;
useMu1 = True;
useRho1 = True;
{epsLayer1, muLayer1, rhoLayer1} = randomMedia[seedMedia1, useIm1, useMu1, useRho1];

Print["epsLayer1 = ", epsLayer1 // MatrixForm];
Print["muLayer1 = ", muLayer1 // MatrixForm];
Print["rhoLayer1 = ", rhoLayer1 // MatrixForm];

layer1 = CreateFilm[thicknessLayer1, rotationAnglesLayer1, epsLayer1, muLayer1, rhoLayer1];
(* ============================================== *)
Print["Оптические параметры второго тонкого слоя."];
seedMedia2 = RandomInteger[10^12];
Print["seedMedia2 = ", seedMedia2];

fiLayer2 = {0, 0, 30, Subscript["φ", "2"], Degree};
thetaLayer2 = {0, 0, 30, Subscript["θ", "2"], Degree};
psiLayer2 = {0, 0, 30, Subscript["ψ", "2"], Degree};
rotationAnglesLayer2 = {fiLayer2, thetaLayer2, psiLayer2};
thk2 = RandomInteger[{50, 250}];
Print["thk2 = ", thk2 // InputForm];
thicknessLayer2 = {thk2, thk2, 10, "h2", nm};

useIm2 = True;
useMu2 = True;
useRho2 = True;
{epsLayer2, muLayer2, rhoLayer2} = randomMedia[seedMedia2, useIm2, useMu2, useRho2];

Print["epsLayer2 = ", epsLayer2 // MatrixForm];
Print["muLayer2 = ", muLayer2 // MatrixForm];
Print["rhoLayer2 = ", rhoLayer2 // MatrixForm];

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

