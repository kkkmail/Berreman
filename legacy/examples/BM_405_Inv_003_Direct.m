PathList = {"C:\\Math\\", "C:\\"};
BaseDir = "C:\\Math\\"; BaseFileName = "TCINV_"; OutDir = BaseDir <> "Calc\\"; OutFileName = "TEST_300_3.CSV";
(*==============================================*)
useParallelTbl = True;
Get["BerremanInit.m", Path -> PathList];
Initialize[PathList, useParallelTbl];
(*==============================================*)
opts = {UseThickLastLayer -> False, PrintTimeEstimate -> True, RotateAll -> True};
(* ConsecutiveRotation  True *)
(*==============================================*)
n1 = 1; n2 = 1.52;
FuncList = {Rx, Ry, Elr, Sin2Xir};

lambda = {600, 600, 0.01, "Lambda", nm};fita = {0, 75, 15, "Fita", Degree};beta = {0, 90, 45, "Beta", Degree};gamm = {-90, 90, 45, "Gamma", Degree};ellipt = {0, 1, 0.5, "ellipticity"};

fi = {7, 7, 20, "Fi", Degree};theta = {-11, -11, 15, "Teta", Degree};psi = {17, 17, 35, "Psi", Degree};
VarList = VarListNew[{lambda, fita, beta, gamm, ellipt}, {fi, theta, psi}];
VarListInfiniteGlass = VarListNew[{lambda, fita, beta, gamm, ellipt}, {fi, theta, psi}];
(*==============================================*)
fi1 = {0, 0, 30, "Fi", Degree};theta1 = {0, 0, 2, "Teta", Degree};psi1 = {0, 0, 4, "Psi", Degree};thk1 = {70, 70, 7, "h", nm};
(*==============================================*)
fi2 = {0, 0, 30, "Fi", Degree};theta2 = {0, 0, 2, "Teta", Degree};psi2 = {0, 0, 4, "Psi", Degree};thk2 = {10^6, 10^6, 50, "h", nm};
(*==============================================*)
eps1 = EpsilonFromN[1.50, 1.75, 1.65];thck1 = N[100 nm];
eps2 = EpsilonFromN[1.52, 1.52, 1.52];thck2 = N[200 nm];
(*==============================================*)
Film = FilmNew[];
FilmInfiniteGlass = FilmNew[];
(*==============================================*)
VarList = VarListAddLayer[VarList, {fi1, theta1, psi1, thk1}];Layer = FilmLayerNew[thck1, eps1]; Film = FilmAddLayer[Film, Layer];
(*==============================================*)
VarListInfiniteGlass = VarListAddLayer[VarListInfiniteGlass, {fi1, theta1, psi1, thk1}];Layer = FilmLayerNew[thck1, eps1]; FilmInfiniteGlass = FilmAddLayer[FilmInfiniteGlass, Layer];
(*==============================================*)
(*==============================================*)
Media = MediaNew[n1, n2, gamm, Film, "Nothing"];
MediaInfiniteGlass = MediaNew[n1, n2, gamm, FilmInfiniteGlass, "Nothing"];

OutputCopyright[];
time1 = SessionTime[];

Calc = CalcNew[Media, VarList, FuncList, "2 layers", opts];
CalcCombRot = CalcNew[Media, VarList, FuncList, "2 layers", opts, ConsecutiveRotation -> False];

coll = CalcCollectionNew[BaseDir, BaseFileName, "No Description so far."];
CalcCollectionAddCalc[coll, Calc];
CalcCollectionPerform[coll];
CalcCollectionSave[coll, True];
CalcCollectionSave[coll];
time2 = SessionTime[]; timeused = time2 - time1; Print["Time used: ", timeused];

coll

