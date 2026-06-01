/// §H.5 — the [Standard] 2D/3D surface & contour view (e.g. R vs. λ × angle),
/// produced by Plotly.NET (hosted in the WebView2 control at runtime — the Part A
/// chart split). It reuses `Analytics.Charting.plot3D`'s `Chart.Surface` construction
/// (`Charting.fs:64-82`), including the `kk:20180922` swap of X with Y
/// (`Charting.fs:74-77`) and the contour toggle `Contours.initXyz(Show = true)`
/// (`Charting.fs:75`). The Z grid comes from the `SeriesData` surface adapter over
/// `calculate3D`/`mapFun`; the solver is never called directly. Custom OpenTK 3D
/// rendering is reserved by Part A for the system/beam viewport and is NOT used here.
module OpticalConstructor.Ui.Charts.Plot3DView

open Plotly.NET
open Berreman.FieldFunctions
open Analytics.Variables
open OpticalConstructor.Ui.Charts.SeriesData

/// Build the Plotly surface from a renderer-neutral `Surface`, mirroring
/// `Charting.plot3D` exactly: the Z grid with the X/Y axes swapped (X gets the
/// y-sweep points/name, Y gets the x-sweep points/name) and contours shown.
/// `xVarName`/`yVarName` are the sweep variables' names (`x.name`/`y.name`).
let surfaceChart (xVarName : string) (yVarName : string) (zName : string) (s : Surface) : Plotly.NET.GenericChart =
    // kk:20180922 The axes are swapped: hand X the y-data and Y the x-data, as plot3D does.
    Chart.Surface(s.z, s.yValues, s.xValues, Opacity = 0.7, Contours = TraceObjects.Contours.initXyz (Show = true), Name = zName)
    |> Chart.withXAxisStyle yVarName
    |> Chart.withYAxisStyle xVarName
    |> Chart.withZAxisStyle zName

/// §H.5 — surface/contour chart for one `OpticalFunction` over an (x, y) sweep,
/// sourcing the Z grid from `SeriesData.surface` (`calculate3D`/`mapFun`).
let render (f : FixedInfo) (fn : OpticalFunction) (x : RangedVariable) (y : RangedVariable) : Plotly.NET.GenericChart =
    surfaceChart x.name y.name fn.info.name (surface f fn x y)
