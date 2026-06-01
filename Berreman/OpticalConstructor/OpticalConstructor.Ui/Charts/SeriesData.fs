/// §H.2 — the renderer-neutral series adapter over the engine's own data extraction
/// (the Part A sweeps seam). Converts engine output into plain `(float*float)[]`
/// (1D) and a `float[][]` grid (surface) WITHOUT re-implementing the solver call or
/// the sweep loop, and WITHOUT the engine's terminal `Chart.show` wrappers. Every
/// view consumes these arrays; no view re-runs the solver. Series caching is out of
/// scope (§H.2).
module OpticalConstructor.Ui.Charts.SeriesData

open Berreman.Constants
open Berreman.Fields
open Berreman.FieldFunctions
open Berreman.Dispersion
open Berreman.Solvers
open Analytics
open Analytics.Variables
open OpticalConstructor.Domain

/// A renderer-neutral 1D series: a legend name and the `(x, y)` points exactly as
/// the engine's `Charting.plot` would choose them.
type Series1D =
    {
        name : string
        points : (float * float) []
    }

/// A renderer-neutral surface: the Z grid plus the X/Y plot-axis values.
type Surface =
    {
        z : float [] []
        xValues : float []
        yValues : float []
    }

/// §H.2 — 1D series for one `OpticalFunction` over a sweep, reproducing the
/// choose-on-`Some` pattern `Charting.plot` uses inside `getFuncData`
/// (`Charting.fs:26-30`): call `Variables.calculate` (`Variables.fs:237`) and
/// project each point through `Solution.func` (`FieldFunctions.fs:192`). The legend
/// name is the engine's `OpticalFunction.info.fullName` (`Charting.fs:31`).
/// The choose-on-`Some` projection `Charting.plot` uses inside `getFuncData`
/// (`Charting.fs:26-30`): map each `(x, Solution)` through `Solution.func` and keep
/// only the `Some` results as `(x, y)` pairs. Shared by `series1D` and
/// `seriesComparison` so the projection is expressed once (F2).
let private projectPoints (fn : OpticalFunction) (data : (float * Solution) []) : (float * float) [] =
    data
    |> Array.map (fun (v, s) -> (v, s.func fn))
    |> Array.choose (fun (xx, yo) -> match yo with | Some y -> Some (xx, y) | None -> None)

let series1D (f : FixedInfo) (fn : OpticalFunction) (x : RangedVariable) : Series1D =
    { name = fn.info.fullName; points = projectPoints fn (calculate f x) }

/// §H.2/§H.3 — one `Series1D` per requested function over a single system.
let series1DMany (f : FixedInfo) (fns : OpticalFunction list) (x : RangedVariable) : Series1D list =
    fns |> List.map (fun fn -> series1D f fn x)

/// §H.4 — overlay/comparison series: the same `OpticalFunction` drawn from several
/// systems, reusing the per-system data layout of `Charting.plotComparison`
/// (`Charting.fs:38-49`) — one `calculate` per system, then one trace per system
/// named `fullName + " (" + index + ")"` exactly as `Charting.fs:49`. No
/// `Chart.show` is involved; only the series assembly is reused.
let seriesComparison (systems : FixedInfo list) (fn : OpticalFunction) (x : RangedVariable) : Series1D list =
    let data = systems |> List.map (fun e -> calculate e x)
    data
    |> List.mapi (fun i d ->
        { name = fn.info.fullName + " (" + i.ToString() + ")"; points = projectPoints fn d })

/// §H.2/§H.5 — surface Z grid for one `OpticalFunction` over a FIXED `calculate3D`
/// run, applying `Charting.mapFun` (`Charting.fs:57`) to the data array it is handed
/// (Part H consumes whatever array it is given; it does not re-run the sweep). The
/// X/Y axes carry the sweep plot-points; the engine's documented X/Y swap is applied
/// by the view (`Plot3DView`), mirroring `Charting.plot3D` (`Charting.fs:74-77`).
let surfaceFromData (data : #seq<#seq<double * double * Solution>>) (fn : OpticalFunction) (xValues : float []) (yValues : float []) : Surface =
    { z = Charting.mapFun data fn; xValues = xValues; yValues = yValues }

/// §H.2/§H.5 — convenience surface builder: run `calculate3D` (`Variables.fs:270`)
/// once, then project it through `surfaceFromData`.
let surface (f : FixedInfo) (fn : OpticalFunction) (x : RangedVariable) (y : RangedVariable) : Surface =
    surfaceFromData (calculate3D f x y) fn x.plotPoints y.plotPoints

/// §H.9 — field-depth profile as a 1D series. Consumes the
/// `(float<meter> * float) list` produced by Part F §F.6 `fieldDepthProfile`
/// (`Analytics/FieldProfile.fs`) and converts each canonical-SI depth to the layer's
/// display unit at the boundary via the §A.10 `Units` seam (`Units.fromMeters`).
/// Part H adds no in-layer field solver and does not re-sample the layer.
let fieldDepthSeries
    (depthUnit : Units.UnitOfMeasure)
    (profile : (float<meter> * float) list)
    : Series1D =

    let points =
        profile
        |> List.map (fun (depth, value) -> (Units.fromMeters depthUnit depth, value))
        |> Array.ofList
    { name = "|E|^2"; points = points }

/// §H.11 — n,k / ρ / ε-component dispersion series. Reuses the engine's dispersion
/// data builders (`Variables.calculateN11Re`…`calculateRho33Im`, the bodies behind
/// `Charting.plotN11`…`plotRho33`, `Charting.fs:95-107`) which already end in
/// `(float*float)[]`; Part H drops only the terminal `Chart.show` (`Charting.fs:92`)
/// and re-derives none of the n/k/ρ formulas (owned by `OpticalPropertiesWithDisp`).
let private dispersion name calc (o : OpticalPropertiesWithDisp) (r : Range<WaveLength>) : Series1D =
    { name = name; points = calc o r }

let plotN11Series  o r = dispersion "Re[e11]" calculateN11Re  o r
let plotXi11Series o r = dispersion "Im[e11]" calculateXi11Im o r
let plotN22Series  o r = dispersion "Re[e22]" calculateN22Re  o r
let plotXi22Series o r = dispersion "Im[e22]" calculateXi22Im o r
let plotN33Series  o r = dispersion "Re[e33]" calculateN33Re  o r
let plotXi33Series o r = dispersion "Im[e33]" calculateXi33Im o r
let plotRho11Series o r = dispersion "rho11" calculateRho11Im o r
let plotRho22Series o r = dispersion "rho22" calculateRho22Im o r
let plotRho33Series o r = dispersion "rho33" calculateRho33Im o r
