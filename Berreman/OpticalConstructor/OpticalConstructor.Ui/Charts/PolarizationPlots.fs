/// §H.8 — the [Standard] polarization-ellipse (ScottPlot) and Poincaré-sphere
/// (Plotly.NET in WebView2) plots, consuming EXISTING engine outputs only. The
/// ellipse is a parametric curve built from the engine's reflected/transmitted
/// `Ellipticity` (`Fields.fs:302`) and azimuth `Polarization` (`Fields.fs:314`),
/// surfaced on `EmFieldSystem` as `ellipticityR`/`ellipticityT`/`azimuthR`/`azimuthT`
/// (`FieldFunctions.fs:157-176`). The Poincaré point is the normalized Stokes vector
/// `(S1,S2,S3)/S0` from `Solution.stokesR`/`stokesT` (`FieldFunctions.fs:227,238`) and
/// `StokesVector` (`Fields.fs:580`). Part H recomputes NO Stokes parameters and does
/// NOT redefine `StokesVector`.
module OpticalConstructor.Ui.Charts.PolarizationPlots

open Plotly.NET
open Berreman.Geometry
open Berreman.Fields
open Berreman.Solvers
open Berreman.FieldFunctions
open OpticalConstructor.Ui.Charts.ChartSettings

/// Parametric polarization ellipse: semi-major axis 1 along the principal direction,
/// signed semi-minor axis = `ellipticity` (its sign carries handedness), rotated by
/// the `azimuth` orientation. `n` is the number of parametric samples.
let ellipseCurve (n : int) (ellipticity : float) (azimuth : float) : (float * float) [] =
    [|
        for i in 0..n ->
            let t = 2.0 * System.Math.PI * float i / float n
            let x0 = cos t
            let y0 = ellipticity * sin t
            let x = x0 * cos azimuth - y0 * sin azimuth
            let y = x0 * sin azimuth + y0 * cos azimuth
            (x, y)
    |]

/// Reflected/transmitted polarization-ellipse curves directly from the engine members.
let reflectedEllipse (n : int) (em : EmFieldSystem) = ellipseCurve n em.ellipticityR em.azimuthR
let transmittedEllipse (n : int) (em : EmFieldSystem) = ellipseCurve n em.ellipticityT em.azimuthT

/// §H.8 — draw a polarization ellipse onto a ScottPlot plot, settings applied through
/// the sole `applyToScottPlot` seam.
let renderEllipse (settings : ChartSettings) (ellipticity : float) (azimuth : float) : ScottPlot.Plot =
    let plot = new ScottPlot.Plot()
    let pts = ellipseCurve 180 ellipticity azimuth
    let scatter = plot.Add.Scatter(pts |> Array.map fst, pts |> Array.map snd)
    scatter.LegendText <- "polarization"
    applyToScottPlot settings plot
    plot

/// Normalized Stokes point `(S1,S2,S3)/S0` from a `StokesVector` (`Fields.fs:580`).
/// The Stokes vector is taken from the engine; no Stokes parameter is recomputed.
let poincarePoint (stokes : StokesVector) : float * float * float =
    let (StokesVector v) = stokes
    let s0 = v.[0]
    if abs s0 < 1.0e-300 then (0.0, 0.0, 0.0)
    else (v.[1] / s0, v.[2] / s0, v.[3] / s0)

/// §H.8 — the Poincaré sphere: a faint reference unit sphere plus the normalized
/// Stokes point, as a Plotly.NET 3D chart (hosted in WebView2 at runtime).
let poincareSphere (n : int) (stokes : StokesVector) : Plotly.NET.GenericChart =
    let (sx, sy, sz) = poincarePoint stokes
    let pts =
        [
            for i in 0..n do
                for j in 0..n do
                    let theta = System.Math.PI * float i / float n
                    let phi = 2.0 * System.Math.PI * float j / float n
                    yield (sin theta * cos phi, sin theta * sin phi, cos theta)
        ]
    let xs = pts |> List.map (fun (a, _, _) -> a)
    let ys = pts |> List.map (fun (_, b, _) -> b)
    let zs = pts |> List.map (fun (_, _, c) -> c)
    let sphere = Chart.Point3D(xs, ys, zs, Name = "sphere", Opacity = 0.15)
    let point = Chart.Point3D([ sx ], [ sy ], [ sz ], Name = "S")
    Chart.combine [ sphere; point ]

/// Reflected/transmitted Poincaré spheres from a `Solution`'s engine Stokes members.
let reflectedPoincare (n : int) (sol : Solution) = poincareSphere n sol.stokesR
let transmittedPoincare (n : int) (sol : Solution) = poincareSphere n sol.stokesT
