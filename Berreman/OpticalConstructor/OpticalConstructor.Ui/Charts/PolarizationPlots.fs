/// §H.8 — the [Standard] polarization-ellipse (ScottPlot) and Poincaré-sphere
/// (Plotly.NET in WebView2) plots, consuming EXISTING engine outputs only. The
/// ellipse is a parametric curve built from the engine's reflected/transmitted
/// `Ellipticity` (`Fields.fs:302`) and azimuth `Polarization` (`Fields.fs:314`),
/// read off `EmFieldSystem`'s reflected / transmitted fields (the values behind
/// `ellipticityR`/`ellipticityT`/`azimuthR`/`azimuthT`, `FieldFunctions.fs:177-191`).
/// The Poincaré point is the normalized Stokes vector
/// `(S1,S2,S3)/S0` from `Solution.stokesR`/`stokesT` (`FieldFunctions.fs:227,238`) and
/// `StokesVector` (`Fields.fs:580`). Part H recomputes NO Stokes parameters and does
/// NOT redefine `StokesVector`.
module OpticalConstructor.Ui.Charts.PolarizationPlots

open Plotly.NET
open Berreman.Geometry
open Berreman.Fields
open Berreman.Solvers
open Berreman.FieldFunctions
open Analytics.Variables
open OpticalConstructor.Controls.ExperimentControls
open OpticalConstructor.Ui.Charts.ChartSettings

/// Parametric polarization ellipse: semi-major axis 1 along the principal direction,
/// signed semi-minor axis = `ellipticity` (its sign carries handedness), rotated by
/// the `azimuth` orientation. `n` is the number of parametric intervals (`n + 1` points,
/// both ends included).
let ellipseCurve (n : NumberOfPoints) (ellipticity : Ellipticity) (azimuth : Polarization) : ChartPoint [] =
    let e = ellipticity.value
    let a = azimuth.value
    [|
        for i in 0..n.value ->
            let t = 2.0 * System.Math.PI * float i / float n.value
            let x0 = cos t
            let y0 = e * sin t
            let x = x0 * cos a - y0 * sin a
            let y = x0 * sin a + y0 * cos a
            { x = x; y = y }
    |]

/// Reflected/transmitted polarization-ellipse curves directly from the engine fields.
let reflectedEllipse (n : NumberOfPoints) (em : EmFieldSystem) : ChartPoint [] = ellipseCurve n em.reflected.ellipticity em.reflected.azimuth
let transmittedEllipse (n : NumberOfPoints) (em : EmFieldSystem) : ChartPoint [] = ellipseCurve n em.transmitted.ellipticity em.transmitted.azimuth

/// §H.8 — draw a polarization ellipse onto a ScottPlot plot, settings applied through
/// the sole `applyToScottPlot` seam.
let renderEllipse (settings : ChartSettings) (ellipticity : Ellipticity) (azimuth : Polarization) : ScottPlot.Plot =
    let plot = new ScottPlot.Plot()
    let pts = ellipseCurve (NumberOfPoints 180) ellipticity azimuth
    let scatter = plot.Add.Scatter(pts |> Array.map (fun p -> p.x), pts |> Array.map (fun p -> p.y))
    scatter.LegendText <- "polarization"
    applyToScottPlot settings plot
    plot

type StokesVector with

    /// Normalized Stokes point `(S1,S2,S3)/S0` (`Fields.fs:580`); the origin when S0 vanishes.
    /// The Stokes vector is taken from the engine; no Stokes parameter is recomputed.
    member this.poincarePoint : RealVector3 =
        let (StokesVector v) = this
        let s0 = v.[0]
        if abs s0 < 1.0e-300 then RealVector3.zeroVector
        else RealVector3.create [ v.[1] / s0; v.[2] / s0; v.[3] / s0 ]

/// §H.8 — the Poincaré sphere: a faint reference unit sphere plus the normalized
/// Stokes point, as a Plotly.NET 3D chart (hosted in WebView2 at runtime). `n` is the
/// number of polar and azimuthal intervals of the reference sphere.
let poincareSphere (n : NumberOfPoints) (stokes : StokesVector) : Plotly.NET.GenericChart =
    let s = stokes.poincarePoint
    let pts =
        [
            for i in 0..n.value do
                for j in 0..n.value do
                    let theta = System.Math.PI * float i / float n.value
                    let phi = 2.0 * System.Math.PI * float j / float n.value
                    yield (sin theta * cos phi, sin theta * sin phi, cos theta)
        ]
    let xs = pts |> List.map (fun (a, _, _) -> a)
    let ys = pts |> List.map (fun (_, b, _) -> b)
    let zs = pts |> List.map (fun (_, _, c) -> c)
    let sphere = Chart.Point3D(xs, ys, zs, Name = "sphere", Opacity = 0.15)
    let point = Chart.Point3D([ s.x ], [ s.y ], [ s.z ], Name = "S")
    Chart.combine [ sphere; point ]

/// Reflected/transmitted Poincaré spheres from a `Solution`'s engine Stokes members.
let reflectedPoincare (n : NumberOfPoints) (sol : Solution) : Plotly.NET.GenericChart = poincareSphere n sol.stokesR
let transmittedPoincare (n : NumberOfPoints) (sol : Solution) : Plotly.NET.GenericChart = poincareSphere n sol.stokesT
