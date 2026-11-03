namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.FieldFunctions
open Analytics
open Analytics.Variables
open Analytics.StandardSystems
open OpticalConstructor.Ui.Charts
open Xunit

/// §H.2 series-adapter tests (slice 012). Prove the renderer-neutral `SeriesData`
/// adapter reuses the engine's own data extraction rather than re-implementing the
/// solver:
///   AC-H1 — the 1D adapter over `Variables.calculate` reproduces exactly the
///           `(x, y)` pairs `Charting.plot`'s `getFuncData` would choose.
///   AC-H3 — the surface adapter's Z grid equals `Charting.mapFun` over a fixed
///           `calculate3D` run.
module SeriesDataTests =

    let private light = IncidentLightInfo.create (WaveLength.nm 550.0<nm>)
    let private f = FixedInfo.transparentGlass light

    // ----------------------------------------------------------------- AC-H1

    [<Fact>]
    let ``AC-H1 series1D reproduces Charting.plot's chosen (x,y) pairs`` () =
        let x =
            IncidenceAngleRange (Range<_>.create (NumberOfPoints 8) (Angle.degree 0.0<deg> |> IncidenceAngle) (Angle.degree 60.0<deg> |> IncidenceAngle))

        // The exact choose-on-`Some` projection `Charting.plot` performs (Charting.fs:26-30).
        let data = calculate f x
        let expected =
            data
            |> Array.choose (fun p -> p.solution.func R |> Option.map (fun y -> (p.plotValue, y.value)))

        let actual = (SeriesData.series1D f R x).points

        Assert.Equal(expected.Length, actual.Length)
        Array.zip expected actual
        |> Array.iter (fun ((ex, ey), a) ->
            Assert.Equal(ex, a.x, 12)
            Assert.Equal(ey, a.y, 12))

    [<Fact>]
    let ``AC-H1 the series legend name is the engine fullName`` () =
        let x =
            IncidenceAngleRange (Range<_>.create (NumberOfPoints 4) (Angle.degree 0.0<deg> |> IncidenceAngle) (Angle.degree 40.0<deg> |> IncidenceAngle))
        Assert.Equal(R.info.fullName, (SeriesData.series1D f R x).name.value)

    // ----------------------------------------------------------------- AC-H3

    [<Fact>]
    let ``AC-H3 surface adapter Z grid equals mapFun output`` () =
        let x =
            IncidenceAngleRange (Range<_>.create (NumberOfPoints 6) (Angle.degree 0.0<deg> |> IncidenceAngle) (Angle.degree 50.0<deg> |> IncidenceAngle))
        let y =
            WaveLengthRange (   Range<_>.create (NumberOfPoints 5) (WaveLength.nm 400.0<nm>) (WaveLength.nm 700.0<nm>))

        // A single fixed calculate3D run, shared between mapFun and the adapter, so
        // the comparison is order-stable (calculate3D parallelises over rows).
        let data = calculate3D f x y
        let expected = Charting.mapFun data R |> Array.map (Array.map (fun v -> v.value))
        let actual = (SeriesData.surfaceFromData data R x y).z

        Assert.Equal(expected.Length, actual.Length)
        Array.zip expected actual
        |> Array.iter (fun (er, ar) ->
            Assert.Equal(er.Length, ar.Length)
            Array.zip er ar |> Array.iter (fun (e, a) -> Assert.Equal(e, a, 12)))
