namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.ExperimentRun
open OpticalConstructor.Domain.ItemEvaluation
open OpticalConstructor.Storage
open OpticalConstructor.Storage.ExperimentExportFile

/// What a detector reads along its light path (`ExperimentRun`, spec 0046): the source's emission through every step's
/// Mueller matrix in light order (spec 0050 S8.1); the CSV layout of an exported run; and a tunable source's validation.
module ExperimentRunTests =

    let private tol = 1.0e-12
    let private deg (degrees : float) : Angle = Angle.degree (degrees * 1.0<deg>)
    let private lp (degrees : float) : RunStep = ElementStep (Propagation.analyzerMueller IdealLinear (deg degrees))
    let private clear : RunStep = ObjectStep Propagation.identityMueller

    /// What the light-source axes' defaults describe: unpolarized light at unit scale (spec 0050 S8.1).
    let private defaultEmission : SourceEmission =
        match SourceEmission.ofValues (ParameterValues.defaultsOf SourcePreset.lightSourceAxes) with
        | Ok emission -> emission
        | Error error -> failwith error.describe

    let private components (sv : StokesVector) : float list = [ sv.s0; sv.s1; sv.s2; sv.s3 ]

    /// Each Stokes component within 1e-12 of the expected one (spec 0050 S8.1).
    let private assertStokesWithin (expected : StokesVector) (actual : StokesVector) : unit =
        List.zip (components expected) (components actual)
        |> List.iteri (fun i (e, a) -> Assert.True(abs (e - a) <= tol, $"S{i}: expected {e:R}, got {a:R}"))

    let private intensityOf (steps : RunStep list) : float =
        match read Intensity defaultEmission steps with
        | IntensityReading i -> i.value
        | EllipsometricReading _ -> failwith "expected an intensity"

    [<Fact>]
    let ``the default source is unpolarized light at unit scale, referenced after the first polarizer`` () =
        let unpolarized = detectorStokes defaultEmission []
        let s0, s1, s2, s3 = unpolarized.s0, unpolarized.s1, unpolarized.s2, unpolarized.s3
        Assert.True(abs (s0 - 1.0) < tol && abs s1 < tol && abs s2 < tol && abs s3 < tol)
        let polarized = detectorStokes defaultEmission [ lp 0.0 ]
        let p0, p1 = polarized.s0, polarized.s1
        Assert.True(abs (p0 - 1.0) < tol && abs (p1 - 1.0) < tol)

    [<Fact>]
    let ``a polarizer AFTER the sample analyzes: crossed polarizers around a clear sample pass nothing`` () =
        Assert.True(abs (intensityOf [ lp 0.0; clear; lp 90.0 ]) < tol, "crossed")
        Assert.True(abs (intensityOf [ lp 0.0; clear; lp 0.0 ] - 1.0) < tol, "parallel")
        // Malus: the analyzer at 60° passes cos²60° = 1/4.
        Assert.True(abs (intensityOf [ lp 0.0; clear; lp 60.0 ] - 0.25) < 1.0e-12)
        // A second polarizer BEFORE any sample also acts through its Mueller matrix (it does not reset).
        Assert.True(abs (intensityOf [ lp 0.0; lp 90.0 ]) < tol)

    [<Fact>]
    let ``an ellipsometer reads Psi and Delta of the arriving state`` () =
        match read Ellipsometer defaultEmission [ lp 45.0; clear ] with
        | EllipsometricReading pd ->
            Assert.True(abs (pd.psi.degrees - 45.0) < 1.0e-9, $"Ψ = %g{pd.psi.degrees}")
            Assert.True(abs pd.delta.degrees < 1.0e-9, $"Δ = %g{pd.delta.degrees}")
        | IntensityReading _ -> Assert.Fail "an ellipsometer reads Ψ / Δ"

    [<Fact>]
    let ``the default source reproduces the old unit-intensity rule on every existing case`` () =
        // The old rule through its kept primitives: `inputStokes` after a prep, `unpolarizedStokes` without one.
        let identity = Propagation.identityMueller
        let lp60 = Propagation.analyzerMueller IdealLinear (deg 60.0)
        let lp90 = Propagation.analyzerMueller IdealLinear (deg 90.0)
        for kind in [ IdealLinear; IdealCircularLeft; IdealCircularRight ] do
            for degrees in [ 0.0; 15.0; 30.0; 45.0; 60.0; 90.0; 120.0; 135.0; 180.0; 225.0; 270.0; 315.0; 330.0 ] do
                let prep = ElementStep (Propagation.analyzerMueller kind (deg degrees))
                let prepared = Propagation.inputStokes kind (deg degrees)
                let cases : (RunStep list * StokesVector) list =
                    [
                        [ prep ], prepared
                        [ prep; clear ], identity * prepared
                        [ prep; clear; lp 60.0 ], lp60 * (identity * prepared)
                        [ prep; lp 90.0 ], lp90 * prepared
                        [ clear; lp 60.0 ], lp60 * (identity * Propagation.unpolarizedStokes)
                        [], Propagation.unpolarizedStokes
                    ]
                for steps, expected in cases do
                    Assert.Equal<float list>(components expected, components (detectorStokes defaultEmission steps))

    [<Fact>]
    let ``objects in series apply their matrices in light order`` () =
        let first = MuellerReconstruction.retarderMueller (deg 30.0) (MuellerReconstruction.Retardance (Math.PI / 2.0))
        let second = MuellerReconstruction.retarderMueller (deg 75.0) (MuellerReconstruction.Retardance (Math.PI / 3.0))
        let analyzer = Propagation.analyzerMueller IdealLinear (deg 45.0)
        let inLightOrder = detectorStokes defaultEmission [ lp 0.0; ObjectStep first; ObjectStep second; lp 45.0 ]
        assertStokesWithin (analyzer * (second * (first * Propagation.inputStokes IdealLinear (deg 0.0)))) inLightOrder
        let swapped = detectorStokes defaultEmission [ lp 0.0; ObjectStep second; ObjectStep first; lp 45.0 ]
        Assert.True(abs (swapped.s0 - inLightOrder.s0) > 1.0e-3, $"S0: {inLightOrder.s0:R} in light order, {swapped.s0:R} swapped")

    let private muellerElements (m : MuellerMatrix) : float list = [ for i in 0 .. 3 do for j in 0 .. 3 -> m.element i j ]

    [<Fact>]
    let ``a path's Mueller matrix is the product of its steps in light order`` () =
        let a = Propagation.analyzerMueller IdealLinear (deg 20.0)
        let b = MuellerReconstruction.retarderMueller (deg 30.0) (MuellerReconstruction.Retardance (Math.PI / 2.0))
        let c = Propagation.analyzerMueller IdealLinear (deg 75.0)
        let path = [ ElementStep a; ObjectStep b; ElementStep c ]
        let product = RunStep.productOf path

        List.zip (muellerElements (c * (b * a))) (muellerElements product)
        |> List.iteri (fun k (e, p) -> Assert.True(abs (e - p) <= tol, $"element ({k / 4},{k % 4}): expected {e:R}, got {p:R}"))

        // Against the light, A · B · C is another matrix, so the order is the light's.
        let againstLight = muellerElements (a * (b * c))
        let largest = List.zip againstLight (muellerElements product) |> List.map (fun (x, p) -> abs (x - p)) |> List.max
        Assert.True(largest > 1.0e-3, $"C · B · A and A · B · C differ by at most {largest:R}")

        Assert.Equal<float list>(muellerElements Propagation.identityMueller, muellerElements (RunStep.productOf []))

    [<Fact>]
    let ``a real source emits its own state times its scale`` () =
        let source : SourceEmission =
            {
                azimuth = deg 30.0
                ellipticity = Angle.zero
                degreeOfPolarization = ParameterValue 1.0
                intensityScale = ParameterValue 2.0
            }
        let sixty = Math.PI / 3.0
        assertStokesWithin (StokesVector.create [ 2.0; 2.0 * cos sixty; 2.0 * sin sixty; 0.0 ]) (detectorStokes source [])
        // The reference is that first polarizer's unpolarized throughput, ½, so r = 2.
        Assert.True(abs ((detectorStokes source [ lp 30.0 ]).s0 - 4.0) <= tol)

    [<Fact>]
    let ``a constant-compound first polarizer passes unit intensity like any other`` () =
        let reference : MuellerComponent = { matrix = Propagation.analyzerMueller IdealLinear (deg 0.0); offset = deg 0.0 }
        let compound = ElementStep (Propagation.behaviorMueller (ConstantMueller [ reference ]) (deg 30.0))
        let sixty = Math.PI / 3.0
        assertStokesWithin (StokesVector.create [ 1.0; cos sixty; sin sixty; 0.0 ]) (detectorStokes defaultEmission [ compound ])

    [<Fact>]
    let ``an exported run is written wide when its series share x, long otherwise`` () =
        let text = ExperimentExport.ExportText
        let point (x : float) (y : float) : Propagation.CurvePoint = { x = x; y = y }
        let run (series : ExperimentExport.ExportedSeries list) : ExperimentExport.ExportedRun =
            { title = text "t"; xLabel = text "x"; yLabel = text "y"; description = text ""; series = series }
        let shared =
            run [ { name = text "Intensity (T)"; points = [ point 0.0 1.0; point 1.0 0.5 ] }
                  { name = text "Intensity (R)"; points = [ point 0.0 0.0; point 1.0 0.5 ] } ]
        let table = shared.exportTable
        Assert.Equal<string list>([ "x"; "Intensity (T)"; "Intensity (R)" ], table.headers |> List.map (fun h -> h.value))
        Assert.Equal<float list list>([ [ 0.0; 1.0; 0.0 ]; [ 1.0; 0.5; 0.5 ] ], table.rows |> Array.map List.ofArray |> List.ofArray)
        let ragged = run [ { name = text "A"; points = [ point 0.0 1.0 ] }; { name = text "B"; points = [ point 2.0 3.0; point 4.0 5.0 ] } ]
        Assert.Equal(3, Array.length ragged.exportTable.rows)
        // The in-memory store refuses a run with no series.
        let store = ExperimentExport.createInMemoryExperimentExport ()
        match store.proxy.exportRun (run []) with
        | Error (ExperimentExport.NothingToExportErr _) -> ()
        | other -> Assert.Fail($"expected NothingToExportErr, got %A{other}")
        Assert.Equal(Ok (), store.proxy.exportRun shared)
        Assert.Equal<ExperimentExport.ExportedRun list>([ shared ], store.exported ())

    [<Fact>]
    let ``a tunable source needs ordered positive limits and a setting inside them`` () =
        let tunable (lo : float) (hi : float) (at : float) : SourcePreset =
            {
                id = Placement.ElementValueId "src-t"
                name = MaterialLibrary.EntryName "T"
                waveLength = WaveLength.nm (at * 1.0<nm>)
                tuning = TunableBetween { Berreman.Dispersion.WaveLengthInterval.lower = WaveLength.nm (lo * 1.0<nm>); upper = WaveLength.nm (hi * 1.0<nm>) }
                protection = UserManaged
            }
        Assert.Equal(Ok (), (tunable 400.0 800.0 600.0).validate)
        for bad in [ tunable 800.0 400.0 600.0; tunable 400.0 800.0 900.0; tunable 0.0 800.0 600.0 ] do
            match bad.validate with
            | Error (InvalidSourceErr _) -> ()
            | other -> Assert.Fail($"expected InvalidSourceErr, got %A{other}")
        Assert.Equal(Some (400.0, 800.0), (tunable 400.0 800.0 600.0).tunableLimitsOpt |> Option.map (fun l -> round (l.lower.value / nmToMeter / oneNanometer), round (l.upper.value / nmToMeter / oneNanometer)))
        let tunables =
            [ PresetIds.xenonMonochromator; PresetIds.tiSapphireTunable; PresetIds.supercontinuum; PresetIds.euvBeamline ]
            |> List.map BuiltinCatalogue.source
        Assert.All(tunables, fun s -> Assert.True(Option.isSome s.tunableLimitsOpt, s.name.value))

    [<Fact>]
    let ``the run's wavelength is a tunable source's clamped request, a fixed line's own, or 600 nm without a source`` () =
        let nanometers (w : WaveLength) : float = (Units.wavelengthToUnit Units.Nanometer w).value
        let assertNanometers (expected : float) (actual : WaveLength) : unit = Assert.Equal(expected, nanometers actual, 1.0e-9)
        let request (at : float) : WaveLength option = Some (WaveLength.nm (at * 1.0<nm>))
        let heNe = BuiltinCatalogue.source PresetIds.heNe633
        let limits : Berreman.Dispersion.WaveLengthInterval = { lower = WaveLength.nm 400.0<nm>; upper = WaveLength.nm 700.0<nm> }
        let tunableHeNe = Some { heNe with tuning = TunableBetween limits }
        assertNanometers 500.0 (SourcePreset.operatingWaveLengthOf tunableHeNe (request 500.0))
        assertNanometers 700.0 (SourcePreset.operatingWaveLengthOf tunableHeNe (request 900.0))
        assertNanometers 400.0 (SourcePreset.operatingWaveLengthOf tunableHeNe (request 300.0))
        assertNanometers (nanometers heNe.waveLength) (SourcePreset.operatingWaveLengthOf tunableHeNe None)
        for requested in [ request 500.0; request 900.0; None ] do
            assertNanometers 632.8 (SourcePreset.operatingWaveLengthOf (Some heNe) requested)
        for requested in [ request 500.0; None ] do
            assertNanometers 600.0 (SourcePreset.operatingWaveLengthOf None requested)
