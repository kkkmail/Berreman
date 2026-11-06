namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ExperimentRun
open OpticalConstructor.Storage
open OpticalConstructor.Storage.ExperimentExportFile

/// Spec 0046 — what a detector reads along its light path (`ExperimentRun`): polarizers before the sample
/// prepare the input, polarizers after it analyze, the detector decides the quantity; the CSV layout of an
/// exported run; and a tunable source's validation.
module ExperimentRunTests =

    let private tol = 1.0e-12
    let private lp (deg : float) : RunStep = PolarizerStep (ComputedIdeal IdealLinear, Angle.degree (deg * 1.0<deg>))
    let private clear : RunStep = SampleStep Propagation.identityMueller

    let private intensityOf (steps : RunStep list) : float =
        match read Intensity steps with
        | IntensityReading i -> i.value
        | EllipsometricReading _ -> failwith "expected an intensity"

    [<Fact>]
    let ``the source's light is unpolarized; the first polarizer sets a unit-intensity polarized input`` () =
        let unpolarized = detectorStokes []
        let s0, s1, s2, s3 = unpolarized.s0, unpolarized.s1, unpolarized.s2, unpolarized.s3
        Assert.True(abs (s0 - 1.0) < tol && abs s1 < tol && abs s2 < tol && abs s3 < tol)
        let polarized = detectorStokes [ lp 0.0 ]
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
        match read Ellipsometer [ lp 45.0; clear ] with
        | EllipsometricReading pd ->
            Assert.True(abs (pd.psi.degrees - 45.0) < 1.0e-9, $"Ψ = %g{pd.psi.degrees}")
            Assert.True(abs pd.delta.degrees < 1.0e-9, $"Δ = %g{pd.delta.degrees}")
        | IntensityReading _ -> Assert.Fail "an ellipsometer reads Ψ / Δ"

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
