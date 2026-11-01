namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.ExperimentRun
open OpticalConstructor.Storage

/// Spec 0046 — what a detector reads along its light path (`ExperimentRun`): polarizers before the sample
/// prepare the input, polarizers after it analyze, the detector decides the quantity; the CSV layout of an
/// exported run; and a tunable source's validation.
module ExperimentRunTests =

    let private tol = 1.0e-12
    let private lp (deg : float) : RunStep = PolarizerStep (ComputedIdeal IdealLinear, Angle.degree deg)
    let private clear : RunStep = SampleStep Propagation.identityMueller

    let private intensityOf (steps : RunStep list) : float =
        match read Intensity steps with
        | IntensityReading i -> i
        | EllipsometricReading _ -> failwith "expected an intensity"

    [<Fact>]
    let ``the source's light is unpolarized; the first polarizer sets a unit-intensity polarized input`` () =
        let s0, s1, s2, s3 = Propagation.stokesComponents (detectorStokes [])
        Assert.True(abs (s0 - 1.0) < tol && abs s1 < tol && abs s2 < tol && abs s3 < tol)
        let p0, p1, _, _ = Propagation.stokesComponents (detectorStokes [ lp 0.0 ])
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
        let run (series : ExperimentExport.ExportedSeries list) : ExperimentExport.ExportedRun =
            { title = "t"; xLabel = "x"; yLabel = "y"; description = ""; series = series }
        let shared =
            run [ { name = "Intensity (T)"; points = [ 0.0, 1.0; 1.0, 0.5 ] }
                  { name = "Intensity (R)"; points = [ 0.0, 0.0; 1.0, 0.5 ] } ]
        let headers, rows = ExperimentExportFile.tableOf shared
        Assert.Equal<string list>([ "x"; "Intensity (T)"; "Intensity (R)" ], headers)
        Assert.Equal<float list list>([ [ 0.0; 1.0; 0.0 ]; [ 1.0; 0.5; 0.5 ] ], rows |> Array.map List.ofArray |> List.ofArray)
        let ragged = run [ { name = "A"; points = [ 0.0, 1.0 ] }; { name = "B"; points = [ 2.0, 3.0; 4.0, 5.0 ] } ]
        let _, longRows = ExperimentExportFile.tableOf ragged
        Assert.Equal(3, Array.length longRows)
        // The in-memory store refuses a run with no series.
        let store = ExperimentExport.createInMemory ()
        match store.proxy.exportRun (run []) with
        | Error (ExperimentExport.NothingToExport _) -> ()
        | other -> Assert.Fail($"expected NothingToExport, got %A{other}")
        Assert.Equal(Ok (), store.proxy.exportRun shared)
        Assert.Equal<ExperimentExport.ExportedRun list>([ shared ], store.exported ())

    [<Fact>]
    let ``a tunable source needs ordered positive limits and a setting inside them`` () =
        let tunable (lo : float) (hi : float) (at : float) : SourcePreset =
            {
                id = "src-t"
                name = "T"
                waveLength = WaveLength.nm (at * 1.0<nm>)
                tuning = TunableBetween (WaveLength.nm (lo * 1.0<nm>), WaveLength.nm (hi * 1.0<nm>))
                protection = UserManaged
            }
        Assert.Equal(Ok (), validateSource (tunable 400.0 800.0 600.0))
        for bad in [ tunable 800.0 400.0 600.0; tunable 400.0 800.0 900.0; tunable 0.0 800.0 600.0 ] do
            match validateSource bad with
            | Error (InvalidSource _) -> ()
            | other -> Assert.Fail($"expected InvalidSource, got %A{other}")
        Assert.Equal(Some (400.0, 800.0), (tunable 400.0 800.0 600.0).tunableRangeNm |> Option.map (fun (a, b) -> round a, round b))
        // The seeded tunable sources are valid presets of their own kind.
        Assert.All(SeedSources.tunables, fun s -> Assert.True(Option.isSome s.tunableRangeNm, s.name))
