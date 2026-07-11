namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments             // DataFilePath (step 025)
open OpticalConstructor.Domain.MeasuredData            // series / point / ExperimentDataError + parsers (step 034)
open OpticalConstructor.Domain.ExperimentData          // ExperimentDataProxy (the DECLARED load seam)

/// Spec 0038 step 035 (ADD_CONTRACT STORE_XDUO_0006): the measured-data LOAD seam — the DECLARED
/// `[<ReferenceEquality>] ExperimentDataProxy` (`tryLoadIntensity` / `tryLoadEllipsometric` :
/// `DataFilePath -> Result<series, ExperimentDataError>`). This file supplies the MOCK (canned series
/// keyed by path, built through the step-34 parsers so the fixtures are honest) and the mock-driven
/// test that exercises BOTH fields through their exact signatures — a hit returns the canned series,
/// an unknown path returns a typed error (never a throw), and the proxy compares by reference. The
/// real disk-backed store lands in a later IMPLEMENT_CONTRACT.
module ExperimentDataProxyTests =

    /// A canned intensity fixture, parsed from an in-memory `X,Y` string through the step-34 parser
    /// (labels row ignored) — no file IO, deterministic across runs. A `failwith` on the impossible
    /// `Error` keeps the fixture total without an option dance.
    let private intensityFixture : IntensitySeries =
        match parseIntensitySeries "wavelength_nm,intensity\n400.0,0.10\n500.0,0.42\n600.0,0.31" with
        | Ok s -> s
        | Error e -> failwith $"the canned intensity fixture unexpectedly failed to parse: %A{e}"

    /// A canned ellipsometric fixture, parsed from an in-memory `wavelength_nm,psi_deg,delta_deg`
    /// string through the step-34 parser (wavelength crosses the sole `Units` seam; Ψ/Δ are angles).
    let private ellipsometricFixture : EllipsometricSeries =
        match parseEllipsometricSeries "wavelength_nm,psi_deg,delta_deg\n400.0,12.0,30.0\n500.0,15.0,45.0" with
        | Ok s -> s
        | Error e -> failwith $"the canned ellipsometric fixture unexpectedly failed to parse: %A{e}"

    /// The MOCK: an inline `ExperimentDataProxy` whose canned series are keyed by the raw path
    /// (`DataFilePath.value`). A hit returns the canned series; a miss returns a typed
    /// `ExperimentDataError` (a mock has no such data file) — staying WITHIN the step-34 declared
    /// four-case channel (`MalformedDataFile`), never a throw and never a fifth case. A later slice
    /// substitutes the real disk-backed store for this and exercises the exact same logic.
    let private makeMock
        (intensityByPath : Map<string, IntensitySeries>)
        (ellipsometricByPath : Map<string, EllipsometricSeries>)
        : ExperimentDataProxy =
        {
            tryLoadIntensity =
                fun (path : DataFilePath) ->
                    match Map.tryFind path.value intensityByPath with
                    | Some s -> Ok s
                    | None -> Error (MalformedDataFile $"the mock has no canned intensity series for path '{path.value}'")
            tryLoadEllipsometric =
                fun (path : DataFilePath) ->
                    match Map.tryFind path.value ellipsometricByPath with
                    | Some s -> Ok s
                    | None -> Error (MalformedDataFile $"the mock has no canned ellipsometric series for path '{path.value}'")
        }

    /// A mock seeded with one intensity file and one ellipsometric file at distinct paths.
    let private seededMock () : ExperimentDataProxy =
        makeMock
            (Map.ofList [ "C:/data/run-intensity.csv", intensityFixture ])
            (Map.ofList [ "C:/data/run-ellipsometric.csv", ellipsometricFixture ])

    // ============================ the ExperimentDataProxy seam ============================

    [<Fact>]
    let ``a mock ExperimentDataProxy loads canned intensity and ellipsometric series through their exact signatures`` () =
        let proxy = seededMock ()

        // Pin the EXACT signatures the acceptance names by binding each field to an explicitly-typed
        // local: the compiler rejects the file if either field drifts from its declared shape.
        let loadIntensity : DataFilePath -> Result<IntensitySeries, ExperimentDataError> = proxy.tryLoadIntensity
        let loadEllipsometric : DataFilePath -> Result<EllipsometricSeries, ExperimentDataError> = proxy.tryLoadEllipsometric

        match loadIntensity (DataFilePath.create "C:/data/run-intensity.csv") with
        | Ok series -> Assert.Equal<IntensitySeries>(intensityFixture, series)
        | Error e -> Assert.Fail($"expected the canned intensity series, got %A{e}")

        match loadEllipsometric (DataFilePath.create "C:/data/run-ellipsometric.csv") with
        | Ok series -> Assert.Equal<EllipsometricSeries>(ellipsometricFixture, series)
        | Error e -> Assert.Fail($"expected the canned ellipsometric series, got %A{e}")

    [<Fact>]
    let ``an unknown path yields a typed ExperimentDataError from both fields, never a throw`` () =
        let proxy = seededMock ()
        let unknown = DataFilePath.create "C:/data/nowhere.csv"

        match proxy.tryLoadIntensity unknown with
        | Error (MalformedDataFile reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (MalformedDataFile _) for an unknown intensity path, got %A{other}")

        match proxy.tryLoadEllipsometric unknown with
        | Error (MalformedDataFile reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (MalformedDataFile _) for an unknown ellipsometric path, got %A{other}")

    [<Fact>]
    let ``an ExperimentDataProxy compares by reference (the Elmish-required equality)`` () =
        // Function-valued fields have no structural equality; the [<ReferenceEquality>] proxy compares
        // by identity so a host model holding one stays comparable.
        let make () : ExperimentDataProxy = seededMock ()
        let p = make ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = make ()))
