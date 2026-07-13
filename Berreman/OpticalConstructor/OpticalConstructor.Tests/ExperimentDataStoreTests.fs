namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open OpticalConstructor.Domain.Experiments             // DataFilePath (step 025)
open OpticalConstructor.Domain.MeasuredData            // series / ExperimentDataError + the pure parsers (step 034)
open OpticalConstructor.Storage                         // ExperimentDataStore.createFileBacked (step 036 real store)

/// Spec 0038 step 036 (IMPLEMENT_CONTRACT STORE_XDUO_0006): the REAL file-backed `ExperimentDataProxy`.
/// `ExperimentDataStore.createFileBacked` is the one real-IO adapter this spec permits (spec-md §0.3c) —
/// it reads a data file's text at the boundary and delegates to the step-34 pure `MeasuredData` parsers.
/// These tests exercise the actual disk read: a temp file written under the test output round-trips a
/// known intensity series and a known ellipsometric series (the store's parsed result equals the step-34
/// parser's own output); a missing path yields a typed `ExperimentDataError` from BOTH fields (never a
/// throw); and an EMPTY but existing file yields the parser's `EmptyDataFile` — distinct from the
/// IO-failure case — proving the adapter reads then delegates and adds no parsing logic of its own.
/// Parser semantics themselves stay covered by the step-34 in-memory string tests (`MeasuredDataTests`).
module ExperimentDataStoreTests =

    /// A private directory UNDER the test output (`AppContext.BaseDirectory` = the bin\...\net10.0 tree),
    /// created once — the real store reads real files, so the round-trip needs real files on disk. This is
    /// ephemeral test scratch, not a durable arc artifact.
    let private testDir =
        let d = Path.Combine(AppContext.BaseDirectory, "ExperimentDataStoreTests")
        Directory.CreateDirectory d |> ignore
        d

    /// Write `text` to a named file under the test output and return its elevated `DataFilePath`.
    let private writeTemp (name : string) (text : string) : DataFilePath =
        let path = Path.Combine(testDir, name)
        File.WriteAllText(path, text)
        DataFilePath.create path

    // ============================ the real file-backed ExperimentDataProxy ============================

    [<Fact>]
    let ``createFileBacked round-trips a known intensity file through the step-34 parser`` () =
        let proxy = ExperimentDataStore.createFileBacked ()
        let text = "wavelength_nm,intensity\n400.0,0.10\n500.0,0.42\n600.0,0.31"
        let path = writeTemp "intensity-roundtrip.csv" text

        // The store must reproduce EXACTLY what the pure step-34 parser yields from the same text — no
        // parsing logic of its own.
        let expected =
            match parseIntensitySeries text with
            | Ok s -> s
            | Error e -> failwith $"the intensity fixture unexpectedly failed to parse: %A{e}"

        match proxy.tryLoadIntensity path with
        | Ok series -> Assert.Equal<IntensitySeries>(expected, series)
        | Error e -> Assert.Fail($"expected the parsed intensity series from the real store, got %A{e}")

    [<Fact>]
    let ``createFileBacked round-trips a known ellipsometric file through the step-34 parser`` () =
        let proxy = ExperimentDataStore.createFileBacked ()
        let text = "wavelength_nm,psi_deg,delta_deg\n400.0,12.0,30.0\n500.0,15.0,45.0"
        let path = writeTemp "ellipsometric-roundtrip.csv" text

        let expected =
            match parseEllipsometricSeries text with
            | Ok s -> s
            | Error e -> failwith $"the ellipsometric fixture unexpectedly failed to parse: %A{e}"

        match proxy.tryLoadEllipsometric path with
        | Ok series -> Assert.Equal<EllipsometricSeries>(expected, series)
        | Error e -> Assert.Fail($"expected the parsed ellipsometric series from the real store, got %A{e}")

    [<Fact>]
    let ``createFileBacked maps a missing file to a typed error from both fields, never a throw`` () =
        let proxy = ExperimentDataStore.createFileBacked ()
        let missing = DataFilePath.create (Path.Combine(testDir, "does-not-exist.csv"))

        match proxy.tryLoadIntensity missing with
        | Error (MalformedDataFile reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (MalformedDataFile _) for a missing intensity path, got %A{other}")

        match proxy.tryLoadEllipsometric missing with
        | Error (MalformedDataFile reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (MalformedDataFile _) for a missing ellipsometric path, got %A{other}")

    [<Fact>]
    let ``createFileBacked delegates parse semantics: an existing but empty file is the parser's EmptyDataFile`` () =
        // The file EXISTS but is empty — File.ReadAllText succeeds with "" (no IO exception), so the store
        // hands "" to the parser, whose EmptyDataFile is a DIFFERENT case than the IO-failure
        // MalformedDataFile. This proves the adapter reads then delegates, adding no parsing logic.
        let proxy = ExperimentDataStore.createFileBacked ()
        let path = writeTemp "empty-intensity.csv" ""

        match proxy.tryLoadIntensity path with
        | Error (EmptyDataFile reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected the parser's Error (EmptyDataFile _) for an empty file, got %A{other}")
