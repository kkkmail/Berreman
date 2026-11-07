namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MeasuredData            // series / point / ExperimentDataError + parsers (step 034)
open OpticalConstructor.Domain.ExperimentData

/// Exact measured-data attachment signatures over a canned kit-path map (spec 0038 step 035; spec 0050 S4.2).
module ExperimentDataProxyTests =

    let private attachmentHash : ContentHash = ContentHash "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

    /// A canned intensity fixture, parsed from an in-memory `X,Y` string through the step-34 parser
    /// (labels row ignored) — no file IO, deterministic across runs. A `failwith` on the impossible
    /// `Error` keeps the fixture total without an option dance.
    let private intensityFixture : IntensitySeries =
        match (DataFileText "wavelength_nm,intensity\n400.0,0.10\n500.0,0.42\n600.0,0.31").parseIntensitySeries with
        | Ok s -> s
        | Error e -> failwith $"the canned intensity fixture unexpectedly failed to parse: %A{e}"

    /// A canned ellipsometric fixture, parsed from an in-memory `wavelength_nm,psi_deg,delta_deg`
    /// string through the step-34 parser (wavelength crosses the sole `Units` seam; Ψ/Δ are angles).
    let private ellipsometricFixture : EllipsometricSeries =
        match (DataFileText "wavelength_nm,psi_deg,delta_deg\n400.0,12.0,30.0\n500.0,15.0,45.0").parseEllipsometricSeries with
        | Ok s -> s
        | Error e -> failwith $"the canned ellipsometric fixture unexpectedly failed to parse: %A{e}"

    let private makeMock
        (intensityByPath : Map<KitRelativePath, IntensitySeries>)
        (ellipsometricByPath : Map<KitRelativePath, EllipsometricSeries>)
        : ExperimentDataProxy =
        {
            attachDataFile =
                fun file ->
                    Ok
                        {
                            path = KitRelativePath $"data/{System.IO.Path.GetFileName file.value}"
                            contentHash = attachmentHash
                        }
            attachmentStatus = fun _ -> Ok AttachedDataUnchanged
            tryLoadIntensity =
                fun (attachment : DataAttachment) ->
                    match Map.tryFind attachment.path intensityByPath with
                    | Some s -> Ok s
                    | None -> Error (MalformedDataFileErr $"the mock has no canned intensity series for path '{attachment.path.value}'")
            tryLoadEllipsometric =
                fun (attachment : DataAttachment) ->
                    match Map.tryFind attachment.path ellipsometricByPath with
                    | Some s -> Ok s
                    | None -> Error (MalformedDataFileErr $"the mock has no canned ellipsometric series for path '{attachment.path.value}'")
        }

    /// A mock seeded with one intensity file and one ellipsometric file at distinct paths.
    let private seededMock () : ExperimentDataProxy =
        makeMock
            (Map.ofList [ KitRelativePath "data/run-intensity.csv", intensityFixture ])
            (Map.ofList [ KitRelativePath "data/run-ellipsometric.csv", ellipsometricFixture ])

    // ============================ the ExperimentDataProxy seam ============================

    [<Fact>]
    let ``a mock ExperimentDataProxy loads canned intensity and ellipsometric series through their exact signatures`` () =
        let proxy = seededMock ()

        // Pin the EXACT signatures the acceptance names by binding each field to an explicitly-typed
        // local: the compiler rejects the file if either field drifts from its declared shape.
        let loadIntensity : DataAttachment -> Result<IntensitySeries, ExperimentDataError> = proxy.tryLoadIntensity
        let loadEllipsometric : DataAttachment -> Result<EllipsometricSeries, ExperimentDataError> = proxy.tryLoadEllipsometric

        let intensityAttachment : DataAttachment =
            {
                path = KitRelativePath "data/run-intensity.csv"
                contentHash = attachmentHash
            }
        let ellipsometricAttachment : DataAttachment =
            {
                path = KitRelativePath "data/run-ellipsometric.csv"
                contentHash = attachmentHash
            }
        let attach : FileName -> Result<DataAttachment, KitError> = proxy.attachDataFile
        let status : DataAttachment -> Result<DataAttachmentStatus, KitError> = proxy.attachmentStatus
        let picked = FileName "C:/outside/run-intensity.csv"
        Assert.Equal<Result<DataAttachment, KitError>>(Ok intensityAttachment, attach picked)
        Assert.Equal<Result<DataAttachmentStatus, KitError>>(Ok AttachedDataUnchanged, status intensityAttachment)

        match loadIntensity intensityAttachment with
        | Ok series -> Assert.Equal<IntensitySeries>(intensityFixture, series)
        | Error e -> Assert.Fail($"expected the canned intensity series, got %A{e}")

        match loadEllipsometric ellipsometricAttachment with
        | Ok series -> Assert.Equal<EllipsometricSeries>(ellipsometricFixture, series)
        | Error e -> Assert.Fail($"expected the canned ellipsometric series, got %A{e}")

    [<Fact>]
    let ``an unknown path yields a typed ExperimentDataError from both fields, never a throw`` () =
        let proxy = seededMock ()
        let unknown : DataAttachment =
            {
                path = KitRelativePath "data/nowhere.csv"
                contentHash = attachmentHash
            }

        match proxy.tryLoadIntensity unknown with
        | Error (MalformedDataFileErr reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (MalformedDataFileErr _) for an unknown intensity path, got %A{other}")

        match proxy.tryLoadEllipsometric unknown with
        | Error (MalformedDataFileErr reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (MalformedDataFileErr _) for an unknown ellipsometric path, got %A{other}")

    [<Fact>]
    let ``an ExperimentDataProxy compares by reference (the Elmish-required equality)`` () =
        // Function-valued fields have no structural equality; the [<ReferenceEquality>] proxy compares
        // by identity so a host model holding one stays comparable.
        let make () : ExperimentDataProxy = seededMock ()
        let p = make ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = make ()))
