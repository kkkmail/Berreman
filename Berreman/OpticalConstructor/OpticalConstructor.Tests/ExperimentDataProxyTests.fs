namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MeasuredData            // series / point / ExperimentDataError + parsers (step 034)
open OpticalConstructor.Domain.ExperimentData
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ExperimentDataLoad
open OpticalConstructor.Domain.ImportProfiles

/// Exact measured-data attachment signatures over a canned kit-path map (spec 0038 step 035; spec 0050 S4.2, S9.2).
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

    /// The bytes the mock answers for its one picked file, a byte-order mark and an X,Y line.
    let private pickedBytes : FileByteContent = FileByteContent [| 0xEFuy; 0xBBuy; 0xBFuy; byte 'X'; byte ','; byte 'Y'; byte '\n' |]

    let private pickedFile : FileName = FileName "C:/outside/run-intensity.csv"

    /// Answers `readMapped` by the attachment's path and `readPickedFile` by the picked file's name (spec 0050 S9.2).
    let private makeMock
        (pointsByPath : Map<KitRelativePath, MappedPoints>)
        (bytesByFile : Map<FileName, FileByteContent>)
        : ExperimentDataProxy =
        {
            readPickedFile =
                fun (file : FileName) ->
                    match Map.tryFind file bytesByFile with
                    | Some content -> Ok { file = file; content = content }
                    | None -> Error (KitIoErr $"the mock has no canned bytes for the picked file '{file.value}'")
            attachDataFile =
                fun file ->
                    Ok
                        {
                            path = KitRelativePath $"data/{System.IO.Path.GetFileName file.value}"
                            contentHash = attachmentHash
                        }
            attachmentStatus = fun _ -> Ok AttachedDataUnchanged
            readMapped =
                fun (data : MappedAttachment) ->
                    match Map.tryFind data.attachment.path pointsByPath with
                    | Some points -> Ok points
                    | None -> Error (MalformedDataFileErr $"the mock has no canned points for path '{data.attachment.path.value}'")
        }

    /// A mock seeded with one intensity file and one ellipsometric file at distinct paths, and one picked file.
    let private seededMock () : ExperimentDataProxy =
        makeMock
            (Map.ofList
                [
                    KitRelativePath "data/run-intensity.csv", SeriesPoints (IntensityData intensityFixture)
                    KitRelativePath "data/run-ellipsometric.csv", SeriesPoints (EllipsometricData ellipsometricFixture)
                ])
            (Map.ofList [ pickedFile, pickedBytes ])

    let private intensityAttachment : DataAttachment =
        {
            path = KitRelativePath "data/run-intensity.csv"
            contentHash = attachmentHash
        }

    /// The canned intensity file read as an intensity series over every row (spec 0050 S9.1).
    let private intensityData : MappedAttachment =
        {
            attachment = intensityAttachment
            profile = ImportProfileName.intensitySeries
            rows = AllDataRows
            exclusions = []
        }

    /// The canned ellipsometric file read as an ellipsometric series over every row (spec 0050 S9.1).
    let private ellipsometricData : MappedAttachment =
        {
            attachment = { intensityAttachment with path = KitRelativePath "data/run-ellipsometric.csv" }
            profile = ImportProfileName.ellipsometricSeries
            rows = AllDataRows
            exclusions = []
        }

    // ============================ the ExperimentDataProxy seam ============================

    [<Fact>]
    let ``a mock ExperimentDataProxy reads canned mapped points and picked files through its exact signature`` () =
        let proxy = seededMock ()

        // Pin the EXACT signatures the acceptance names by binding each field to an explicitly-typed
        // local: the compiler rejects the file if any field drifts from its declared shape.
        let readPicked : FileName -> Result<LabFileSource, KitError> = proxy.readPickedFile
        let attach : FileName -> Result<DataAttachment, KitError> = proxy.attachDataFile
        let status : DataAttachment -> Result<DataAttachmentStatus, KitError> = proxy.attachmentStatus
        let read : MappedAttachment -> Result<MappedPoints, ExperimentDataError> = proxy.readMapped

        Assert.Equal<Result<DataAttachment, KitError>>(Ok intensityAttachment, attach pickedFile)
        Assert.Equal<Result<DataAttachmentStatus, KitError>>(Ok AttachedDataUnchanged, status intensityAttachment)

        match readPicked pickedFile with
        | Ok source ->
            Assert.Equal(pickedFile, source.file)
            Assert.Equal<byte>(pickedBytes.value, source.content.value)
        | Error e -> Assert.Fail($"expected the canned picked file, got {e.describe}")

        match read intensityData with
        | Ok (SeriesPoints (IntensityData series)) -> Assert.Equal<IntensitySeries>(intensityFixture, series)
        | other -> Assert.Fail($"expected the canned intensity series, got %A{other}")

        match read ellipsometricData with
        | Ok (SeriesPoints (EllipsometricData series)) -> Assert.Equal<EllipsometricSeries>(ellipsometricFixture, series)
        | other -> Assert.Fail($"expected the canned ellipsometric series, got %A{other}")

    [<Fact>]
    let ``an unknown path yields a typed ExperimentDataError, never a throw`` () =
        let proxy = seededMock ()
        let unknown : DataAttachment =
            {
                path = KitRelativePath "data/nowhere.csv"
                contentHash = attachmentHash
            }

        match proxy.readMapped { intensityData with attachment = unknown } with
        | Error (MalformedDataFileErr reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (MalformedDataFileErr _) for an unknown intensity path, got %A{other}")

        match proxy.readMapped { ellipsometricData with attachment = unknown } with
        | Error (MalformedDataFileErr reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (MalformedDataFileErr _) for an unknown ellipsometric path, got %A{other}")

        match proxy.readPickedFile (FileName "C:/outside/nowhere.csv") with
        | Error (KitIoErr reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (KitIoErr _) for an unknown picked file, got %A{other}")

    /// An intensity detector bound to the Built-in intensity preset: the slots a loaded experiment resolves to.
    let private intensityDetectorSlots : Setups.SetupSlot list =
        let detectorSlot : Setups.SetupSlot =
            {
                id = Library.ElementId "det"
                placement =
                    {
                        Placement.ElementPlacement.create Placement.Detector Placement.TablePoint.origin with
                            valueId = Some Library.PresetIds.detectorIntensity
                    }
                zoom = BeamLayout.ElementZoom 1.0
                beam = BeamLayout.OnMainBeam
                content = Setups.EmptySlotContent
            }
        [ detectorSlot ]

    let private sweepOver (lo : float) (hi : float) : ScanAxis =
        {
            elementId = Library.ElementId "src"
            label = ElementLabel "Light source #1"
            variable = VaryWaveLength
            range =
                {
                    min = Propagation.SweptValue lo
                    max = Propagation.SweptValue hi
                    points = Propagation.CurvePointCount 5
                }
        }

    /// Step 288's one-axis experiment: 300…700 nm holds every abscissa of the canned series, 400, 500 and 600 nm.
    let private oneAxis : Experiment =
        {
            id = ExperimentId 1
            name = (ExperimentId 1).defaultName
            setup = { name = SetupName "Bench"; revision = SetupRevision 1 }
            scan = ExperimentScan.oneAxis (sweepOver 300.0 700.0) CaptureTransmitted None
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = Some intensityData
            corrections = []
        }

    [<Fact>]
    let ``loading data validates a one-axis scan as before and refuses an X,Y file for a multi-variable scan`` () : unit =
        let library = BuiltinCatalogue.library ()
        let slots = intensityDetectorSlots
        Assert.Equal<Library.DetectorKind>(Library.Intensity, ExperimentDataLoad.detectorKindOf library slots)

        let load (experiment : Experiment) : Result<MappedPoints, ExperimentDataError> =
            ExperimentDataLoad.loadAndValidate library (seededMock ()) slots experiment intensityData
        Assert.Equal<Result<MappedPoints, ExperimentDataError>>(Ok (SeriesPoints (IntensityData intensityFixture)), load oneAxis)

        // As before, the one axis's range is the check: 450…700 nm misses the 400 nm abscissa.
        match load { oneAxis with scan = ExperimentScan.oneAxis (sweepOver 450.0 700.0) CaptureTransmitted None } with
        | Error (DataRangeMismatchErr reason) -> Assert.Contains("400", reason)
        | other -> Assert.Fail($"expected DataRangeMismatchErr for 400 nm, got %A{other}")

        // A second axis makes the scan multi-variable, which an X,Y file cannot hold.
        let analyzerTurn : ScanAxis =
            {
                elementId = Library.ElementId "lp"
                label = ElementLabel "Linear polarizer #2"
                variable = VaryR1
                range = VariableRange.forVariable VaryR1
            }
        let twoAxes : Experiment =
            {
                oneAxis with
                    scan = { oneAxis.scan with blocks = [ { axes = [ sweepOver 300.0 700.0; analyzerTurn ]; capture = CaptureTransmitted } ] }
            }
        Assert.Equal<Result<MappedPoints, ExperimentDataError>>(
            Error (DataRangeMismatchErr "an X,Y data file holds one variable, and the scan varies 2 in 1 block(s)"), load twoAxes)

    [<Fact>]
    let ``loading data validates the points the attachment's profile maps`` () : unit =
        let library = BuiltinCatalogue.library ()
        let load (proxy : ExperimentDataProxy) (data : MappedAttachment) : Result<MappedPoints, ExperimentDataError> =
            ExperimentDataLoad.loadAndValidate library proxy intensityDetectorSlots { oneAxis with dataOpt = Some data } data

        Assert.Equal<Result<MappedPoints, ExperimentDataError>>(Ok (SeriesPoints (IntensityData intensityFixture)), load (seededMock ()) intensityData)

        // Spec 0050 S9.1, S9.2: the profile, not the detector, maps the file; an ellipsometric series under an intensity
        // detector is the validation's units mismatch.
        match load (seededMock ()) ellipsometricData with
        | Error (DataUnitsMismatchErr reason) -> Assert.Contains("ellipsometric", reason)
        | other -> Assert.Fail($"expected DataUnitsMismatchErr for an ellipsometric series, got %A{other}")

    [<Fact>]
    let ``the data badge follows the attachment, its file's hash and its validation`` () : unit =
        let validated : Result<MappedPoints, ExperimentDataError> = Ok (SeriesPoints (IntensityData intensityFixture))
        let changedHash = ContentHash "abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789"
        let rangeProblem = DataRangeMismatchErr "a measured wavelength 800 nm lies outside the experiment range [300, 700] nm"
        let unitsProblem = DataUnitsMismatchErr "the detector records intensity (S0), but the data file is an ellipsometric Ψ/Δ series"
        let parseProblem = MalformedDataFileErr "row 3 is not a valid comma-separated X,Y pair: '400.0;0.10'"
        let emptyProblem = EmptyDataFileErr "the intensity data file has a header row but no data rows"
        let statuses : DataAttachmentStatus option list =
            [ None; Some AttachedDataUnchanged; Some (AttachedDataChanged changedHash); Some AttachedDataMissing ]
        let outcomes : Result<MappedPoints, ExperimentDataError> option list =
            [ None; Some validated; Some (Error rangeProblem); Some (Error parseProblem) ]

        // No data is no data, whatever the file's status and the validation say.
        for statusOpt in statuses do
            for outcomeOpt in outcomes do
                Assert.Equal<ExperimentDataBadge>(NoDataBadge, ExperimentDataBadge.ofCheck None statusOpt outcomeOpt)

        // A changed file names its hash now and a missing one names none, ahead of any outcome.
        let mapped = Some intensityData
        for outcomeOpt in outcomes do
            Assert.Equal<ExperimentDataBadge>(
                DataChangedBadge (intensityAttachment, Some changedHash),
                ExperimentDataBadge.ofCheck mapped (Some (AttachedDataChanged changedHash)) outcomeOpt)
            Assert.Equal<ExperimentDataBadge>(
                DataChangedBadge (intensityAttachment, None), ExperimentDataBadge.ofCheck mapped (Some AttachedDataMissing) outcomeOpt)

        // Otherwise the validation decides, whether the file read unchanged or its status was not read.
        Assert.Equal<MeasuredPointCount>(MeasuredPointCount 3, (IntensityData intensityFixture).pointCount)
        for statusOpt in [ None; Some AttachedDataUnchanged ] do
            let badgeOf (outcomeOpt : Result<MappedPoints, ExperimentDataError> option) : ExperimentDataBadge =
                ExperimentDataBadge.ofCheck mapped statusOpt outcomeOpt
            Assert.Equal<ExperimentDataBadge>(ValidatedDataBadge (IntensityData intensityFixture).pointCount, badgeOf (Some validated))
            Assert.Equal<ExperimentDataBadge>(OutOfRangeBadge rangeProblem, badgeOf (Some (Error rangeProblem)))
            Assert.Equal<ExperimentDataBadge>(OutOfRangeBadge unitsProblem, badgeOf (Some (Error unitsProblem)))
            Assert.Equal<ExperimentDataBadge>(MappedDataBadge (Some parseProblem), badgeOf (Some (Error parseProblem)))
            Assert.Equal<ExperimentDataBadge>(MappedDataBadge (Some emptyProblem), badgeOf (Some (Error emptyProblem)))
            Assert.Equal<ExperimentDataBadge>(MappedDataBadge None, badgeOf None)

        Assert.Equal("no data", NoDataBadge.text)
        Assert.Equal("mapped", (MappedDataBadge None).text)
        Assert.Equal("mapped", (MappedDataBadge (Some parseProblem)).text)
        Assert.Equal("validated (3 points)", (ValidatedDataBadge (MeasuredPointCount 3)).text)
        Assert.Equal("data changed", (DataChangedBadge (intensityAttachment, Some changedHash)).text)
        Assert.Equal("data changed", (DataChangedBadge (intensityAttachment, None)).text)
        Assert.Equal("out of range", (OutOfRangeBadge unitsProblem).text)

    [<Fact>]
    let ``an ExperimentDataProxy compares by reference (the Elmish-required equality)`` () =
        // Function-valued fields have no structural equality; the [<ReferenceEquality>] proxy compares
        // by identity so a host model holding one stays comparable.
        let make () : ExperimentDataProxy = seededMock ()
        let p = make ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = make ()))
