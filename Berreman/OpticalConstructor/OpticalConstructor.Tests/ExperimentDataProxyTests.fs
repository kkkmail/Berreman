namespace OpticalConstructor.Tests

open Xunit
open OpticalConstructor.Domain
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MeasuredData            // series / point / ExperimentDataError + parsers (step 034)
open OpticalConstructor.Domain.ExperimentData
open OpticalConstructor.Domain.Experiments

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
        }

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
        }

    [<Fact>]
    let ``loading data validates a one-axis scan as before and refuses an X,Y file for a multi-variable scan`` () : unit =
        let library = BuiltinCatalogue.library ()
        let slots = intensityDetectorSlots
        Assert.Equal<Library.DetectorKind>(Library.Intensity, ExperimentDataLoad.detectorKindOf library slots)

        let load (experiment : Experiment) : Result<MeasuredSeries, ExperimentDataError> =
            ExperimentDataLoad.loadAndValidate library (seededMock ()) slots experiment intensityData
        Assert.Equal<Result<MeasuredSeries, ExperimentDataError>>(Ok (IntensityData intensityFixture), load oneAxis)

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
        Assert.Equal<Result<MeasuredSeries, ExperimentDataError>>(
            Error (DataRangeMismatchErr "an X,Y data file holds one variable, and the scan varies 2 in 1 block(s)"), load twoAxes)

    [<Fact>]
    let ``loading data reads it through its import profile and refuses a profile or a row selection the X,Y files cannot serve`` () : unit =
        let library = BuiltinCatalogue.library ()
        let load (proxy : ExperimentDataProxy) (data : MappedAttachment) : Result<MeasuredSeries, ExperimentDataError> =
            ExperimentDataLoad.loadAndValidate library proxy intensityDetectorSlots { oneAxis with dataOpt = Some data } data

        Assert.Equal<Result<MeasuredSeries, ExperimentDataError>>(Ok (IntensityData intensityFixture), load (seededMock ()) intensityData)

        // Spec 0050 S9.1: the profile, not the detector, picks the reader; an ellipsometric series under an intensity
        // detector is the validation's units mismatch.
        let ellipsometricData : MappedAttachment =
            {
                attachment =
                    {
                        intensityAttachment with path = KitRelativePath "data/run-ellipsometric.csv"
                    }
                profile = ImportProfileName.ellipsometricSeries
                rows = AllDataRows
            }
        match load (seededMock ()) ellipsometricData with
        | Error (DataUnitsMismatchErr reason) -> Assert.Contains("ellipsometric", reason)
        | other -> Assert.Fail($"expected DataUnitsMismatchErr for an ellipsometric series, got %A{other}")

        // An unavailable profile and a key selection read nothing.
        let readsNothing : ExperimentDataProxy =
            {
                seededMock () with
                    tryLoadIntensity = fun _ -> failwith "a refused mapping must not load intensity data"
                    tryLoadEllipsometric = fun _ -> failwith "a refused mapping must not load ellipsometric data"
            }
        let phoneRoi =
            match ImportProfileName.tryCreate "Phone ROI CSV" with
            | Some profile -> profile
            | None -> failwith "a non-blank profile name must be kept"
        Assert.Equal<Result<MeasuredSeries, ExperimentDataError>>(
            Error (MalformedDataFileErr "the import profile 'Phone ROI CSV' is not available"),
            load readsNothing { intensityData with profile = phoneRoi })
        Assert.Equal<Result<MeasuredSeries, ExperimentDataError>>(
            Error (MalformedDataFileErr "data/run-intensity.csv: an X,Y data file has no key column to select 'CPL-(QZ#45)-CPL' by"),
            load readsNothing { intensityData with rows = KeyedDataRows (DataRowKey "CPL-(QZ#45)-CPL") })

    [<Fact>]
    let ``an ExperimentDataProxy compares by reference (the Elmish-required equality)`` () =
        // Function-valued fields have no structural equality; the [<ReferenceEquality>] proxy compares
        // by identity so a host model holding one stays comparable.
        let make () : ExperimentDataProxy = seededMock ()
        let p = make ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = make ()))
