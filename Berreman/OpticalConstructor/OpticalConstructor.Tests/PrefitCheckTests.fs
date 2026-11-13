namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Setups
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.ImportProfiles
open OpticalConstructor.Domain.ExperimentData
open OpticalConstructor.Domain.PrefitChecks

/// Spec 0050 S11.2 — an experiment's pre-fit checks, moved from the solver hand-off into Domain: every slot but a sample's
/// is specified, data is attached, and its kept points read and fit the scan.
module PrefitCheckTests =

    let private library : Library.LibraryProxy = BuiltinCatalogue.library ()

    let private emptySlot (id : Library.ElementId) (kind : CatalogueKind) (point : TablePoint) : SetupSlot =
        {
            id = id
            placement = ElementPlacement.create kind point
            zoom = BeamLayout.ElementZoom 5.0
            beam = BeamLayout.OnMainBeam
            content = EmptySlotContent
        }

    /// The source, bound to the HeNe 633 preset as the table binds it.
    let private src : SetupSlot =
        let slot = emptySlot (Library.ElementId "src") LightSource RayModel.defaultSourcePoint
        { slot with placement = { slot.placement with valueId = Some Library.PresetIds.heNe633 } }

    let private lp : SetupSlot = emptySlot (Library.ElementId "lp") LinearPolarizer { x = -0.3<meter>; y = 0.0<meter> }
    let private sample : SetupSlot = emptySlot (Library.ElementId "sample") Sample { x = 0.0<meter>; y = 0.0<meter> }
    let private emptyDet : SetupSlot = emptySlot (Library.ElementId "det") Detector RayModel.defaultDetectorPoint

    let private itemOf (id : ItemId) (label : ItemLabel) (model : ItemModelChoice) : Item =
        let draft : ItemDraft =
            {
                id = id
                label = label
                model = model
                dial = FixedMounting
            }

        draft.toItem

    let private linearPolarizerChoice : ItemModelChoice =
        match BuiltinCatalogue.preset Library.PresetIds.polarizerLp with
        | Library.PolarizerItem polarizer -> PolarizerPresetChoice (BuiltinCatalogue.itemModel ItemModelIds.idealPolarizerItemModelId, polarizer)
        | Library.SampleItem _ | Library.SourceItem _ | Library.DetectorItem _ -> failwith "The Built-in LP preset is not a polarizer."

    let private intensityDetectorChoice : ItemModelChoice = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.intensityDetectorModelId)

    let private lp1 : Item = itemOf (ItemId (Guid.Parse "5a6b7c8d-9e0f-4a1b-8c2d-3e4f5a6b7c01")) (ItemLabel "LP1") linearPolarizerChoice
    let private pd1 : Item = itemOf (ItemId (Guid.Parse "6b7c8d9e-0f1a-4b2c-9d3e-4f5a6b7c8d02")) (ItemLabel "PD1") intensityDetectorChoice

    let private assigned (item : Item) (model : ItemModelChoice) (slot : SetupSlot) : SetupSlot =
        match slot.withItem item model with
        | Ok held -> held
        | Error error -> failwith $"'{item.label.value}' did not fit '{slot.id.value}': {error.describe}"

    let private det : SetupSlot = assigned pd1 intensityDetectorChoice emptyDet
    let private heldLp : SetupSlot = assigned lp1 linearPolarizerChoice lp

    let private bench : SetupName = SetupName "Bench"

    let private benchOver (slots : SetupSlot list) : Setup list =
        match Setup.create bench slots [] with
        | Ok setup -> [ setup ]
        | Error error -> failwith $"'{bench.value}' was refused: {error.describe}"

    /// The source, the polarizer holding LP1, the empty sample and the detector holding PD1.
    let private readySetups : Setup list = benchOver [ src; heldLp; sample; det ]

    /// A run over `Bench` revision 1 turning the polarizer over 0…90° in 4 points, as `StudySessionTests`' run.
    let private benchRun : Experiment =
        {
            id = ExperimentId 1
            name = ExperimentName "E1"
            setup = { name = bench; revision = SetupRevision 1 }
            scan =
                ExperimentScan.oneAxis
                    {
                        elementId = lp.id
                        label = ElementLabel "Linear polarizer #1"
                        variable = VaryR1
                        range = { min = SweptValue 0.0; max = SweptValue 90.0; points = CurvePointCount 4 }
                    }
                    CaptureTransmitted
                    None
            role = ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataOpt = None
            corrections = []
        }

    let private goodFile : KitRelativePath = KitRelativePath "data/good.csv"
    let private malformedFile : KitRelativePath = KitRelativePath "data/malformed.csv"
    let private emptyFile : KitRelativePath = KitRelativePath "data/empty.csv"
    let private outsideFile : KitRelativePath = KitRelativePath "data/outside.csv"

    let private malformed : ExperimentDataError = MalformedDataFileErr "row 3 is not a valid X,Y pair"
    let private emptied : ExperimentDataError = EmptyDataFileErr "the file holds no data rows"

    /// The intensity series over these (R1, intensity) points, as the `Intensity series` profile reads it.
    let private seriesOf (points : (float * float) list) : MappedPoints =
        let series : IntensitySeries =
            { points = points |> List.map (fun (x, intensity) -> ({ x = MeasuredAbscissa x; intensity = MeasuredIntensity intensity } : IntensityPoint)) }

        SeriesPoints (IntensityData series)

    /// A proxy whose `readMapped` answers by the attachment's path; nothing else is read or attached.
    let private dataProxy : ExperimentDataProxy =
        let byPath : Map<KitRelativePath, Result<MappedPoints, ExperimentDataError>> =
            Map.ofList
                [
                    goodFile, Ok (seriesOf [ 0.0, 1.0; 30.0, 0.75; 60.0, 0.25; 90.0, 0.0 ])
                    malformedFile, Error malformed
                    emptyFile, Error emptied
                    outsideFile, Ok (seriesOf [ 0.0, 1.0; 120.0, 0.25 ])
                ]

        {
            readPickedFile = fun file -> Error (KitIoErr $"the test reads no picked file '{file.value}'")
            attachDataFile = fun file -> Error (KitIoErr $"the test attaches no file '{file.value}'")
            attachmentStatus = fun attachment -> Error (KitIoErr $"the test reads no status of '{attachment.path.value}'")
            readMapped =
                fun data ->
                    match byPath |> Map.tryFind data.attachment.path with
                    | Some read -> read
                    | None -> Error (MalformedDataFileErr $"the test holds no file '{data.attachment.path.value}'")
        }

    let private attachedAt (path : KitRelativePath) (exclusions : PointExclusion list) (experiment : Experiment) : Experiment =
        let data : MappedAttachment =
            {
                attachment = { path = path; contentHash = ContentHash "0f3c" }
                profile = ImportProfileName.intensitySeries
                rows = AllDataRows
                exclusions = exclusions
            }

        { experiment with dataOpt = Some data }

    let private checkedOver (setups : Setup list) (experiment : Experiment) : Result<MappedPoints, PrefitError> =
        experiment.prefitCheckWith library dataProxy setups

    [<Fact>]
    let ``a slot other than a sample's must hold an item or a binding, and a setup the kit lacks is named`` () : unit =
        let run = benchRun |> attachedAt goodFile []

        match checkedOver (benchOver [ src; lp; sample; det ]) run with
        | Error (UnspecifiedSlotsErr reason) ->
            Assert.Contains("Prep 1", reason)
            Assert.DoesNotContain("Obj 1", reason)
            Assert.DoesNotContain("Source", reason)
            Assert.DoesNotContain("Detector 1", reason)
        | other -> Assert.Fail($"expected the empty polarizer slot, got %A{other}")

        Assert.Equal(UnspecifiedSlot, lp.specification)
        Assert.Equal(SpecifiedSlot, src.specification)
        Assert.Equal(SpecifiedSlot, heldLp.specification)
        Assert.Equal(UnspecifiedSlot, sample.specification)

        // The sample is the unknown a fit finds, so the slot check passes and the data check is next.
        match checkedOver readySetups { run with dataOpt = None } with
        | Error (MissingDataErr _) -> ()
        | other -> Assert.Fail($"expected the slot check to pass with the sample empty, got %A{other}")

        let missing = { run with setup = { name = SetupName "Missing"; revision = SetupRevision 1 } }

        match checkedOver readySetups missing with
        | Error (UnspecifiedSlotsErr reason) -> Assert.Contains("'Missing' revision 1", reason)
        | other -> Assert.Fail($"expected the missing setup to be named, got %A{other}")

    [<Fact>]
    let ``an experiment without data fails the data check`` () : unit =
        match checkedOver readySetups benchRun with
        | Error (MissingDataErr reason) -> Assert.Contains("no measured-data file", reason)
        | other -> Assert.Fail($"expected no data, got %A{other}")

    [<Fact>]
    let ``a data file that does not read is unreadable, and points outside the scan are inconsistent`` () : unit =
        for path, error in [ malformedFile, malformed; emptyFile, emptied ] do
            match checkedOver readySetups (benchRun |> attachedAt path []) with
            | Error (UnreadableDataErr carried) -> Assert.Equal(error, carried)
            | other -> Assert.Fail($"expected '{path.value}' to be unreadable, got %A{other}")

        match checkedOver readySetups (benchRun |> attachedAt outsideFile []) with
        | Error (InconsistentDataErr (DataRangeMismatchErr reason)) -> Assert.Contains("120", reason)
        | other -> Assert.Fail($"expected a range mismatch, got %A{other}")

    [<Fact>]
    let ``a ready experiment answers its kept, validated points`` () : unit =
        let draught : PointExclusion = { point = MappedPointNumber 2; reason = ExclusionReason "a draught moved the beam" }

        match checkedOver readySetups (benchRun |> attachedAt goodFile [ draught ]) with
        | Ok (SeriesPoints (IntensityData series)) ->
            Assert.Equal<float list>([ 0.0; 60.0; 90.0 ], series.points |> List.map (fun point -> point.x.value))
            Assert.Equal<float list>([ 1.0; 0.25; 0.0 ], series.points |> List.map (fun point -> point.intensity.value))
        | other -> Assert.Fail($"expected the three kept points, got %A{other}")

    [<Fact>]
    let ``every pre-fit error renders its condition and its payload`` () : unit =
        let rendered : (PrefitError * string) list =
            [
                UnspecifiedSlotsErr "still unbound: Prep 1", "a slot is not specified: still unbound: Prep 1"
                MissingDataErr "nothing attached to E1", "no data is attached: nothing attached to E1"
                UnreadableDataErr malformed, "the data file does not read: row 3 is not a valid X,Y pair"
                UnreadableDataErr emptied, "the data file does not read: the file holds no data rows"
                InconsistentDataErr (DataRangeMismatchErr "x = 120 lies outside [0, 90]"), "the data does not fit the scan: x = 120 lies outside [0, 90]"
                InconsistentDataErr (DataUnitsMismatchErr "an ellipsometric file"), "the data does not fit the scan: an ellipsometric file"
            ]

        for error, text in rendered do
            Assert.Equal(text, error.describe)
