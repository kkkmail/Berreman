namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Propagation
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

/// The Main scene's scan and bookkeeping messages over a kit opened read-write: Add variable moves the edited axis into
/// the draft's current block, Add block closes that block on the capture the detectors allow, neither writes the kit, a
/// commit records the draft's whole scan as one experiment, and a recorded experiment's role, session and notes are
/// written to its file (spec 0050 S9.1; 001 §E, §K, §P).
module ExperimentModelTests =

    let private kitValue (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private openedValue (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// The 1 mm glass plate at its first version on the shipped stack-sample item model.
    let private qzDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "29200000-0000-4000-8000-000000000001")
            label = ItemLabel "QZ"
            model =
                StackSampleChoice (
                    BuiltinCatalogue.itemModel ItemModels.ItemModelIds.stackSampleItemModelId,
                    Library.SampleVersionId.firstOf Library.SampleIds.glassPlate1mmId,
                    BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId
                )
            dial = FixedMounting
        }

    /// The scan's table: the seeded source and detector, an added linear polarizer and an added sample holding QZ, which
    /// the light meets in that order.
    type private ScanTable =
        {
            table : TableAndElementRotationModel
            polarizer : Library.ElementId
            sample : Library.ElementId
        }

    /// Runs over the Main scene loaded with a fresh kit opened read-write in a GUID-named folder under the class's test
    /// directory, as `SetupModelTests` opens its kit, its table built as step 263's facts add and bind elements; the kits
    /// are closed and the folder deleted at the end.
    let private withScanKit (run : KitFolder -> ScanTable -> unit) : unit =
        let root = FolderName (Path.Combine(AppContext.BaseDirectory, "ExperimentModelTests", Guid.NewGuid().ToString("N")))
        let folder = KitFolder root
        let imports = CatalogueImportContext.create (BuiltinCatalogue.freshStores ()).proxy
        let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system (KitAuthor "test") imports |> openedValue
        let scene = initMain () |> kitValue
        try
            let stocked = scene.loadKit opened |> updateTableAndElementRotation (AddItem qzDraft)
            Assert.Equal<ControlCaption option>(None, stocked.kitStatus)
            let slotOfKind (model : TableAndElementRotationModel) (kind : CatalogueKind) : Library.ElementId =
                (model.elements |> List.find (fun slot -> slot.placement.catalogueKind = kind)).id
            let withPolarizer = stocked |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let withSample = withPolarizer |> updateTableAndElementRotation (AddElement Sample)
            let sample = slotOfKind withSample Sample
            let table = withSample |> updateTableAndElementRotation (AssignSlot (sample, Some qzDraft.id))
            Assert.Equal<ControlCaption option>(None, table.kitStatus)
            Assert.Equal<CatalogueKind list>(
                [ LightSource; LinearPolarizer; Sample; Detector ],
                table.elements |> List.map (fun slot -> slot.placement.catalogueKind) |> List.sort)
            run folder
                {
                    table = table
                    polarizer = slotOfKind table LinearPolarizer
                    sample = sample
                }
        finally
            scene.kit.close () |> kitValue
            opened.library.kit.close () |> kitValue
            if Directory.Exists root.value then Directory.Delete(root.value, true)

    /// Every file under the kit folder by its path relative to the folder, with its bytes.
    let private kitFileBytes (folder : KitFolder) : Map<string, FileByteContent> =
        Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun path -> Path.GetRelativePath(folder.value.value, path), FileByteContent (File.ReadAllBytes path))
        |> Map.ofArray

    /// The kit folder's experiment files, by name in ordinal order.
    let private experimentFilesIn (folder : KitFolder) : string list =
        Directory.GetFiles(folder.value.value, "*.experiment-json")
        |> Array.map Path.GetFileName
        |> Array.sort
        |> List.ofArray

    /// The element chosen in the draft, varying `variable` over `range`, through the bay's messages.
    let private varying
        (element : Library.ElementId)
        (variable : ExperimentControls.VariableChoice)
        (range : Experiments.VariableRange)
        (model : TableAndElementRotationModel)
        : TableAndElementRotationModel =
        model
        |> updateTableAndElementRotation (ExpChooseElement element)
        |> updateTableAndElementRotation (ExpChooseVariable variable)
        |> updateTableAndElementRotation (ExpSetRangeMin (ExperimentControls.ExperimentRangeEnd range.min.value))
        |> updateTableAndElementRotation (ExpSetRangeMax (ExperimentControls.ExperimentRangeEnd range.max.value))
        |> updateTableAndElementRotation (ExpSetRangePoints (ExperimentControls.ExperimentPointCount range.points.value))

    /// The draft's edited axis, failing when the draft has none.
    let private editedAxis (model : TableAndElementRotationModel) : Experiments.ScanAxis =
        model.experimentCollection.draft.axisOpt |> Option.defaultWith (fun () -> failwith "The draft must have an edited axis.")

    /// Runs over the scan kit with `E1`, the sample's incidence over 20…60° in 3 points, committed through the bay, and
    /// that experiment as the kit holds it (spec 0050 S9.1).
    let private withCommittedE1 (run : KitFolder -> TableAndElementRotationModel -> Experiments.Experiment -> unit) : unit =
        withScanKit (fun folder scan ->
            let incidenceRange : Experiments.VariableRange =
                {
                    min = SweptValue 20.0
                    max = SweptValue 60.0
                    points = CurvePointCount 3
                }
            let committed =
                scan.table
                |> varying scan.sample ExperimentControls.VaryR2 incidenceRange
                |> updateTableAndElementRotation ExpCommit
            Assert.Equal<ControlCaption option>(None, committed.kitStatus)
            let e1 : Experiments.Experiment = Assert.Single(committed.kit.proxy.readExperiments () |> kitValue)
            Assert.Equal<Experiments.ExperimentId>(Experiments.ExperimentId 1, e1.id)
            Assert.Equal<Experiments.Experiment list>([ e1 ], committed.experimentCollection.experiments)
            run folder committed e1)

    /// The kit file of the experiment, by its path relative to the kit folder.
    let private experimentFileOf (experiment : Experiments.Experiment) : string = $"{experiment.name.value}.experiment-json"

    /// Every kit file but the experiment's, with its bytes.
    let private otherFileBytes (folder : KitFolder) (experiment : Experiments.Experiment) : Map<string, FileByteContent> =
        kitFileBytes folder |> Map.remove (experimentFileOf experiment)

    [<Fact>]
    let ``Add variable and Add block build the draft's scan, and a commit records it as one experiment`` () : unit =
        withScanKit (fun folder scan ->
            // Only the detector at the end of the beam captures, through the sample's T, whichever element varies.
            Assert.Equal<Experiments.MeasurementMode list>([ Experiments.CaptureTransmitted ], availableCaptures scan.table scan.sample)
            Assert.Equal<Experiments.MeasurementMode list>([ Experiments.CaptureTransmitted ], availableCaptures scan.table scan.polarizer)
            let before = kitFileBytes folder

            let incidenceRange : Experiments.VariableRange =
                {
                    min = SweptValue 20.0
                    max = SweptValue 60.0
                    points = CurvePointCount 3
                }
            let incidenceDraft = scan.table |> varying scan.sample ExperimentControls.VaryR2 incidenceRange
            let incidence = editedAxis incidenceDraft
            let incidenceExpected : Experiments.ScanAxis =
                {
                    elementId = scan.sample
                    label = incidence.label
                    variable = Experiments.VaryR2
                    range = incidenceRange
                }
            Assert.Equal<Experiments.ScanAxis>(incidenceExpected, incidence)
            Assert.NotEqual<Experiments.ElementLabel>(Experiments.ExperimentDraft.empty.elementLabel, incidence.label)

            let outer = incidenceDraft |> updateTableAndElementRotation ExpAddScanAxis
            Assert.Equal<Experiments.ScanAxis list>([ incidence ], outer.experimentCollection.draft.outerAxes)
            Assert.Equal<Experiments.ScanAxis option>(None, outer.experimentCollection.draft.axisOpt)
            Assert.Empty(outer.experimentCollection.draft.closedBlocks)

            let azimuthRange : Experiments.VariableRange =
                {
                    min = SweptValue 0.0
                    max = SweptValue 90.0
                    points = CurvePointCount 2
                }
            let azimuthDraft = outer |> varying scan.sample ExperimentControls.VaryR1 azimuthRange
            let azimuth : Experiments.ScanAxis =
                {
                    incidence with
                        variable = Experiments.VaryR1
                        range = azimuthRange
                }
            Assert.Equal<Experiments.ScanAxis>(azimuth, editedAxis azimuthDraft)

            // Both is asked for, and the block closes on T, the only capture the detectors allow.
            let closed =
                azimuthDraft
                |> updateTableAndElementRotation (ExpChooseMeasurement ExperimentControls.CaptureBoth)
                |> updateTableAndElementRotation ExpAddScanBlock
            let incidenceByAzimuth : Experiments.ScanBlock =
                {
                    axes = [ incidence; azimuth ]
                    capture = Experiments.CaptureTransmitted
                }
            Assert.Equal<Experiments.ScanBlock list>([ incidenceByAzimuth ], closed.experimentCollection.draft.closedBlocks)
            Assert.Empty(closed.experimentCollection.draft.outerAxes)
            Assert.Equal<Experiments.ScanAxis option>(None, closed.experimentCollection.draft.axisOpt)

            // Building the scan writes nothing.
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)
            Assert.Empty(closed.experimentCollection.experiments)

            let analyzerRange : Experiments.VariableRange =
                {
                    min = SweptValue 0.0
                    max = SweptValue 67.5
                    points = CurvePointCount 4
                }
            let analyzerDraft = closed |> varying scan.polarizer ExperimentControls.VaryR1 analyzerRange
            let analyzer = editedAxis analyzerDraft
            let analyzerExpected : Experiments.ScanAxis =
                {
                    elementId = scan.polarizer
                    label = analyzer.label
                    variable = Experiments.VaryR1
                    range = analyzerRange
                }
            Assert.Equal<Experiments.ScanAxis>(analyzerExpected, analyzer)
            Assert.NotEqual<Experiments.ElementLabel>(incidence.label, analyzer.label)

            let committed = analyzerDraft |> updateTableAndElementRotation ExpCommit
            Assert.Equal<ControlCaption option>(None, committed.kitStatus)
            let stored : Experiments.Experiment = Assert.Single(committed.kit.proxy.readExperiments () |> kitValue)
            Assert.Equal<string list>([ $"{stored.name.value}.experiment-json" ], experimentFilesIn folder)
            let analyzerBlock : Experiments.ScanBlock =
                {
                    axes = [ analyzer ]
                    capture = Experiments.CaptureTransmitted
                }
            let expectedScan : Experiments.ExperimentScan =
                {
                    blocks = [ incidenceByAzimuth; analyzerBlock ]
                    sourceWaveLengthOpt = None
                }
            Assert.Equal<Experiments.ExperimentScan>(expectedScan, stored.scan)
            Assert.Equal<Experiments.Experiment list>([ stored ], committed.experimentCollection.experiments)
            Assert.Equal<Experiments.ExperimentId option>(Some stored.id, committed.experimentCollection.draft.editingId))

    [<Fact>]
    let ``Add variable without a chosen element and variable changes nothing and writes nothing`` () : unit =
        withScanKit (fun folder scan ->
            let chosen =
                scan.table
                |> updateTableAndElementRotation (ExpChooseElement scan.sample)
                |> updateTableAndElementRotation (ExpChooseVariable ExperimentControls.VaryR2)
            match chosen.experimentCollection.draft.axisOpt with
            | Some _ -> ()
            | None -> Assert.Fail "the sample and its incidence must make an edited axis before ExpNew"
            let fresh = chosen |> updateTableAndElementRotation ExpNew
            Assert.Equal<Experiments.ExperimentDraft>(Experiments.ExperimentDraft.empty, fresh.experimentCollection.draft)
            let before = kitFileBytes folder

            Assert.Equal<TableAndElementRotationModel>(fresh, fresh |> updateTableAndElementRotation ExpAddScanAxis)
            Assert.Equal<TableAndElementRotationModel>(fresh, fresh |> updateTableAndElementRotation ExpAddScanBlock)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)
            Assert.Empty(experimentFilesIn folder))

    [<Fact>]
    let ``SetExperimentRole, SetExperimentSession and SetExperimentNotes change the stored experiment and its kit file`` () : unit =
        withCommittedE1 (fun folder committed e1 ->
            let fileBefore = (kitFileBytes folder).[experimentFileOf e1]
            let othersBefore = otherFileBytes folder e1
            let changed =
                committed
                |> updateTableAndElementRotation (SetExperimentRole (e1.id, Experiments.CalibrationRun))
                |> updateTableAndElementRotation (SetExperimentSession (e1.id, ControlCaption "Day 1"))
                |> updateTableAndElementRotation (SetExperimentNotes (e1.id, ControlCaption "LR re-seated before this run"))
            Assert.Equal<ControlCaption option>(None, changed.kitStatus)

            // Only the role, the session and the notes change: the scan, setup, name and data stay, the draft too.
            let expected : Experiments.Experiment =
                {
                    e1 with
                        role = Experiments.CalibrationRun
                        sessionOpt = Some (Experiments.SessionName "Day 1")
                        notes = Experiments.ExperimentNotes "LR re-seated before this run"
                }
            let held : Experiments.Experiment = Assert.Single(changed.experimentCollection.experiments)
            Assert.Equal<Experiments.ExperimentScan>(e1.scan, held.scan)
            Assert.Equal<Experiments.Experiment>(expected, held)
            Assert.Equal<Experiments.ExperimentCollection>(
                {
                    committed.experimentCollection with experiments = [ expected ]
                },
                changed.experimentCollection)

            // The experiment's kit file holds the three; no other file is written.
            Assert.Equal<string list>([ experimentFileOf e1 ], experimentFilesIn folder)
            Assert.NotEqual<FileByteContent>(fileBefore, (kitFileBytes folder).[experimentFileOf e1])
            Assert.Equal<Map<string, FileByteContent>>(othersBefore, otherFileBytes folder e1)
            let stored : Experiments.Experiment = Assert.Single(changed.kit.proxy.readExperiments () |> kitValue)
            Assert.Equal<Experiments.Experiment>(expected, stored))

    [<Fact>]
    let ``a blank session clears it, and a change to an experiment the kit lacks writes nothing`` () : unit =
        withCommittedE1 (fun folder committed e1 ->
            let inSession = committed |> updateTableAndElementRotation (SetExperimentSession (e1.id, ControlCaption "Day 1"))
            let sessioned : Experiments.Experiment = Assert.Single(inSession.experimentCollection.experiments)
            Assert.Equal<Experiments.SessionName option>(Some (Experiments.SessionName "Day 1"), sessioned.sessionOpt)
            let sessionedStored : Experiments.Experiment = Assert.Single(inSession.kit.proxy.readExperiments () |> kitValue)
            Assert.Equal<Experiments.SessionName option>(Some (Experiments.SessionName "Day 1"), sessionedStored.sessionOpt)
            let cleared = inSession |> updateTableAndElementRotation (SetExperimentSession (e1.id, ControlCaption "  "))
            Assert.Equal<ControlCaption option>(None, cleared.kitStatus)
            let clearedHeld : Experiments.Experiment = Assert.Single(cleared.experimentCollection.experiments)
            Assert.Equal<Experiments.SessionName option>(None, clearedHeld.sessionOpt)
            Assert.Equal<Experiments.Experiment>(e1, clearedHeld)
            let clearedStored : Experiments.Experiment = Assert.Single(cleared.kit.proxy.readExperiments () |> kitValue)
            Assert.Equal<Experiments.SessionName option>(None, clearedStored.sessionOpt)
            Assert.Equal<Experiments.Experiment>(e1, clearedStored)

            // The kit holds no experiment 9: the model stays equal and no file's bytes change.
            let before = kitFileBytes folder
            let absent = Experiments.ExperimentId 9
            Assert.DoesNotContain(absent, cleared.experimentCollection.experiments |> List.map (fun experiment -> experiment.id))
            let unchanged = cleared |> updateTableAndElementRotation (SetExperimentRole (absent, Experiments.DarkRun))
            Assert.Equal<TableAndElementRotationModel>(cleared, unchanged)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder))
