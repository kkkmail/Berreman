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

    /// The Data step's kit: the scan table with `E1` and `E2` committed, E1 holding the attached test-local intensity file,
    /// E2 no data, and that lab file outside the kit (spec 0050 S9.3).
    type private DataStepKit =
        {
            model : TableAndElementRotationModel
            e1 : Experiments.Experiment
            e2 : Experiments.Experiment
            labFile : FileName
        }

    /// Runs over the scan kit with `E1`, the polarizer's R1 over 0…90° in 3 points, committed and given an intensity file
    /// of three points written outside the kit, then `E2`, the same scan, committed with no data; the lab file's folder is
    /// deleted at the end (spec 0050 S9.3).
    let private withDataStepKit (run : KitFolder -> DataStepKit -> unit) : unit =
        withScanKit (fun folder scan ->
            let lab = Path.Combine(AppContext.BaseDirectory, "ExperimentModelTests", $"lab-{Guid.NewGuid():N}")
            try
                Directory.CreateDirectory lab |> ignore
                let labFile = FileName (Path.Combine(lab, "intensity.csv"))
                File.WriteAllText(labFile.value, "angle_deg,intensity\n0,1.0\n45,0.5\n90,0.0\n")
                let azimuthRange : Experiments.VariableRange =
                    {
                        min = SweptValue 0.0
                        max = SweptValue 90.0
                        points = CurvePointCount 3
                    }
                let committed =
                    scan.table
                    |> varying scan.polarizer ExperimentControls.VaryR1 azimuthRange
                    |> updateTableAndElementRotation ExpCommit
                    |> updateTableAndElementRotation ExpNew
                    |> varying scan.polarizer ExperimentControls.VaryR1 azimuthRange
                    |> updateTableAndElementRotation ExpCommit
                Assert.Equal<ControlCaption option>(None, committed.kitStatus)
                let attached =
                    committed
                    |> updateTableAndElementRotation (AttachDataFile (Experiments.ExperimentId 1, labFile, ImportProfileName.intensitySeries))
                Assert.Equal<ControlCaption option>(None, attached.kitStatus)
                match attached.experimentCollection.experiments with
                | [ e1; e2 ] ->
                    Assert.Equal<Experiments.ExperimentName list>(
                        [ Experiments.ExperimentName "E1"; Experiments.ExperimentName "E2" ],
                        [ e1.name; e2.name ])
                    Assert.Equal<Experiments.ExperimentId list>([ Experiments.ExperimentId 1; Experiments.ExperimentId 2 ], [ e1.id; e2.id ])
                    match e1.dataOpt, e2.dataOpt with
                    | Some data, None -> Assert.Empty(data.exclusions)
                    | _ -> Assert.Fail "E1 must hold the attached file and E2 no data"
                    // Three points that validate, so the badge counts what an exclusion keeps.
                    Assert.Equal<ExperimentDataLoad.ExperimentDataBadge>(
                        ExperimentDataLoad.ValidatedDataBadge (MeasuredData.MeasuredPointCount 3),
                        attached.dataBadgeOf e1)
                    run folder
                        {
                            model = attached
                            e1 = e1
                            e2 = e2
                            labFile = labFile
                        }
                | experiments -> Assert.Fail $"the kit must hold E1 and E2, and it holds {experiments.Length} experiment(s)"
            finally
                if Directory.Exists lab then Directory.Delete(lab, true))

    /// Every kit file with its bytes, and the lab file outside the kit under its full name.
    let private everyFileBytes (folder : KitFolder) (kit : DataStepKit) : Map<string, FileByteContent> =
        kitFileBytes folder |> Map.add kit.labFile.value (FileByteContent (File.ReadAllBytes kit.labFile.value))

    /// The model's experiment of that id, failing when its collection lacks it.
    let private heldExperiment (model : TableAndElementRotationModel) (id : Experiments.ExperimentId) : Experiments.Experiment =
        model.experimentCollection.experiments |> List.find (fun experiment -> experiment.id = id)

    /// The kit's stored experiment of that id, read back through `readExperiments`.
    let private storedExperiment (model : TableAndElementRotationModel) (id : Experiments.ExperimentId) : Experiments.Experiment =
        model.kit.proxy.readExperiments () |> kitValue |> List.find (fun experiment -> experiment.id = id)

    /// The experiment's mapped attachment, failing when it has no data.
    let private dataOf (experiment : Experiments.Experiment) : MappedAttachment =
        experiment.dataOpt |> Option.defaultWith (fun () -> failwith $"{experiment.name.value} must hold data")

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

    [<Fact>]
    let ``reviewing an experiment's data reads every point, the excluded ones included, and writes nothing`` () : unit =
        withDataStepKit (fun folder kit ->
            let before = everyFileBytes folder kit
            let reviewed = kit.model |> updateTableAndElementRotation (ReviewExperimentData kit.e1.id)
            let threePointsOf (model : TableAndElementRotationModel) : unit =
                match model.reviewedDataOpt with
                | Some { experimentId = reviewedId; points = Ok (ImportProfiles.SeriesPoints series) } ->
                    Assert.Equal<Experiments.ExperimentId>(kit.e1.id, reviewedId)
                    Assert.Equal<MeasuredData.MeasuredPointCount>(MeasuredData.MeasuredPointCount 3, series.pointCount)
                | Some _
                | None -> Assert.Fail "the Data step must review E1's three series points"
            threePointsOf reviewed
            // Only the review changes, and no file's bytes change.
            Assert.Equal<TableAndElementRotationModel>(
                {
                    kit.model with reviewedDataOpt = reviewed.reviewedDataOpt
                },
                reviewed)
            Assert.Equal<Map<string, FileByteContent>>(before, everyFileBytes folder kit)

            // An excluded point stays in the review, read again from the unchanged file.
            let excluded =
                reviewed
                |> updateTableAndElementRotation (ExcludePoint (kit.e1.id, MappedPointNumber 2, ControlCaption "beam blocked"))
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Excluded point 2 of E1: beam blocked."), excluded.kitStatus)
            threePointsOf excluded
            Assert.Equal<ReviewedData option>(reviewed.reviewedDataOpt, excluded.reviewedDataOpt)

            // E2 has no data: its review says so. The kit lacks experiment 9, whose review changes nothing.
            let reviewedE2 = excluded |> updateTableAndElementRotation (ReviewExperimentData kit.e2.id)
            let noData : ReviewedData =
                {
                    experimentId = kit.e2.id
                    points = Error (MeasuredData.EmptyDataFileErr "E2 has no data file")
                }
            Assert.Equal<ReviewedData option>(Some noData, reviewedE2.reviewedDataOpt)
            let absent = Experiments.ExperimentId 9
            Assert.DoesNotContain(absent, excluded.experimentCollection.experiments |> List.map (fun experiment -> experiment.id))
            Assert.Equal<TableAndElementRotationModel>(excluded, excluded |> updateTableAndElementRotation (ReviewExperimentData absent)))

    [<Fact>]
    let ``ExcludePoint records a point's exclusion and its reason in the experiment file, and IncludePoint takes it back`` () : unit =
        withDataStepKit (fun folder kit ->
            let e1FileBefore = (kitFileBytes folder).[experimentFileOf kit.e1]
            let othersBefore = otherFileBytes folder kit.e1
            let labBefore = File.ReadAllBytes kit.labFile.value
            let attachedData = dataOf kit.e1

            // The typed reason is the Data step's draft and writes nothing.
            let typed = kit.model |> updateTableAndElementRotation (SetExclusionReason (ControlCaption "beam blocked"))
            Assert.Equal<DataStepDraft>(
                {
                    DataStepDraft.empty with exclusionReason = ControlCaption "beam blocked"
                },
                typed.dataDraft)
            Assert.Equal<FileByteContent>(e1FileBefore, (kitFileBytes folder).[experimentFileOf kit.e1])

            let excluded =
                typed
                |> updateTableAndElementRotation (ExcludePoint (kit.e1.id, MappedPointNumber 2, typed.dataDraft.exclusionReason))
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Excluded point 2 of E1: beam blocked."), excluded.kitStatus)
            let exclusion : PointExclusion =
                {
                    point = MappedPointNumber 2
                    reason = ExclusionReason "beam blocked"
                }
            // Only the exclusions change: the attachment, the profile and the row selection stay.
            let expected : Experiments.Experiment =
                {
                    kit.e1 with
                        dataOpt =
                            Some
                                {
                                    attachedData with exclusions = [ exclusion ]
                                }
                }
            let heldExcluded = heldExperiment excluded kit.e1.id
            Assert.Equal<Experiments.Experiment>(expected, heldExcluded)
            Assert.Equal<Experiments.Experiment>(expected, storedExperiment excluded kit.e1.id)
            Assert.Equal<Experiments.Experiment>(kit.e2, heldExperiment excluded kit.e2.id)
            let counts : ExperimentDataLoad.KeptPointsInfo =
                {
                    kept = MeasuredData.MeasuredPointCount 2
                    excluded = MeasuredData.MeasuredPointCount 1
                }
            Assert.Equal<ExperimentDataLoad.ExperimentDataBadge>(ExperimentDataLoad.ExcludedPointsBadge counts, excluded.dataBadgeOf heldExcluded)

            // Only the experiment's file is written: the attached file and the lab file keep their bytes.
            Assert.NotEqual<FileByteContent>(e1FileBefore, (kitFileBytes folder).[experimentFileOf kit.e1])
            Assert.Equal<Map<string, FileByteContent>>(othersBefore, otherFileBytes folder kit.e1)
            Assert.Equal<byte[]>(labBefore, File.ReadAllBytes kit.labFile.value)

            let included = excluded |> updateTableAndElementRotation (IncludePoint (kit.e1.id, MappedPointNumber 2))
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Included point 2 of E1 again."), included.kitStatus)
            let heldIncluded = heldExperiment included kit.e1.id
            Assert.Equal<Experiments.Experiment>(kit.e1, heldIncluded)
            Assert.Equal<Experiments.Experiment>(kit.e1, storedExperiment included kit.e1.id)
            Assert.Empty((dataOf (storedExperiment included kit.e1.id)).exclusions)
            Assert.Equal<ExperimentDataLoad.ExperimentDataBadge>(
                ExperimentDataLoad.ValidatedDataBadge (MeasuredData.MeasuredPointCount 3),
                included.dataBadgeOf heldIncluded)
            Assert.Equal<Map<string, FileByteContent>>(othersBefore, otherFileBytes folder kit.e1)
            Assert.Equal<byte[]>(labBefore, File.ReadAllBytes kit.labFile.value)

            // The point holds no exclusion now: a second IncludePoint changes nothing and writes nothing.
            let before = everyFileBytes folder kit
            Assert.Equal<TableAndElementRotationModel>(
                included,
                included |> updateTableAndElementRotation (IncludePoint (kit.e1.id, MappedPointNumber 2)))
            Assert.Equal<Map<string, FileByteContent>>(before, everyFileBytes folder kit))

    [<Fact>]
    let ``an exclusion without a reason, of a point the data lacks, or of an experiment without data writes nothing`` () : unit =
        withDataStepKit (fun folder kit ->
            let before = everyFileBytes folder kit
            let refusedWith (status : string) (message : TableAndElementRotationMsg) : unit =
                let answer = kit.model |> updateTableAndElementRotation message
                Assert.Equal<ControlCaption option>(Some (ControlCaption status), answer.kitStatus)
                Assert.Equal<TableAndElementRotationModel>(
                    {
                        kit.model with kitStatus = answer.kitStatus
                    },
                    answer)
                Assert.Equal<Map<string, FileByteContent>>(before, everyFileBytes folder kit)
            refusedWith "Not excluded — an exclusion needs a reason." (ExcludePoint (kit.e1.id, MappedPointNumber 2, ControlCaption "  "))
            refusedWith "Not excluded — E1 has 3 point(s)." (ExcludePoint (kit.e1.id, MappedPointNumber 4, ControlCaption "beam blocked"))
            refusedWith "Not excluded — E2 has no data file." (ExcludePoint (kit.e2.id, MappedPointNumber 1, ControlCaption "beam blocked"))

            // An experiment the kit lacks, and taking back what was never excluded, change nothing at all.
            let absent = Experiments.ExperimentId 9
            Assert.Equal<TableAndElementRotationModel>(
                kit.model,
                kit.model |> updateTableAndElementRotation (ExcludePoint (absent, MappedPointNumber 1, ControlCaption "beam blocked")))
            Assert.Equal<TableAndElementRotationModel>(kit.model, kit.model |> updateTableAndElementRotation (IncludePoint (absent, MappedPointNumber 1)))
            Assert.Equal<TableAndElementRotationModel>(kit.model, kit.model |> updateTableAndElementRotation (IncludePoint (kit.e1.id, MappedPointNumber 1)))
            Assert.Equal<TableAndElementRotationModel>(kit.model, kit.model |> updateTableAndElementRotation (IncludePoint (kit.e2.id, MappedPointNumber 1)))
            Assert.Equal<Map<string, FileByteContent>>(before, everyFileBytes folder kit))

    /// `Setup 1` at its first revision: the setup the fresh kit's first commit records the table as (spec 0050 S8.2).
    let private setup1Revision1 : SetupReference = { name = SetupName "Setup 1"; revision = SetupRevision.first }

    /// Runs over the Data step's kit, E1 and E2 over `Setup 1` revision 1, with the polarizer then turned to R1 30° and
    /// the table recorded by `CreateSetup` as `Bench B`, at its first revision and referenced by no experiment; the kit's
    /// model is the one after it (spec 0050 S8.2, S9.3).
    let private withCorrectionKit (run : KitFolder -> DataStepKit -> Setups.Setup -> unit) : unit =
        withDataStepKit (fun folder kit ->
            Assert.Equal<SetupReference list>([ setup1Revision1; setup1Revision1 ], [ kit.e1.setup; kit.e2.setup ])
            let polarizer = kit.model.elements |> List.findIndex (fun slot -> slot.placement.catalogueKind = LinearPolarizer)
            let created =
                {
                    kit.model with selection = ElementSelected (SceneElementIndex polarizer)
                }
                |> updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 30.0))
                |> updateTableAndElementRotation (SetSetupName (ControlCaption "Bench B"))
                |> updateTableAndElementRotation CreateSetup
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Created setup 'Bench B'."), created.kitStatus)
            match created.kitSetups with
            | [ benchB; setup1 ] ->
                Assert.Equal<SetupReference>({ name = SetupName "Bench B"; revision = SetupRevision.first }, benchB.reference)
                Assert.Equal<SetupReference>(setup1Revision1, setup1.reference)
                // The turned polarizer is what tells the two setups apart.
                Assert.NotEqual<Setups.SetupSlot list>(setup1.slots, benchB.slots)
                Assert.Equal<Experiments.Experiment list>([ kit.e1; kit.e2 ], created.experimentCollection.experiments)
                run folder
                    {
                        kit with model = created
                    }
                    benchB
            | setups -> Assert.Fail $"the kit must hold Bench B and Setup 1, and it holds {setups.Length} setup(s)")

    [<Fact>]
    let ``CorrectSetup re-points the experiment to a setup of the kit with its audit note, and the lab file and the experiment's name are untouched`` () : unit =
        withCorrectionKit (fun folder kit benchB ->
            let e1FileBefore = (kitFileBytes folder).[experimentFileOf kit.e1]
            let othersBefore = otherFileBytes folder kit.e1
            let labBefore = File.ReadAllBytes kit.labFile.value
            let attachedFile = Path.Combine(folder.value.value, (dataOf kit.e1).attachment.path.value)
            let attachedBefore = File.ReadAllBytes attachedFile
            Assert.Equal<Lifecycle.PinState>(Lifecycle.VersionUnpinned, benchB.pinFor kit.model.experimentCollection.experiments)

            // The typed note is the Data step's draft and writes nothing; E1 is the experiment the Data step reviews.
            let typed =
                kit.model
                |> updateTableAndElementRotation (ReviewExperimentData kit.e1.id)
                |> updateTableAndElementRotation (SetCorrectionNote (ControlCaption "LP1 was at 30"))
            Assert.Equal<DataStepDraft>(
                {
                    DataStepDraft.empty with correctionNote = ControlCaption "LP1 was at 30"
                },
                typed.dataDraft)
            Assert.Equal<FileByteContent>(e1FileBefore, (kitFileBytes folder).[experimentFileOf kit.e1])

            let corrected =
                typed
                |> updateTableAndElementRotation (CorrectSetup (Experiments.ExperimentId 1, benchB.reference, typed.dataDraft.correctionNote))
            Assert.Equal<ControlCaption option>(
                Some (ControlCaption "Corrected E1: 'Setup 1' revision 1 → 'Bench B' revision 1 (LP1 was at 30)."),
                corrected.kitStatus)
            let correction : Experiments.SetupCorrection =
                {
                    previous = setup1Revision1
                    corrected = benchB.reference
                    note = Experiments.CorrectionNote "LP1 was at 30"
                }
            // Only the setup and the corrections change: the name, scan, role and data stay, and E2 is untouched.
            let expected : Experiments.Experiment =
                {
                    kit.e1 with
                        setup = benchB.reference
                        corrections = [ correction ]
                }
            Assert.Equal<Experiments.Experiment>(expected, heldExperiment corrected kit.e1.id)
            Assert.Equal<Experiments.Experiment>(expected, storedExperiment corrected kit.e1.id)
            Assert.Equal<Experiments.ExperimentName>(Experiments.ExperimentName "E1", (storedExperiment corrected kit.e1.id).name)
            Assert.Equal<Experiments.Experiment>(kit.e2, heldExperiment corrected kit.e2.id)

            // E1 now references Bench B's revision, which pins it; Bench B's detector validates the same three points,
            // and the review, read again, is of the same file.
            Assert.Equal<Lifecycle.PinState>(Lifecycle.VersionPinned, benchB.pinFor corrected.experimentCollection.experiments)
            Assert.Equal<ExperimentDataLoad.ExperimentDataBadge>(
                ExperimentDataLoad.ValidatedDataBadge (MeasuredData.MeasuredPointCount 3),
                corrected.dataBadgeOf expected)
            Assert.Equal<ReviewedData option>(typed.reviewedDataOpt, corrected.reviewedDataOpt)

            // Only E1's file is rewritten, under its name: the attached file and the lab file keep their bytes.
            Assert.Equal<string list>([ "E1.experiment-json"; "E2.experiment-json" ], experimentFilesIn folder)
            Assert.NotEqual<FileByteContent>(e1FileBefore, (kitFileBytes folder).[experimentFileOf kit.e1])
            Assert.Equal<Map<string, FileByteContent>>(othersBefore, otherFileBytes folder kit.e1)
            Assert.Equal<byte[]>(attachedBefore, File.ReadAllBytes attachedFile)
            Assert.Equal<byte[]>(labBefore, File.ReadAllBytes kit.labFile.value))

    [<Fact>]
    let ``a correction without a note, to a setup the kit lacks, or to the setup it already references writes nothing`` () : unit =
        withCorrectionKit (fun folder kit benchB ->
            let before = everyFileBytes folder kit
            let refusedWith (status : string) (message : TableAndElementRotationMsg) : unit =
                let answer = kit.model |> updateTableAndElementRotation message
                Assert.Equal<ControlCaption option>(Some (ControlCaption status), answer.kitStatus)
                Assert.Equal<TableAndElementRotationModel>(
                    {
                        kit.model with kitStatus = answer.kitStatus
                    },
                    answer)
                Assert.Equal<Map<string, FileByteContent>>(before, everyFileBytes folder kit)
            let note = ControlCaption "LP1 was at 30"
            let gone : SetupReference = { name = SetupName "Gone"; revision = SetupRevision.first }
            // A revision Bench B never had.
            let benchBRevision2 : SetupReference = { benchB.reference with revision = SetupRevision 2 }
            refusedWith "Not corrected — a correction needs an audit note." (CorrectSetup (kit.e1.id, benchB.reference, ControlCaption "  "))
            refusedWith "Not corrected — the kit holds no setup 'Gone' revision 1." (CorrectSetup (kit.e1.id, gone, note))
            refusedWith "Not corrected — the kit holds no setup 'Bench B' revision 2." (CorrectSetup (kit.e1.id, benchBRevision2, note))
            refusedWith "Not corrected — E1 already references 'Setup 1' revision 1." (CorrectSetup (kit.e1.id, setup1Revision1, note))

            // An experiment the kit lacks changes nothing at all.
            let absent = Experiments.ExperimentId 9
            Assert.DoesNotContain(absent, kit.model.experimentCollection.experiments |> List.map (fun experiment -> experiment.id))
            Assert.Equal<TableAndElementRotationModel>(kit.model, kit.model |> updateTableAndElementRotation (CorrectSetup (absent, benchB.reference, note)))
            Assert.Equal<Map<string, FileByteContent>>(before, everyFileBytes folder kit))
