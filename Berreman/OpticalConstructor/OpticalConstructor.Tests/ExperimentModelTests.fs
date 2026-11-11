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

/// The Main scene's scan messages over a kit opened read-write: Add variable moves the edited axis into the draft's
/// current block, Add block closes that block on the capture the detectors allow, neither writes the kit, and a commit
/// records the draft's whole scan as one experiment (spec 0050 S9.1; 001 §K, §P).
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
