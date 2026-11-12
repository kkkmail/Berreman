namespace OpticalConstructor.Tests

open System
open System.IO
open System.Text
open Xunit
open Softellect.Sys.Primitives
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.DataSplits
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.MeasuredData
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

/// The constructor's split messages over a kit opened read-write: SplitByColumn reads a picked file through the chosen
/// profile and holds its groups matched, offered and unmatched, writing nothing; ToggleSplitOffer ticks an offered group
/// for creation; and CancelDataSplit forgets the split (spec 0050 S9.3; 001 §K, §P).
module DataSplitModelTests =

    let private kitValue (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private openedValue (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// The Built-in polarizer preset of that id, failing when the catalogue holds another kind under it.
    let private polarizerPresetOf (id : ElementValueId) : Library.PolarizerPreset =
        match BuiltinCatalogue.preset id with
        | Library.PolarizerItem polarizer -> polarizer
        | Library.SampleItem _
        | Library.SourceItem _
        | Library.DetectorItem _ -> failwith $"The Built-in preset '{id.value}' is not a polarizer."

    /// The ideal linear polarizer preset on the shipped ideal-polarizer item model (step 210).
    let private linearPolarizerChoice : ItemModelChoice =
        PolarizerPresetChoice (
            BuiltinCatalogue.itemModel ItemModels.ItemModelIds.idealPolarizerItemModelId,
            polarizerPresetOf Library.PresetIds.polarizerLp
        )

    /// A glass plate at its first version on the shipped stack-sample item model.
    let private plateChoice (sample : Library.SampleId) : ItemModelChoice =
        StackSampleChoice (
            BuiltinCatalogue.itemModel ItemModels.ItemModelIds.stackSampleItemModelId,
            Library.SampleVersionId.firstOf sample,
            BuiltinCatalogue.sample sample
        )

    let private lp1Draft : ItemDraft =
        {
            id = ItemId (Guid.Parse "31900000-0000-4000-8000-000000000001")
            label = ItemLabel "LP1"
            model = linearPolarizerChoice
            dial = FixedMounting
        }

    let private qzDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "31900000-0000-4000-8000-000000000002")
            label = ItemLabel "QZ"
            model = plateChoice Library.SampleIds.glassPlate1mmId
            dial = FixedMounting
        }

    let private qz2Draft : ItemDraft =
        {
            id = ItemId (Guid.Parse "31900000-0000-4000-8000-000000000003")
            label = ItemLabel "QZ2"
            model = plateChoice Library.SampleIds.glassPlate2mmId
            dial = FixedMounting
        }

    let private laserDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "31900000-0000-4000-8000-000000000004")
            label = ItemLabel "Laser"
            model =
                SourcePresetChoice (
                    BuiltinCatalogue.itemModel ItemModels.ItemModelIds.sourcePresetItemModelId,
                    BuiltinCatalogue.source Library.PresetIds.heNe633
                )
            dial = FixedMounting
        }

    /// The linear polarizer preset again, on a dial read with the negative sign.
    let private lpdDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "31900000-0000-4000-8000-000000000005")
            label = ItemLabel "LPd"
            model = linearPolarizerChoice
            dial = DialMounted ItemModels.DialNegative
        }

    /// The Phone ROI file's rows: two of the kit's experiment, one of a second run over its setup, one of a setup the kit
    /// lacks and one the template cannot read.
    let private phoneRoiText : string =
        String.Join(
            "",
            [
                "experiment,capture_index,captured_at,description,avg_total\n"
                "LP1-(QZ#45)-LPd,0,2026-07-15T12:00:00+00:00,0,869.5\n"
                "LP1-(QZ#45)-LPd,1,2026-07-15T12:01:00+00:00,45,912.25\n"
                "LP1-(QZ#45)-LPd-2,0,2026-07-15T12:02:00+00:00,0,870\n"
                "LP1-(QZ2#0)-LPd,0,2026-07-15T12:03:00+00:00,0,801.5\n"
                "Darkness,0,2026-07-15T12:04:00+00:00,0,12.5\n"
            ]
        )

    /// The split's kit: its table, the slot id of the table's sample, and the Phone ROI file written outside the kit.
    type private SplitKit =
        {
            committed : TableAndElementRotationModel
            sample : Library.ElementId
            phoneRoiFile : FileName
        }

    /// Runs over the Main scene loaded with a fresh kit opened read-write in a GUID-named folder under the class's test
    /// directory, as `SetupModelTests` opens its kit, stocked and laid out as step 272's template table and committed as
    /// `E1`, beside a Phone ROI file outside the kit; the kits are closed and the folder deleted at the end.
    let private withSplitKit (run : KitFolder -> SplitKit -> unit) : unit =
        let root = Path.Combine(AppContext.BaseDirectory, "DataSplitModelTests", Guid.NewGuid().ToString("N"))
        let folder = KitFolder (FolderName (Path.Combine(root, "kit")))
        let imports = CatalogueImportContext.create (BuiltinCatalogue.freshStores ()).proxy
        let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system (KitAuthor "test") imports |> openedValue
        let scene = initMain () |> kitValue
        try
            let loaded = scene.loadKit opened
            Assert.Equal<NamingTemplate>(NamingTemplate.standard, loaded.kit.kitFile.namingTemplate)
            let stocked =
                [ lp1Draft; qzDraft; qz2Draft; laserDraft; lpdDraft ]
                |> List.fold (fun current draft -> current |> updateTableAndElementRotation (AddItem draft)) loaded
                |> updateTableAndElementRotation (SetParameterState (lpdDraft.id, ParameterName.dialZero, KnownParameter (ItemModels.ParameterValue (Angle.degree 10.0<deg>).value)))
            Assert.Equal<ControlCaption option>(None, stocked.kitStatus)
            let slotOfKind (model : TableAndElementRotationModel) (kind : CatalogueKind) : Library.ElementId =
                (model.elements |> List.find (fun slot -> slot.placement.catalogueKind = kind)).id
            let withPolarizer = stocked |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let polarizer = slotOfKind withPolarizer LinearPolarizer
            // The add selects the sample, so the turn is its own.
            let withSample = withPolarizer |> updateTableAndElementRotation (AddElement Sample)
            let sample = slotOfKind withSample Sample
            let placed =
                withSample
                |> updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 45.0))
                |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let analyzer =
                placed.elements |> List.find (fun slot -> slot.placement.catalogueKind = LinearPolarizer && slot.id <> polarizer)
            let committed =
                placed
                |> updateTableAndElementRotation (AssignSlot (polarizer, Some lp1Draft.id))
                |> updateTableAndElementRotation (AssignSlot (sample, Some qzDraft.id))
                |> updateTableAndElementRotation (AssignSlot (analyzer.id, Some lpdDraft.id))
                |> updateTableAndElementRotation (ExpChooseElement polarizer)
                |> updateTableAndElementRotation ExpCommit
            Assert.Equal<ControlCaption option>(None, committed.kitStatus)
            Assert.Equal<SetupName list>([ SetupName "LP1-(QZ#45)-LPd" ], committed.kitSetups |> List.map (fun setup -> setup.name))
            let e1 = Assert.Single(committed.experimentCollection.experiments)
            Assert.Equal<Experiments.ExperimentId>(Experiments.ExperimentId 1, e1.id)
            Assert.Equal<Experiments.ExperimentName>(Experiments.ExperimentName "LP1-(QZ#45)-LPd", e1.name)
            let phoneRoiFile = FileName (Path.Combine(root, "phone-roi.csv"))
            File.WriteAllBytes(phoneRoiFile.value, Encoding.UTF8.GetBytes phoneRoiText)
            run folder
                {
                    committed = committed
                    sample = sample
                    phoneRoiFile = phoneRoiFile
                }
        finally
            scene.kit.close () |> kitValue
            opened.library.kit.close () |> kitValue
            if Directory.Exists root then Directory.Delete(root, true)

    /// Every file under the kit folder by its path relative to the folder, with its bytes.
    let private kitFileBytes (folder : KitFolder) : Map<string, FileByteContent> =
        Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun path -> Path.GetRelativePath(folder.value.value, path), FileByteContent (File.ReadAllBytes path))
        |> Map.ofArray

    /// The held split, failing when none is held.
    let private heldSplit (model : TableAndElementRotationModel) : DataSplitState =
        model.dataSplitOpt |> Option.defaultWith (fun () -> failwith "A split must be held.")

    [<Fact>]
    let ``splitting a picked file lists its groups matched, offered and unmatched, and writes nothing`` () : unit =
        withSplitKit (fun folder kit ->
            let heldSetup = Assert.Single(kit.committed.kitSetups)
            let before = kitFileBytes folder
            let fileBefore = File.ReadAllBytes kit.phoneRoiFile.value

            let split = kit.committed |> updateTableAndElementRotation (SplitByColumn (kit.phoneRoiFile, ImportProfileName.phoneRoiCsv))

            let held = heldSplit split
            Assert.Equal<FileName>(kit.phoneRoiFile, held.file)
            Assert.Equal<ImportProfileName>(ImportProfileName.phoneRoiCsv, held.profile)
            Assert.Equal<SplitKeyColumn>(SplitKeyColumn "experiment", held.key)
            Assert.Empty(held.creating)
            Assert.Equal<(DataRowKey * MeasuredPointCount) list>(
                [
                    DataRowKey "LP1-(QZ#45)-LPd", MeasuredPointCount 2
                    DataRowKey "LP1-(QZ#45)-LPd-2", MeasuredPointCount 1
                    DataRowKey "LP1-(QZ2#0)-LPd", MeasuredPointCount 1
                    DataRowKey "Darkness", MeasuredPointCount 1
                ],
                held.split.groups |> List.map (fun group -> group.key, group.points))
            match held.split.groups |> List.map (fun group -> group.matching) with
            | [ ExperimentForGroup matched; SetupOfferForGroup secondRun; SetupOfferForGroup newBench; GroupWithoutMatch (SplitKeyNameErr _) ] ->
                Assert.Equal<Experiments.ExperimentName>(Experiments.ExperimentName "LP1-(QZ#45)-LPd", matched)
                Assert.Equal<Setups.Setup>(heldSetup, secondRun.setup)
                Assert.Equal<Experiments.ExperimentName>(Experiments.ExperimentName "LP1-(QZ#45)-LPd-2", secondRun.experiment)
                Assert.Equal<SetupName>(SetupName "LP1-(QZ2#0)-LPd", newBench.setup.name)
                Assert.Equal<SetupRevision>(SetupRevision.first, newBench.setup.revision)
                let plate = newBench.setup.slots |> List.find (fun slot -> slot.id = kit.sample)
                Assert.Equal<Setups.SlotContent>(Setups.ItemInSlot (qz2Draft.id, Setups.PlacementTurn), plate.content)
                Assert.Equal(0.0, plate.placement.r1.value, 1e-12)
                Assert.Equal<Experiments.ExperimentName>(Experiments.ExperimentName "LP1-(QZ2#0)-LPd", newBench.experiment)
            | other -> Assert.Fail($"the groups were not matched, offered, offered and unmatched: %A{other}")
            Assert.Equal<DataRowKey list>([ DataRowKey "LP1-(QZ#45)-LPd" ], held.split.matched |> List.map (fun group -> group.key))
            Assert.Equal<DataRowKey list>(
                [ DataRowKey "LP1-(QZ#45)-LPd-2"; DataRowKey "LP1-(QZ2#0)-LPd" ],
                held.split.offered |> List.map (fun group -> group.key))
            Assert.Equal<DataRowKey list>([ DataRowKey "Darkness" ], held.split.unmatched |> List.map (fun group -> group.key))
            Assert.Equal<ControlCaption option>(
                Some (ControlCaption "Split phone-roi.csv by 'experiment': 1 matched, 2 offered, 1 unmatched."),
                split.kitStatus)
            // Nothing is attached or created until the split is applied (spec 0050 S9.3).
            Assert.Equal<TableAndElementRotationModel>({ kit.committed with dataSplitOpt = split.dataSplitOpt; kitStatus = split.kitStatus }, split)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)
            Assert.Equal<byte[]>(fileBefore, File.ReadAllBytes kit.phoneRoiFile.value))

    [<Fact>]
    let ``a split offer is ticked and unticked, and cancelling the split forgets it`` () : unit =
        withSplitKit (fun folder kit ->
            let before = kitFileBytes folder
            let newBench = DataRowKey "LP1-(QZ2#0)-LPd"
            Assert.Equal<TableAndElementRotationModel>(kit.committed, kit.committed |> updateTableAndElementRotation (ToggleSplitOffer newBench))
            let split = kit.committed |> updateTableAndElementRotation (SplitByColumn (kit.phoneRoiFile, ImportProfileName.phoneRoiCsv))
            Assert.Empty((heldSplit split).creating)

            let ticked = split |> updateTableAndElementRotation (ToggleSplitOffer newBench)
            Assert.Equal<DataRowKey list>([ newBench ], (heldSplit ticked).creating)
            Assert.Equal<TableAndElementRotationModel>({ split with dataSplitOpt = Some { heldSplit split with creating = [ newBench ] } }, ticked)

            let unticked = ticked |> updateTableAndElementRotation (ToggleSplitOffer newBench)
            Assert.Equal<TableAndElementRotationModel>(split, unticked)

            // A matched group, an unmatched one and a key of no group are no offers.
            for other in [ DataRowKey "LP1-(QZ#45)-LPd"; DataRowKey "Darkness"; DataRowKey "Nowhere" ] do
                Assert.Equal<TableAndElementRotationModel>(ticked, ticked |> updateTableAndElementRotation (ToggleSplitOffer other))

            let cancelled = ticked |> updateTableAndElementRotation CancelDataSplit
            Assert.Equal<DataSplitState option>(None, cancelled.dataSplitOpt)
            Assert.Equal<TableAndElementRotationModel>({ ticked with dataSplitOpt = None }, cancelled)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder))

    [<Fact>]
    let ``a file the profile cannot split, or a profile the kit lacks, is told and nothing is split`` () : unit =
        withSplitKit (fun folder kit ->
            let before = kitFileBytes folder
            let missing = FileName (Path.Combine(Path.GetDirectoryName kit.phoneRoiFile.value, "missing.csv"))
            let readReason =
                match kit.committed.experimentData.readPickedFile missing with
                | Error error -> error.describe
                | Ok _ -> failwith "A file that does not exist must not be read."

            let refusals =
                [
                    SplitByColumn (kit.phoneRoiFile, ImportProfileName.intensitySeries), "Not split — the import profile 'Intensity series' maps no key column."
                    SplitByColumn (kit.phoneRoiFile, ImportProfileName "Nope"), "Not split — the import profile 'Nope' is not available."
                    SplitByColumn (missing, ImportProfileName.phoneRoiCsv), $"Not split — {readReason}"
                ]
            let split = kit.committed |> updateTableAndElementRotation (SplitByColumn (kit.phoneRoiFile, ImportProfileName.phoneRoiCsv))
            for message, told in refusals do
                let refused = kit.committed |> updateTableAndElementRotation message
                Assert.Equal<DataSplitState option>(None, refused.dataSplitOpt)
                Assert.Equal<ControlCaption option>(Some (ControlCaption told), refused.kitStatus)
                Assert.Equal<TableAndElementRotationModel>({ kit.committed with kitStatus = refused.kitStatus }, refused)
                // A refusal keeps a split already held.
                let kept = split |> updateTableAndElementRotation message
                Assert.Equal<TableAndElementRotationModel>({ split with kitStatus = Some (ControlCaption told) }, kept)
            Assert.Contains("could not read the picked data file", readReason)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder))
