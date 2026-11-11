namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

/// The constructor's setup messages over a kit opened read-write: CreateSetup records the table as a kit setup,
/// SaveSetup writes it over the active one, ActivateSetup draws one, the window keeping its view, snap mode and pivot,
/// AssignSlot puts a kit item into a slot, SwapSlot makes the table the setup holding the swapped item, and a commit
/// records the table as the active setup its experiment references, a new one named by the kit's naming template,
/// RecordExperiment appends one more over it each time, a slot's chip lists the items that fit it, one list open at a
/// time, and the grid panel's messages edit its draft, whose grid GenerateSetupGrid records (spec 0050 S8.2–S8.4; 001 §J,
/// §P).
module SetupModelTests =

    let private kitValue (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private openedValue (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// Runs over the Main scene loaded with a fresh kit opened read-write in a GUID-named folder under the test output
    /// directory, as `KitWindowModelTests` opens its kits; the kits are closed and the folder deleted at the end.
    let private withSetupKit (run : KitFolder -> TableAndElementRotationModel -> unit) : unit =
        let root = FolderName (Path.Combine(AppContext.BaseDirectory, "SetupModelTests", Guid.NewGuid().ToString("N")))
        let folder = KitFolder root
        let imports = CatalogueImportContext.create (BuiltinCatalogue.freshStores ()).proxy
        let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system (KitAuthor "test") imports |> openedValue
        let scene = initMain () |> kitValue
        try run folder (scene.loadKit opened)
        finally
            scene.kit.close () |> kitValue
            opened.library.kit.close () |> kitValue
            if Directory.Exists root.value then Directory.Delete(root.value, true)

    let private createdAs (name : string) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        model
        |> updateTableAndElementRotation (SetSetupName (ControlCaption name))
        |> updateTableAndElementRotation CreateSetup

    /// The kit folder's experiment files, by id in ordinal order.
    let private experimentFilesIn (folder : KitFolder) : string list =
        Directory.GetFiles(folder.value.value, "*.experiment-json")
        |> Array.map Path.GetFileName
        |> Array.sort
        |> List.ofArray

    /// The table with a linear polarizer added, which the add selects.
    let private withPolarizer (model : TableAndElementRotationModel) : TableAndElementRotationModel * Library.ElementId =
        let placed = model |> updateTableAndElementRotation (AddElement LinearPolarizer)
        let polarizer = placed.elements |> List.find (fun slot -> slot.placement.catalogueKind = LinearPolarizer)
        placed, polarizer.id

    /// The draft varying the element, committed.
    let private committedVarying (id : Library.ElementId) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        model
        |> updateTableAndElementRotation (ExpChooseElement id)
        |> updateTableAndElementRotation ExpCommit

    /// The selected element turned to R1 = 30°.
    let private turned (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        model |> updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 30.0))

    /// The kit folder's setup files, by name in ordinal order.
    let private setupFilesIn (folder : KitFolder) : string list =
        Directory.GetFiles(folder.value.value, "*.setup-json")
        |> Array.map Path.GetFileName
        |> Array.sort
        |> List.ofArray

    /// Every file under the kit folder by its path relative to the folder, with its bytes.
    let private kitFileBytes (folder : KitFolder) : Map<string, FileByteContent> =
        Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun path -> Path.GetRelativePath(folder.value.value, path), FileByteContent (File.ReadAllBytes path))
        |> Map.ofArray

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

    let private lp1Draft : ItemDraft =
        {
            id = ItemId (Guid.Parse "26300000-0000-4000-8000-000000000001")
            label = ItemLabel "LP1"
            model = linearPolarizerChoice
            dial = FixedMounting
        }

    /// The 1 mm glass plate at its first version on the shipped stack-sample item model.
    let private qzDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "26300000-0000-4000-8000-000000000002")
            label = ItemLabel "QZ"
            model =
                StackSampleChoice (
                    BuiltinCatalogue.itemModel ItemModels.ItemModelIds.stackSampleItemModelId,
                    Library.SampleVersionId.firstOf Library.SampleIds.glassPlate1mmId,
                    BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId
                )
            dial = FixedMounting
        }

    let private laserDraft : ItemDraft =
        {
            id = ItemId (Guid.Parse "26300000-0000-4000-8000-000000000003")
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
            id = ItemId (Guid.Parse "26300000-0000-4000-8000-000000000004")
            label = ItemLabel "LPd"
            model = linearPolarizerChoice
            dial = DialMounted ItemModels.DialNegative
        }

    /// A known angle parameter of that many degrees, in radians, an angle parameter's unit (spec 0050 S8.1).
    let private knownDegrees (degrees : float<deg>) : ParameterState =
        KnownParameter (ItemModels.ParameterValue (Angle.degree degrees).value)

    /// The kit with LP1, QZ, Laser and LPd added through the model, LPd's dial zero at 10°.
    let private stocked (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let added =
            [ lp1Draft; qzDraft; laserDraft; lpdDraft ]
            |> List.fold (fun current draft -> current |> updateTableAndElementRotation (AddItem draft)) model
            |> updateTableAndElementRotation (SetParameterState (lpdDraft.id, ParameterName.dialZero, knownDegrees 10.0<deg>))
        Assert.Equal<ControlCaption option>(None, added.kitStatus)
        Assert.Equal<ItemId list>(
            [ lp1Draft.id; lpdDraft.id; laserDraft.id; qzDraft.id ] |> List.sort,
            added.kitItems |> List.map (fun item -> item.id) |> List.sort)
        added

    /// The table's slot of that id.
    let private slotOf (id : Library.ElementId) (model : TableAndElementRotationModel) : Setups.SetupSlot =
        model.elements |> List.find (fun slot -> slot.id = id)

    /// The table with a sample added, which the add leaves unbound.
    let private withSample (model : TableAndElementRotationModel) : TableAndElementRotationModel * Library.ElementId =
        let placed = model |> updateTableAndElementRotation (AddElement Sample)
        let sample = placed.elements |> List.find (fun slot -> slot.placement.catalogueKind = Sample)
        placed, sample.id

    /// The 2 mm glass plate at its first version on the shipped stack-sample item model.
    let private qz2Draft : ItemDraft =
        {
            id = ItemId (Guid.Parse "26400000-0000-4000-8000-000000000001")
            label = ItemLabel "QZ2"
            model =
                StackSampleChoice (
                    BuiltinCatalogue.itemModel ItemModels.ItemModelIds.stackSampleItemModelId,
                    Library.SampleVersionId.firstOf Library.SampleIds.glassPlate2mmId,
                    BuiltinCatalogue.sample Library.SampleIds.glassPlate2mmId
                )
            dial = FixedMounting
        }

    /// The kit with QZ and QZ2 added through the model, over a table of the source, an added sample and the seeded
    /// detector, and the sample's slot id.
    let private platesStocked (model : TableAndElementRotationModel) : TableAndElementRotationModel * Library.ElementId =
        let added =
            [ qzDraft; qz2Draft ]
            |> List.fold (fun current draft -> current |> updateTableAndElementRotation (AddItem draft)) model
        Assert.Equal<ControlCaption option>(None, added.kitStatus)
        let placed, sample = withSample added
        Assert.Equal<CatalogueKind list>(
            [ LightSource; Sample; Detector ],
            placed.elements |> List.map (fun slot -> slot.placement.catalogueKind) |> List.sort)
        placed, sample

    /// The table's sample holding the item, then created as a setup of that name.
    let private benchHolding (name : string) (item : ItemId) (sample : Library.ElementId) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        model
        |> updateTableAndElementRotation (AssignSlot (sample, Some item))
        |> createdAs name

    /// `Bench A` holding QZ and `Bench B` holding QZ2, with `Bench A` active again, and the sample's slot id.
    let private twoBenches (model : TableAndElementRotationModel) : TableAndElementRotationModel * Library.ElementId =
        let stocked, sample = platesStocked model
        let benches =
            stocked
            |> benchHolding "Bench A" qzDraft.id sample
            |> benchHolding "Bench B" qz2Draft.id sample
            |> updateTableAndElementRotation (ActivateSetup (SetupName "Bench A"))
        Assert.Equal<SetupName list>([ SetupName "Bench A"; SetupName "Bench B" ], benches.kitSetups |> List.map (fun setup -> setup.name))
        Assert.Equal<SetupName option>(Some (SetupName "Bench A"), benches.activeSetupOpt |> Option.map (fun setup -> setup.name))
        Assert.Equal<Setups.SlotContent>(Setups.ItemInSlot (qzDraft.id, Setups.PlacementTurn), (slotOf sample benches).content)
        benches, sample

    /// The held setup of that name.
    let private heldSetup (name : string) (model : TableAndElementRotationModel) : Setups.Setup =
        model.kitSetups |> List.find (fun setup -> setup.name = SetupName name)

    /// Runs over a Main scene loaded with the kit at `folder` reopened read-only; a read-only open takes no lock, so the
    /// fact may still hold the folder read-write. The reopened kit and the scene's own kit are closed at the end.
    let private withReadOnlyReopen (folder : KitFolder) (run : TableAndElementRotationModel -> unit) : unit =
        let imports = CatalogueImportContext.create (BuiltinCatalogue.freshStores ()).proxy
        let reopened = openKitAt folder ReadOnlyKitAccess ClockProvider.system (KitAuthor "test") imports |> openedValue
        let scene = initMain () |> kitValue
        try run (scene.loadKit reopened)
        finally
            scene.kit.close () |> kitValue
            reopened.library.kit.close () |> kitValue

    /// A table, step 272's or the chip facts', and the slot ids of its first polarizer and its sample.
    type private TemplateTable =
        {
            table : TableAndElementRotationModel
            polarizer : Library.ElementId
            sample : Library.ElementId
        }

    /// Over the kit `stocked` stocks and QZ2: LP1 in a linear polarizer, QZ in a sample turned to R1 45° and LPd in a
    /// second linear polarizer, which the light meets in that order before the seeded detector (spec 0050 S8.3).
    let private templateTable (model : TableAndElementRotationModel) : TemplateTable =
        let kit = stocked model |> updateTableAndElementRotation (AddItem qz2Draft)
        Assert.Equal<ControlCaption option>(None, kit.kitStatus)
        let polarizerPlaced, polarizer = withPolarizer kit
        let samplePlaced, sample = withSample polarizerPlaced
        // The add selects the sample, so the turn is its own.
        let placed =
            samplePlaced
            |> updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 45.0))
            |> updateTableAndElementRotation (AddElement LinearPolarizer)
        let analyzer = placed.elements |> List.find (fun slot -> slot.placement.catalogueKind = LinearPolarizer && slot.id <> polarizer)
        let assigned =
            placed
            |> updateTableAndElementRotation (AssignSlot (polarizer, Some lp1Draft.id))
            |> updateTableAndElementRotation (AssignSlot (sample, Some qzDraft.id))
            |> updateTableAndElementRotation (AssignSlot (analyzer.id, Some lpdDraft.id))
        Assert.Equal<ControlCaption option>(None, assigned.kitStatus)
        Assert.Equal((Angle.degree 45.0<deg>).value, (slotOf sample assigned).placement.r1.value, 1e-12)
        let idOf (kind : CatalogueKind) : Library.ElementId =
            (assigned.elements |> List.find (fun slot -> slot.placement.catalogueKind = kind)).id
        Assert.Equal<(Setups.SlotRole * Library.ElementId) list>(
            [
                Setups.SourceSlotRole, idOf LightSource
                Setups.PrepSlotRole (Setups.SlotPosition 1), polarizer
                Setups.ObjectSlotRole (Setups.SlotPosition 1), sample
                Setups.AnalyzerSlotRole (Setups.SlotPosition 1), analyzer.id
                Setups.DetectorSlotRole (Setups.SlotPosition 1), idOf Detector
            ],
            Setups.SetupSlot.inLightOrder assigned.elements |> List.map (fun roled -> roled.role, roled.slot.id))
        let held : TemplateTable =
            {
                table = assigned
                polarizer = polarizer
                sample = sample
            }
        held

    /// Step 272's table committed varying its first polarizer, and the sample's slot id (spec 0050 S8.3).
    let private templateCommitted (model : TableAndElementRotationModel) : TableAndElementRotationModel * Library.ElementId =
        let held = templateTable model
        held.table |> committedVarying held.polarizer, held.sample

    /// The sample's option of holding the item turned to R1 of that many degrees.
    let private plateChoice (item : ItemId) (degrees : float<deg>) : SetupGrids.SlotChoice =
        {
            itemOpt = Some item
            r1 = Angle.degree degrees
        }

    /// The grid `SetupGrid.create` makes over the table's sample holding QZ at 0°, QZ at 45° or QZ2 at 0°, with
    /// (QZ2, 0°) unticked (spec 0050 S8.3).
    let private plateGrid (held : TemplateTable) : SetupGrids.SetupGrid =
        let axis : SetupGrids.SlotOptions =
            {
                slot = held.sample
                choices = [ plateChoice qzDraft.id 0.0<deg>; plateChoice qzDraft.id 45.0<deg>; plateChoice qz2Draft.id 0.0<deg> ]
            }
        let made = SetupGrids.SetupGrid.create [ axis ] held.table.elements held.table.kitItems held.table.tryItemModelChoice
        Assert.Empty(made.unticked)
        let qz2Pick : SetupGrids.SlotPick =
            {
                slot = held.sample
                choice = plateChoice qz2Draft.id 0.0<deg>
            }
        let grid = made.untick { picks = [ qz2Pick ] }
        Assert.Equal(2, grid.ticked.Length)
        grid

    /// The table's sample slot in the setup.
    let private sampleIn (held : TemplateTable) (setup : Setups.Setup) : Setups.SetupSlot =
        setup.slots |> List.find (fun slot -> slot.id = held.sample)

    [<Fact>]
    let ``CreateSetup records the table as a new kit setup and makes it the active one`` () : unit =
        withSetupKit (fun folder model ->
            let placed = model |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let polarizer = placed.elements |> List.find (fun slot -> slot.placement.catalogueKind = LinearPolarizer)
            let presetId =
                polarizer.placement.valueId
                |> Option.bind (fun valueId -> valueId.tryToLibraryEntryId)
                |> Option.defaultWith (fun () -> failwith "The added linear polarizer must be bound to its preset.")
            let created = placed |> createdAs "Bench"

            let bench = Assert.Single(created.kitSetups)
            Assert.Equal<SetupName>(SetupName "Bench", bench.name)
            Assert.Equal<SetupRevision>(SetupRevision.first, bench.revision)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, bench.slots)
            Assert.Empty(bench.history)
            Assert.Equal<Setups.Setup option>(Some bench, created.activeSetupOpt)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Created setup 'Bench'."), created.kitStatus)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, created.elements)

            Assert.Equal<string list>([ "Bench.setup-json" ], setupFilesIn folder)
            Assert.Equal<Setups.Setup list>([ bench ], created.kit.proxy.readSetups () |> kitValue)
            let snapshot =
                created.kit.proxy.listSnapshots ()
                |> kitValue
                |> List.find (fun envelope -> envelope.content.id = presetId)
            Assert.StartsWith("library/", snapshot.snapshotPath.value)
            Assert.True(File.Exists(Path.Combine(folder.value.value, snapshot.snapshotPath.value))))

    [<Fact>]
    let ``CreateSetup of a blank name, or of a name whose file the kit holds, is told and records nothing`` () : unit =
        withSetupKit (fun folder model ->
            let assertNotCreated (before : TableAndElementRotationModel) (answer : TableAndElementRotationModel) : unit =
                match answer.kitStatus with
                | Some status -> Assert.StartsWith("Not created", status.value)
                | None -> Assert.Fail "a refused setup must be told in the kit status"
                Assert.Equal<Setups.Setup list>(before.kitSetups, answer.kitSetups)
                Assert.Equal<Setups.Setup option>(before.activeSetupOpt, answer.activeSetupOpt)
                Assert.Equal<Setups.SetupSlot list>(before.elements, answer.elements)

            let blank = model |> createdAs "   "
            assertNotCreated model blank
            Assert.Empty(setupFilesIn folder)
            Assert.Empty(blank.kit.proxy.listSnapshots () |> kitValue)

            let bench = model |> createdAs "Bench"
            Assert.Equal<string list>([ "Bench.setup-json" ], setupFilesIn folder)
            let benchFile = Path.Combine(folder.value.value, "Bench.setup-json")
            let benchText = File.ReadAllText benchFile
            let clash = bench |> createdAs "bench"
            assertNotCreated bench clash
            Assert.Equal<string list>([ "Bench.setup-json" ], setupFilesIn folder)
            Assert.Equal(benchText, File.ReadAllText benchFile)
            Assert.Equal<Setups.Setup list>(bench.kitSetups, clash.kit.proxy.readSetups () |> kitValue))

    [<Fact>]
    let ``ActivateSetup draws the kit's setup on the table and keeps the window's view, snap mode and pivot`` () : unit =
        withSetupKit (fun _ model ->
            let placed = model |> updateTableAndElementRotation (AddElement LinearPolarizer)
            let created = placed |> createdAs "Bench"
            let bench = Assert.Single(created.kitSetups)
            let changed =
                created
                |> updateTableAndElementRotation RemoveSelectedElement
                |> updateTableAndElementRotation (Wheel (Set.empty, WheelNotches 3))
                |> updateTableAndElementRotation (SetRotationPivot BeamLayout.PivotAtIncidentFace)
            Assert.DoesNotContain<Setups.SetupSlot>(changed.elements, fun slot -> slot.placement.catalogueKind = LinearPolarizer)
            Assert.NotEqual<Table.TableViewState>(created.view, changed.view)
            Assert.NotEqual<BeamLayout.RotationPivot>(created.rotationPivot, changed.rotationPivot)

            let activated = changed |> updateTableAndElementRotation (ActivateSetup (SetupName "Bench"))
            Assert.Equal<Setups.SetupSlot list>(bench.slots, activated.elements)
            Assert.Contains<Setups.SetupSlot>(activated.elements, fun slot -> slot.placement.catalogueKind = LinearPolarizer)
            Assert.Equal<Table.TableViewState>(changed.view, activated.view)
            Assert.Equal<Scene.SnapMode>(changed.snapChain, activated.snapChain)
            Assert.Equal<BeamLayout.RotationPivot>(BeamLayout.PivotAtIncidentFace, activated.rotationPivot)
            Assert.Equal<Selection>(TableSelected, activated.selection)
            Assert.Equal<Setups.Setup option>(Some bench, activated.activeSetupOpt)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Loaded setup 'Bench'."), activated.kitStatus)

            let missing = activated |> updateTableAndElementRotation (ActivateSetup (SetupName "nowhere"))
            match missing.kitStatus with
            | Some status ->
                Assert.StartsWith("Load failed", status.value)
                Assert.Contains("nowhere", status.value)
            | None -> Assert.Fail "a setup the kit does not hold must be told in the kit status"
            Assert.Equal<Setups.SetupSlot list>(activated.elements, missing.elements)
            Assert.Equal<Setups.Setup option>(activated.activeSetupOpt, missing.activeSetupOpt))

    [<Fact>]
    let ``a commit on a table that is no setup yet records it as the first free setup and references it`` () : unit =
        withSetupKit (fun folder model ->
            let placed, polarizer = withPolarizer model
            Assert.Equal<Setups.Setup option>(None, placed.activeSetupOpt)

            let committed = placed |> committedVarying polarizer

            Assert.Equal<string list>([ "Setup 1.setup-json" ], setupFilesIn folder)
            Assert.Equal<string list>([ "E1.experiment-json" ], experimentFilesIn folder)
            let setup1 = Assert.Single(committed.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<SetupName>(SetupName "Setup 1", setup1.name)
            Assert.Equal<SetupRevision>(SetupRevision.first, setup1.revision)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, setup1.slots)
            let e1 = Assert.Single(committed.kit.proxy.readExperiments () |> kitValue)
            let expected : SetupReference = { name = SetupName "Setup 1"; revision = SetupRevision.first }
            Assert.Equal<SetupReference>(expected, e1.setup)
            Assert.Equal<Experiments.Experiment list>([ e1 ], committed.experimentCollection.experiments)
            Assert.Equal<Setups.Setup option>(Some setup1, committed.activeSetupOpt)
            Assert.Equal<Setups.Setup list>([ setup1 ], committed.kitSetups)
            Assert.Equal<ControlCaption option>(None, committed.kitStatus))

    [<Fact>]
    let ``a commit over a changed table whose revision an experiment references mints the next revision, and the earlier experiment keeps its own`` () : unit =
        withSetupKit (fun folder model ->
            let placed, polarizer = withPolarizer model
            let first = placed |> committedVarying polarizer
            let firstSlots = first.elements
            let rotated = turned first
            Assert.NotEqual<Setups.SetupSlot list>(firstSlots, rotated.elements)

            let second =
                rotated
                |> updateTableAndElementRotation ExpNew
                |> committedVarying polarizer

            Assert.Equal<string list>([ "Setup 1.setup-json" ], setupFilesIn folder)
            Assert.Equal<string list>([ "E1.experiment-json"; "E2.experiment-json" ], experimentFilesIn folder)
            let setup1 = Assert.Single(second.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<SetupRevision>(SetupRevision 2, setup1.revision)
            Assert.Equal<Setups.SetupSlot list>(rotated.elements, setup1.slots)
            let firstRecord : Setups.SetupRevisionRecord =
                {
                    revision = SetupRevision.first
                    slots = firstSlots
                }
            Assert.Equal<Setups.SetupRevisionRecord list>([ firstRecord ], setup1.history)
            match second.kit.proxy.readExperiments () |> kitValue with
            | [ e1; e2 ] ->
                Assert.Equal<SetupReference>({ name = SetupName "Setup 1"; revision = SetupRevision.first }, e1.setup)
                Assert.Equal<SetupReference>({ name = SetupName "Setup 1"; revision = SetupRevision 2 }, e2.setup)
            | experiments -> Assert.Fail($"expected E1 and E2, read {List.length experiments} experiment(s)")
            Assert.Equal<Setups.Setup option>(Some setup1, second.activeSetupOpt)
            Assert.Equal<Setups.Setup list>([ setup1 ], second.kitSetups))

    [<Fact>]
    let ``a commit over a changed table no experiment references changes the setup in place`` () : unit =
        withSetupKit (fun folder model ->
            let placed, polarizer = withPolarizer model
            let created = placed |> createdAs "Bench"
            let rotated = turned created
            Assert.NotEqual<Setups.SetupSlot list>(created.elements, rotated.elements)

            let committed = rotated |> committedVarying polarizer

            Assert.Equal<string list>([ "Bench.setup-json" ], setupFilesIn folder)
            let bench = Assert.Single(committed.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<SetupName>(SetupName "Bench", bench.name)
            Assert.Equal<SetupRevision>(SetupRevision.first, bench.revision)
            Assert.Equal<Setups.SetupSlot list>(rotated.elements, bench.slots)
            Assert.Empty(bench.history)
            let e1 = Assert.Single(committed.kit.proxy.readExperiments () |> kitValue)
            Assert.Equal<SetupReference>(bench.reference, e1.setup)
            Assert.Equal<Setups.Setup option>(Some bench, committed.activeSetupOpt))

    [<Fact>]
    let ``SaveSetup writes the active setup in place until an experiment references its revision, then mints the next`` () : unit =
        withSetupKit (fun folder model ->
            let placed, polarizer = withPolarizer model
            let created = placed |> createdAs "Bench"
            let firstTurn = turned created
            Assert.NotEqual<Setups.SetupSlot list>(created.elements, firstTurn.elements)

            let inPlace = firstTurn |> updateTableAndElementRotation SaveSetup
            Assert.Equal<string list>([ "Bench.setup-json" ], setupFilesIn folder)
            let bench = Assert.Single(inPlace.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<SetupName>(SetupName "Bench", bench.name)
            Assert.Equal<SetupRevision>(SetupRevision.first, bench.revision)
            Assert.Equal<Setups.SetupSlot list>(firstTurn.elements, bench.slots)
            Assert.Empty(bench.history)
            Assert.Equal<Setups.Setup list>([ bench ], inPlace.kitSetups)
            Assert.Equal<Setups.Setup option>(Some bench, inPlace.activeSetupOpt)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Saved setup 'Bench', revision 1."), inPlace.kitStatus)

            // The table is unchanged, so the commit references `Bench` at revision 1 and rewrites nothing.
            let committed = inPlace |> committedVarying polarizer
            Assert.Equal<Setups.Setup list>([ bench ], committed.kit.proxy.readSetups () |> kitValue)
            let e1 = Assert.Single(committed.kit.proxy.readExperiments () |> kitValue)
            Assert.Equal<SetupReference>(bench.reference, e1.setup)

            let secondTurn = committed |> updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 60.0))
            Assert.NotEqual<Setups.SetupSlot list>(committed.elements, secondTurn.elements)
            let minted = secondTurn |> updateTableAndElementRotation SaveSetup

            Assert.Equal<string list>([ "Bench.setup-json" ], setupFilesIn folder)
            let revised = Assert.Single(minted.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<SetupRevision>(SetupRevision 2, revised.revision)
            Assert.Equal<Setups.SetupSlot list>(secondTurn.elements, revised.slots)
            let firstRecord : Setups.SetupRevisionRecord =
                {
                    revision = SetupRevision.first
                    slots = firstTurn.elements
                }
            Assert.Equal<Setups.SetupRevisionRecord list>([ firstRecord ], revised.history)
            Assert.Equal<Setups.Setup list>([ revised ], minted.kitSetups)
            Assert.Equal<Setups.Setup option>(Some revised, minted.activeSetupOpt)
            Assert.Equal<Experiments.Experiment list>([ e1 ], minted.kit.proxy.readExperiments () |> kitValue)
            Assert.Equal<Experiments.Experiment list>(committed.experimentCollection.experiments, minted.experimentCollection.experiments)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Saved setup 'Bench', revision 2."), minted.kitStatus))

    [<Fact>]
    let ``SaveSetup with no active setup, or over an unchanged table, writes nothing and says why`` () : unit =
        withSetupKit (fun folder model ->
            let placed, _ = withPolarizer model
            let noActive = placed |> updateTableAndElementRotation SaveSetup
            Assert.Empty(setupFilesIn folder)
            Assert.Empty(noActive.kit.proxy.listSnapshots () |> kitValue)
            Assert.Empty(noActive.kitSetups)
            Assert.Equal<Setups.Setup option>(None, noActive.activeSetupOpt)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, noActive.elements)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "No setup is active: create one first."), noActive.kitStatus)

            let created = placed |> createdAs "Bench"
            let benchFile = Path.Combine(folder.value.value, "Bench.setup-json")
            let benchBytes = File.ReadAllBytes benchFile
            let benchWritten = File.GetLastWriteTimeUtc benchFile
            let unchanged = created |> updateTableAndElementRotation SaveSetup
            Assert.Equal<string list>([ "Bench.setup-json" ], setupFilesIn folder)
            Assert.Equal<byte[]>(benchBytes, File.ReadAllBytes benchFile)
            Assert.Equal(benchWritten, File.GetLastWriteTimeUtc benchFile)
            Assert.Equal<Setups.Setup list>(created.kitSetups, unchanged.kitSetups)
            Assert.Equal<Setups.Setup option>(created.activeSetupOpt, unchanged.activeSetupOpt)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Setup 'Bench' is unchanged."), unchanged.kitStatus))

    [<Fact>]
    let ``AssignSlot puts a kit item into a table slot bound to its library entry, and AssignSlot of nothing empties it`` () : unit =
        withSetupKit (fun folder model ->
            let polarizerPlaced, polarizer = withPolarizer (stocked model)
            let placed, sample = withSample polarizerPlaced
            Assert.Equal<ElementValueId option>(None, (slotOf sample placed).placement.valueId)
            let before = kitFileBytes folder

            let lp1Held = placed |> updateTableAndElementRotation (AssignSlot (polarizer, Some lp1Draft.id))
            let lp1Slot = slotOf polarizer lp1Held
            Assert.Equal<Setups.SlotContent>(Setups.ItemInSlot (lp1Draft.id, Setups.PlacementTurn), lp1Slot.content)
            Assert.Equal<ElementValueId option>(Some Library.PresetIds.polarizerLp, lp1Slot.placement.valueId)
            Assert.Equal<Setups.SetupSlot>(slotOf sample placed, slotOf sample lp1Held)
            Assert.Equal<ControlCaption option>(None, lp1Held.kitStatus)

            let qzHeld = lp1Held |> updateTableAndElementRotation (AssignSlot (sample, Some qzDraft.id))
            let plate = BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId
            let qzSlot = slotOf sample qzHeld
            Assert.Equal<Setups.SlotContent>(Setups.ItemInSlot (qzDraft.id, Setups.PlacementTurn), qzSlot.content)
            Assert.Equal<ElementValueId option>(Some Library.SampleIds.glassPlate1mmId.toElementValueId, qzSlot.placement.valueId)
            Assert.Equal<Emission>(plate.supportedEmission, qzSlot.placement.emission)
            Assert.Equal<Setups.SetupSlot>(lp1Slot, slotOf polarizer qzHeld)

            let emptied = qzHeld |> updateTableAndElementRotation (AssignSlot (polarizer, None))
            let emptiedSlot = slotOf polarizer emptied
            Assert.Equal<Setups.SlotContent>(Setups.EmptySlotContent, emptiedSlot.content)
            Assert.Equal<ElementPlacement>(lp1Slot.placement, emptiedSlot.placement)
            Assert.Equal<Setups.SetupSlot>(qzSlot, slotOf sample emptied)
            Assert.Equal<ControlCaption option>(None, emptied.kitStatus)

            // Spec 0050 S8.2: an assignment writes nothing; the setup records the slot when it is next written.
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)
            Assert.Empty(setupFilesIn folder)
            Assert.Empty(emptied.kitSetups)

            // Reopened read-only against an empty library, the items' models, preset and sample answer only from the
            // kit's snapshots, and the assignment still binds.
            let empty = CatalogueImportProxy.createInMemory ()
            let reopened =
                openKitAt folder ReadOnlyKitAccess ClockProvider.system (KitAuthor "test") (CatalogueImportContext.create empty.proxy)
                |> openedValue
            try
                let emptyStores : KitLibraryStores =
                    {
                        library = Library.liveLibrary empty.libraryEntries empty.samples (Library.SourceProxy.createInMemory ())
                        materials = empty.materials
                        samples = empty.samples
                        categories = empty.categories
                    }
                let session = emptyStores.overlaidWith reopened.overlay
                let seeded =
                    initMainWithKit session.library (Experiments.createInMemoryExperiments ()) session.materials session.samples session.categories reopened.library.kit
                let overlaid =
                    {
                        seeded with itemModels = empty.itemModels.overlaidWith reopened.overlay
                    }
                let snapshotOnly = overlaid.loadKit reopened
                let lp1 = snapshotOnly.kitItems |> List.find (fun item -> item.id = lp1Draft.id)
                Assert.Equal<Result<Library.LibraryEntry option, Library.LibraryError>>(Ok None, empty.libraryEntries.tryGetEntry Library.PresetIds.polarizerLp)
                Assert.Equal<ItemModelChoice option>(Some linearPolarizerChoice, snapshotOnly.tryItemModelChoice lp1)

                let reopenedPolarizerPlaced, reopenedPolarizer = withPolarizer snapshotOnly
                let reopenedPlaced, reopenedSample = withSample reopenedPolarizerPlaced
                let reassigned =
                    reopenedPlaced
                    |> updateTableAndElementRotation (AssignSlot (reopenedPolarizer, Some lp1Draft.id))
                    |> updateTableAndElementRotation (AssignSlot (reopenedSample, Some qzDraft.id))
                Assert.Equal<ControlCaption option>(None, reassigned.kitStatus)
                let reassignedPolarizer = slotOf reopenedPolarizer reassigned
                Assert.Equal<Setups.SlotContent>(Setups.ItemInSlot (lp1Draft.id, Setups.PlacementTurn), reassignedPolarizer.content)
                Assert.Equal<ElementValueId option>(Some Library.PresetIds.polarizerLp, reassignedPolarizer.placement.valueId)
                let reassignedSample = slotOf reopenedSample reassigned
                Assert.Equal<Setups.SlotContent>(Setups.ItemInSlot (qzDraft.id, Setups.PlacementTurn), reassignedSample.content)
                Assert.Equal<Emission>(plate.supportedEmission, reassignedSample.placement.emission)
                Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)
            finally
                reopened.library.kit.close () |> kitValue)

    [<Fact>]
    let ``AssignSlot of an item that does not fit the slot, that the kit lacks, or whose model the library lacks is told and changes nothing`` () : unit =
        withSetupKit (fun folder model ->
            let polarizerPlaced, polarizer = withPolarizer (stocked model)
            let placed, sample = withSample polarizerPlaced
            let assertNotAssigned (named : string) (answer : TableAndElementRotationModel) : unit =
                match answer.kitStatus with
                | Some status ->
                    Assert.StartsWith("Not assigned", status.value)
                    Assert.Contains(named, status.value)
                | None -> Assert.Fail $"a refused assignment naming '{named}' must be told in the kit status"
                Assert.Equal<Setups.SetupSlot list>(placed.elements, answer.elements)
                Assert.Equal<Item list>(placed.kitItems, answer.kitItems)
                Assert.Equal<Setups.Setup option>(placed.activeSetupOpt, answer.activeSetupOpt)
            let before = kitFileBytes folder

            placed |> updateTableAndElementRotation (AssignSlot (polarizer, Some laserDraft.id)) |> assertNotAssigned "Laser"

            let unheld = ItemId (Guid.Parse "26300000-0000-4000-8000-000000000099")
            placed |> updateTableAndElementRotation (AssignSlot (polarizer, Some unheld)) |> assertNotAssigned (unheld.value.ToString())

            placed |> updateTableAndElementRotation (AssignSlot (Library.ElementId "nowhere", Some lp1Draft.id)) |> assertNotAssigned "nowhere"

            // A test-local item whose sample version the store lacks: its model resolves, its sample does not.
            let qz = placed.kitItems |> List.find (fun item -> item.id = qzDraft.id)
            let lostSample = Library.SampleVersionId.firstOf (Library.SampleId (Guid.Parse "26300000-0000-4000-8000-0000000000aa"))
            let lost : Item =
                match qz.model with
                | SampleInstance (stackModel, _) ->
                    {
                        qz with
                            id = ItemId (Guid.Parse "26300000-0000-4000-8000-000000000005")
                            label = ItemLabel "Lost plate"
                            model = SampleInstance (stackModel, lostSample)
                    }
                | PresetInstance _
                | ItemModelVersionInstance _ -> failwith "QZ must be a stack-sample item."
            let withLost =
                {
                    placed with kitItems = placed.kitItems @ [ lost ]
                }
            Assert.Equal<ItemModelChoice option>(None, withLost.tryItemModelChoice lost)
            let lostAnswer = withLost |> updateTableAndElementRotation (AssignSlot (sample, Some lost.id))
            match lostAnswer.kitStatus with
            | Some status ->
                Assert.StartsWith("Not assigned", status.value)
                Assert.Contains("Lost plate", status.value)
            | None -> Assert.Fail "an item whose sample the store lacks must be told in the kit status"
            Assert.Equal<Setups.SetupSlot list>(placed.elements, lostAnswer.elements)

            // Spec 0050 S7.2: the model resolves first, so a preset the library still holds does not stand in for it.
            let modelless =
                {
                    placed with itemModels = ItemModels.ItemModelProxy.createInMemory ()
                }
            let lp1 = placed.kitItems |> List.find (fun item -> item.id = lp1Draft.id)
            Assert.Equal<Result<Library.LibraryEntry option, Library.LibraryError>>(
                Ok (Some (BuiltinCatalogue.preset Library.PresetIds.polarizerLp)), modelless.library.tryGetEntry Library.PresetIds.polarizerLp)
            Assert.Equal<ItemModelChoice option>(None, modelless.tryItemModelChoice lp1)
            Assert.Equal<ItemModelChoice option>(Some linearPolarizerChoice, placed.tryItemModelChoice lp1)
            let modellessAnswer = modelless |> updateTableAndElementRotation (AssignSlot (polarizer, Some lp1.id))
            match modellessAnswer.kitStatus with
            | Some status ->
                Assert.StartsWith("Not assigned", status.value)
                Assert.Contains("LP1", status.value)
            | None -> Assert.Fail "an item whose model the store lacks must be told in the kit status"
            Assert.Equal<Setups.SetupSlot list>(placed.elements, modellessAnswer.elements)

            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder))

    [<Fact>]
    let ``a dial-mounted item is recorded with its reading and drawn through its current zero`` () : unit =
        withSetupKit (fun folder model ->
            let placed, polarizer = withPolarizer (stocked model)
            let assigned = turned placed |> updateTableAndElementRotation (AssignSlot (polarizer, Some lpdDraft.id))
            Assert.Equal<ControlCaption option>(None, assigned.kitStatus)

            let created = assigned |> createdAs "Dial"
            let dial = created.kitSetups |> List.find (fun setup -> setup.name = SetupName "Dial")
            Assert.Equal<Setups.Setup list>(created.kitSetups, created.kit.proxy.readSetups () |> kitValue)
            match (dial.slots |> List.find (fun slot -> slot.id = polarizer)).content with
            | Setups.ItemInSlot (held, Setups.DialTurn reading) ->
                Assert.Equal<ItemId>(lpdDraft.id, held)
                Assert.Equal((Angle.degree (-20.0<deg>)).value, reading.value, 1e-12)
            | other -> Assert.Fail $"the polarizer slot must hold LPd on its dial, held %A{other}"

            let rezeroed =
                created
                |> updateTableAndElementRotation (SetParameterState (lpdDraft.id, ParameterName.dialZero, knownDegrees 20.0<deg>))
            Assert.Equal<ControlCaption option>(None, rezeroed.kitStatus)
            let drawn = rezeroed |> updateTableAndElementRotation (ActivateSetup (SetupName "Dial"))
            Assert.Equal((Angle.degree 40.0<deg>).value, (slotOf polarizer drawn).placement.r1.value, 1e-12)
            Assert.Equal<string list>([ "Dial.setup-json" ], setupFilesIn folder))

    [<Fact>]
    let ``SwapSlot switches to the kit's setup that holds the swapped content and writes nothing`` () : unit =
        withSetupKit (fun folder model ->
            let benches, sample = twoBenches model
            let benchB = heldSetup "Bench B" benches
            let before = kitFileBytes folder

            let swapped = benches |> updateTableAndElementRotation (SwapSlot (sample, qz2Draft.id))

            Assert.Equal<Setups.Setup option>(Some benchB, swapped.activeSetupOpt)
            Assert.Equal<Setups.SetupSlot list>(benchB.slots, swapped.elements)
            Assert.Equal<Setups.Setup list>(benches.kitSetups, swapped.kitSetups)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Loaded setup 'Bench B'."), swapped.kitStatus)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)

            // Spec 0050 S8.2: a swap refuses as AssignSlot does, told as `Not swapped`, and changes nothing else.
            let unheld = ItemId (Guid.Parse "26400000-0000-4000-8000-000000000099")
            for slotId, itemId, named in [ (Library.ElementId "nowhere", qz2Draft.id, "nowhere"); (sample, unheld, unheld.value.ToString()) ] do
                let refused = benches |> updateTableAndElementRotation (SwapSlot (slotId, itemId))
                match refused.kitStatus with
                | Some status ->
                    Assert.StartsWith("Not swapped", status.value)
                    Assert.Contains(named, status.value)
                | None -> Assert.Fail $"a refused swap naming '{named}' must be told in the kit status"
                Assert.Equal<Setups.SetupSlot list>(benches.elements, refused.elements)
                Assert.Equal<Setups.Setup option>(benches.activeSetupOpt, refused.activeSetupOpt)
                Assert.Equal<Setups.Setup list>(benches.kitSetups, refused.kitSetups)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder))

    [<Fact>]
    let ``SwapSlot to content no setup holds creates and activates a setup of the next free name, leaving the swapped-from setup as it was`` () : unit =
        withSetupKit (fun folder model ->
            let stocked, sample = platesStocked model
            let benchA = stocked |> benchHolding "Bench A" qzDraft.id sample
            let held = Assert.Single(benchA.kitSetups)
            let benchAFile = Path.Combine(folder.value.value, "Bench A.setup-json")
            let benchABytes = File.ReadAllBytes benchAFile

            let swapped = benchA |> updateTableAndElementRotation (SwapSlot (sample, qz2Draft.id))

            Assert.Equal<string list>([ "Bench A (2).setup-json"; "Bench A.setup-json" ], setupFilesIn folder)
            Assert.Equal<byte[]>(benchABytes, File.ReadAllBytes benchAFile)
            let created = heldSetup "Bench A (2)" swapped
            Assert.Equal<SetupRevision>(SetupRevision.first, created.revision)
            Assert.Empty(created.history)
            Assert.Equal<Setups.SlotContent>(Setups.ItemInSlot (qz2Draft.id, Setups.PlacementTurn), (created.slots |> List.find (fun slot -> slot.id = sample)).content)
            Assert.Equal<Setups.Setup list>([ held; created ], swapped.kitSetups)
            Assert.Equal<Setups.Setup list>(swapped.kitSetups, swapped.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<Setups.Setup option>(Some created, swapped.activeSetupOpt)
            Assert.Equal<Setups.SetupSlot list>(created.slots, swapped.elements)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Created setup 'Bench A (2)' by the swap."), swapped.kitStatus))

        // Spec 0050 S8.2: a table that is no setup yet becomes the first free `Setup <n>`.
        withSetupKit (fun folder model ->
            let stocked, sample = platesStocked model
            Assert.Equal<Setups.Setup option>(None, stocked.activeSetupOpt)

            let swapped = stocked |> updateTableAndElementRotation (SwapSlot (sample, qz2Draft.id))

            Assert.Equal<string list>([ "Setup 1.setup-json" ], setupFilesIn folder)
            let setup1 = Assert.Single(swapped.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<SetupName>(SetupName "Setup 1", setup1.name)
            Assert.Equal<SetupRevision>(SetupRevision.first, setup1.revision)
            Assert.Equal<Setups.SlotContent>(Setups.ItemInSlot (qz2Draft.id, Setups.PlacementTurn), (setup1.slots |> List.find (fun slot -> slot.id = sample)).content)
            Assert.Equal<Setups.Setup list>([ setup1 ], swapped.kitSetups)
            Assert.Equal<Setups.Setup option>(Some setup1, swapped.activeSetupOpt)
            Assert.Equal<Setups.SetupSlot list>(setup1.slots, swapped.elements)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Created setup 'Setup 1' by the swap."), swapped.kitStatus))

    [<Fact>]
    let ``a kit opened read-only switches setups by a swap but creates none`` () : unit =
        withSetupKit (fun folder model ->
            let _, sample = twoBenches model
            withReadOnlyReopen folder (fun readOnly ->
                Assert.Equal<ControlAvailability>(ControlWithheld, readOnly.kitWrites)
                let benchA = readOnly |> updateTableAndElementRotation (ActivateSetup (SetupName "Bench A"))
                let benchB = heldSetup "Bench B" readOnly
                let before = kitFileBytes folder

                let switched = benchA |> updateTableAndElementRotation (SwapSlot (sample, qz2Draft.id))
                Assert.Equal<Setups.Setup option>(Some benchB, switched.activeSetupOpt)
                Assert.Equal<Setups.SetupSlot list>(benchB.slots, switched.elements)
                Assert.Equal<ControlCaption option>(Some (ControlCaption "Loaded setup 'Bench B'."), switched.kitStatus)
                Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)

                // A table with one more slot than either setup holds content no setup holds, so the swap would create one.
                let widened = switched |> updateTableAndElementRotation (AddElement LinearPolarizer)
                let refused = widened |> updateTableAndElementRotation (SwapSlot (sample, qzDraft.id))
                Assert.Equal<ControlCaption option>(Some (ControlCaption "The kit is open read-only: nothing is saved."), refused.kitStatus)
                Assert.Equal<Setups.SetupSlot list>(widened.elements, refused.elements)
                Assert.Equal<Setups.Setup list>(widened.kitSetups, refused.kitSetups)
                Assert.Equal<Setups.Setup option>(widened.activeSetupOpt, refused.activeSetupOpt)
                Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)))

    [<Fact>]
    let ``a commit on a table the kit's template names records it under that name`` () : unit =
        withSetupKit (fun folder model ->
            Assert.Equal<NamingTemplate>(NamingTemplate.standard, model.kit.kitFile.namingTemplate)

            let committed, _ = templateCommitted model

            Assert.Equal<string list>([ "LP1-(QZ#45)-LPd.setup-json" ], setupFilesIn folder)
            Assert.Equal<string list>([ "E1.experiment-json" ], experimentFilesIn folder)
            let named = Assert.Single(committed.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<SetupName>(SetupName "LP1-(QZ#45)-LPd", named.name)
            Assert.Equal<SetupRevision>(SetupRevision.first, named.revision)
            Assert.Empty(named.history)
            let e1 = Assert.Single(committed.kit.proxy.readExperiments () |> kitValue)
            let expected : SetupReference = { name = SetupName "LP1-(QZ#45)-LPd"; revision = SetupRevision.first }
            Assert.Equal<SetupReference>(expected, e1.setup)
            Assert.Equal<Experiments.Experiment list>([ e1 ], committed.experimentCollection.experiments)
            Assert.Equal<Setups.Setup option>(Some named, committed.activeSetupOpt)
            Assert.Equal<Setups.Setup list>([ named ], committed.kitSetups)
            Assert.Equal<ControlCaption option>(None, committed.kitStatus))

    [<Fact>]
    let ``a swap to content no setup holds takes the template's name of the swapped table`` () : unit =
        withSetupKit (fun folder model ->
            let committed, sample = templateCommitted model
            let held = Assert.Single(committed.kitSetups)
            let heldFile = Path.Combine(folder.value.value, "LP1-(QZ#45)-LPd.setup-json")
            let heldBytes = File.ReadAllBytes heldFile

            let swapped = committed |> updateTableAndElementRotation (SwapSlot (sample, qz2Draft.id))

            Assert.Equal<string list>([ "LP1-(QZ#45)-LPd.setup-json"; "LP1-(QZ2#45)-LPd.setup-json" ], setupFilesIn folder)
            Assert.Equal<byte[]>(heldBytes, File.ReadAllBytes heldFile)
            let created = heldSetup "LP1-(QZ2#45)-LPd" swapped
            Assert.Equal<SetupRevision>(SetupRevision.first, created.revision)
            Assert.Empty(created.history)
            Assert.Equal<Setups.SlotContent>(Setups.ItemInSlot (qz2Draft.id, Setups.PlacementTurn), (created.slots |> List.find (fun slot -> slot.id = sample)).content)
            Assert.Equal<Setups.Setup list>([ held; created ], swapped.kitSetups)
            Assert.Equal<Setups.Setup list>(swapped.kitSetups, swapped.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<Setups.Setup option>(Some created, swapped.activeSetupOpt)
            Assert.Equal<Setups.SetupSlot list>(created.slots, swapped.elements)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Created setup 'LP1-(QZ2#45)-LPd' by the swap."), swapped.kitStatus))

    [<Fact>]
    let ``GenerateSetupGrid records a setup per ticked combination, named by the kit's template, and an experiment of the draft over each`` () : unit =
        withSetupKit (fun folder model ->
            let held = templateTable model
            let chosen = held.table |> updateTableAndElementRotation (ExpChooseElement held.polarizer)
            Assert.True(chosen.experimentCollection.canCommit)
            Assert.Equal<Setups.Setup option>(None, chosen.activeSetupOpt)

            let generated = chosen |> updateTableAndElementRotation (GenerateSetupGrid (plateGrid held))

            Assert.Equal<ControlCaption option>(Some (ControlCaption "Generated 2 setups (2 new) and 2 experiments."), generated.kitStatus)
            Assert.Equal<string list>([ "LP1-(QZ#0)-LPd.setup-json"; "LP1-(QZ#45)-LPd.setup-json" ], setupFilesIn folder)
            Assert.Equal<string list>([ "E1.experiment-json"; "E2.experiment-json" ], experimentFilesIn folder)
            let setups = generated.kit.proxy.readSetups () |> kitValue
            Assert.Equal<SetupName list>([ SetupName "LP1-(QZ#0)-LPd"; SetupName "LP1-(QZ#45)-LPd" ], setups |> List.map (fun setup -> setup.name))
            Assert.Equal<SetupRevision list>([ SetupRevision.first; SetupRevision.first ], setups |> List.map (fun setup -> setup.revision))
            Assert.Equal<Setups.Setup list>(setups, generated.kitSetups)
            for setup, degrees in List.zip setups [ 0.0<deg>; 45.0<deg> ] do
                let plate = sampleIn held setup
                Assert.Equal<Setups.SlotContent>(Setups.ItemInSlot (qzDraft.id, Setups.PlacementTurn), plate.content)
                Assert.Equal((Angle.degree degrees).value, plate.placement.r1.value, 1e-12)

            let draft = chosen.experimentCollection.draft
            let experiments = generated.kit.proxy.readExperiments () |> kitValue
            Assert.Equal<Experiments.ExperimentId list>([ Experiments.ExperimentId 1; Experiments.ExperimentId 2 ], experiments |> List.map (fun experiment -> experiment.id))
            Assert.Equal<SetupReference list>(setups |> List.map (fun setup -> setup.reference), experiments |> List.map (fun experiment -> experiment.setup))
            for experiment in experiments do
                Assert.Equal<Library.ElementId list>([ held.polarizer ], experiment.scan.axes |> List.map (fun axis -> axis.elementId))
                Assert.Equal<Experiments.VariableParameter list>(Option.toList draft.variable, experiment.scan.axes |> List.map (fun axis -> axis.variable))
                Assert.Equal<Experiments.VariableRange list>([ draft.range ], experiment.scan.axes |> List.map (fun axis -> axis.range))
                Assert.Equal<DataAttachment option>(None, experiment.dataFileOpt)
            Assert.Equal<Experiments.Experiment list>(experiments, generated.experimentCollection.experiments)

            Assert.Equal<Setups.SetupSlot list>(chosen.elements, generated.elements)
            Assert.Equal<Setups.Setup option>(chosen.activeSetupOpt, generated.activeSetupOpt)
            Assert.Equal<Library.ElementId option>(draft.elementId, generated.experimentCollection.draft.elementId)
            Assert.Equal<Experiments.VariableParameter option>(draft.variable, generated.experimentCollection.draft.variable))

    [<Fact>]
    let ``a grid over setups the kit holds writes no setup again, and a grid it cannot make writes nothing`` () : unit =
        withSetupKit (fun folder model ->
            let held = templateTable model
            let grid = plateGrid held
            let first =
                held.table
                |> updateTableAndElementRotation (ExpChooseElement held.polarizer)
                |> updateTableAndElementRotation (GenerateSetupGrid grid)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Generated 2 setups (2 new) and 2 experiments."), first.kitStatus)
            let setupFiles = [ "LP1-(QZ#0)-LPd.setup-json"; "LP1-(QZ#45)-LPd.setup-json" ]
            let setupBytes () : byte[] list = setupFiles |> List.map (fun name -> File.ReadAllBytes(Path.Combine(folder.value.value, name)))
            let firstBytes = setupBytes ()

            let again = first |> updateTableAndElementRotation (GenerateSetupGrid grid)

            Assert.Equal<ControlCaption option>(Some (ControlCaption "Generated 2 setups (0 new) and 2 experiments."), again.kitStatus)
            Assert.Equal<string list>(setupFiles, setupFilesIn folder)
            Assert.Equal<byte[] list>(firstBytes, setupBytes ())
            Assert.Equal<Setups.Setup list>(first.kitSetups, again.kitSetups)
            Assert.Equal<string list>([ "E1.experiment-json"; "E2.experiment-json"; "E3.experiment-json"; "E4.experiment-json" ], experimentFilesIn folder)
            match again.kit.proxy.readExperiments () |> kitValue with
            | [ e1; e2; e3; e4 ] ->
                Assert.Equal<Experiments.ExperimentId list>([ Experiments.ExperimentId 3; Experiments.ExperimentId 4 ], [ e3.id; e4.id ])
                Assert.Equal<SetupReference list>([ e1.setup; e2.setup ], [ e3.setup; e4.setup ])
                Assert.Equal<SetupReference list>(first.kitSetups |> List.map (fun setup -> setup.reference), [ e3.setup; e4.setup ])
                Assert.Equal<Experiments.Experiment list>([ e1; e2; e3; e4 ], again.experimentCollection.experiments)
            | experiments -> Assert.Fail($"expected E1 to E4, read {List.length experiments} experiment(s)")

            // A ticked combination that puts the laser in the sample refuses the whole grid, the new QZ2 setup with it.
            let laserAxis : SetupGrids.SlotOptions =
                {
                    slot = held.sample
                    choices = [ plateChoice qz2Draft.id 0.0<deg>; plateChoice laserDraft.id 0.0<deg> ]
                }
            let laserGrid : SetupGrids.SetupGrid =
                {
                    axes = [ laserAxis ]
                    unticked = []
                }
            let before = kitFileBytes folder
            let refused = again |> updateTableAndElementRotation (GenerateSetupGrid laserGrid)
            match refused.kitStatus with
            | Some status ->
                Assert.StartsWith("Not generated", status.value)
                Assert.Contains("Laser", status.value)
            | None -> Assert.Fail "a grid the table cannot make must be told in the kit status"
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)
            Assert.Equal<Setups.Setup list>(again.kitSetups, refused.kitSetups)
            Assert.Equal<Experiments.ExperimentCollection>(again.experimentCollection, refused.experimentCollection)
            Assert.Equal<Setups.SetupSlot list>(again.elements, refused.elements))

    [<Fact>]
    let ``a grid over a draft that cannot be committed records its setups and no experiment`` () : unit =
        withSetupKit (fun folder model ->
            let held = templateTable model
            let cleared =
                held.table
                |> updateTableAndElementRotation (ExpChooseElement held.polarizer)
                |> updateTableAndElementRotation ExpNew
            Assert.False(cleared.experimentCollection.canCommit)

            let generated = cleared |> updateTableAndElementRotation (GenerateSetupGrid (plateGrid held))

            Assert.Equal<ControlCaption option>(Some (ControlCaption "Generated 2 setups (2 new) and 0 experiments."), generated.kitStatus)
            Assert.Equal<string list>([ "LP1-(QZ#0)-LPd.setup-json"; "LP1-(QZ#45)-LPd.setup-json" ], setupFilesIn folder)
            Assert.Empty(experimentFilesIn folder)
            Assert.Equal<Setups.Setup list>(generated.kit.proxy.readSetups () |> kitValue, generated.kitSetups)
            Assert.Equal<Experiments.ExperimentCollection>(cleared.experimentCollection, generated.experimentCollection)
            Assert.Equal<Setups.SetupSlot list>(cleared.elements, generated.elements)
            Assert.Equal<Setups.Setup option>(cleared.activeSetupOpt, generated.activeSetupOpt))

    [<Fact>]
    let ``a kit opened read-only generates nothing`` () : unit =
        withSetupKit (fun folder model ->
            let held = templateTable model
            // The table recorded, so the read-only kit can draw it and the grid alone is refused.
            let bench = held.table |> createdAs "Bench"
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Created setup 'Bench'."), bench.kitStatus)
            withReadOnlyReopen folder (fun readOnly ->
                Assert.Equal<ControlAvailability>(ControlWithheld, readOnly.kitWrites)
                let drawn =
                    readOnly
                    |> updateTableAndElementRotation (ActivateSetup (SetupName "Bench"))
                    |> updateTableAndElementRotation (ExpChooseElement held.polarizer)
                let grid = plateGrid held
                match grid.generate drawn.elements drawn.kitItems drawn.tryItemModelChoice drawn.kit.kitFile.namingTemplate drawn.kitSetups with
                | Ok setups -> Assert.Equal(2, setups.Length)
                | Error error -> Assert.Fail $"the grid must be one the read-only kit could make: {error.describe}"
                let before = kitFileBytes folder

                let refused = drawn |> updateTableAndElementRotation (GenerateSetupGrid grid)

                Assert.Equal<ControlCaption option>(Some (ControlCaption "The kit is open read-only: nothing is saved."), refused.kitStatus)
                Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)
                Assert.Equal<Setups.Setup list>(drawn.kitSetups, refused.kitSetups)
                Assert.Equal<Experiments.ExperimentCollection>(drawn.experimentCollection, refused.experimentCollection)
                Assert.Equal<Setups.SetupSlot list>(drawn.elements, refused.elements)
                Assert.Equal<Setups.Setup option>(drawn.activeSetupOpt, refused.activeSetupOpt)))

    // Spec 0050 S8.4: the direct problem, several experiments over one setup.

    /// The draft's range with its maximum at 180°.
    let private narrowed (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        model |> updateTableAndElementRotation (ExpSetRangeMax (ExperimentControls.ExperimentRangeEnd 180.0))

    [<Fact>]
    let ``RecordExperiment appends a new experiment over the active setup each time, so one setup holds several experiments`` () : unit =
        withSetupKit (fun folder model ->
            let placed, polarizer = withPolarizer model
            let created = placed |> createdAs "Bench"
            let bench = Assert.Single(created.kitSetups)
            Assert.Equal<SetupReference>({ name = SetupName "Bench"; revision = SetupRevision.first }, bench.reference)
            let benchFile = Path.Combine(folder.value.value, "Bench.setup-json")
            let benchBytes = File.ReadAllBytes benchFile
            let chosen = created |> updateTableAndElementRotation (ExpChooseElement polarizer)
            let firstRange = chosen.experimentCollection.draft.range

            let first = chosen |> updateTableAndElementRotation RecordExperiment
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Recorded E1 over 'Bench' revision 1."), first.kitStatus)
            let changed = narrowed first
            let secondRange = changed.experimentCollection.draft.range
            Assert.NotEqual<Experiments.VariableRange>(firstRange, secondRange)
            let second = changed |> updateTableAndElementRotation RecordExperiment

            Assert.Equal<string list>([ "E1.experiment-json"; "E2.experiment-json" ], experimentFilesIn folder)
            match second.kit.proxy.readExperiments () |> kitValue with
            | [ e1; e2 ] ->
                Assert.Equal<Experiments.ExperimentId list>([ Experiments.ExperimentId 1; Experiments.ExperimentId 2 ], [ e1.id; e2.id ])
                Assert.Equal<SetupReference list>([ bench.reference; bench.reference ], [ e1.setup; e2.setup ])
                Assert.Equal<Experiments.VariableRange list>([ firstRange ], e1.scan.axes |> List.map (fun axis -> axis.range))
                Assert.Equal<Experiments.VariableRange list>([ secondRange ], e2.scan.axes |> List.map (fun axis -> axis.range))
                Assert.Equal<Experiments.Experiment list>([ e1; e2 ], second.experimentCollection.experiments)
            | experiments -> Assert.Fail($"expected E1 and E2, read {List.length experiments} experiment(s)")
            Assert.Equal<string list>([ "Bench.setup-json" ], setupFilesIn folder)
            Assert.Equal<byte[]>(benchBytes, File.ReadAllBytes benchFile)
            Assert.Equal<Setups.Setup list>([ bench ], second.kitSetups)
            Assert.Equal<Setups.Setup option>(Some bench, second.activeSetupOpt)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Recorded E2 over 'Bench' revision 1."), second.kitStatus)
            Assert.Equal<Experiments.ExperimentId option>(None, second.experimentCollection.draft.editingId))

    [<Fact>]
    let ``RecordExperiment over a draft that edits an experiment appends another and leaves the edited one as it was`` () : unit =
        withSetupKit (fun folder model ->
            let placed, polarizer = withPolarizer model
            let first =
                placed
                |> createdAs "Bench"
                |> updateTableAndElementRotation (ExpChooseElement polarizer)
                |> updateTableAndElementRotation RecordExperiment
            let e1 = Assert.Single(first.experimentCollection.experiments)
            let e1File = Path.Combine(folder.value.value, "E1.experiment-json")
            let e1Bytes = File.ReadAllBytes e1File
            let editing = first |> updateTableAndElementRotation (ExpEdit e1.id) |> narrowed
            Assert.Equal<Experiments.ExperimentId option>(Some e1.id, editing.experimentCollection.draft.editingId)
            let changedRange = editing.experimentCollection.draft.range
            Assert.DoesNotContain(changedRange, e1.scan.axes |> List.map (fun axis -> axis.range))

            let second = editing |> updateTableAndElementRotation RecordExperiment

            Assert.Equal<string list>([ "E1.experiment-json"; "E2.experiment-json" ], experimentFilesIn folder)
            Assert.Equal<byte[]>(e1Bytes, File.ReadAllBytes e1File)
            match second.kit.proxy.readExperiments () |> kitValue with
            | [ storedE1; e2 ] ->
                Assert.Equal<Experiments.Experiment>(e1, storedE1)
                Assert.Equal<Experiments.ExperimentId>(Experiments.ExperimentId 2, e2.id)
                Assert.Equal<Experiments.VariableRange list>([ changedRange ], e2.scan.axes |> List.map (fun axis -> axis.range))
                Assert.Equal<SetupReference>(e1.setup, e2.setup)
                Assert.Equal<Experiments.Experiment list>([ e1; e2 ], second.experimentCollection.experiments)
            | experiments -> Assert.Fail($"expected E1 and E2, read {List.length experiments} experiment(s)")
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Recorded E2 over 'Bench' revision 1."), second.kitStatus)
            Assert.Equal<Experiments.ExperimentId option>(None, second.experimentCollection.draft.editingId))

    [<Fact>]
    let ``RecordExperiment records the table as ExpCommit does: a first setup for a table that is no setup yet, and the next revision over one an experiment references`` () : unit =
        withSetupKit (fun folder model ->
            let placed, polarizer = withPolarizer model
            Assert.Equal<Setups.Setup option>(None, placed.activeSetupOpt)

            let first =
                placed
                |> updateTableAndElementRotation (ExpChooseElement polarizer)
                |> updateTableAndElementRotation RecordExperiment

            let revision1 : SetupReference = { name = SetupName "Setup 1"; revision = SetupRevision.first }
            Assert.Equal<string list>([ "Setup 1.setup-json" ], setupFilesIn folder)
            Assert.Equal<string list>([ "E1.experiment-json" ], experimentFilesIn folder)
            let setup1 = Assert.Single(first.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<SetupReference>(revision1, setup1.reference)
            Assert.Equal<Setups.SetupSlot list>(placed.elements, setup1.slots)
            let e1 = Assert.Single(first.kit.proxy.readExperiments () |> kitValue)
            Assert.Equal<SetupReference>(revision1, e1.setup)
            Assert.Equal<Setups.Setup option>(Some setup1, first.activeSetupOpt)
            Assert.Equal<Setups.Setup list>([ setup1 ], first.kitSetups)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Recorded E1 over 'Setup 1' revision 1."), first.kitStatus)

            // The add selected the polarizer, so the turn is its own.
            let rotated = turned first
            Assert.NotEqual<Setups.SetupSlot list>(first.elements, rotated.elements)
            let second = rotated |> updateTableAndElementRotation RecordExperiment

            Assert.Equal<string list>([ "Setup 1.setup-json" ], setupFilesIn folder)
            Assert.Equal<string list>([ "E1.experiment-json"; "E2.experiment-json" ], experimentFilesIn folder)
            let revised = Assert.Single(second.kit.proxy.readSetups () |> kitValue)
            Assert.Equal<SetupRevision>(SetupRevision 2, revised.revision)
            Assert.Equal<Setups.SetupSlot list>(rotated.elements, revised.slots)
            let firstRecord : Setups.SetupRevisionRecord =
                {
                    revision = SetupRevision.first
                    slots = first.elements
                }
            Assert.Equal<Setups.SetupRevisionRecord list>([ firstRecord ], revised.history)
            match second.kit.proxy.readExperiments () |> kitValue with
            | [ storedE1; e2 ] ->
                Assert.Equal<SetupReference>(revision1, storedE1.setup)
                Assert.Equal<SetupReference>({ name = SetupName "Setup 1"; revision = SetupRevision 2 }, e2.setup)
            | experiments -> Assert.Fail($"expected E1 and E2, read {List.length experiments} experiment(s)")
            Assert.Equal<Setups.Setup option>(Some revised, second.activeSetupOpt)
            Assert.Equal<Setups.Setup list>([ revised ], second.kitSetups)
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Recorded E2 over 'Setup 1' revision 2."), second.kitStatus))

    [<Fact>]
    let ``RecordExperiment without a chosen element and variable is told and writes nothing`` () : unit =
        withSetupKit (fun folder model ->
            let placed, _ = withPolarizer model
            let cleared = placed |> updateTableAndElementRotation ExpNew
            Assert.False(cleared.experimentCollection.canCommit)
            let before = kitFileBytes folder

            let refused = cleared |> updateTableAndElementRotation RecordExperiment

            match refused.kitStatus with
            | Some status -> Assert.StartsWith("Not recorded", status.value)
            | None -> Assert.Fail "a draft that cannot be recorded must be told in the kit status"
            Assert.Equal<ControlCaption option>(
                Some (ControlCaption "Not recorded — choose the element and the variable to vary in the Experiments bay first."),
                refused.kitStatus)
            Assert.Empty(experimentFilesIn folder)
            Assert.Empty(setupFilesIn folder)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)
            Assert.Equal<Experiments.ExperimentCollection>(cleared.experimentCollection, refused.experimentCollection)
            Assert.Equal<Setups.Setup list>(cleared.kitSetups, refused.kitSetups)
            Assert.Equal<Setups.Setup option>(cleared.activeSetupOpt, refused.activeSetupOpt))

    /// Over the kit `stocked` stocks and QZ2, the table with a linear polarizer and a sample added, both empty (spec 0050
    /// S8.4).
    let private chipTable (model : TableAndElementRotationModel) : TemplateTable =
        let kit = stocked model |> updateTableAndElementRotation (AddItem qz2Draft)
        Assert.Equal<ControlCaption option>(None, kit.kitStatus)
        let polarizerPlaced, polarizer = withPolarizer kit
        let placed, sample = withSample polarizerPlaced
        let held : TemplateTable =
            {
                table = placed
                polarizer = polarizer
                sample = sample
            }
        held

    /// The table's first slot of that kind.
    let private slotOfKind (kind : CatalogueKind) (model : TableAndElementRotationModel) : Setups.SetupSlot =
        model.elements |> List.find (fun slot -> slot.placement.catalogueKind = kind)

    [<Fact>]
    let ``a slot's chip lists the kit's items that fit it`` () : unit =
        withSetupKit (fun _ model ->
            let held = chipTable model
            let table = held.table
            let listed (slot : Setups.SetupSlot) : ItemId list = table.slotChipItems slot |> List.map (fun item -> item.id)
            let inKitOrder (ids : ItemId list) : ItemId list =
                table.kitItems |> List.map (fun item -> item.id) |> List.filter (fun id -> List.contains id ids)

            // Spec 0050 S8.4: the kit holds its items in label order, so each chip lists them in that order.
            Assert.Equal<ItemLabel list>(
                [ ItemLabel "LP1"; ItemLabel "LPd"; ItemLabel "Laser"; ItemLabel "QZ"; ItemLabel "QZ2" ],
                table.kitItems |> List.map (fun item -> item.label))
            Assert.Equal<ItemId list>([ lp1Draft.id; lpdDraft.id ], listed (slotOf held.polarizer table))
            Assert.Equal<ItemId list>(inKitOrder [ lpdDraft.id; lp1Draft.id ], listed (slotOf held.polarizer table))
            Assert.Equal<ItemId list>([ qzDraft.id; qz2Draft.id ], listed (slotOf held.sample table))
            Assert.Equal<ItemId list>(inKitOrder [ qz2Draft.id; qzDraft.id ], listed (slotOf held.sample table))
            Assert.Equal<ItemId list>([ laserDraft.id ], listed (slotOfKind LightSource table))
            Assert.Empty(listed (slotOfKind Detector table))

            // A slot holding an item still lists every item that fits it, its own included.
            let assigned = table |> updateTableAndElementRotation (AssignSlot (held.polarizer, Some lp1Draft.id))
            Assert.Equal<ControlCaption option>(None, assigned.kitStatus)
            Assert.Equal<ItemId list>(
                [ lp1Draft.id; lpdDraft.id ],
                assigned.slotChipItems (slotOf held.polarizer assigned) |> List.map (fun item -> item.id)))

    [<Fact>]
    let ``ToggleSlotChip opens one chip's list at a time, and a swap closes it`` () : unit =
        withSetupKit (fun folder model ->
            let held = chipTable model
            let closed = held.table
            let toggled (slotId : Library.ElementId) (current : TableAndElementRotationModel) : TableAndElementRotationModel =
                current |> updateTableAndElementRotation (ToggleSlotChip slotId)
            Assert.Equal<Library.ElementId option>(None, closed.slotChipOpenOpt)
            let before = kitFileBytes folder

            let sampleOpen = closed |> toggled held.sample
            Assert.Equal<Library.ElementId option>(Some held.sample, sampleOpen.slotChipOpenOpt)
            let polarizerOpen = sampleOpen |> toggled held.polarizer
            Assert.Equal<Library.ElementId option>(Some held.polarizer, polarizerOpen.slotChipOpenOpt)
            let reclosed = polarizerOpen |> toggled held.polarizer
            Assert.Equal<Library.ElementId option>(None, reclosed.slotChipOpenOpt)

            // An id the table lacks changes nothing, whether a list is open or not.
            for current in [ closed; sampleOpen; polarizerOpen ] do
                let answered = current |> toggled (Library.ElementId "nowhere")
                Assert.Equal<Library.ElementId option>(current.slotChipOpenOpt, answered.slotChipOpenOpt)
                Assert.Equal<Setups.SetupSlot list>(current.elements, answered.elements)
                Assert.Equal<Selection>(current.selection, answered.selection)
                Assert.Equal<ControlCaption option>(current.kitStatus, answered.kitStatus)

            // A toggle changes only which list is open, and writes nothing.
            for current in [ sampleOpen; polarizerOpen; reclosed ] do
                Assert.Equal<Setups.SetupSlot list>(closed.elements, current.elements)
                Assert.Equal<Selection>(closed.selection, current.selection)
                Assert.Equal<ControlCaption option>(closed.kitStatus, current.kitStatus)
                Assert.Equal<Item list>(closed.kitItems, current.kitItems)
                Assert.Equal<Setups.Setup list>(closed.kitSetups, current.kitSetups)
                Assert.Equal<Setups.Setup option>(closed.activeSetupOpt, current.activeSetupOpt)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder)

            // Spec 0050 S8.4: a pick closes the open list, whether the swap created a setup, was refused or switched.
            let created = sampleOpen |> updateTableAndElementRotation (SwapSlot (held.sample, qz2Draft.id))
            Assert.Equal<Library.ElementId option>(None, created.slotChipOpenOpt)
            let qz2Setup = Assert.Single(created.kitSetups)
            Assert.Equal<Setups.Setup option>(Some qz2Setup, created.activeSetupOpt)
            Assert.Equal<ControlCaption option>(Some (ControlCaption $"Created setup '{qz2Setup.name.value}' by the swap."), created.kitStatus)

            let unheld = ItemId (Guid.Parse "27900000-0000-4000-8000-000000000099")
            let refusedOpen = created |> toggled held.sample
            Assert.Equal<Library.ElementId option>(Some held.sample, refusedOpen.slotChipOpenOpt)
            let refused = refusedOpen |> updateTableAndElementRotation (SwapSlot (held.sample, unheld))
            Assert.Equal<Library.ElementId option>(None, refused.slotChipOpenOpt)
            match refused.kitStatus with
            | Some status -> Assert.StartsWith("Not swapped", status.value)
            | None -> Assert.Fail "a refused swap must be told in the kit status"
            Assert.Equal<Setups.SetupSlot list>(created.elements, refused.elements)

            let qz = refused |> toggled held.sample |> updateTableAndElementRotation (SwapSlot (held.sample, qzDraft.id))
            Assert.Equal<Library.ElementId option>(None, qz.slotChipOpenOpt)
            Assert.Equal(2, qz.kitSetups.Length)

            let switchedOpen = qz |> toggled held.sample
            Assert.Equal<Library.ElementId option>(Some held.sample, switchedOpen.slotChipOpenOpt)
            let switched = switchedOpen |> updateTableAndElementRotation (SwapSlot (held.sample, qz2Draft.id))
            Assert.Equal<Library.ElementId option>(None, switched.slotChipOpenOpt)
            Assert.Equal<Setups.Setup option>(Some qz2Setup, switched.activeSetupOpt)
            Assert.Equal<ControlCaption option>(Some (ControlCaption $"Loaded setup '{qz2Setup.name.value}'."), switched.kitStatus))

    // Spec 0050 S8.4: the grid panel's messages edit its draft, and GenerateSetupGrid records the draft's grid.

    /// The grid's one-pick combination of the sample holding the item turned to R1 of that many degrees.
    let private plateCombination (sample : Library.ElementId) (item : ItemId) (degrees : float<deg>) : SetupGrids.GridCombination =
        let pick : SetupGrids.SlotPick =
            {
                slot = sample
                choice = plateChoice item degrees
            }
        { picks = [ pick ] }

    /// The open draft's grid over the table, failing while the panel is closed.
    let private openGrid (model : TableAndElementRotationModel) : SetupGrids.SetupGrid =
        match model.setupGrid with
        | Some grid -> grid
        | None -> failwith "the grid panel must be open"

    /// The panel opened over step 272's table, its sample trying QZ and QZ2 at 0° and 45°.
    let private platesTried (held : TemplateTable) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        model
        |> updateTableAndElementRotation OpenSetupGrid
        |> updateTableAndElementRotation (ToggleSetupGridItem (held.sample, qzDraft.id))
        |> updateTableAndElementRotation (ToggleSetupGridItem (held.sample, qz2Draft.id))
        |> updateTableAndElementRotation (AddSetupGridAngle (held.sample, BarDegrees 0.0))
        |> updateTableAndElementRotation (AddSetupGridAngle (held.sample, BarDegrees 45.0))

    [<Fact>]
    let ``the grid panel's draft lists the product of the items and angles tried in a slot, and writes nothing`` () : unit =
        withSetupKit (fun folder model ->
            let held = templateTable model
            let table = held.table
            let before = kitFileBytes folder
            let at (item : ItemId) (degrees : float<deg>) : SetupGrids.GridCombination = plateCombination held.sample item degrees
            let listsTicked (expected : SetupGrids.GridCombination list) (current : TableAndElementRotationModel) : unit =
                let grid = openGrid current
                Assert.Equal<Library.ElementId list>([ held.sample ], grid.axes |> List.map (fun axis -> axis.slot))
                Assert.Equal<SetupGrids.GridCombination list>(expected, grid.combinations)
                Assert.Equal<SetupGrids.GridCombination list>(expected, grid.ticked)
                Assert.Empty(grid.unticked)
            Assert.Equal<SetupGrids.SetupGridDraft option>(None, table.setupGridOpt)
            Assert.Equal<SetupGrids.SetupGrid option>(None, table.setupGrid)

            let opened = table |> updateTableAndElementRotation OpenSetupGrid
            Assert.Equal<SetupGrids.SetupGridDraft option>(Some SetupGrids.SetupGridDraft.empty, opened.setupGridOpt)
            Assert.Empty((openGrid opened).axes)

            let tried = platesTried held table
            tried |> listsTicked [ at qzDraft.id 0.0<deg>; at qzDraft.id 45.0<deg>; at qz2Draft.id 0.0<deg>; at qz2Draft.id 45.0<deg> ]

            let at0 = tried |> updateTableAndElementRotation (RemoveSetupGridAngle (held.sample, Angle.degree 45.0<deg>))
            at0 |> listsTicked [ at qzDraft.id 0.0<deg>; at qz2Draft.id 0.0<deg> ]
            let qzAlone = at0 |> updateTableAndElementRotation (ToggleSetupGridItem (held.sample, qz2Draft.id))
            qzAlone |> listsTicked [ at qzDraft.id 0.0<deg> ]
            // The table's angle convention: −45° on the bar is R1 315°.
            let wrapped = qzAlone |> updateTableAndElementRotation (AddSetupGridAngle (held.sample, BarDegrees (-45.0)))
            wrapped |> listsTicked [ at qzDraft.id 0.0<deg>; at qzDraft.id 315.0<deg> ]

            for current in [ opened; tried; at0; qzAlone; wrapped ] do
                Assert.Equal<Setups.SetupSlot list>(table.elements, current.elements)
                Assert.Equal<Setups.Setup list>(table.kitSetups, current.kitSetups)
                Assert.Equal<Experiments.ExperimentCollection>(table.experimentCollection, current.experimentCollection)
                Assert.Equal<ControlCaption option>(table.kitStatus, current.kitStatus)
            Assert.Empty(setupFilesIn folder)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder))

    [<Fact>]
    let ``unticking a combination in the panel leaves it out, and closing the panel forgets the draft`` () : unit =
        withSetupKit (fun folder model ->
            let held = templateTable model
            let before = kitFileBytes folder
            let at (item : ItemId) (degrees : float<deg>) : SetupGrids.GridCombination = plateCombination held.sample item degrees
            let tried = platesTried held held.table
            let all = [ at qzDraft.id 0.0<deg>; at qzDraft.id 45.0<deg>; at qz2Draft.id 0.0<deg>; at qz2Draft.id 45.0<deg> ]
            Assert.Equal<SetupGrids.GridCombination list>(all, (openGrid tried).ticked)

            let unticked = tried |> updateTableAndElementRotation (ToggleSetupGridCombination (at qz2Draft.id 0.0<deg>))
            let leftOut = openGrid unticked
            Assert.Equal<SetupGrids.GridCombination list>(all, leftOut.combinations)
            Assert.Equal<SetupGrids.GridCombination list>([ at qzDraft.id 0.0<deg>; at qzDraft.id 45.0<deg>; at qz2Draft.id 45.0<deg> ], leftOut.ticked)
            Assert.Equal<SetupGrids.GridCombination list>([ at qz2Draft.id 0.0<deg> ], leftOut.unticked)
            let reticked = unticked |> updateTableAndElementRotation (ToggleSetupGridCombination (at qz2Draft.id 0.0<deg>))
            Assert.Equal<SetupGrids.GridCombination list>(all, (openGrid reticked).ticked)
            Assert.Empty((openGrid reticked).unticked)

            // Opening the panel again keeps the draft already open.
            let reopened = unticked |> updateTableAndElementRotation OpenSetupGrid
            Assert.Equal<SetupGrids.SetupGridDraft option>(unticked.setupGridOpt, reopened.setupGridOpt)

            let closed = unticked |> updateTableAndElementRotation CloseSetupGrid
            Assert.Equal<SetupGrids.SetupGridDraft option>(None, closed.setupGridOpt)
            Assert.Equal<SetupGrids.SetupGrid option>(None, closed.setupGrid)
            // With the panel closed, an edit leaves the model as it is.
            let closedEdits : TableAndElementRotationMsg list =
                [
                    ToggleSetupGridItem (held.sample, qzDraft.id)
                    AddSetupGridAngle (held.sample, BarDegrees 30.0)
                    RemoveSetupGridAngle (held.sample, Angle.degree 0.0<deg>)
                    ToggleSetupGridCombination (at qzDraft.id 0.0<deg>)
                ]
            for edit in closedEdits do
                Assert.Same(closed, closed |> updateTableAndElementRotation edit)

            let fresh = closed |> updateTableAndElementRotation OpenSetupGrid
            Assert.Equal<SetupGrids.SetupGridDraft option>(Some SetupGrids.SetupGridDraft.empty, fresh.setupGridOpt)
            Assert.Empty((openGrid fresh).axes)
            Assert.Empty((openGrid fresh).unticked)
            Assert.Equal<Map<string, FileByteContent>>(before, kitFileBytes folder))

    [<Fact>]
    let ``the panel's grid generates its ticked setups through GenerateSetupGrid and stays open`` () : unit =
        withSetupKit (fun folder model ->
            let held = templateTable model
            let at (item : ItemId) (degrees : float<deg>) : SetupGrids.GridCombination = plateCombination held.sample item degrees
            let chosen = held.table |> updateTableAndElementRotation (ExpChooseElement held.polarizer)
            Assert.True(chosen.experimentCollection.canCommit)
            let drafted = platesTried held chosen |> updateTableAndElementRotation (ToggleSetupGridCombination (at qz2Draft.id 45.0<deg>))
            let grid = openGrid drafted
            Assert.Equal<SetupGrids.GridCombination list>([ at qzDraft.id 0.0<deg>; at qzDraft.id 45.0<deg>; at qz2Draft.id 0.0<deg> ], grid.ticked)

            let generated = drafted |> updateTableAndElementRotation (GenerateSetupGrid grid)

            Assert.Equal<ControlCaption option>(Some (ControlCaption "Generated 3 setups (3 new) and 3 experiments."), generated.kitStatus)
            let names = [ "LP1-(QZ#0)-LPd"; "LP1-(QZ#45)-LPd"; "LP1-(QZ2#0)-LPd" ]
            Assert.Equal<string list>(names |> List.map (fun name -> $"{name}.setup-json"), setupFilesIn folder)
            Assert.Equal<string list>([ "E1.experiment-json"; "E2.experiment-json"; "E3.experiment-json" ], experimentFilesIn folder)
            let setups = generated.kit.proxy.readSetups () |> kitValue
            Assert.Equal<SetupName list>(names |> List.map SetupName, setups |> List.map (fun setup -> setup.name))
            Assert.Equal<Setups.Setup list>(setups, generated.kitSetups)
            let experiments = generated.kit.proxy.readExperiments () |> kitValue
            Assert.Equal<Experiments.ExperimentId list>(
                [ Experiments.ExperimentId 1; Experiments.ExperimentId 2; Experiments.ExperimentId 3 ],
                experiments |> List.map (fun experiment -> experiment.id))
            let firstRevisionOf (name : string) : SetupReference = { name = SetupName name; revision = SetupRevision.first }
            Assert.Equal<SetupReference list>(names |> List.map firstRevisionOf, experiments |> List.map (fun experiment -> experiment.setup))
            Assert.Equal<Experiments.Experiment list>(experiments, generated.experimentCollection.experiments)

            // The draft stays open over the same grid, so it can be edited and generated again.
            Assert.Equal<SetupGrids.SetupGridDraft option>(drafted.setupGridOpt, generated.setupGridOpt)
            Assert.Equal<SetupGrids.SetupGrid option>(Some grid, generated.setupGrid)
            Assert.Equal<Setups.SetupSlot list>(chosen.elements, generated.elements)
            Assert.Equal<Setups.Setup option>(chosen.activeSetupOpt, generated.activeSetupOpt))
