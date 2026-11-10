namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
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
/// and a commit records the table as the active setup its experiment references (spec 0050 S8.2; 001 §P).
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
