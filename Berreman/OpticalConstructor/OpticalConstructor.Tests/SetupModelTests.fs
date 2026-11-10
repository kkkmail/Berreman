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

/// The constructor's setup messages over a kit opened read-write: CreateSetup records the table as a kit setup and
/// ActivateSetup draws one, the window keeping its view, snap mode and pivot (spec 0050 S8.2; 001 §P).
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
