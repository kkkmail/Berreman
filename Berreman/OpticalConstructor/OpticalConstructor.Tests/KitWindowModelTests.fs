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
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

module KitWindowModelTests =

    let private titleAuthor : KitAuthor = KitAuthor "test"

    let private openedValue (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private kitValue (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// Runs over an uncreated `Optics QZ` kit folder inside a fresh GUID-named folder, deleted at the end.
    let private withTitledFolder (run : KitFolder -> CatalogueImportContext -> unit) : unit =
        let root = FolderName (Path.Combine(System.AppContext.BaseDirectory, "KitWindowModelTests", Guid.NewGuid().ToString("N")))
        let location = FolderName (Path.Combine(root.value, "Optics QZ"))
        let folder = KitFolder location
        let imports = CatalogueImportContext.create (BuiltinCatalogue.freshStores ()).proxy
        try run folder imports
        finally
            if Directory.Exists root.value then Directory.Delete(root.value, true)

    /// Loads the opened kit into a fresh Main scene; the scene's scratch kit and the opened kit are closed at the end.
    let private withLoadedScene (opened : OpenedKit) (run : TableAndElementRotationModel -> unit) : unit =
        let scene = initMain () |> kitValue
        try run (scene.loadKit opened)
        finally
            scene.kit.close () |> kitValue
            opened.library.kit.close () |> kitValue

    /// The experiment `E1` a kit holds before it is opened read-only.
    let private storedExperiment : Experiments.Experiment =
        {
            id = Experiments.ExperimentId 1
            setup = []
            varied = None
            measurement = Experiments.CaptureTransmitted
            range = Experiments.VariableRange.forVariable Experiments.VaryR1
            dataFileOpt = None
            sourceWaveLengthOpt = None
        }

    /// A shell that records every folder it is asked to show and every window close it is asked for.
    let private recordingShell (shown : ResizeArray<FolderName>) (closes : ResizeArray<unit>) : KitWindowShell =
        {
            KitWindowShell.explorer = ExplorerProxy.createInMemory shown.Add
            requestClose = fun () -> closes.Add ()
        }

    /// The kit folder's lock file.
    let private lockFile (folder : KitFolder) : string = Path.Combine(folder.value.value, ".oc", "lock")

    /// Every file under the kit folder, with its bytes.
    let private folderBytes (folder : KitFolder) : Map<FileName, FileByteContent> =
        Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun path -> FileName path, FileByteContent (File.ReadAllBytes path))
        |> Map.ofArray

    [<Fact>]
    let ``the window title names the kit, its folder and the constructor`` () : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                Assert.Equal<KitFolder option>(Some folder, model.kitFolderOpt)
                Assert.Equal($"Optics QZ — {folder.value.value} — Optical Constructor — Main", model.kitWindowTitle.value)
                let inverse =
                    {
                        model with constructorMode = InverseConstructor
                    }
                Assert.Equal($"Optics QZ — {folder.value.value} — Optical Constructor — Inverse", inverse.kitWindowTitle.value)))

    [<Fact>]
    let ``a kit opened read-only is marked in the title`` () : unit =
        withTitledFolder (fun folder imports ->
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            created.library.kit.close () |> kitValue
            let opened = openKitAt folder ReadOnlyKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                Assert.Equal($"Optics QZ (read-only) — {folder.value.value} — Optical Constructor — Main", model.kitWindowTitle.value)))

    [<Fact>]
    let ``a scene that opened no folder is titled by its kit alone`` () : unit =
        let model = initMain () |> kitValue
        try
            Assert.Equal<KitFolder option>(None, model.kitFolderOpt)
            Assert.Equal("Scratch kit — Optical Constructor — Main", model.kitWindowTitle.value)
        finally model.kit.close () |> kitValue

    [<Fact>]
    let ``a kit opened read-only refuses every write message and writes nothing`` () : unit =
        withTitledFolder (fun folder imports ->
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            try created.library.kit.proxy.writeExperiment storedExperiment |> kitValue
            finally created.library.kit.close () |> kitValue
            let opened = openKitAt folder ReadOnlyKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                let placed = model |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
                let polarizer = placed.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
                let ready = placed |> updateTableAndElementRotation (ExpChooseElement polarizer.id)
                Assert.True(ready.experimentCollection.canCommit, "the draft must be ready to commit")
                Assert.Equal<Experiments.Experiment list>([ storedExperiment ], ready.experimentCollection.experiments)
                let dataFile = (FileName "measured.csv").combine folder.value
                File.WriteAllText(dataFile.value, "x,intensity\n0,1\n45,0.5\n")
                let before = folderBytes folder
                let refused =
                    [
                        ready |> updateTableAndElementRotation ExpCommit
                        ready |> updateTableAndElementRotation (ExpRemove storedExperiment.id)
                        ready |> updateTableAndElementRotation (AttachDataFileTo (storedExperiment.id, dataFile))
                        ready |> updateTableAndElementRotation (SceneSetName (ControlCaption "Bench")) |> updateTableAndElementRotation SceneSave
                    ]
                for answer in refused do
                    Assert.Equal<Experiments.ExperimentCollection>(ready.experimentCollection, answer.experimentCollection)
                    Assert.Equal<Scene.SceneName list>(ready.savedScenes, answer.savedScenes)
                    Assert.Equal<ControlCaption option>(Some (ControlCaption "The kit is open read-only: nothing is saved."), answer.kitStatus)
                Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)))

    [<Fact>]
    let ``a kit's access decides whether its writes are offered`` () : unit =
        withTitledFolder (fun folder imports ->
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene created (fun model -> Assert.Equal<ControlAvailability>(ControlOffered, model.kitWrites))
            let opened = openKitAt folder ReadOnlyKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model -> Assert.Equal<ControlAvailability>(ControlWithheld, model.kitWrites)))

    [<Fact>]
    let ``switching the constructor keeps the kit, the table and the experiments`` () : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                let placed = model |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
                let polarizer = placed.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
                let forward =
                    placed
                    |> updateTableAndElementRotation (ExpChooseElement polarizer.id)
                    |> updateTableAndElementRotation ExpCommit
                    |> updateTableAndElementRotation (SelectBay BayNames.experiments)
                Assert.Equal(1, List.length forward.experimentCollection.experiments)
                Assert.Equal<Ribbon.BayName>(BayNames.experiments, forward.ribbon)
                Assert.Equal<ConstructorMode>(ForwardConstructor, forward.constructorMode)
                let inverse = forward |> updateTableAndElementRotation (SwitchConstructorMode InverseConstructor)
                Assert.Equal<ConstructorMode>(InverseConstructor, inverse.constructorMode)
                Assert.Equal<KitContext>(forward.kit, inverse.kit)
                Assert.Equal<TestElement list>(forward.elements, inverse.elements)
                Assert.Equal<Experiments.ExperimentCollection>(forward.experimentCollection, inverse.experimentCollection)
                Assert.Equal<Ribbon.BayName>(forward.ribbon, inverse.ribbon)
                Assert.EndsWith("— Inverse", inverse.kitWindowTitle.value)
                let back = inverse |> updateTableAndElementRotation (SwitchConstructorMode ForwardConstructor)
                Assert.Equal<TableAndElementRotationModel>(forward, back)))

    [<Fact>]
    let ``Show in Explorer asks the explorer for the kit's folder`` () : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun loaded ->
                let shown = ResizeArray<FolderName> ()
                let closes = ResizeArray<unit> ()
                let model =
                    {
                        loaded with kitShell = recordingShell shown closes
                    }
                let answer = model |> updateTableAndElementRotation ShowKitInExplorer
                Assert.Equal<FolderName list>([ folder.value ], List.ofSeq shown)
                Assert.Empty(closes)
                Assert.Equal<TableAndElementRotationModel>(model, answer)))

    [<Fact>]
    let ``a kit folder the explorer cannot show is told in the kit status`` () : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun loaded ->
                let refusing : KitWindowShell =
                    {
                        explorer =
                            {
                                ExplorerProxy.showFolder = fun _ -> Error (MissingFolderErr "gone")
                            }
                        requestClose = ignore
                    }
                let model =
                    {
                        loaded with kitShell = refusing
                    }
                let answer = model |> updateTableAndElementRotation ShowKitInExplorer
                match answer.kitStatus with
                | Some status ->
                    Assert.Contains("gone", status.value)
                    Assert.StartsWith("The kit folder could not be shown — ", status.value)
                | None -> Assert.Fail "the refusal was not told in the kit status"
                Assert.Equal<TableAndElementRotationModel>({ model with kitStatus = answer.kitStatus }, answer)))

    [<Fact>]
    let ``a scene that opened no folder shows nothing in Explorer`` () : unit =
        let scene = initMain () |> kitValue
        try
            let shown = ResizeArray<FolderName> ()
            let closes = ResizeArray<unit> ()
            let model =
                {
                    scene with kitShell = recordingShell shown closes
                }
            let answer = model |> updateTableAndElementRotation ShowKitInExplorer
            Assert.Empty(shown)
            Assert.Empty(closes)
            Assert.Equal<TableAndElementRotationModel>(model, answer)
        finally scene.kit.close () |> kitValue

    [<Fact>]
    let ``Close kit asks the window to close and changes nothing else`` () : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun loaded ->
                let shown = ResizeArray<FolderName> ()
                let closes = ResizeArray<unit> ()
                let model =
                    {
                        loaded with kitShell = recordingShell shown closes
                    }
                let answer = model |> updateTableAndElementRotation CloseKit
                Assert.Equal(1, closes.Count)
                Assert.Empty(shown)
                Assert.Equal<TableAndElementRotationModel>(model, answer)
                Assert.True(File.Exists(lockFile folder), "the kit stays locked until its window releases it"))
            Assert.False(File.Exists(lockFile folder)))
