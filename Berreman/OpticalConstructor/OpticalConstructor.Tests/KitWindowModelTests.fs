namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
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
