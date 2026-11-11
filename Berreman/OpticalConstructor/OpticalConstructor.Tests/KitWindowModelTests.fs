namespace OpticalConstructor.Tests

open System
open System.IO
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitArchiveStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Domain.Setups

module KitWindowModelTests =

    let private titleAuthor : KitAuthor = KitAuthor "test"

    let private openedValue (result : Result<'Value, KitLibraryError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private kitValue (result : Result<'Value, KitError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    let private archiveValue (result : Result<'Value, KitArchiveError>) : 'Value =
        result |> Result.defaultWith (fun error -> failwith error.describe)

    /// Runs over a fresh GUID-named fact folder and an uncreated `Optics QZ` kit folder inside it, deleted at the end.
    let private withFactFolder (run : FolderName -> KitFolder -> CatalogueImportContext -> unit) : unit =
        let root = FolderName (Path.Combine(System.AppContext.BaseDirectory, "KitWindowModelTests", Guid.NewGuid().ToString("N")))
        let location = FolderName (Path.Combine(root.value, "Optics QZ"))
        let folder = KitFolder location
        let imports = CatalogueImportContext.create (BuiltinCatalogue.freshStores ()).proxy
        try run root folder imports
        finally
            if Directory.Exists root.value then Directory.Delete(root.value, true)

    /// Runs over an uncreated `Optics QZ` kit folder inside a fresh GUID-named folder, deleted at the end.
    let private withTitledFolder (run : KitFolder -> CatalogueImportContext -> unit) : unit =
        withFactFolder (fun _ folder imports -> run folder imports)

    /// Loads the opened kit into a fresh Main scene; the scene's scratch kit and the opened kit are closed at the end.
    let private withLoadedScene (opened : OpenedKit) (run : TableAndElementRotationModel -> 'Outcome) : 'Outcome =
        let scene = initMain () |> kitValue
        try run (scene.loadKit opened)
        finally
            scene.kit.close () |> kitValue
            opened.library.kit.close () |> kitValue

    /// The dark line's setup, holding no slot (spec 0050 S8.2).
    let private darkSetup : Setups.Setup =
        {
            name = SetupName "Dark"
            revision = SetupRevision.first
            slots = []
            history = []
        }

    /// The experiment `E1` a kit holds before it is opened read-only.
    let private storedExperiment : Experiments.Experiment =
        {
            id = Experiments.ExperimentId 1
            name = (Experiments.ExperimentId 1).defaultName
            setup = darkSetup.reference
            scan = Experiments.ExperimentScan.darkLine Experiments.CaptureTransmitted
            role = Experiments.ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataFileOpt = None
        }

    /// A shell that records every folder it is asked to show and every window close it is asked for.
    let private recordingShell (shown : ResizeArray<FolderName>) (closes : ResizeArray<unit>) : KitWindowShell =
        {
            KitWindowShell.inert with
                explorer = ExplorerProxy.createInMemory shown.Add
                requestClose = fun () -> closes.Add ()
        }

    /// A shell over the real archive whose job host runs each send at once, recording its progress and its end as the
    /// messages the window would receive, in order (spec 0050 S6).
    let private sendingShell (messages : ResizeArray<TableAndElementRotationMsg>) : KitWindowShell =
        {
            KitWindowShell.inert with
                archive = KitArchiveProxy.create ()
                archiveJobs = KitArchiveJobHost.immediate (SendKitProgressed >> messages.Add) (SendKitEnded >> messages.Add)
        }

    /// Passes the recorded messages through the update in order (spec 0050 S6).
    let private passedThrough (messages : TableAndElementRotationMsg list) (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        messages |> List.fold (fun current msg -> updateTableAndElementRotation msg current) model

    /// The zip a fact sends to: `exports/Optics QZ.zip` under the fact's folder, beside the kit and never inside it.
    /// Its folder is made, as a save dialog's folder exists (spec 0050 S6).
    let private exportsZipUnder (factFolder : FolderName) : FileName =
        let exports = factFolder.combine (FolderName "exports")
        Directory.CreateDirectory exports.value |> ignore
        (FileName "Optics QZ.zip").combine exports

    /// The kit folder's lock file.
    let private lockFile (folder : KitFolder) : string = Path.Combine(folder.value.value, ".oc", "lock")

    /// Every file under the kit folder, with its bytes.
    let private folderBytes (folder : KitFolder) : Map<FileName, FileByteContent> =
        Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun path -> FileName path, FileByteContent (File.ReadAllBytes path))
        |> Map.ofArray

    /// Commits an experiment over a pre-bound linear polarizer through the model, which records it in the kit as `E1`.
    let private committedPolarizerExperiment (model : TableAndElementRotationModel) : TableAndElementRotationModel =
        let placed = model |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
        let polarizer = placed.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
        placed
        |> updateTableAndElementRotation (ExpChooseElement polarizer.id)
        |> updateTableAndElementRotation ExpCommit

    /// Another writer's `E2`: the committed experiment given `id = ExperimentId 2` and its name (spec 0050 S5.3, S9.1).
    let private writeE2 (writer : KitProxy) (committed : Experiments.Experiment) : unit =
        let e2 =
            {
                committed with
                    id = Experiments.ExperimentId 2
                    name = (Experiments.ExperimentId 2).defaultName
            }
        writer.writeExperiment e2 |> kitValue

    /// Another writer's kit file, renamed (spec 0050 S5.3, step 125).
    let private renameKit (writer : KitProxy) (name : KitName) : unit =
        let kitFile = writer.readKitFile () |> kitValue
        let renamed =
            {
                kitFile with name = name
            }
        writer.writeKitFile renamed |> kitValue

    /// Opens the kit read-write and commits `E1` through the model; then another writer, `KitProxy.create` over the
    /// kit's folder with no lock (step 126), writes `E2` and names the kit `Renamed`. Runs over the writer and the model.
    let private withOtherWritersChanges (run : KitProxy -> TableAndElementRotationModel -> unit) : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun loaded ->
                let committed = committedPolarizerExperiment loaded
                let writer = KitProxy.create folder
                writeE2 writer (List.exactlyOne committed.experimentCollection.experiments)
                renameKit writer (KitName "Renamed")
                run writer committed))

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
            // Spec 0050 S8.2: a setup recorded while the kit was read-write is active when SaveSetup is refused.
            let storedBench = SetupName "Stored bench"
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene created (fun model ->
                created.library.kit.proxy.writeExperiment storedExperiment |> kitValue
                let recorded =
                    model
                    |> updateTableAndElementRotation (SetSetupName (ControlCaption "Stored bench"))
                    |> updateTableAndElementRotation CreateSetup
                Assert.Equal<SetupName list>([ storedBench ], recorded.kitSetups |> List.map (fun setup -> setup.name)))
            let opened = openKitAt folder ReadOnlyKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                let active = model |> updateTableAndElementRotation (ActivateSetup storedBench)
                let placed = active |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
                let polarizer = placed.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
                let ready = placed |> updateTableAndElementRotation (ExpChooseElement polarizer.id)
                Assert.True(ready.experimentCollection.canCommit, "the draft must be ready to commit")
                Assert.Equal<Experiments.Experiment list>([ storedExperiment ], ready.experimentCollection.experiments)
                match ready.activeSetupOpt with
                | Some setup ->
                    Assert.Equal<SetupName>(storedBench, setup.name)
                    Assert.NotEqual<SetupSlot list>(setup.slots, ready.elements)
                | None -> Assert.Fail "SaveSetup must be refused with a setup active"
                let dataFile = (FileName "measured.csv").combine folder.value
                File.WriteAllText(dataFile.value, "x,intensity\n0,1\n45,0.5\n")
                let before = folderBytes folder
                let refused =
                    [
                        ready |> updateTableAndElementRotation ExpCommit
                        ready |> updateTableAndElementRotation (ExpRemove storedExperiment.id)
                        ready |> updateTableAndElementRotation (AttachDataFileTo (storedExperiment.id, dataFile))
                        ready |> updateTableAndElementRotation (SetSetupName (ControlCaption "Bench")) |> updateTableAndElementRotation CreateSetup
                        ready |> updateTableAndElementRotation SaveSetup
                        // Spec 0050 S8.3: a typed template is refused as SaveSetup is.
                        ready
                        |> updateTableAndElementRotation (SetNamingTemplateText (ControlCaption "{obj1}"))
                        |> updateTableAndElementRotation SaveNamingTemplate
                        // Spec 0050 S8.4: a recorded experiment is refused as a commit is.
                        ready |> updateTableAndElementRotation RecordExperiment
                    ]
                for answer in refused do
                    Assert.Equal<Experiments.ExperimentCollection>(ready.experimentCollection, answer.experimentCollection)
                    Assert.Equal<Setups.Setup list>(ready.kitSetups, answer.kitSetups)
                    Assert.Equal<KitContext>(ready.kit, answer.kit)
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
                Assert.Equal<Setups.SetupSlot list>(forward.elements, inverse.elements)
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
                        KitWindowShell.inert with
                            explorer =
                                {
                                    ExplorerProxy.showFolder = fun _ -> Error (MissingFolderErr "gone")
                                }
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

    [<Fact>]
    let ``files another writer changed are offered, and the window's own writes are not`` () : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun loaded ->
                let committed = committedPolarizerExperiment loaded
                Assert.Equal(1, List.length committed.experimentCollection.experiments)
                let own = committed |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
                Assert.Equal<TableAndElementRotationModel>(committed, own)
                let writer = KitProxy.create folder
                writeE2 writer (List.exactlyOne committed.experimentCollection.experiments)
                renameKit writer (KitName "Renamed")
                let offered = committed |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
                Assert.Equal<FileName list>([ FileName ".kit-json"; FileName "E2.experiment-json" ], offered.kitChanges)
                Assert.Equal<TableAndElementRotationModel>({ committed with kitChanges = offered.kitChanges }, offered)))

    [<Fact>]
    let ``Reload reads the kit's files in and clears the offer`` () : unit =
        withOtherWritersChanges (fun writer committed ->
            let bench =
                Setups.Setup.create (SetupName "Bench") [] []
                |> Result.defaultWith (fun error -> failwith error.describe)
            writer.writeSetup bench |> kitValue
            // Spec 0050 S8.3: the other writer also saves a naming template, which Reload shows in the field.
            let kitFile = writer.readKitFile () |> kitValue
            let templated =
                {
                    kitFile with namingTemplate = NamingTemplate "{obj1}"
                }
            writer.writeKitFile templated |> kitValue
            let offered = committed |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
            Assert.NotEmpty(offered.kitChanges)
            Assert.Equal<ControlCaption>(NamingTemplate.standard.caption, offered.namingTemplateText)
            let reloaded = offered |> updateTableAndElementRotation ReloadKitChanges
            Assert.Equal<ControlCaption>(ControlCaption "{obj1}", reloaded.namingTemplateText)
            let held = writer.readExperiments () |> kitValue
            Assert.Equal<Experiments.ExperimentId list>(
                [ Experiments.ExperimentId 1; Experiments.ExperimentId 2 ],
                held |> List.map (fun experiment -> experiment.id))
            Assert.Equal<Experiments.ExperimentCollection>(Experiments.ExperimentCollection.ofLoaded held, reloaded.experimentCollection)
            Assert.Equal<KitName>(KitName "Renamed", reloaded.kit.kitFile.name)
            Assert.StartsWith("Renamed", reloaded.kitWindowTitle.value)
            Assert.Equal<KitContext>({ offered.kit with kitFile = reloaded.kit.kitFile }, reloaded.kit)
            // The commit recorded the table as `Setup 1` (spec 0050 S8.2); the other writer's `Bench` arrives on reload.
            Assert.Equal<SetupName list>([ SetupName "Setup 1" ], offered.kitSetups |> List.map (fun setup -> setup.name))
            Assert.Equal<Setups.Setup list>(bench :: offered.kitSetups, reloaded.kitSetups)
            Assert.Empty(reloaded.kitChanges)
            Assert.Equal<ControlCaption option>(None, reloaded.kitStatus)
            let again = reloaded |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
            Assert.Empty(again.kitChanges))

    [<Fact>]
    let ``Keep mine keeps the model and clears the offer until the next change`` () : unit =
        withOtherWritersChanges (fun writer committed ->
            let offered = committed |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
            Assert.NotEmpty(offered.kitChanges)
            let kept = offered |> updateTableAndElementRotation KeepMyKitChanges
            Assert.Equal<TableAndElementRotationModel>({ offered with kitChanges = [] }, kept)
            Assert.Equal<Experiments.ExperimentCollection>(committed.experimentCollection, kept.experimentCollection)
            Assert.Equal<KitName>(KitName "Optics QZ", kept.kit.kitFile.name)
            let again = kept |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
            Assert.Empty(again.kitChanges)
            renameKit writer (KitName "Renamed again")
            let reoffered = again |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
            Assert.Equal<FileName list>([ FileName ".kit-json" ], reoffered.kitChanges))

    [<Fact>]
    let ``a requested check with nothing changed says so, and an activation check stays silent`` () : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                let requested = model |> updateTableAndElementRotation (CheckKitChanges KitCheckOnRequest)
                Assert.Equal<ControlCaption option>(Some (ControlCaption "No kit file changed outside the app."), requested.kitStatus)
                Assert.Equal<TableAndElementRotationModel>({ model with kitStatus = requested.kitStatus }, requested)
                let activated = model |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
                Assert.Equal<ControlCaption option>(None, activated.kitStatus)
                Assert.Equal<TableAndElementRotationModel>(model, activated)))

    [<Fact>]
    let ``a kit opened read-only is offered changes and reloads them`` () : unit =
        withTitledFolder (fun folder imports ->
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            let first =
                withLoadedScene created (fun loaded ->
                    (committedPolarizerExperiment loaded).experimentCollection.experiments |> List.exactlyOne)
            let opened = openKitAt folder ReadOnlyKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                Assert.Equal<Experiments.ExperimentId list>(
                    [ first.id ],
                    model.experimentCollection.experiments |> List.map (fun experiment -> experiment.id))
                let writer = KitProxy.create folder
                writeE2 writer first
                let before = folderBytes folder
                let offered = model |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
                Assert.Equal<FileName list>([ FileName "E2.experiment-json" ], offered.kitChanges)
                let reloaded = offered |> updateTableAndElementRotation ReloadKitChanges
                let held = writer.readExperiments () |> kitValue
                Assert.Equal<Experiments.ExperimentId list>(
                    [ Experiments.ExperimentId 1; Experiments.ExperimentId 2 ],
                    reloaded.experimentCollection.experiments |> List.map (fun experiment -> experiment.id))
                Assert.Equal<Experiments.Experiment list>(held, reloaded.experimentCollection.experiments)
                Assert.Equal<KitAccess>(ReadOnlyKitAccess, reloaded.kit.access)
                Assert.Empty(reloaded.kitChanges)
                Assert.Equal<ControlCaption option>(None, reloaded.kitStatus)
                let again = reloaded |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
                Assert.Empty(again.kitChanges)
                Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)))

    [<Fact>]
    let ``Save template writes the typed naming template into the kit file`` () : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                // Spec 0050 S8.3: the lab's template (S12.1), its source and analyzer aliased.
                let template = NamingTemplate "{source:CPL1=CPL}-({obj1}#{phi1})-{analyzer:CPL2=CPL}"
                Assert.Equal<ControlCaption>(NamingTemplate.standard.caption, model.namingTemplateText)
                let saved =
                    model
                    |> updateTableAndElementRotation (SetNamingTemplateText template.caption)
                    |> updateTableAndElementRotation SaveNamingTemplate
                Assert.Equal<NamingTemplate>(template, saved.kit.kitFile.namingTemplate)
                let stored = saved.kit.proxy.readKitFile () |> kitValue
                Assert.Equal<NamingTemplate>(template, stored.namingTemplate)
                Assert.Equal<KitFile>(saved.kit.kitFile, stored)
                Assert.Equal<ControlCaption>(template.caption, saved.namingTemplateText)
                Assert.Equal<ControlCaption option>(Some (ControlCaption "Saved the naming template."), saved.kitStatus)
                let checkedKit = saved |> updateTableAndElementRotation (CheckKitChanges KitCheckOnRequest)
                Assert.Empty(checkedKit.kitChanges)
                Assert.Equal<ControlCaption option>(Some (ControlCaption "No kit file changed outside the app."), checkedKit.kitStatus)))

    [<Fact>]
    let ``a naming template that does not parse, or the kit's own, is told and writes nothing`` () : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                let kitFilePath = KitFile.fileName.combine folder.value
                let kitFileBytes () : FileByteContent = FileByteContent (File.ReadAllBytes kitFilePath.value)
                let before = kitFileBytes ()
                Assert.Equal<NamingTemplate>(NamingTemplate.standard, model.kit.kitFile.namingTemplate)
                let unparsed =
                    model
                    |> updateTableAndElementRotation (SetNamingTemplateText (ControlCaption "{sauce}"))
                    |> updateTableAndElementRotation SaveNamingTemplate
                match unparsed.kitStatus with
                | Some status ->
                    Assert.StartsWith("Not saved", status.value)
                    Assert.Contains("the naming template is not valid", status.value)
                | None -> Assert.Fail "a template that does not parse must be told in the kit status"
                Assert.Equal<KitContext>(model.kit, unparsed.kit)
                Assert.Equal<FileByteContent>(before, kitFileBytes ())
                let unchanged =
                    model
                    |> updateTableAndElementRotation (SetNamingTemplateText NamingTemplate.standard.caption)
                    |> updateTableAndElementRotation SaveNamingTemplate
                Assert.Equal<ControlCaption option>(Some (ControlCaption "The naming template is unchanged."), unchanged.kitStatus)
                Assert.Equal<KitContext>(model.kit, unchanged.kit)
                Assert.Equal<FileByteContent>(before, kitFileBytes ())))

    let private windowCpl1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "6e2a4c81-9d07-4b3f-a5e6-1c8f0b2d7a49")
                label = ItemLabel "CPL1"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.circularPolarizerModelId)
                dial = DialMounted DialPositive
            }
        draft.toItem

    let private windowLp1 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "d05b7f3a-2c64-4e91-8b2d-7a3e9c1f6058")
                label = ItemLabel "LP1"
                model = LibraryModelChoice (BuiltinCatalogue.itemModel ItemModelIds.realLinearPolarizerModelId)
                dial = DialMounted DialNegative
            }
        draft.toItem

    [<Fact>]
    let ``a loaded kit's items are the window's, and Reload reads an item another writer wrote`` () : unit =
        withTitledFolder (fun folder imports ->
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            try created.library.kit.proxy.writeItem windowCpl1 |> kitValue
            finally created.library.kit.close () |> kitValue
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                Assert.Equal<Item list>([ windowCpl1 ], model.kitItems)
                let writer = KitProxy.create folder
                writer.writeItem windowLp1 |> kitValue
                let offered = model |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
                Assert.Equal<FileName list>([ FileName "LP1.item-json" ], offered.kitChanges)
                Assert.Equal<Item list>([ windowCpl1 ], offered.kitItems)
                let reloaded = offered |> updateTableAndElementRotation ReloadKitChanges
                Assert.Equal<Item list>([ windowCpl1; windowLp1 ], reloaded.kitItems)
                Assert.Empty(reloaded.kitChanges)
                Assert.Equal<ControlCaption option>(None, reloaded.kitStatus)))

    [<Fact>]
    let ``Send kit zips the kit's folder and tells how many files it sent`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun loaded ->
                let messages = ResizeArray<TableAndElementRotationMsg> ()
                let model =
                    {
                        loaded with kitShell = sendingShell messages
                    }
                let zip = exportsZipUnder factFolder
                Assert.True(File.Exists(lockFile folder), "the kit holds its lock while it is sent")
                let sent = model |> updateTableAndElementRotation (SendKit zip) |> passedThrough (List.ofSeq messages)
                Assert.True(File.Exists zip.value, "the zip must be written where it was asked for")
                let entries = KitArchiveProxy.create().listZipEntries zip |> archiveValue
                Assert.Contains(KitTreeFile (KitRelativePath ".kit-json"), entries)
                Assert.Contains(KitTreeFolder (KitRelativePath "data"), entries)
                Assert.Contains(KitTreeFolder (KitRelativePath "library"), entries)
                let cacheEntries =
                    entries
                    |> List.choose (fun entry ->
                        match entry.path.pathRole with
                        | KitAppCachePath -> Some entry
                        | KitContentPath -> None)
                Assert.Empty(cacheEntries)
                let zippedFiles = KitTreeEntry.filePathsOf entries |> List.length
                Assert.Equal<ControlCaption option>(Some (ControlCaption $"The kit was sent: {zippedFiles} file(s) zipped."), sent.kitStatus)
                Assert.Equal<KitArchiveJobState>(KitArchiveJobState.idle, sent.kitSend)))

    [<Fact>]
    let ``a running send shows each zipped file as progress`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun loaded ->
                let messages = ResizeArray<TableAndElementRotationMsg> ()
                let model =
                    {
                        loaded with kitShell = sendingShell messages
                    }
                let started = model |> updateTableAndElementRotation (SendKit (exportsZipUnder factFolder))
                Assert.True(started.kitSend.job.isRunning, "the send must run once it starts")
                Assert.Equal<string option>(None, started.kitSend.job.progressText)
                let progresses =
                    messages
                    |> Seq.choose (fun msg ->
                        match msg with
                        | SendKitProgressed progress -> Some progress
                        | _ -> None)
                    |> List.ofSeq
                Assert.NotEmpty(progresses)
                let running = started |> updateTableAndElementRotation (SendKitProgressed (List.head progresses))
                Assert.True(running.kitSend.job.isRunning, "the send must still run after its first file")
                match running.kitSend.job.progressText with
                | Some text -> Assert.StartsWith("1 / ", text)
                | None -> Assert.Fail "the first zipped file was not shown as progress"
                let total = List.length progresses
                progresses
                |> List.iteri (fun index progress ->
                    let shown = started |> updateTableAndElementRotation (SendKitProgressed progress)
                    match shown.kitSend.job.progressText with
                    | Some text -> Assert.StartsWith($"{index + 1} / {total}", text)
                    | None -> Assert.Fail $"zipped file {index + 1} was not shown as progress")))

    [<Fact>]
    let ``a cancelled send writes no zip and says so`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun loaded ->
                use source = new CancellationTokenSource ()
                let kept = ResizeArray<KitArchiveObserver -> Result<KitArchiveOutcome, KitArchiveError>> ()
                let deferred : KitArchiveJobHost =
                    {
                        startArchiveJob =
                            fun job ->
                                kept.Add job
                                {
                                    cancelArchiveJob = fun () -> source.Cancel ()
                                }
                    }
                let model =
                    {
                        loaded with
                            kitShell =
                                {
                                    KitWindowShell.inert with
                                        archive = KitArchiveProxy.create ()
                                        archiveJobs = deferred
                                }
                    }
                let zip = exportsZipUnder factFolder
                let started = model |> updateTableAndElementRotation (SendKit zip)
                Assert.True(started.kitSend.job.isRunning, "the send must run until it ends")
                Assert.False(File.Exists zip.value, "a kept job writes nothing")
                let job = Assert.Single(kept)
                let cancelling = started |> updateTableAndElementRotation CancelSendKit
                Assert.True(source.IsCancellationRequested, "Cancel must cancel the running send")
                Assert.Equal<TableAndElementRotationModel>(started, cancelling)
                let observer : KitArchiveObserver =
                    {
                        reportArchiveProgress = ignore
                        cancellation = source.Token
                    }
                let archiveEnd = job observer
                Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(Ok KitArchiveCancelled, archiveEnd)
                let ended = cancelling |> updateTableAndElementRotation (SendKitEnded archiveEnd)
                Assert.Equal<ControlCaption option>(Some (ControlCaption "Send kit was cancelled: no zip was written."), ended.kitStatus)
                Assert.Equal<KitArchiveJobState>(KitArchiveJobState.idle, ended.kitSend)
                Assert.False(File.Exists zip.value, "a cancelled send writes no zip")
                Assert.False(File.Exists $"{zip.value}.tmp", "a cancelled send leaves no partial zip")))

    [<Fact>]
    let ``a scratch kit is not sent and no job starts`` () : unit =
        withFactFolder (fun factFolder _ _ ->
            let scene = initMain () |> kitValue
            try
                let messages = ResizeArray<TableAndElementRotationMsg> ()
                let model =
                    {
                        scene with kitShell = sendingShell messages
                    }
                let zip = exportsZipUnder factFolder
                let answer = model |> updateTableAndElementRotation (SendKit zip)
                Assert.Equal<ControlCaption option>(Some (ControlCaption "A kit with no folder cannot be sent."), answer.kitStatus)
                Assert.Empty(messages)
                Assert.Equal<TableAndElementRotationModel>({ model with kitStatus = answer.kitStatus }, answer)
                Assert.False(answer.kitSend.job.isRunning, "no send may start for a scratch kit")
                Assert.False(File.Exists zip.value, "a scratch kit writes no zip")
            finally scene.kit.close () |> kitValue)

    [<Fact>]
    let ``a send that fails tells why and leaves the window idle`` () : unit =
        withTitledFolder (fun folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun loaded ->
                let messages = ResizeArray<TableAndElementRotationMsg> ()
                let model =
                    {
                        loaded with kitShell = sendingShell messages
                    }
                let inside = (FileName "Optics QZ.zip").combine folder.value
                let ended = model |> updateTableAndElementRotation (SendKit inside) |> passedThrough (List.ofSeq messages)
                match ended.kitStatus with
                | Some status ->
                    Assert.StartsWith("The kit could not be sent — ", status.value)
                    Assert.Contains("inside the kit it holds", status.value)
                | None -> Assert.Fail "the failed send was not told in the kit status"
                Assert.Equal<KitArchiveJobState>(KitArchiveJobState.idle, ended.kitSend)
                Assert.False(File.Exists inside.value, "a refused send writes no zip")))

    [<Fact>]
    let ``a kit opened read-only is sent, and its folder is unchanged`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            created.library.kit.close () |> kitValue
            let opened = openKitAt folder ReadOnlyKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun loaded ->
                let messages = ResizeArray<TableAndElementRotationMsg> ()
                let model =
                    {
                        loaded with kitShell = sendingShell messages
                    }
                let before = folderBytes folder
                let zip = exportsZipUnder factFolder
                let sent = model |> updateTableAndElementRotation (SendKit zip) |> passedThrough (List.ofSeq messages)
                Assert.True(File.Exists zip.value, "a kit opened read-only must be sent")
                let zippedFiles = KitArchiveProxy.create().listZipEntries zip |> archiveValue |> KitTreeEntry.filePathsOf |> List.length
                Assert.Equal<ControlCaption option>(Some (ControlCaption $"The kit was sent: {zippedFiles} file(s) zipped."), sent.kitStatus)
                Assert.Equal<KitAccess>(ReadOnlyKitAccess, sent.kit.access)
                Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)))

    /// The kit a fact copies experiments from, and the two experiments it holds (spec 0050 S6).
    type private CopySourceKit =
        {
            sourceFolder : KitFolder
            e1 : Experiments.Experiment
            e2 : Experiments.Experiment
        }

    /// A dark line, no element varied (spec 0038 Part I), attaching the data file when one is given.
    let private darkLine (id : Experiments.ExperimentId) (dataFileOpt : DataAttachment option) : Experiments.Experiment =
        {
            storedExperiment with
                id = id
                name = id.defaultName
                dataFileOpt = dataFileOpt
        }

    /// `Copy source` beside the fact's kit folder, made through its library, then closed with the app cache its lock
    /// left removed, so a fact sees all that a read-only open writes (spec 0050 S6).
    let private sourceKitUnder
        (factFolder : FolderName)
        (imports : CatalogueImportContext)
        (make : KitFolder -> KitLibraryContext -> CopySourceKit)
        : CopySourceKit =
        let folder = KitFolder (factFolder.combine (FolderName "Copy source"))
        let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
        let source =
            try make folder created.library
            finally created.library.kit.close () |> kitValue
        Directory.Delete(Path.Combine(folder.value.value, ".oc"), true)
        source

    /// The copy source holding the dark setup, E1, a dark line, and E2, which attaches `measured.csv` from outside the
    /// kit through `attachLabFile` (spec 0050 S6, S8.2).
    let private copySourceUnder (factFolder : FolderName) (imports : CatalogueImportContext) : CopySourceKit =
        sourceKitUnder factFolder imports (fun folder library ->
            let proxy = library.kit.proxy
            library.recordSetup darkSetup |> openedValue
            let picked = (FileName "measured.csv").combine factFolder
            File.WriteAllText(picked.value, "x,intensity\n0,1\n45,0.5\n")
            let lab : LabFileSource =
                {
                    file = picked
                    content = FileByteContent (File.ReadAllBytes picked.value)
                }
            let attachment = proxy.attachLabFile KitDataFolder.root lab |> kitValue
            let made : CopySourceKit =
                {
                    sourceFolder = folder
                    e1 = darkLine (Experiments.ExperimentId 1) None
                    e2 = darkLine (Experiments.ExperimentId 2) (Some attachment)
                }
            proxy.writeExperiment made.e1 |> kitValue
            proxy.writeExperiment made.e2 |> kitValue
            made)

    /// The `.oc` app cache of a kit folder.
    let private appCacheOf (folder : KitFolder) : string = Path.Combine(folder.value.value, ".oc")

    /// The files of the pattern at the kit folder's root, in ordinal order.
    let private rootFilesIn (folder : KitFolder) (pattern : string) : string list =
        Directory.GetFiles(folder.value.value, pattern) |> Array.map Path.GetFileName |> Array.sort |> List.ofArray

    /// The experiment files at the kit folder's root.
    let private experimentFilesIn (folder : KitFolder) : string list = rootFilesIn folder "*.experiment-json"

    let private readOnlyRefusal : ControlCaption option = Some (ControlCaption "The kit is open read-only: nothing is saved.")

    [<Fact>]
    let ``choosing a kit to copy from opens it read-only and lists its experiments`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let source = copySourceUnder factFolder imports
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                let before = folderBytes source.sourceFolder
                let listed = model |> updateTableAndElementRotation (OpenCopySource source.sourceFolder)
                match listed.kitCopyOpt with
                | Some copySource ->
                    Assert.Equal<KitAccess>(ReadOnlyKitAccess, copySource.source.access)
                    Assert.Equal<KitName>(KitName "Copy source", copySource.source.kitFile.name)
                    Assert.Equal<Experiments.Experiment list>([ source.e1; source.e2 ], copySource.experiments)
                    Assert.Empty(copySource.chosen)
                | None -> Assert.Fail "the chosen kit was not opened to copy from"
                Assert.Equal<ControlCaption option>(None, listed.kitStatus)
                Assert.Equal<Experiments.ExperimentCollection>(model.experimentCollection, listed.experimentCollection)
                Assert.False(Directory.Exists(appCacheOf source.sourceFolder), "a read-only open writes no .oc")
                Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes source.sourceFolder)))

    [<Fact>]
    let ``copying the chosen experiments brings them into the kit and the window, and closes the source`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let source = copySourceUnder factFolder imports
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            try created.library.kit.proxy.writeItem windowCpl1 |> kitValue
            finally created.library.kit.close () |> kitValue
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                Assert.Equal<Item list>([ windowCpl1 ], model.kitItems)
                let sourceBefore = folderBytes source.sourceFolder
                let copied =
                    model
                    |> updateTableAndElementRotation (OpenCopySource source.sourceFolder)
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e2.id)
                    |> updateTableAndElementRotation CopyChosenExperiments
                Assert.Equal<Experiments.Experiment list>([ source.e2 ], copied.experimentCollection.experiments)
                Assert.Equal<Experiments.ExperimentId list>(
                    [ source.e2.id ],
                    copied.experimentDataStatus |> Map.toList |> List.map fst)
                Assert.Equal<string list>([ "E2.experiment-json" ], experimentFilesIn folder)
                let copiedData = Path.Combine(folder.value.value, "data", "measured.csv")
                Assert.True(File.Exists copiedData, "E2's data file must be copied under data")
                Assert.Equal<byte[]>(File.ReadAllBytes(Path.Combine(source.sourceFolder.value.value, "data", "measured.csv")), File.ReadAllBytes copiedData)
                Assert.Equal<KitCopySource option>(None, copied.kitCopyOpt)
                Assert.Equal<ControlCaption option>(Some (ControlCaption "Copied 1 experiment(s) from Copy source."), copied.kitStatus)
                Assert.Equal<Item list>(model.kitItems, copied.kitItems)
                Assert.Empty(model.kitSetups)
                Assert.Equal<Setups.Setup list>([ darkSetup ], copied.kitSetups)
                Assert.Equal<string list>([ "Dark.setup-json" ], rootFilesIn folder "*.setup-json")
                Assert.Equal<KitContext>(model.kit, copied.kit)
                Assert.Equal<Map<FileName, FileByteContent>>(sourceBefore, folderBytes source.sourceFolder)
                let checkedChanges = copied |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
                Assert.Empty(checkedChanges.kitChanges)
                Assert.Equal<TableAndElementRotationModel>(copied, checkedChanges)))

    [<Fact>]
    let ``toggling an experiment twice leaves it unchosen, and Cancel copies nothing`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let source = copySourceUnder factFolder imports
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                Assert.Equal<TableAndElementRotationModel>(model, model |> updateTableAndElementRotation (ToggleCopiedExperiment source.e1.id))
                let listed = model |> updateTableAndElementRotation (OpenCopySource source.sourceFolder)
                let chosenOf (current : TableAndElementRotationModel) : Experiments.ExperimentId list =
                    match current.kitCopyOpt with
                    | Some copySource -> copySource.chosen
                    | None -> failwith "the copy source must stay open while experiments are chosen"
                let twice =
                    listed
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e1.id)
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e1.id)
                Assert.Empty(chosenOf twice)
                let both =
                    listed
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e2.id)
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e1.id)
                Assert.Equal<Experiments.ExperimentId list>([ source.e1.id; source.e2.id ], chosenOf both)
                let once = listed |> updateTableAndElementRotation (ToggleCopiedExperiment source.e1.id)
                Assert.Equal<Experiments.ExperimentId list>([ source.e1.id ], chosenOf once)
                let cancelled = once |> updateTableAndElementRotation CloseCopySource
                Assert.Equal<KitCopySource option>(None, cancelled.kitCopyOpt)
                Assert.Equal<TableAndElementRotationModel>({ once with kitCopyOpt = None }, cancelled)
                Assert.Empty(experimentFilesIn folder)))

    [<Fact>]
    let ``a kit opened read-only neither opens a source nor copies`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let source = copySourceUnder factFolder imports
            let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            created.library.kit.close () |> kitValue
            let opened = openKitAt folder ReadOnlyKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                let before = folderBytes folder
                let sourceBefore = folderBytes source.sourceFolder
                let refused = model |> updateTableAndElementRotation (OpenCopySource source.sourceFolder)
                Assert.Equal<ControlCaption option>(readOnlyRefusal, refused.kitStatus)
                Assert.Equal<KitCopySource option>(None, refused.kitCopyOpt)
                Assert.Equal<TableAndElementRotationModel>({ model with kitStatus = readOnlyRefusal }, refused)
                let sourceContext = KitContext.openKit (KitProxy.create source.sourceFolder) ReadOnlyKitAccess ClockProvider.system |> kitValue
                let withSource =
                    {
                        model with
                            kitCopyOpt =
                                Some
                                    {
                                        source = sourceContext
                                        experiments = [ source.e1; source.e2 ]
                                        chosen = [ source.e2.id ]
                                        items = []
                                        setups = [ darkSetup ]
                                        itemRows = []
                                    }
                    }
                let notCopied = withSource |> updateTableAndElementRotation CopyChosenExperiments
                Assert.Equal<ControlCaption option>(readOnlyRefusal, notCopied.kitStatus)
                Assert.Equal<TableAndElementRotationModel>({ withSource with kitStatus = readOnlyRefusal }, notCopied)
                Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
                Assert.Equal<Map<FileName, FileByteContent>>(sourceBefore, folderBytes source.sourceFolder)))

    [<Fact>]
    let ``a kit cannot copy from itself, and a folder that is no kit is told`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let opened = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
            withLoadedScene opened (fun model ->
                let itself = model |> updateTableAndElementRotation (OpenCopySource (KitFolder (FolderName $"{folder.value.value}/")))
                let selfRefusal = Some (ControlCaption "A kit cannot copy experiments from itself.")
                Assert.Equal<ControlCaption option>(selfRefusal, itself.kitStatus)
                Assert.Equal<TableAndElementRotationModel>({ model with kitStatus = selfRefusal }, itself)
                let notes = factFolder.combine (FolderName "Notes")
                Directory.CreateDirectory notes.value |> ignore
                File.WriteAllText(Path.Combine(notes.value, "notes.txt"), "Not a kit\n")
                let noKit = model |> updateTableAndElementRotation (OpenCopySource (KitFolder notes))
                match noKit.kitStatus with
                | Some status ->
                    Assert.StartsWith("The kit could not be opened to copy from — ", status.value)
                    Assert.Contains("the folder is not a kit", status.value)
                | None -> Assert.Fail "a folder that is no kit was not told in the kit status"
                Assert.Equal<KitCopySource option>(None, noKit.kitCopyOpt)
                Assert.Equal<string list>(
                    [ "notes.txt" ],
                    Directory.GetFileSystemEntries notes.value |> Array.map Path.GetFileName |> List.ofArray)))

    /// The 600 nm source preset over the shipped source-preset item model (spec 0050 S7.1, step 210).
    let private laser600Choice : ItemModelChoice =
        SourcePresetChoice (BuiltinCatalogue.itemModel ItemModelIds.sourcePresetItemModelId, BuiltinCatalogue.source Library.PresetIds.source600)

    /// The 1 mm glass plate's first version over the shipped stack-sample item model (spec 0050 S7.1, step 210).
    let private plateChoice : ItemModelChoice =
        StackSampleChoice (
            BuiltinCatalogue.itemModel ItemModelIds.stackSampleItemModelId,
            Library.SampleVersionId.firstOf Library.SampleIds.glassPlate1mmId,
            BuiltinCatalogue.sample Library.SampleIds.glassPlate1mmId
        )

    /// The source's Laser600, which E1 assigns at its light source (spec 0050 S7.2).
    let private sourceLaser600 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "22800000-0000-4000-8000-000000000001")
                label = ItemLabel "Laser600"
                model = laser600Choice
                dial = FixedMounting
            }
        draft.toItem

    /// The source's QZ, which E2 assigns at its plate (spec 0050 S7.2).
    let private sourceQz : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "22800000-0000-4000-8000-000000000002")
                label = ItemLabel "QZ"
                model = plateChoice
                dial = DialMounted DialPositive
            }
        draft.toItem

    /// The target's own Laser600 of the same preset under another id, onto which the source's Laser600 maps by default.
    let private targetLaser600 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "22800000-0000-4000-8000-000000000003")
                label = ItemLabel "Laser600"
                model = laser600Choice
                dial = FixedMounting
            }
        draft.toItem

    /// A second target piece of the same preset under a third id; on a dial, so its parameters differ from both
    /// Laser600s.
    let private dialLaser600 : Item =
        let draft : ItemDraft =
            {
                id = ItemId (Guid.Parse "22800000-0000-4000-8000-000000000004")
                label = ItemLabel "Laser600 dial"
                model = laser600Choice
                dial = DialMounted DialNegative
            }
        draft.toItem

    /// A rest-pose slot of that kind at the table's origin, bound to the entry, holding the item (spec 0050 S8.2).
    let private slotHolding (id : string) (kind : Placement.CatalogueKind) (valueId : Placement.ElementValueId) (item : Item) : Setups.SetupSlot =
        {
            id = Library.ElementId id
            placement =
                {
                    Placement.ElementPlacement.create kind Placement.TablePoint.origin with valueId = Some valueId
                }
            zoom = BeamLayout.ElementZoom 1.0
            beam = BeamLayout.OnMainBeam
            content = Setups.ItemInSlot (item.id, Setups.PlacementTurn)
        }

    /// E1's `Laser bench` binds the 600 nm source preset at its light source, which holds Laser600.
    let private laserBench : Setups.Setup =
        {
            darkSetup with
                name = SetupName "Laser bench"
                slots = [ slotHolding "copied-source" Placement.CatalogueKind.LightSource Library.PresetIds.source600 sourceLaser600 ]
        }

    /// E2's `Plate bench` binds the 1 mm glass plate at its sample, which holds QZ.
    let private plateBench : Setups.Setup =
        {
            darkSetup with
                name = SetupName "Plate bench"
                slots = [ slotHolding "copied-plate" Placement.CatalogueKind.Sample Library.SampleIds.glassPlate1mmId.toElementValueId sourceQz ]
        }

    let private itemE1 : Experiments.Experiment =
        {
            storedExperiment with setup = laserBench.reference
        }

    let private itemE2 : Experiments.Experiment =
        {
            storedExperiment with
                id = Experiments.ExperimentId 2
                name = (Experiments.ExperimentId 2).defaultName
                setup = plateBench.reference
        }

    /// The copy source holding Laser600 and QZ, recorded through `recordItem`, their setups through `recordSetup`, then
    /// E1 and E2, recorded through `recordExperiment` (spec 0050 S6, S7.2, S8.2).
    let private itemSourceUnder (factFolder : FolderName) (imports : CatalogueImportContext) : CopySourceKit =
        sourceKitUnder factFolder imports (fun folder library ->
            for item in [ sourceLaser600; sourceQz ] do
                library.recordItem item |> openedValue
            for setup in [ laserBench; plateBench ] do
                library.recordSetup setup |> openedValue
            for experiment in [ itemE1; itemE2 ] do
                library.recordExperiment experiment |> openedValue
            {
                sourceFolder = folder
                e1 = itemE1
                e2 = itemE2
            })

    /// Makes the kit at the folder holding the items, each recorded through `recordItem` with its model snapshots, then
    /// opens it read-write as a window would (spec 0050 S7.2).
    let private openedHolding (folder : KitFolder) (imports : CatalogueImportContext) (items : Item list) : OpenedKit =
        let created = openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue
        try
            for item in items do
                created.library.recordItem item |> openedValue
        finally created.library.kit.close () |> kitValue
        openKitAt folder ReadWriteKitAccess ClockProvider.system titleAuthor imports |> openedValue

    /// The open copy source; a fact fails when none is open.
    let private copySourceOf (model : TableAndElementRotationModel) : KitCopySource =
        match model.kitCopyOpt with
        | Some copySource -> copySource
        | None -> failwith "the copy source must stay open while its experiments and items are chosen"

    /// What a kit holds after a copy, read once it is closed and reopened read-only: its experiments, the items their
    /// setups' slots resolve to, all its items and its setups (spec 0050 S7.2, S8.2).
    type private ReopenedCopy =
        {
            experiments : Experiments.Experiment list
            resolved : Result<Item list, SetupError>
            items : Item list
            setups : Setups.Setup list
        }

    /// Reopens the kit at the folder read-only and resolves its experiments through its setups and items.
    let private reopenedCopy (folder : KitFolder) : ReopenedCopy =
        let later = KitContext.openKit (KitProxy.create folder) ReadOnlyKitAccess ClockProvider.system |> kitValue
        try
            let experiments = later.proxy.readExperiments () |> kitValue
            let items = later.proxy.readItems () |> kitValue
            let setups = later.proxy.readSetups () |> kitValue
            {
                experiments = experiments
                resolved = Experiments.Experiment.itemsUsedBy experiments setups items
                items = items
                setups = setups
            }
        finally later.close () |> kitValue

    [<Fact>]
    let ``choosing experiments lists the items they use, each at its default`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let source = itemSourceUnder factFolder imports
            let opened = openedHolding folder imports [ targetLaser600 ]
            withLoadedScene opened (fun model ->
                Assert.Equal<TableAndElementRotationModel>(model, model |> updateTableAndElementRotation (ChooseCopiedItem (sourceLaser600.id, AddCopiedItem)))
                let listed = model |> updateTableAndElementRotation (OpenCopySource source.sourceFolder)
                try
                    let listedSource = copySourceOf listed
                    Assert.Equal<Item list>([ sourceLaser600; sourceQz ], listedSource.items)
                    Assert.Empty(listedSource.itemRows)
                    let qzRow : ItemCopyRow =
                        {
                            sourceItem = sourceQz
                            choice = AddCopiedItem
                        }
                    let laserRow : ItemCopyRow =
                        {
                            sourceItem = sourceLaser600
                            choice = MapOntoKitItem targetLaser600.id
                        }
                    let e2Chosen = listed |> updateTableAndElementRotation (ToggleCopiedExperiment source.e2.id)
                    Assert.Equal<ItemCopyRow list>([ qzRow ], (copySourceOf e2Chosen).itemRows)
                    let bothChosen = e2Chosen |> updateTableAndElementRotation (ToggleCopiedExperiment source.e1.id)
                    Assert.Equal<Experiments.ExperimentId list>([ source.e1.id; source.e2.id ], (copySourceOf bothChosen).chosen)
                    Assert.Equal<ItemCopyRow list>([ laserRow; qzRow ], (copySourceOf bothChosen).itemRows)
                    let e1Chosen = bothChosen |> updateTableAndElementRotation (ToggleCopiedExperiment source.e2.id)
                    Assert.Equal<ItemCopyRow list>([ laserRow ], (copySourceOf e1Chosen).itemRows)
                    Assert.Equal<ControlCaption option>(None, e1Chosen.kitStatus)
                    Assert.Equal<Item list>([ targetLaser600 ], e1Chosen.kitItems)
                    Assert.Empty(experimentFilesIn folder)
                finally listed.closeCopySource () |> ignore))

    [<Fact>]
    let ``the user's choice for an item is kept and copied`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let source = itemSourceUnder factFolder imports
            let addedLaser600 =
                {
                    sourceLaser600 with label = ItemLabel "Laser600 (2)"
                }
            // Added: the source's Laser600 comes in under its own id beside the target's, relabelled.
            let opened = openedHolding folder imports [ targetLaser600 ]
            withLoadedScene opened (fun model ->
                let chosen =
                    model
                    |> updateTableAndElementRotation (OpenCopySource source.sourceFolder)
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e1.id)
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e2.id)
                    |> updateTableAndElementRotation (ChooseCopiedItem (sourceLaser600.id, AddCopiedItem))
                let addedRows : ItemCopyRow list =
                    [
                        {
                            sourceItem = sourceLaser600
                            choice = AddCopiedItem
                        }
                        {
                            sourceItem = sourceQz
                            choice = AddCopiedItem
                        }
                    ]
                Assert.Equal<ItemCopyRow list>(addedRows, (copySourceOf chosen).itemRows)
                let kept =
                    chosen
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e2.id)
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e2.id)
                Assert.Equal<ItemCopyRow list>(addedRows, (copySourceOf kept).itemRows)
                let noSuchRow = kept |> updateTableAndElementRotation (ChooseCopiedItem (dialLaser600.id, AddCopiedItem))
                Assert.Equal<TableAndElementRotationModel>(kept, noSuchRow)
                let copied = kept |> updateTableAndElementRotation CopyChosenExperiments
                Assert.Equal<string list>([ "Laser600 (2).item-json"; "Laser600.item-json"; "QZ.item-json" ], rootFilesIn folder "*.item-json")
                Assert.Equal<Item list>([ targetLaser600; addedLaser600; sourceQz ], copied.kitItems)
                Assert.Equal<ControlCaption option>(Some (ControlCaption "Copied 2 experiment(s) and added 2 item(s) from Copy source."), copied.kitStatus)
                Assert.Equal<KitCopySource option>(None, copied.kitCopyOpt))
            let added = reopenedCopy folder
            Assert.Equal<Experiments.ExperimentId list>([ source.e1.id; source.e2.id ], added.experiments |> List.map (fun experiment -> experiment.id))
            Assert.Equal<Result<Item list, SetupError>>(Ok [ addedLaser600; sourceQz ], added.resolved)

            // Mapped: the user's choice of a differently identified piece of the same preset, not the default Laser600.
            let benchFolder = KitFolder (factFolder.combine (FolderName "Optics bench"))
            let bench = openedHolding benchFolder imports [ targetLaser600; dialLaser600 ]
            withLoadedScene bench (fun model ->
                let e1Chosen =
                    model
                    |> updateTableAndElementRotation (OpenCopySource source.sourceFolder)
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e1.id)
                let defaultRow : ItemCopyRow =
                    {
                        sourceItem = sourceLaser600
                        choice = MapOntoKitItem targetLaser600.id
                    }
                Assert.Equal<ItemCopyRow list>([ defaultRow ], (copySourceOf e1Chosen).itemRows)
                let onDial = e1Chosen |> updateTableAndElementRotation (ChooseCopiedItem (sourceLaser600.id, MapOntoKitItem dialLaser600.id))
                Assert.Equal<ItemCopyRow list>(
                    [
                        {
                            defaultRow with choice = MapOntoKitItem dialLaser600.id
                        }
                    ],
                    (copySourceOf onDial).itemRows)
                let copied = onDial |> updateTableAndElementRotation CopyChosenExperiments
                Assert.Equal<ControlCaption option>(Some (ControlCaption "Copied 1 experiment(s) from Copy source."), copied.kitStatus))
            let mapped = reopenedCopy benchFolder
            match mapped.experiments, mapped.resolved with
            | [ copiedE1 ], Ok [ onPiece ] ->
                Assert.Equal<ItemId>(dialLaser600.id, onPiece.id)
                Assert.Equal<Parameter list>(dialLaser600.parameters, onPiece.parameters)
                Assert.NotEqual<Parameter list>(targetLaser600.parameters, onPiece.parameters)
                Assert.Equal<Result<ItemId option list, SetupError>>(
                    Ok [ Some dialLaser600.id ], copiedE1.slotsIn mapped.setups |> Result.map (List.map (fun slot -> slot.content.itemOpt)))
            | experiments, _ -> Assert.Fail $"E1 must be copied alone and resolve exactly the chosen piece, not {List.length experiments} experiment(s)"
            Assert.Equal<Item list>([ targetLaser600; dialLaser600 ], mapped.items))

    [<Fact>]
    let ``a choice the copy refuses keeps the panel and writes nothing`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let source = itemSourceUnder factFolder imports
            let opened = openedHolding folder imports [ sourceQz ]
            withLoadedScene opened (fun model ->
                let e2Chosen =
                    model
                    |> updateTableAndElementRotation (OpenCopySource source.sourceFolder)
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e2.id)
                try
                    let mappedRow : ItemCopyRow =
                        {
                            sourceItem = sourceQz
                            choice = MapOntoKitItem sourceQz.id
                        }
                    Assert.Equal<ItemCopyRow list>([ mappedRow ], (copySourceOf e2Chosen).itemRows)
                    let refusing = e2Chosen |> updateTableAndElementRotation (ChooseCopiedItem (sourceQz.id, AddCopiedItem))
                    Assert.Equal<ItemCopyRow list>(
                        [
                            {
                                mappedRow with choice = AddCopiedItem
                            }
                        ],
                        (copySourceOf refusing).itemRows)
                    let before = folderBytes folder
                    let sourceBefore = folderBytes source.sourceFolder
                    let refused = refusing |> updateTableAndElementRotation CopyChosenExperiments
                    match refused.kitStatus with
                    | Some status ->
                        Assert.StartsWith("The experiments could not be copied", status.value)
                        Assert.Contains("the kit already has an item of that id", status.value)
                    | None -> Assert.Fail "the refused copy was not told in the kit status"
                    Assert.Equal<TableAndElementRotationModel>({ refusing with kitStatus = refused.kitStatus }, refused)
                    Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
                    Assert.Equal<Map<FileName, FileByteContent>>(sourceBefore, folderBytes source.sourceFolder)
                    Assert.Empty(experimentFilesIn folder)
                finally e2Chosen.closeCopySource () |> ignore))

    [<Fact>]
    let ``choosing an experiment with a dangling physical binding reports its id and writes nothing`` () : unit =
        withFactFolder (fun factFolder folder imports ->
            let source = itemSourceUnder factFolder imports
            let opened = openedHolding folder imports [ targetLaser600 ]
            withLoadedScene opened (fun model ->
                let e1Chosen =
                    model
                    |> updateTableAndElementRotation (OpenCopySource source.sourceFolder)
                    |> updateTableAndElementRotation (ToggleCopiedExperiment source.e1.id)
                try
                    let missing = ItemId (Guid.Parse "22800000-0000-4000-8000-0000000000ff")
                    let plate = List.exactlyOne plateBench.slots
                    let danglingBench =
                        {
                            plateBench with
                                slots =
                                    [
                                        {
                                            plate with content = Setups.ItemInSlot (missing, Setups.PlacementTurn)
                                        }
                                    ]
                        }
                    let dangling = source.e2
                    let loaded = copySourceOf e1Chosen
                    let injected =
                        {
                            e1Chosen with
                                kitCopyOpt =
                                    Some
                                        {
                                            loaded with
                                                experiments = [ source.e1; dangling ]
                                                setups = loaded.setups |> List.map (fun setup -> if setup.name = danglingBench.name then danglingBench else setup)
                                        }
                        }
                    let before = folderBytes folder
                    let sourceBefore = folderBytes source.sourceFolder
                    let refused = injected |> updateTableAndElementRotation (ToggleCopiedExperiment dangling.id)
                    let role = (Setups.SetupSlot.inLightOrder danglingBench.slots |> List.exactlyOne).role
                    let slot = $"'{role.text}' ({plate.id.value})"
                    let held = $"{missing.value}, held in the slot {slot} of {danglingBench.reference.describe}, which E2 references"
                    let diagnostic = SetupItemReferenceErr (UnknownKitItemErr held)
                    Assert.Equal<ControlCaption option>(
                        Some (ControlCaption $"The experiment could not be chosen to copy — {diagnostic.describe}"),
                        refused.kitStatus)
                    Assert.Equal<KitCopySource option>(injected.kitCopyOpt, refused.kitCopyOpt)
                    Assert.Equal<Experiments.ExperimentId list>([ source.e1.id ], (copySourceOf refused).chosen)
                    Assert.Equal<ItemCopyRow list>(
                        [
                            {
                                sourceItem = sourceLaser600
                                choice = MapOntoKitItem targetLaser600.id
                            }
                        ],
                        (copySourceOf refused).itemRows)
                    Assert.Equal<TableAndElementRotationModel>({ injected with kitStatus = refused.kitStatus }, refused)
                    Assert.Equal<Map<FileName, FileByteContent>>(before, folderBytes folder)
                    Assert.Equal<Map<FileName, FileByteContent>>(sourceBefore, folderBytes source.sourceFolder)
                    Assert.Empty(experimentFilesIn folder)
                finally e1Chosen.closeCopySource () |> ignore))

    [<Fact>]
    let ``the flow strip's steps follow 001's order and a window opens on Setups`` () : unit =
        Assert.Equal<ConstructorFlowStep list>([ ItemsFlowStep; SetupsFlowStep; ExperimentsFlowStep ], ConstructorFlowStep.all)
        Assert.Equal<ControlCaption list>(
            [ ControlCaption "Items"; ControlCaption "Setups"; ControlCaption "Experiments" ],
            ConstructorFlowStep.all |> List.map _.caption)
        let scene = initMain () |> kitValue
        try
            Assert.Equal<ConstructorFlowStep>(SetupsFlowStep, scene.flowStep)
            Assert.Equal<Ribbon.BayName>(BayNames.add, scene.ribbon)
            let inverse = initInverseWithKit scene.library scene.experiments scene.materials scene.samples scene.categories scene.kit
            Assert.Equal<ConstructorFlowStep>(SetupsFlowStep, inverse.flowStep)
            Assert.Equal<Ribbon.BayName>(BayNames.add, inverse.ribbon)
        finally scene.kit.close () |> kitValue

    [<Fact>]
    let ``showing a flow step shows its surface, and the Experiments step is the Experiments bay`` () : unit =
        let scene = initMain () |> kitValue
        try
            let experiments = scene |> updateTableAndElementRotation (ShowFlowStep ExperimentsFlowStep)
            Assert.Equal<TableAndElementRotationModel>(
                {
                    scene with
                        flowStep = ExperimentsFlowStep
                        ribbon = BayNames.experiments
                },
                experiments)
            let items = experiments |> updateTableAndElementRotation (ShowFlowStep ItemsFlowStep)
            Assert.Equal<TableAndElementRotationModel>(
                {
                    experiments with flowStep = ItemsFlowStep
                },
                items)
            let setups = items |> updateTableAndElementRotation (ShowFlowStep SetupsFlowStep)
            Assert.Equal<TableAndElementRotationModel>(
                {
                    items with
                        flowStep = SetupsFlowStep
                        ribbon = BayNames.add
                },
                setups)
            let details =
                scene
                |> updateTableAndElementRotation (SelectBay BayNames.details)
                |> updateTableAndElementRotation (ShowFlowStep ItemsFlowStep)
            Assert.Equal<Ribbon.BayName>(BayNames.details, details.ribbon)
            let detailsKept = details |> updateTableAndElementRotation (ShowFlowStep SetupsFlowStep)
            Assert.Equal<TableAndElementRotationModel>(
                {
                    details with flowStep = SetupsFlowStep
                },
                detailsKept)
        finally scene.kit.close () |> kitValue

    [<Fact>]
    let ``choosing a ribbon bay keeps the flow strip on Setups or Experiments`` () : unit =
        let scene = initMain () |> kitValue
        try
            let placed = scene |> updateTableAndElementRotation (AddElement Placement.LinearPolarizer)
            let polarizer = placed.elements |> List.find (fun element -> element.placement.catalogueKind = Placement.LinearPolarizer)
            let model =
                placed
                |> updateTableAndElementRotation (ExpChooseElement polarizer.id)
                |> updateTableAndElementRotation ExpCommit
                |> updateTableAndElementRotation (ShowFlowStep ItemsFlowStep)
            Assert.Equal(1, List.length model.experimentCollection.experiments)
            let experiments = model |> updateTableAndElementRotation (SelectBay BayNames.experiments)
            Assert.Equal<ConstructorFlowStep>(ExperimentsFlowStep, experiments.flowStep)
            Assert.Equal<Ribbon.BayName>(BayNames.experiments, experiments.ribbon)
            let rotation = experiments |> updateTableAndElementRotation (SelectBay BayNames.rotation)
            Assert.Equal<ConstructorFlowStep>(SetupsFlowStep, rotation.flowStep)
            Assert.Equal<Ribbon.BayName>(BayNames.rotation, rotation.ribbon)
            let rotationFromItems = model |> updateTableAndElementRotation (SelectBay BayNames.rotation)
            Assert.Equal<ConstructorFlowStep>(SetupsFlowStep, rotationFromItems.flowStep)
            for answer in [ experiments; rotation; rotationFromItems ] do
                Assert.Equal<KitContext>(model.kit, answer.kit)
                Assert.Equal<Setups.SetupSlot list>(model.elements, answer.elements)
                Assert.Equal<Experiments.ExperimentCollection>(model.experimentCollection, answer.experimentCollection)
                Assert.Equal<TableAndElementRotationModel>(
                    {
                        model with
                            ribbon = answer.ribbon
                            flowStep = answer.flowStep
                    },
                    answer)
        finally scene.kit.close () |> kitValue
