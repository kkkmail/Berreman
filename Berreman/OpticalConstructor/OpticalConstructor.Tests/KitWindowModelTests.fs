namespace OpticalConstructor.Tests

open System
open System.IO
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitArchiveStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Storage.KitFolderStore
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView

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

    /// Another writer's `E2`: the committed experiment given `id = ExperimentId 2` (spec 0050 S5.3, step 148).
    let private writeE2 (writer : KitProxy) (committed : Experiments.Experiment) : unit =
        let e2 =
            {
                committed with id = Experiments.ExperimentId 2
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
            let offered = committed |> updateTableAndElementRotation (CheckKitChanges KitCheckOnActivation)
            Assert.NotEmpty(offered.kitChanges)
            let reloaded = offered |> updateTableAndElementRotation ReloadKitChanges
            let held = writer.readExperiments () |> kitValue
            Assert.Equal<Experiments.ExperimentId list>(
                [ Experiments.ExperimentId 1; Experiments.ExperimentId 2 ],
                held |> List.map (fun experiment -> experiment.id))
            Assert.Equal<Experiments.ExperimentCollection>(Experiments.ExperimentCollection.ofLoaded held, reloaded.experimentCollection)
            Assert.Equal<KitName>(KitName "Renamed", reloaded.kit.kitFile.name)
            Assert.StartsWith("Renamed", reloaded.kitWindowTitle.value)
            Assert.Equal<KitContext>({ offered.kit with kitFile = reloaded.kit.kitFile }, reloaded.kit)
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
