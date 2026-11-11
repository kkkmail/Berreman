namespace OpticalConstructor.Tests

open System
open System.IO
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Storage.CatalogueImportStore
open OpticalConstructor.Storage.KitArchiveStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Ui.JobRunner
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.TableAndElementRotationView
open OpticalConstructor.Ui.KitStartView

module KitStartTests =

    let private firstOpen : DateTime = DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc)

    let private folderOf (kit : string) : KitFolder = KitFolder (FolderName $"C:/kits/{kit}")

    let private entryOf (kit : string) (id : string) (minutes : float) (star : KitStar) : KitHistoryEntry =
        {
            folder = folderOf kit
            kitId = KitId (Guid.Parse id)
            name = KitName $"Kit {kit}"
            lastOpened = ClockTime (firstOpen.AddMinutes minutes)
            star = star
        }

    let private kitA : KitHistoryEntry = entryOf "a" "6f1c2b8e-4d37-4a95-8e02-b7d4c9a13f56" 5.0 UnstarredKit
    let private kitB : KitHistoryEntry = entryOf "b" "a03d7e94-1b6c-4f28-9d5a-3c8e0f2b7a41" 1.0 StarredKit
    let private kitC : KitHistoryEntry = entryOf "c" "c58b1f06-7e29-4d43-b1a7-e6f9d204c83b" 9.0 UnstarredKit
    let private kitD : KitHistoryEntry = entryOf "d" "2e9a4c71-d085-4b6f-a3e8-81c5f7d0b924" 7.0 StarredKit
    let private kitE : KitHistoryEntry = entryOf "e" "d7f30a5b-96c2-4e81-8f4d-0b2a6e9c15d7" 5.0 UnstarredKit

    let private stored : KitHistory = { KitHistory.empty with entries = [ kitA; kitB; kitC; kitD; kitE ] }

    let private presentFolders : KitFolder list = [ folderOf "a"; folderOf "c"; folderOf "d"; folderOf "a2" ]

    let private present (entry : KitHistoryEntry) : KitHistoryRow = { entry = entry; presence = KitFolderPresent }

    let private missing (entry : KitHistoryEntry) : KitHistoryRow = { entry = entry; presence = KitFolderMissing }

    /// The five stored kits in display order: starred `d`, `b`, then `c`, `a`, `e`; `b` and `e` are not on disk.
    let private displayedRows : KitHistoryRow list = [ present kitD; missing kitB; present kitC; present kitA; missing kitE ]

    /// An in-memory store holding `seeded` that sees only `folders` on disk.
    let private historyOver (folders : KitFolder list) (seeded : KitHistory) : KitHistoryProxy =
        let store = KitHistoryProxy.createInMemory folders

        match store.writeHistory seeded with
        | Ok () -> store
        | Error error -> failwith error.describe

    /// An in-memory store holding `stored`, over the present folders.
    let private storedHistory () : KitHistoryProxy = historyOver presentFolders stored

    /// Starts at `start` and advances one second per call (spec 0050 S5.2).
    let private steppingClock (start : DateTime) : ClockProvider =
        let next = ref start

        {
            now =
                fun () ->
                    let current = next.Value
                    next.Value <- current.AddSeconds 1.0
                    ClockTime current
        }

    let private kitStartAuthor : KitAuthor = KitAuthor "test"

    /// The start screen's seams over `kitHistory`; `showKit` records every kit it is given into `shown`.
    let private contextOver (kitHistory : KitHistoryProxy) (shown : ResizeArray<OpenedKit>) : KitStartContext =
        let stores = BuiltinCatalogue.freshStores ()

        {
            kitHistory = kitHistory
            limit = KitHistoryLimit.defaultValue
            clock = steppingClock firstOpen
            author = kitStartAuthor
            imports = CatalogueImportContext.create stores.proxy
            picker = { pickKitFolder = fun () -> async { return None } }
            showKit = shown.Add
            activateKitWindow = fun _ -> NoKitWindowOpen
            zipPicker = KitZipPicker.none
            archive = KitArchiveProxy.createInMemory (InMemoryArchiveDisk.create ())
            archiveJobs = KitArchiveJobHost.inert
        }

    /// `clock`, keeping its last answer in `last`, so a fact reads the instant a bump recorded (spec 0050 S5.2).
    let private remembering (clock : ClockProvider) (last : ClockTime option ref) : ClockProvider =
        {
            now =
                fun () ->
                    let answer = clock.now ()
                    last.Value <- Some answer
                    answer
        }

    /// Runs a fact over its own root under the test output. At its end every kit `shown` recorded is closed, then the
    /// root is deleted; a failed close fails the fact (spec 0050 S5.2).
    let private withKitStartRoot (run : FolderName -> ResizeArray<OpenedKit> -> unit) : unit =
        let root = FolderName (Path.Combine(AppContext.BaseDirectory, "KitStartTests", Guid.NewGuid().ToString("N")))
        let shown = ResizeArray<OpenedKit> ()

        try
            run root shown
        finally
            let closes = shown |> Seq.map (fun kit -> kit.library.kit.close ()) |> List.ofSeq
            if Directory.Exists root.value then Directory.Delete(root.value, true)
            closes |> List.iter (Result.defaultWith (fun (error : KitError) -> failwith error.describe))

    let private kitIn (root : FolderName) (name : string) : KitFolder = KitFolder (FolderName (Path.Combine(root.value, name)))

    /// Closes a kit the fact holds open itself.
    let private closeHeld (kit : OpenedKit) : unit =
        match kit.library.kit.close () with
        | Ok () -> ()
        | Error error -> failwith error.describe

    /// Opens or initialises `folder` read-write through `openKitAt`, as another session would.
    let private heldKitAt (folder : KitFolder) (imports : CatalogueImportContext) : OpenedKit =
        match openKitAt folder ReadWriteKitAccess ClockProvider.system kitStartAuthor imports with
        | Ok kit -> kit
        | Error error -> failwith error.describe

    /// Makes a kit in `folder` and closes it, answering its kit file.
    let private madeKitAt (folder : KitFolder) (imports : CatalogueImportContext) : KitFile =
        let made = heldKitAt folder imports
        closeHeld made
        made.library.kit.kitFile

    /// An entry of `folder` opened a day before any instant the facts' clock answers.
    let private olderEntryOf (folder : KitFolder) : KitHistoryEntry =
        {
            folder = folder
            kitId = KitId (Guid.Parse "4b8e2d61-0c7a-4f39-a5d2-9e13b6f80c47")
            name = KitName "Bench A"
            lastOpened = ClockTime (firstOpen.AddDays(-1.0))
            star = UnstarredKit
        }

    /// Every file and folder under `folder`, as `Directory.GetFileSystemEntries` names them.
    let private entriesOf (folder : KitFolder) : FileName list =
        Directory.GetFileSystemEntries(folder.value.value, "*", SearchOption.AllDirectories) |> Array.map FileName |> List.ofArray

    /// Every file under `folder` with its bytes.
    let private kitFolderBytes (folder : KitFolder) : Map<FileName, FileByteContent> =
        Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
        |> Array.map (fun path -> FileName path, FileByteContent (File.ReadAllBytes path))
        |> Map.ofArray

    /// Asserts `folder` holds a kit's `.kit-json`, `data` and `library`.
    let private assertKitLayout (folder : KitFolder) : unit =
        Assert.True(File.Exists(Path.Combine(folder.value.value, ".kit-json")))
        Assert.True(Directory.Exists(Path.Combine(folder.value.value, "data")))
        Assert.True(Directory.Exists(Path.Combine(folder.value.value, "library")))

    /// The experiment `E1` a sent kit holds (spec 0050 S6).
    let private sentExperiment : Experiments.Experiment =
        {
            id = Experiments.ExperimentId 1
            setup = { name = SetupName "Dark"; revision = SetupRevision 1 }
            scan = Experiments.ExperimentScan.darkLine Experiments.CaptureTransmitted
            role = Experiments.ScienceRun
            sessionOpt = None
            notes = ExperimentNotes.none
            dataFileOpt = None
        }

    /// A kit sent as a zip, and the kit file and experiments it held when it was sent (spec 0050 S6).
    type private KitStartSentZip =
        {
            sentKitFile : KitFile
            sentExperiments : Experiments.Experiment list
            sentZip : FileName
        }

    /// Makes `Optics QZ` under `root` through `openKitAt`, gives it `E1` through `writeExperiment` and closes it, then
    /// sends it through the real archive into `<root>/exports/Optics QZ.zip`; the exports folder is made, as a save
    /// dialog's folder exists (spec 0050 S6).
    let private sentKitZipUnder (root : FolderName) (imports : CatalogueImportContext) : KitStartSentZip =
        let folder = kitIn root "Optics QZ"
        let made = heldKitAt folder imports

        let written =
            try
                made.library.kit.proxy.writeExperiment sentExperiment
                |> Result.bind (fun () -> made.library.kit.proxy.readExperiments ())
            finally
                closeHeld made

        let experiments = written |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Experiments.Experiment list>([ sentExperiment ], experiments)
        let exports = root.combine (FolderName "exports")
        Directory.CreateDirectory exports.value |> ignore
        let kitFile = made.library.kit.kitFile
        let zip = kitFile.name.zipFileName.combine exports

        match KitArchiveProxy.create().sendKit kitFile folder zip KitArchiveObserver.unobserved with
        | Ok (KitArchiveFinished _) ->
            {
                sentKitFile = kitFile
                sentExperiments = experiments
                sentZip = zip
            }
        | Ok KitArchiveCancelled -> failwith "the kit's send was cancelled"
        | Error error -> failwith error.describe

    /// The folder a fact extracts a zip into: `<root>/received/k`, which does not exist yet (spec 0050 S6).
    let private receivedKitIn (root : FolderName) : KitFolder = KitFolder (FolderName (Path.Combine(root.value, "received", "k")))

    /// The screen's seams over `kitHistory` and the real archive, whose job host runs each extraction at once,
    /// recording its progress and its end as the messages the screen would receive, in order (spec 0050 S6).
    let private extractingContext (kitHistory : KitHistoryProxy) (shown : ResizeArray<OpenedKit>) (messages : ResizeArray<KitStartMsg>) : KitStartContext =
        {
            contextOver kitHistory shown with
                archive = KitArchiveProxy.create ()
                archiveJobs = KitArchiveJobHost.immediate (OpenKitZipProgressed >> messages.Add) (OpenKitZipEnded >> messages.Add)
        }

    /// A job host that keeps each job unrun, and whose handle cancels `source` (spec 0050 S6).
    let private deferredHost
        (kept : ResizeArray<KitArchiveObserver -> Result<KitArchiveOutcome, KitArchiveError>>)
        (source : CancellationTokenSource)
        : KitArchiveJobHost =
        {
            startArchiveJob =
                fun job ->
                    kept.Add job
                    {
                        cancelArchiveJob = fun () -> source.Cancel ()
                    }
        }

    /// Passes the recorded messages through the update in order (spec 0050 S6).
    let private passedThrough (messages : KitStartMsg seq) (model : KitStartModel) : KitStartModel =
        messages |> Seq.fold (fun current msg -> updateKitStart msg current) model

    [<Fact>]
    let ``the start screen opens on the history in display order, each row with its folder's presence`` () : unit =
        let shown = ResizeArray<OpenedKit> ()
        let model = initKitStart (contextOver (storedHistory ()) shown) ForwardConstructor

        Assert.Equal<KitHistory>(stored, model.history)
        Assert.Equal<KitHistoryFilter>(KitHistoryFilter.none, model.filter)
        Assert.Equal<KitHistoryRow list>(displayedRows, model.rows)
        Assert.Equal<KitStartOffer>(NoKitStartOffer, model.offer)
        Assert.Equal<ControlCaption option>(None, model.status)
        Assert.Equal<ConstructorMode>(ForwardConstructor, model.mode)
        Assert.Empty shown

    [<Fact>]
    let ``starring, unstarring and forgetting a kit write the history and re-list it`` () : unit =
        let store = storedHistory ()
        let shown = ResizeArray<OpenedKit> ()
        let opened = initKitStart (contextOver store shown) ForwardConstructor
        let starredA = { kitA with star = StarredKit }

        let starred = updateKitStart (StarKit (folderOf "a")) opened
        let starredHistory = { stored with entries = [ starredA; kitB; kitC; kitD; kitE ] }
        Assert.Equal<KitHistory>(starredHistory, store.readHistory ())
        Assert.Equal<KitHistory>(starredHistory, starred.history)
        Assert.Equal<KitHistoryRow list>([ present kitD; present starredA; missing kitB; present kitC; missing kitE ], starred.rows)
        Assert.Equal<ControlCaption option>(None, starred.status)

        let unstarred = updateKitStart (UnstarKit (folderOf "a")) starred
        Assert.Equal<KitHistory>(stored, store.readHistory ())
        Assert.Equal<KitHistory>(stored, unstarred.history)
        Assert.Equal<KitHistoryRow list>(displayedRows, unstarred.rows)

        let forgotten = updateKitStart (ForgetKit (folderOf "b")) unstarred
        let forgottenHistory = { stored with entries = [ kitA; kitC; kitD; kitE ] }
        Assert.Equal<KitHistory>(forgottenHistory, store.readHistory ())
        Assert.Equal<KitHistory>(forgottenHistory, forgotten.history)
        Assert.Equal<KitHistoryRow list>([ present kitD; present kitC; present kitA; missing kitE ], forgotten.rows)
        Assert.Equal<ControlCaption option>(None, forgotten.status)
        Assert.Empty shown

    [<Fact>]
    let ``locating a missing kit re-points its row and keeps its star`` () : unit =
        let store = storedHistory ()
        let shown = ResizeArray<OpenedKit> ()
        let opened = initKitStart (contextOver store shown) ForwardConstructor
        let relocation : KitRelocation = { missing = folderOf "b"; found = folderOf "a2" }

        let located = updateKitStart (LocateKit relocation) opened
        let relocatedB = { kitB with folder = folderOf "a2" }
        let locatedHistory = { stored with entries = [ kitA; relocatedB; kitC; kitD; kitE ] }
        Assert.Equal<KitStar>(StarredKit, relocatedB.star)
        Assert.Equal<KitHistory>(locatedHistory, store.readHistory ())
        Assert.Equal<KitHistory>(locatedHistory, located.history)
        Assert.Equal<KitHistoryRow list>([ present kitD; present relocatedB; present kitC; present kitA; missing kitE ], located.rows)
        Assert.Equal<ControlCaption option>(None, located.status)
        Assert.Empty shown

    [<Fact>]
    let ``the filter narrows the rows and writes nothing`` () : unit =
        let store = storedHistory ()
        let writes = ResizeArray<KitHistory> ()

        let watched : KitHistoryProxy =
            {
                store with
                    writeHistory =
                        fun history ->
                            writes.Add history
                            store.writeHistory history
            }

        let shown = ResizeArray<OpenedKit> ()
        let opened = initKitStart (contextOver watched shown) ForwardConstructor
        let kitCFilter = KitHistoryFilter "kit c"

        let narrowed = updateKitStart (FilterKitHistory kitCFilter) opened
        Assert.Equal<KitHistoryFilter>(kitCFilter, narrowed.filter)
        Assert.Equal<KitHistoryRow list>([ present kitC ], narrowed.rows)
        Assert.Equal<KitHistory>(stored, narrowed.history)

        let widened = updateKitStart (FilterKitHistory KitHistoryFilter.none) narrowed
        Assert.Equal<KitHistoryFilter>(KitHistoryFilter.none, widened.filter)
        Assert.Equal<KitHistoryRow list>(displayedRows, widened.rows)

        Assert.Empty writes
        Assert.Equal<KitHistory>(stored, store.readHistory ())
        Assert.Empty shown

    [<Fact>]
    let ``a history the store cannot write is shown and the rows are kept`` () : unit =
        let store = storedHistory ()

        let refusing : KitHistoryProxy =
            {
                store with
                    writeHistory = fun _ -> Error (KitHistoryWriteErr "disk full")
            }

        let shown = ResizeArray<OpenedKit> ()
        let opened = initKitStart (contextOver refusing shown) ForwardConstructor

        let refused = updateKitStart (StarKit (folderOf "a")) opened
        Assert.Equal<KitHistoryRow list>(displayedRows, refused.rows)
        Assert.Equal<KitHistory>(stored, refused.history)
        Assert.Equal<KitHistory>(stored, refusing.readHistory ())

        match refused.status with
        | Some caption -> Assert.Contains("disk full", caption.value)
        | None -> Assert.Fail "a refused history write must set the status"

        Assert.Empty shown

    [<Fact>]
    let ``New kit initialises an empty folder, bumps the history and hands the kit over`` () : unit =
        withKitStartRoot (fun root shown ->
            let folder = kitIn root "Optics QZ"
            let store = historyOver [ folder ] KitHistory.empty
            let last : ClockTime option ref = ref None
            let context = contextOver store shown
            let opened = initKitStart { context with clock = remembering context.clock last } ForwardConstructor
            Assert.False(Directory.Exists folder.value.value)

            let created = updateKitStart (CreateKit folder) opened

            let kit = Assert.Single<OpenedKit>(shown)
            Assert.Equal<KitAccess>(ReadWriteKitAccess, kit.library.kit.access)
            Assert.Equal<KitName>(KitName "Optics QZ", kit.library.kit.kitFile.name)
            assertKitLayout folder

            match last.Value with
            | Some bumpedAt ->
                let entry : KitHistoryEntry =
                    {
                        folder = folder
                        kitId = kit.library.kit.kitFile.id
                        name = KitName "Optics QZ"
                        lastOpened = bumpedAt
                        star = UnstarredKit
                    }

                let bumped = { KitHistory.empty with entries = [ entry ] }
                Assert.Equal<KitHistory>(bumped, store.readHistory ())
                Assert.Equal<KitHistory>(bumped, created.history)
                Assert.Equal<KitHistoryRow list>([ present entry ], created.rows)
            | None -> Assert.Fail "opening a kit must read the clock"

            Assert.Equal<KitStartOffer>(NoKitStartOffer, created.offer)
            Assert.Equal<ControlCaption option>(None, created.status))

    [<Fact>]
    let ``Open kit opens an existing kit and bumps it above the others`` () : unit =
        withKitStartRoot (fun root shown ->
            let folder = kitIn root "Optics QZ"
            let other = kitIn root "Bench A"
            let older = olderEntryOf other
            let store = historyOver [ folder; other ] { KitHistory.empty with entries = [ older ] }
            let context = contextOver store shown
            let made = madeKitAt folder context.imports
            let opened = initKitStart context ForwardConstructor
            Assert.Equal<KitHistoryRow list>([ present older ], opened.rows)

            let reopened = updateKitStart (OpenKit folder) opened

            let kit = Assert.Single<OpenedKit>(shown)
            Assert.Equal<KitAccess>(ReadWriteKitAccess, kit.library.kit.access)
            Assert.Equal<KitFile>(made, kit.library.kit.kitFile)

            match (store.readHistory ()).entries with
            | first :: rest ->
                Assert.Equal<KitFolder>(folder, first.folder)
                Assert.Equal<KitId>(made.id, first.kitId)
                Assert.Equal<KitHistoryEntry list>([ older ], rest)
            | [] -> Assert.Fail "opening a kit must bump it into the history"

            Assert.Equal<KitFolder list>([ folder; other ], reopened.rows |> List.map _.entry.folder)
            Assert.Equal<KitStartOffer>(NoKitStartOffer, reopened.offer)
            Assert.Equal<ControlCaption option>(None, reopened.status))

    [<Fact>]
    let ``Open kit on a folder that is not a kit offers Make this a kit, and accepting it keeps the folder's files`` () : unit =
        withKitStartRoot (fun root shown ->
            let folder = kitIn root "Lab notes"
            let dismissed = kitIn root "Bench notes"
            let notesIn (kit : KitFolder) : FileName = FileName (Path.Combine(kit.value.value, "notes.txt"))

            for kit in [ folder; dismissed ] do
                Directory.CreateDirectory kit.value.value |> ignore
                File.WriteAllText((notesIn kit).value, "not a kit")

            let store = historyOver [ folder; dismissed ] KitHistory.empty
            let opened = initKitStart (contextOver store shown) ForwardConstructor

            let offered = updateKitStart (OpenKit folder) opened
            Assert.Empty shown
            Assert.Equal<KitStartOffer>(MakeKitOffer folder, offered.offer)
            Assert.Equal<ControlCaption option>(None, offered.status)
            Assert.Equal<FileName list>([ notesIn folder ], entriesOf folder)
            Assert.Equal<KitHistory>(KitHistory.empty, store.readHistory ())

            let made = updateKitStart (CreateKit folder) offered
            let kit = Assert.Single<OpenedKit>(shown)
            Assert.Equal<KitAccess>(ReadWriteKitAccess, kit.library.kit.access)
            assertKitLayout folder
            Assert.Equal("not a kit", File.ReadAllText((notesIn folder).value))
            Assert.Equal<KitStartOffer>(NoKitStartOffer, made.offer)
            Assert.Equal<ControlCaption option>(None, made.status)

            let offeredAgain = updateKitStart (OpenKit dismissed) made
            Assert.Equal<KitStartOffer>(MakeKitOffer dismissed, offeredAgain.offer)

            let cancelled = updateKitStart DismissKitStartOffer offeredAgain
            Assert.Equal<KitStartOffer>(NoKitStartOffer, cancelled.offer)
            Assert.Equal<FileName list>([ notesIn dismissed ], entriesOf dismissed)
            Assert.Single<OpenedKit>(shown) |> ignore)

    [<Fact>]
    let ``a kit open elsewhere offers Open read-only, which opens it and writes nothing to the kit`` () : unit =
        withKitStartRoot (fun root shown ->
            let folder = kitIn root "Optics QZ"
            let store = historyOver [ folder ] KitHistory.empty
            let context = contextOver store shown
            let owner = heldKitAt folder context.imports

            try
                let before = kitFolderBytes folder
                let opened = initKitStart context ForwardConstructor

                let offered = updateKitStart (OpenKit folder) opened
                Assert.Empty shown

                match offered.offer with
                | ReadOnlyKitOffer (offeredFolder, holder) ->
                    Assert.Equal<KitFolder>(folder, offeredFolder)
                    Assert.Equal<ProcessId>(ProcessId.getCurrentProcessId (), holder.owner.processId)
                    Assert.Equal<MachineName>(MachineName Environment.MachineName, holder.owner.machine)
                    Assert.Equal<UserName>(UserName Environment.UserName, holder.owner.user)
                | NoKitStartOffer
                | MakeKitOffer _ -> Assert.Fail "a kit open elsewhere must be offered read-only"

                Assert.Equal<ControlCaption option>(None, offered.status)
                Assert.Equal<Map<FileName, FileByteContent>>(before, kitFolderBytes folder)

                let readOnly = updateKitStart (OpenKitReadOnly folder) offered
                let kit = Assert.Single<OpenedKit>(shown)
                Assert.Equal<KitAccess>(ReadOnlyKitAccess, kit.library.kit.access)
                Assert.Equal<Map<FileName, FileByteContent>>(before, kitFolderBytes folder)

                match (store.readHistory ()).entries with
                | first :: _ -> Assert.Equal<KitFolder>(folder, first.folder)
                | [] -> Assert.Fail "a read-only open must bump the history"

                Assert.Equal<KitStartOffer>(NoKitStartOffer, readOnly.offer)
                Assert.Equal<ControlCaption option>(None, readOnly.status)
            finally
                closeHeld owner)

    [<Fact>]
    let ``a kit that cannot be opened is shown and nothing is handed over or bumped`` () : unit =
        withKitStartRoot (fun root shown ->
            let folder = kitIn root "Optics QZ"
            let other = kitIn root "Bench A"
            let seeded = { KitHistory.empty with entries = [ olderEntryOf other ] }
            let store = historyOver [ folder; other ] seeded
            let context = contextOver store shown
            madeKitAt folder context.imports |> ignore
            File.WriteAllText(Path.Combine(folder.value.value, "E1.experiment-json"), "{")
            let opened = initKitStart context ForwardConstructor

            let refused = updateKitStart (OpenKit folder) opened
            Assert.Empty shown
            Assert.Equal<KitStartOffer>(NoKitStartOffer, refused.offer)

            match refused.status with
            | Some caption -> Assert.Contains("E1.experiment-json", caption.value)
            | None -> Assert.Fail "a kit that cannot be opened must set the status"

            Assert.Equal<KitHistory>(seeded, store.readHistory ())
            Assert.Equal<KitHistory>(seeded, refused.history)
            Assert.Equal<KitHistoryRow list>(opened.rows, refused.rows)
            Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock"))))

    [<Fact>]
    let ``a kit whose window is open is activated, not opened, bumped or handed over`` () : unit =
        withKitStartRoot (fun root shown ->
            let folder = kitIn root "Bench A"
            let seeded = { KitHistory.empty with entries = [ olderEntryOf folder ] }
            let store = historyOver [ folder ] seeded
            let activations = ResizeArray<KitFolder> ()
            let context = contextOver store shown

            let activating : KitStartContext =
                {
                    context with
                        activateKitWindow =
                            fun (chosen : KitFolder) ->
                                activations.Add chosen
                                if chosen.windowKey = folder.windowKey then KitWindowActivated else NoKitWindowOpen
                }

            madeKitAt folder activating.imports |> ignore
            let before = kitFolderBytes folder
            let opened = initKitStart activating ForwardConstructor
            // A pending question and an earlier failure, which bringing the kit's window forward drops.
            let asked = { opened with offer = MakeKitOffer folder; status = Some (ControlCaption "an earlier failure") }

            for msg in [ OpenKit folder; CreateKit folder; OpenKitReadOnly folder ] do
                activations.Clear ()
                let answered = updateKitStart msg asked

                Assert.Equal<KitFolder list>([ folder ], List.ofSeq activations)
                Assert.Empty shown
                Assert.Equal<KitStartOffer>(NoKitStartOffer, answered.offer)
                Assert.Equal<ControlCaption option>(None, answered.status)
                Assert.Equal<KitHistory>(seeded, store.readHistory ())
                Assert.Equal<KitHistory>(seeded, answered.history)
                Assert.Equal<KitHistoryRow list>(opened.rows, answered.rows)
                Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock")))
                Assert.Equal<Map<FileName, FileByteContent>>(before, kitFolderBytes folder))

    [<Fact>]
    let ``a failed resolution reported by the window is shown`` () : unit =
        withKitStartRoot (fun root shown ->
            let folder = kitIn root "Lab notes"
            let store = historyOver [ folder ] KitHistory.empty
            let opened = initKitStart (contextOver store shown) ForwardConstructor
            let offered = { opened with offer = MakeKitOffer folder }

            let failed = updateKitStart (KitOpenFailed (KitLibraryKitErr (KitIoErr "lost"))) offered
            Assert.Equal<KitStartOffer>(NoKitStartOffer, failed.offer)

            match failed.status with
            | Some caption -> Assert.Contains("lost", caption.value)
            | None -> Assert.Fail "a failed resolution must set the status"

            Assert.Equal<KitHistory>(KitHistory.empty, store.readHistory ())
            Assert.Empty shown)

    [<Fact>]
    let ``Open kit from zip extracts a sent kit into the chosen folder, opens it and bumps the history`` () : unit =
        withKitStartRoot (fun root shown ->
            let target = receivedKitIn root
            let store = historyOver [ target ] KitHistory.empty
            let messages = ResizeArray<KitStartMsg> ()
            let context = extractingContext store shown messages
            let sent = sentKitZipUnder root context.imports
            Assert.False(Directory.Exists target.value.value)
            let opened = initKitStart context ForwardConstructor
            let opening : KitZipOpening = { zip = sent.sentZip; target = target }

            let started = updateKitStart (OpenKitZip opening) opened
            Assert.True(started.zipJob.job.isRunning, "the extraction must run once it starts")
            Assert.Equal<KitZipOpening option>(Some opening, started.zipOpeningOpt)
            Assert.Empty shown

            match List.rev (List.ofSeq messages) with
            | OpenKitZipEnded (Ok (KitArchiveFinished files)) :: progresses ->
                Assert.Equal(files.value, List.length progresses)

                for msg in progresses do
                    match msg with
                    | OpenKitZipProgressed _ -> ()
                    | _ -> Assert.Fail "every message before the end must be a file's progress"
            | _ -> Assert.Fail "the extraction must end, finished, after its files' progress"

            let ended = started |> passedThrough (List.ofSeq messages)
            let kit = Assert.Single<OpenedKit>(shown)
            Assert.Equal<KitFolder>(target, kit.folder)
            Assert.Equal<KitAccess>(ReadWriteKitAccess, kit.library.kit.access)
            Assert.Equal<KitFile>(sent.sentKitFile, kit.library.kit.kitFile)
            Assert.Equal<Experiments.Experiment list>(sent.sentExperiments, kit.experiments)
            assertKitLayout target

            match (store.readHistory ()).entries with
            | first :: _ ->
                Assert.Equal<KitFolder>(target, first.folder)
                Assert.Equal<KitId>(sent.sentKitFile.id, first.kitId)
            | [] -> Assert.Fail "opening the extracted kit must bump it into the history"

            Assert.Equal<KitFolder list>([ target ], ended.rows |> List.map _.entry.folder)
            Assert.Equal<KitArchiveJobState>(KitArchiveJobState.idle, ended.zipJob)
            Assert.Equal<KitZipOpening option>(None, ended.zipOpeningOpt)
            Assert.Equal<KitStartOffer>(NoKitStartOffer, ended.offer)
            Assert.Equal<ControlCaption option>(None, ended.status))

    [<Fact>]
    let ``a zip extracted into a folder that holds files is refused, and nothing opens`` () : unit =
        withKitStartRoot (fun root shown ->
            let target = receivedKitIn root
            let notes = FileName (Path.Combine(target.value.value, "notes.txt"))
            Directory.CreateDirectory target.value.value |> ignore
            File.WriteAllText(notes.value, "not a kit")
            let store = historyOver [ target ] KitHistory.empty
            let messages = ResizeArray<KitStartMsg> ()
            let context = extractingContext store shown messages
            let sent = sentKitZipUnder root context.imports
            let opening : KitZipOpening = { zip = sent.sentZip; target = target }

            let started = updateKitStart (OpenKitZip opening) (initKitStart context ForwardConstructor)
            let ended = started |> passedThrough (List.ofSeq messages)

            match ended.status with
            | Some status ->
                Assert.StartsWith("The zip could not be opened — ", status.value)
                Assert.Contains("not empty", status.value)
            | None -> Assert.Fail "a refused extraction must set the status"

            Assert.Empty shown
            Assert.Equal<FileName list>([ notes ], entriesOf target)
            Assert.Equal("not a kit", File.ReadAllText notes.value)
            Assert.Equal<KitHistory>(KitHistory.empty, store.readHistory ())
            Assert.Equal<KitArchiveJobState>(KitArchiveJobState.idle, ended.zipJob)
            Assert.Equal<KitZipOpening option>(None, ended.zipOpeningOpt))

    [<Fact>]
    let ``a cancelled extraction opens nothing and leaves no folder`` () : unit =
        withKitStartRoot (fun root shown ->
            use source = new CancellationTokenSource ()
            let kept = ResizeArray<KitArchiveObserver -> Result<KitArchiveOutcome, KitArchiveError>> ()
            let target = receivedKitIn root
            let store = historyOver [ target ] KitHistory.empty

            let context =
                {
                    contextOver store shown with
                        archive = KitArchiveProxy.create ()
                        archiveJobs = deferredHost kept source
                }

            let sent = sentKitZipUnder root context.imports
            let opening : KitZipOpening = { zip = sent.sentZip; target = target }

            let started = updateKitStart (OpenKitZip opening) (initKitStart context ForwardConstructor)
            Assert.True(started.zipJob.job.isRunning, "the extraction must run until it ends")
            let job = Assert.Single(kept)
            Assert.False(Directory.Exists target.value.value, "a kept job extracts nothing")

            let cancelling = updateKitStart CancelOpenKitZip started
            Assert.True(source.IsCancellationRequested, "Cancel must cancel the running extraction")
            Assert.Equal<KitStartModel>(started, cancelling)

            let observer : KitArchiveObserver =
                {
                    reportArchiveProgress = ignore
                    cancellation = source.Token
                }

            let archiveEnd = job observer
            Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(Ok KitArchiveCancelled, archiveEnd)

            let ended = updateKitStart (OpenKitZipEnded archiveEnd) cancelling
            Assert.Equal<ControlCaption option>(Some (ControlCaption "Open kit from zip was cancelled: nothing was extracted."), ended.status)
            Assert.Empty shown
            Assert.False(Directory.Exists target.value.value, "a cancelled extraction leaves no target folder")
            Assert.False(Directory.Exists(Path.Combine(root.value, "received")), "a cancelled extraction removes the folders it made")
            Assert.Equal<KitHistory>(KitHistory.empty, store.readHistory ())
            Assert.Equal<KitArchiveJobState>(KitArchiveJobState.idle, ended.zipJob)
            Assert.Equal<KitZipOpening option>(None, ended.zipOpeningOpt))

    [<Fact>]
    let ``a zip that holds no kit is told, and nothing opens`` () : unit =
        withKitStartRoot (fun root shown ->
            let notesFolder = kitIn root "Lab notes"
            Directory.CreateDirectory notesFolder.value.value |> ignore
            File.WriteAllText(Path.Combine(notesFolder.value.value, "notes.txt"), "not a kit")
            let exports = root.combine (FolderName "exports")
            Directory.CreateDirectory exports.value |> ignore
            let zip = (FileName "Lab notes.zip").combine exports
            let archive = KitArchiveProxy.create ()
            let tree = archive.listKitTree notesFolder |> Result.defaultWith (fun error -> failwith error.describe)
            Assert.Equal<KitTreeEntry list>([ KitTreeFile (KitRelativePath "notes.txt") ], tree)
            let zipJob : KitZipJob = { kit = notesFolder; entries = tree; zip = zip }

            Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(
                Ok (KitArchiveFinished (KitArchiveFileCount 1)),
                archive.writeKitZip zipJob KitArchiveObserver.unobserved)

            let target = receivedKitIn root
            let store = historyOver [ target ] KitHistory.empty
            let messages = ResizeArray<KitStartMsg> ()
            let context = extractingContext store shown messages
            let opening : KitZipOpening = { zip = zip; target = target }

            let ended =
                updateKitStart (OpenKitZip opening) (initKitStart context ForwardConstructor)
                |> passedThrough (List.ofSeq messages)

            match ended.status with
            | Some status ->
                Assert.StartsWith("The zip could not be opened — ", status.value)
                Assert.Contains("holds no kit", status.value)
            | None -> Assert.Fail "a zip without a kit must set the status"

            Assert.Empty shown
            Assert.False(Directory.Exists target.value.value, "a zip without a kit extracts nothing")
            Assert.Equal<KitHistory>(KitHistory.empty, store.readHistory ())
            Assert.Equal<KitArchiveJobState>(KitArchiveJobState.idle, ended.zipJob)
            Assert.Equal<KitZipOpening option>(None, ended.zipOpeningOpt))

    [<Fact>]
    let ``a running extraction shows its progress, and a second Open kit from zip waits for it`` () : unit =
        withKitStartRoot (fun root shown ->
            use source = new CancellationTokenSource ()
            let kept = ResizeArray<KitArchiveObserver -> Result<KitArchiveOutcome, KitArchiveError>> ()
            let first = receivedKitIn root
            let second = KitFolder (FolderName (Path.Combine(root.value, "received", "other")))
            let store = historyOver [ first; second ] KitHistory.empty

            let context =
                {
                    contextOver store shown with
                        archive = KitArchiveProxy.create ()
                        archiveJobs = deferredHost kept source
                }

            let sent = sentKitZipUnder root context.imports
            let firstOpening : KitZipOpening = { zip = sent.sentZip; target = first }
            let secondOpening : KitZipOpening = { zip = sent.sentZip; target = second }

            let started = updateKitStart (OpenKitZip firstOpening) (initKitStart context ForwardConstructor)
            Assert.True(started.zipJob.job.isRunning, "the extraction must run until it ends")
            Assert.Equal<string option>(None, started.zipJob.job.progressText)

            let waiting = updateKitStart (OpenKitZip secondOpening) started
            Assert.Equal<KitStartModel>(started, waiting)
            let job = Assert.Single(kept)

            let progresses = ResizeArray<JobProgress> ()

            let observer : KitArchiveObserver =
                {
                    reportArchiveProgress = fun progress -> progresses.Add (progress.jobProgress (JobElapsed TimeSpan.Zero))
                    cancellation = source.Token
                }

            let archiveEnd = job observer

            match archiveEnd with
            | Ok (KitArchiveFinished files) -> Assert.Equal(files.value, progresses.Count)
            | Ok KitArchiveCancelled
            | Error _ -> Assert.Fail "the kept extraction must finish"

            let total = progresses.Count
            Assert.True(total > 0, "the sent kit holds files")

            let showProgress (current : KitStartModel) (index : int, progress : JobProgress) : KitStartModel =
                let shownProgress = updateKitStart (OpenKitZipProgressed progress) current
                Assert.True(shownProgress.zipJob.job.isRunning, $"the extraction must still run after file {index + 1}")

                match shownProgress.zipJob.job.progressText with
                | Some text -> Assert.StartsWith($"{index + 1} / {total}", text)
                | None -> Assert.Fail $"extracted file {index + 1} was not shown as progress"

                shownProgress

            let running = progresses |> Seq.indexed |> Seq.fold showProgress waiting
            Assert.Equal<KitZipOpening option>(Some firstOpening, running.zipOpeningOpt)
            Assert.Empty shown

            let ended = updateKitStart (OpenKitZipEnded archiveEnd) running
            let kit = Assert.Single<OpenedKit>(shown)
            Assert.Equal<KitFolder>(first, kit.folder)
            Assert.False(Directory.Exists second.value.value, "the second Open kit from zip must not extract")
            Assert.Single(kept) |> ignore
            Assert.Equal<KitArchiveJobState>(KitArchiveJobState.idle, ended.zipJob)
            Assert.Equal<ControlCaption option>(None, ended.status))
