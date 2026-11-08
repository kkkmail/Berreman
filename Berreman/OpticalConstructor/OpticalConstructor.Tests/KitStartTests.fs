namespace OpticalConstructor.Tests

open System
open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Storage.CatalogueImportStore
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
