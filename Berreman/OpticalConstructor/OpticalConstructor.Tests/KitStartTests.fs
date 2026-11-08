namespace OpticalConstructor.Tests

open System
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitHistory
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

    /// An in-memory store holding `stored`, over the present folders.
    let private storedHistory () : KitHistoryProxy =
        let store = KitHistoryProxy.createInMemory presentFolders

        match store.writeHistory stored with
        | Ok () -> store
        | Error error -> failwith error.describe

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

    /// The start screen's seams over `kitHistory`; `showKit` records every kit it is given into `shown`.
    let private contextOver (kitHistory : KitHistoryProxy) (shown : ResizeArray<OpenedKit>) : KitStartContext =
        let stores = BuiltinCatalogue.freshStores ()

        {
            kitHistory = kitHistory
            limit = KitHistoryLimit.defaultValue
            clock = steppingClock firstOpen
            author = KitAuthor "test"
            imports = CatalogueImportContext.create stores.proxy
            picker = { pickKitFolder = fun () -> async { return None } }
            showKit = shown.Add
        }

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
