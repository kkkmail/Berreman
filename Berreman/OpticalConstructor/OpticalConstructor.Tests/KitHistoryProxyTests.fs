namespace OpticalConstructor.Tests

open System
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitHistory

/// A kit history store keeps the last history written, whole, and probes folders on disk (spec 0050 S5.1).
[<AbstractClass>]
type KitHistoryProxyContract () =

    let firstOpen : DateTime = DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc)

    let entryOf (kit : string) (id : string) (ticks : int64) (star : KitStar) : KitHistoryEntry =
        {
            folder = KitFolder (FolderName $"C:/kits/{kit}")
            kitId = KitId (Guid.Parse id)
            name = KitName $"Kit {kit}"
            lastOpened = ClockTime (firstOpen.AddTicks ticks)
            star = star
        }

    let kitA : KitHistoryEntry = entryOf "a" "4c8e2f17-9b3a-4d65-a1e0-7f52b9c84d36" 987_654_321L StarredKit
    let kitB : KitHistoryEntry = entryOf "b" "d29a6c04-5e71-4b8f-93c2-0a6e18f7b54d" 123_456_789L UnstarredKit

    let twoEntries : KitHistory = { KitHistory.empty with entries = [ kitA; kitB ] }
    let oneEntry : KitHistory = { KitHistory.empty with entries = [ kitB ] }

    /// Returns a fresh store holding no history, whose probe sees `presentFolder`.
    abstract freshHistory : unit -> KitHistoryProxy

    /// A folder that exists.
    abstract presentFolder : KitFolder

    /// A folder that does not exist.
    abstract missingFolder : KitFolder

    [<Fact>]
    member this.``a new history store reads the empty history`` () : unit =
        let store = this.freshHistory ()
        Assert.Equal<KitHistory>(KitHistory.empty, store.readHistory ())

    [<Fact>]
    member this.``a written history is read back equal`` () : unit =
        let store = this.freshHistory ()
        Assert.Equal<Result<unit, KitHistoryError>>(Ok (), store.writeHistory twoEntries)
        Assert.Equal<KitHistory>(twoEntries, store.readHistory ())

    [<Fact>]
    member this.``writing replaces the whole history`` () : unit =
        let store = this.freshHistory ()
        Assert.Equal<Result<unit, KitHistoryError>>(Ok (), store.writeHistory twoEntries)
        Assert.Equal<Result<unit, KitHistoryError>>(Ok (), store.writeHistory oneEntry)
        Assert.Equal<KitHistory>(oneEntry, store.readHistory ())

    [<Fact>]
    member this.``the folder probe answers present for a folder that exists and missing for one that does not`` () : unit =
        let store = this.freshHistory ()
        Assert.Equal<KitFolderPresence>(KitFolderPresent, store.probeFolder this.presentFolder)
        Assert.Equal<KitFolderPresence>(KitFolderMissing, store.probeFolder this.missingFolder)

type KitHistoryProxyTests () =
    inherit KitHistoryProxyContract ()

    override this.freshHistory () : KitHistoryProxy = KitHistoryProxy.createInMemory [ this.presentFolder ]

    override _.presentFolder : KitFolder = KitFolder (FolderName "C:/in-memory/kits/present")

    override _.missingFolder : KitFolder = KitFolder (FolderName "C:/in-memory/kits/missing")
