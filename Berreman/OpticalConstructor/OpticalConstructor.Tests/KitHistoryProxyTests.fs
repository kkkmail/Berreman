namespace OpticalConstructor.Tests

open System
open System.IO
open System.Text
open System.Text.Json.Nodes
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitHistory
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.KitHistoryStore

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

    [<Fact>]
    member this.``changing the history writes the change and answers what was written`` () : unit =
        let store = this.freshHistory ()
        Assert.Equal<Result<unit, KitHistoryError>>(Ok (), store.writeHistory twoEntries)

        let unstarred : KitHistory = { twoEntries with entries = [ { kitA with star = UnstarredKit }; kitB ] }
        let changed = store.changeHistory (fun (history : KitHistory) -> history.unstar (KitFolder (FolderName "C:/kits/a")))

        Assert.Equal<Result<KitHistory, KitHistoryError>>(Ok unstarred, changed)
        Assert.Equal<KitHistory>(unstarred, store.readHistory ())

type KitHistoryProxyTests () =
    inherit KitHistoryProxyContract ()

    override this.freshHistory () : KitHistoryProxy = KitHistoryProxy.createInMemory [ this.presentFolder ]

    override _.presentFolder : KitFolder = KitFolder (FolderName "C:/in-memory/kits/present")

    override _.missingFolder : KitFolder = KitFolder (FolderName "C:/in-memory/kits/missing")

/// Runs the shared kit-history contract over real folders under the test output, never `%TEMP%`, `%APPDATA%` or
/// Documents (spec 0050 S5.1; .spec-md §4).
type KitHistoryProxyFolderTests () =
    inherit KitHistoryProxyContract ()

    let root : FolderName = FolderName (Path.Combine(AppContext.BaseDirectory, "KitHistoryProxyFolderTests", Guid.NewGuid().ToString("N")))
    let preferences : FolderName = FolderName (Path.Combine(root.value, "Preferences"))
    let historyFile : FileName = FileName (Path.Combine(preferences.value, "kit-history.json"))
    let presentKitFolder : KitFolder = KitFolder (FolderName (Path.Combine(root.value, "kits", "present")))
    let missingKitFolder : KitFolder = KitFolder (FolderName (Path.Combine(root.value, "kits", "missing")))

    do Directory.CreateDirectory presentKitFolder.value.value |> ignore

    let openedHistory : KitHistory =
        let entry : KitHistoryEntry =
            {
                folder = presentKitFolder
                kitId = KitId (Guid.Parse "5b3e9d21-7c4a-4f86-a0d2-e18c6f7b2a94")
                name = KitName "Optics QZ — µ"
                lastOpened = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc).AddTicks 1_234_567L)
                star = StarredKit
            }

        { KitHistory.empty with entries = [ entry ] }

    let historyText (history : KitHistory) : KitHistoryText =
        match history.fileText with
        | Ok text -> text
        | Error error -> failwith error.describe

    let editedHistoryText (edit : JsonObject -> unit) : string =
        let text = historyText openedHistory
        let node = (JsonNode.Parse text.value).AsObject()
        edit node
        node.canonicalFileText

    let preferencesEntries () : string list =
        Directory.GetFileSystemEntries(preferences.value)
        |> Array.map (fun (path : string) -> Path.GetFileName path)
        |> Array.sort
        |> Array.toList

    override _.freshHistory () : KitHistoryProxy = KitHistoryProxy.create preferences

    override _.presentFolder : KitFolder = presentKitFolder

    override _.missingFolder : KitFolder = missingKitFolder

    [<Fact>]
    member this.``a history written by one store is read back by a new store over the same preferences folder`` () : unit =
        let writer = this.freshHistory ()
        Assert.False(Directory.Exists preferences.value, "the preferences folder exists before the first write")
        Assert.Equal<Result<unit, KitHistoryError>>(Ok (), writer.writeHistory openedHistory)

        let reader = KitHistoryProxy.create preferences
        Assert.Equal<KitHistory>(openedHistory, reader.readHistory ())
        Assert.Equal<string list>([ "kit-history.json" ], preferencesEntries ())
        let written = historyText openedHistory
        let withoutByteOrderMark = Encoding.UTF8.GetBytes written.value
        Assert.Equal<byte>(withoutByteOrderMark, File.ReadAllBytes historyFile.value)

    [<Fact>]
    member this.``a missing history file reads as the empty history and creates nothing`` () : unit =
        let store = this.freshHistory ()
        Assert.Equal<KitHistory>(KitHistory.empty, store.readHistory ())
        Assert.False(Directory.Exists preferences.value, "a read created the preferences folder")

    [<Fact>]
    member this.``an unreadable history file reads as the empty history and is left as it was`` () : unit =
        let unreadableTexts : string list =
            [
                "{"
                editedHistoryText (fun (node : JsonObject) -> node["schemaVersion"] <- JsonValue.Create 2)
                editedHistoryText (fun (node : JsonObject) ->
                    let entry = node.["entries"].[0].AsObject()
                    entry["star"] <- JsonValue.Create 3)
            ]

        Directory.CreateDirectory preferences.value |> ignore

        for text in unreadableTexts do
            let bytes = Encoding.UTF8.GetBytes text
            File.WriteAllBytes(historyFile.value, bytes)
            let store = this.freshHistory ()
            Assert.Equal<KitHistory>(KitHistory.empty, store.readHistory ())
            Assert.Equal<byte>(bytes, File.ReadAllBytes historyFile.value)
            Assert.Equal<string list>([ "kit-history.json" ], preferencesEntries ())

    [<Fact>]
    member this.``a write the preferences folder cannot take is KitHistoryWriteErr, never a throw`` () : unit =
        File.WriteAllText(preferences.value, "a file where the preferences folder belongs")
        let store = this.freshHistory ()

        match store.writeHistory openedHistory with
        | Error (KitHistoryWriteErr reason) -> Assert.Contains(preferences.value, reason)
        | Ok () -> Assert.Fail "wrote a history where the preferences folder is a file"

    [<Fact>]
    member this.``a change the preferences folder cannot take is KitHistoryWriteErr, never a throw`` () : unit =
        let bytes = Encoding.UTF8.GetBytes "a file where the preferences folder belongs"
        File.WriteAllBytes(preferences.value, bytes)
        let store = this.freshHistory ()

        match store.changeHistory (fun (history : KitHistory) -> history.star (KitFolder (FolderName "C:/kits/a"))) with
        | Error (KitHistoryWriteErr reason) -> Assert.Contains(preferences.value, reason)
        | Ok _ -> Assert.Fail "changed a history where the preferences folder is a file"

        Assert.Equal<byte>(bytes, File.ReadAllBytes preferences.value)
