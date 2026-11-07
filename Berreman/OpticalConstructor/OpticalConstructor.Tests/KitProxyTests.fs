namespace OpticalConstructor.Tests

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Storage.KitFolderStore

/// One folder viewed through fresh proxies and an outside writer (spec 0050 S4.1).
type KitTestFolder =
    {
        openProxy : unit -> KitProxy
        placeLabFile : string -> string -> unit
        paths : unit -> string list
    }

module private KitProxyTestData =

    /// Starts at the fixed UTC instant and advances one second per call (spec 0050 S4.1).
    let steppingClock () : ClockProvider =
        let mutable next = ClockTime (DateTime(2026, 11, 3, 12, 0, 0, DateTimeKind.Utc))
        {
            now =
                fun () ->
                    let current = next
                    next <- ClockTime (current.value.AddSeconds 1.0)
                    current
        }

    /// Creates the contract's kit metadata at the supplied clock's next instant (spec 0050 S4.1).
    let newKitFile (clock : ClockProvider) : KitFile =
        let id = KitId (Guid.Parse "6f1c2a9e-3b7d-4e25-9a41-0c8d5e7f2b16")
        let name = KitName "Optics: QZ/LR"
        let author = KitAuthor "Test author"
        KitFile.create id name author (clock.now ())

    let freshKitFolderPath () : FolderName =
        FolderName (Path.Combine(AppContext.BaseDirectory, "KitProxyFolderTests", Guid.NewGuid().ToString("N")))

    let kitFolderAdapter (folder : FolderName) : KitTestFolder =
        let kitFolder = KitFolder folder
        {
            openProxy = fun () -> KitProxy.create kitFolder
            placeLabFile =
                fun path text ->
                    let file = FileName (Path.Combine(folder.value, path))
                    Directory.CreateDirectory(Path.GetDirectoryName file.value) |> ignore
                    File.WriteAllText(file.value, text, UTF8Encoding false)
            paths =
                fun () ->
                    Directory.GetFileSystemEntries(folder.value, "*", SearchOption.AllDirectories)
                    |> Array.map (fun path -> Path.GetRelativePath(folder.value, path).Replace('\\', '/'))
                    |> Array.sortWith (fun first second -> String.CompareOrdinal(first, second))
                    |> Array.toList
        }

    let takeKitLock (proxy : KitProxy) (time : ClockTime) : KitLockHolder =
        proxy.acquireLock time |> Result.defaultWith (fun error -> failwith error.describe)

    let storedKitLock (proxy : KitProxy) : KitLockHolder =
        let path = KitRelativePath ".oc/lock"
        let data = proxy.readDataFile path |> Result.defaultWith (fun error -> failwith error.describe)
        data.text.parseLockHolder |> Result.defaultWith (fun error -> failwith error.describe)

    let placeKitLock (folder : KitTestFolder) (holder : KitLockHolder) : unit =
        let text = holder.lockFileText |> Result.defaultWith (fun error -> failwith error.describe)
        folder.placeLabFile ".oc/lock" text.value

    let currentKitLockOwner () : KitLockOwner =
        {
            machine = MachineName Environment.MachineName
            user = UserName Environment.UserName
            processId = ProcessId.getCurrentProcessId ()
        }

    let contendForKitLock (folder : KitTestFolder) (clock : ClockProvider) : unit =
        let proxies = [ folder.openProxy (); folder.openProxy (); folder.openProxy () ]
        let time = clock.now ()
        use start = new Barrier(proxies.Length)
        let tasks =
            proxies
            |> List.map (fun proxy ->
                Task.Factory.StartNew<Result<KitLockHolder, KitError>>(
                    (fun () ->
                        start.SignalAndWait () |> ignore
                        proxy.acquireLock time),
                    CancellationToken.None, TaskCreationOptions.LongRunning, TaskScheduler.Default))
        let results = (Task.WhenAll tasks).GetAwaiter().GetResult() |> Array.toList
        let outcomes = List.zip proxies results
        let winners = outcomes |> List.choose (fun (proxy, result) -> result |> Result.toOption |> Option.map (fun holder -> proxy, holder))
        let winner, holder = Assert.Single winners
        Assert.Equal<ClockTime>(time, holder.acquiredAt)
        for proxy, result in outcomes do
            match result with
            | Ok acquired -> Assert.Equal<KitLockHolder>(holder, acquired)
            | Error (KitLockedErr held) ->
                Assert.Equal<KitLockHolder>(holder, held)
                Assert.Equal<Result<unit, KitError>>(Ok (), proxy.releaseLock ())
            | Error error -> Assert.Fail($"A competing opener must report the winner: {error.describe}")
        Assert.Equal<KitLockHolder>(holder, storedKitLock winner)
        Assert.Equal<Result<KitLockHolder, KitError>>(Ok holder, winner.acquireLock (clock.now ()))
        Assert.Equal<string list>([ ".kit-json"; ".oc"; ".oc/lock"; "data"; "library" ], folder.paths ())
        Assert.Equal<Result<unit, KitError>>(Ok (), winner.releaseLock ())
        Assert.Equal<string list>([ ".kit-json"; ".oc"; "data"; "library" ], folder.paths ())

open KitProxyTestData

/// The kit-file facts shared by in-memory and filesystem folders (spec 0050 S4.1).
[<AbstractClass>]
type KitProxyContract () =

    /// Returns an existing folder that holds no files or subfolders (spec 0050 S4.1).
    abstract freshFolder : unit -> KitTestFolder

    /// Reading or replacing metadata cannot turn an empty folder into a kit (spec 0050 S4.1).
    [<Fact>]
    member this.``a folder holds no kit until it is initialised`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock

        match proxy.readKitFile () with
        | Error (NotAKitFolderErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected NotAKitFolderErr, got {error.describe}")
        | Ok _ -> Assert.Fail("An empty folder must not hold kit metadata.")

        match proxy.writeKitFile kit with
        | Error (NotAKitFolderErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected NotAKitFolderErr, got {error.describe}")
        | Ok () -> Assert.Fail("Replacing metadata must not initialise a kit.")

        match (folder.openProxy ()).readKitFile () with
        | Error (NotAKitFolderErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected NotAKitFolderErr after the refused write, got {error.describe}")
        | Ok _ -> Assert.Fail("The refused write must leave the folder without a kit.")

        Assert.Empty(folder.paths ())

    /// Initialisation adds only the kit metadata and its two content folders (spec 0050 S4.1).
    [<Fact>]
    member this.``initialising an empty folder writes the kit file, data and library, and nothing else`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let otherProxy = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock

        Assert.NotSame(proxy, otherProxy)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        Assert.Equal<string list>([ ".kit-json"; "data"; "library" ], folder.paths ())
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, otherProxy.readKitFile ())

    /// A lab folder retains its existing files and parent folders when made a kit (spec 0050 S4.1).
    [<Fact>]
    member this.``initialising a folder that holds files keeps every one of them`` () : unit =
        let folder = this.freshFolder ()
        folder.placeLabFile "notes.txt" "Lab notes"
        folder.placeLabFile "raw/run 1.csv" "wavelength,intensity\n600,1\n"
        Assert.Equal<string list>([ "notes.txt"; "raw"; "raw/run 1.csv" ], folder.paths ())
        let proxy = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock

        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        Assert.Equal<string list>([ ".kit-json"; "data"; "library"; "notes.txt"; "raw"; "raw/run 1.csv" ], folder.paths ())
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, (folder.openProxy ()).readKitFile ())

    /// Another proxy cannot initialise an existing kit or change its contents (spec 0050 S4.1).
    [<Fact>]
    member this.``a folder that already holds a kit is not initialised again`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let otherProxy = folder.openProxy ()
        let clock = steppingClock ()
        let first = newKitFile clock
        let second = newKitFile clock
        Assert.NotEqual<KitFile>(first, second)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit first)
        folder.placeLabFile "notes.txt" "Keep these notes"
        let before = folder.paths ()

        match otherProxy.initialiseKit second with
        | Error (KitAlreadyExistsErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | Error error -> Assert.Fail($"Expected KitAlreadyExistsErr, got {error.describe}")
        | Ok () -> Assert.Fail("An existing kit must not be initialised again.")

        Assert.Equal<Result<KitFile, KitError>>(Ok first, proxy.readKitFile ())
        Assert.Equal<Result<KitFile, KitError>>(Ok first, otherProxy.readKitFile ())
        Assert.Equal<string list>(before, folder.paths ())

    /// Whole-file replacement is visible to both existing and newly opened proxies (spec 0050 S4.1).
    [<Fact>]
    member this.``the kit file is replaced in place and read back equal`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let otherProxy = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let before = folder.paths ()
        let reviewed =
            {
                kit with
                    notes = KitNotes "Reviewed"
                    sendExclude = [ SendExcludeGlob "**/*.dng" ]
            }

        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKitFile reviewed)
        Assert.Equal<Result<KitFile, KitError>>(Ok reviewed, otherProxy.readKitFile ())
        Assert.Equal<Result<KitFile, KitError>>(Ok reviewed, (folder.openProxy ()).readKitFile ())
        Assert.Equal<string list>(before, folder.paths ())

        Assert.Equal<Result<unit, KitError>>(Ok (), otherProxy.writeKitFile kit)
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())
        Assert.Equal<string list>(before, folder.paths ())

    [<Fact>]
    member this.``kit files of every kind are listed, read, replaced and deleted by their names`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let otherProxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        let firstStem = FileStem.ofText "QZ-LR"
        let secondStem = FileStem.ofText "CPL-(QZ#45-LR#45)-CPL"
        let firstTexts =
            KitFileKind.all
            |> List.map (fun kind -> kind, KitFileText $"First {kind.extension.value}: μ, λ\r\n")
            |> Map.ofList
        let secondTexts =
            KitFileKind.all
            |> List.map (fun kind -> kind, KitFileText $"Second {kind.extension.value}: 測定\n")
            |> Map.ofList
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)

        for kind in KitFileKind.all do
            Assert.Equal<Result<FileStem list, KitError>>(Ok [], proxy.listKindFiles kind)
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile kind firstStem (firstTexts[kind]))
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile kind secondStem (secondTexts[kind]))

        let expectedPaths =
            [ ".kit-json"; "data"; "library" ]
            @ [ for kind in KitFileKind.all do
                    (kind.fileNameOf firstStem).value
                    (kind.fileNameOf secondStem).value ]
            |> List.sortWith (fun first second -> String.CompareOrdinal(first, second))
        Assert.Equal<string list>(expectedPaths, folder.paths ())

        for kind in KitFileKind.all do
            let replacement = KitFileText $"Replaced {kind.extension.value}\n"
            Assert.Equal<Result<FileStem list, KitError>>(Ok [ secondStem; firstStem ], otherProxy.listKindFiles kind)
            Assert.Equal<Result<KitFileText, KitError>>(Ok (firstTexts[kind]), otherProxy.readKindFile kind firstStem)
            Assert.Equal<Result<KitFileText, KitError>>(Ok (secondTexts[kind]), otherProxy.readKindFile kind secondStem)
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile kind firstStem replacement)
            Assert.Equal<Result<KitFileText, KitError>>(Ok replacement, otherProxy.readKindFile kind firstStem)
            Assert.Equal<Result<unit, KitError>>(Ok (), otherProxy.deleteKindFile kind secondStem)
            Assert.Equal<Result<FileStem list, KitError>>(Ok [ firstStem ], (folder.openProxy ()).listKindFiles kind)
            Assert.Contains((kind.fileNameOf firstStem).value, folder.paths ())
            Assert.DoesNotContain((kind.fileNameOf secondStem).value, folder.paths ())

            folder.placeLabFile $"ignored{kind.extension.value}.tmp" "Temporary text"
            folder.placeLabFile $"data/hidden{kind.extension.value}" "Nested text"
            let upperStem = FileStem.ofText "Z-lab"
            let lowerStem = FileStem.ofText "a-lab"
            let unicodeStem = FileStem.ofText "Å-lab"
            let labText = KitFileText $"Lab {kind.extension.value}: μ\n"
            for stem in [ lowerStem; unicodeStem; upperStem ] do
                folder.placeLabFile $"{stem.value}{kind.extension.value.ToUpperInvariant()}" $"\uFEFF{labText.value}"

            Assert.Equal<Result<FileStem list, KitError>>(
                Ok [ firstStem; upperStem; lowerStem; unicodeStem ], proxy.listKindFiles kind)
            for stem in [ upperStem; lowerStem; unicodeStem ] do
                Assert.Equal<Result<KitFileText, KitError>>(Ok labText, proxy.readKindFile kind stem)
                Assert.Equal<Result<unit, KitError>>(Ok (), otherProxy.writeKindFile kind stem replacement)
                Assert.Equal<Result<KitFileText, KitError>>(Ok replacement, proxy.readKindFile kind stem)
                folder.placeLabFile (kind.fileNameOf stem).value labText.value
                Assert.Equal<Result<KitFileText, KitError>>(Ok labText, otherProxy.readKindFile kind stem)
                Assert.Equal<Result<unit, KitError>>(Ok (), proxy.deleteKindFile kind stem)

            Assert.Equal<Result<FileStem list, KitError>>(Ok [ firstStem ], otherProxy.listKindFiles kind)
            Assert.Contains($"ignored{kind.extension.value}.tmp", folder.paths ())
            Assert.Contains($"data/hidden{kind.extension.value}", folder.paths ())

        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())

    [<Fact>]
    member this.``reading or deleting a missing kit file is MissingKitFileErr and changes nothing`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        let stem = FileStem.ofText "Never written"
        let keptStem = FileStem.ofText "QZ-LR"
        let keptText = KitFileText "Keep this text"
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        for kind in KitFileKind.all do
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile kind keptStem keptText)
        let before = folder.paths ()

        for kind in KitFileKind.all do
            let file = kind.fileNameOf stem
            match proxy.readKindFile kind stem with
            | Error (MissingKitFileErr reason) -> Assert.Contains(file.value, reason)
            | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
            | Ok _ -> Assert.Fail("Reading a missing kit file must fail.")
            Assert.Equal<string list>(before, folder.paths ())

            match proxy.deleteKindFile kind stem with
            | Error (MissingKitFileErr reason) -> Assert.Contains(file.value, reason)
            | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
            | Ok () -> Assert.Fail("Deleting a missing kit file must fail.")
            Assert.Equal<string list>(before, folder.paths ())
            Assert.Equal<Result<KitFileText, KitError>>(Ok keptText, proxy.readKindFile kind keptStem)

        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())

    [<Fact>]
    member this.``replacing a kit file leaves only its new text and no temporary file`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        let stem = FileStem.ofText "QZ-LR"
        let original = KitFileText "Long original setup text that must be completely replaced\n"
        let replacement = KitFileText "μ\n"
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile SetupKitFile stem original)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile SetupKitFile stem replacement)
        Assert.Equal<string list>([ ".kit-json"; "QZ-LR.setup-json"; "data"; "library" ], folder.paths ())
        Assert.False(folder.paths () |> List.exists (fun path -> path.EndsWith(".tmp", StringComparison.OrdinalIgnoreCase)))
        Assert.Equal<Result<KitFileText, KitError>>(Ok replacement, (folder.openProxy ()).readKindFile SetupKitFile stem)

    [<Fact>]
    member this.``a lab file is read by its kit-relative path with the SHA-256 of its bytes`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let otherProxy = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)

        let assertDataFile (path : KitRelativePath) (storedText : KitFileText) (expectedText : KitFileText) : unit =
            let before = folder.paths ()
            let expectedHash = ContentHash (Convert.ToHexStringLower(SHA256.HashData(Encoding.UTF8.GetBytes storedText.value)))
            for reader in [ proxy; otherProxy; folder.openProxy () ] do
                match reader.readDataFile path with
                | Ok data ->
                    Assert.Equal<KitRelativePath>(path, data.path)
                    Assert.Equal<KitFileText>(expectedText, data.text)
                    Assert.Equal<ContentHash>(expectedHash, data.contentHash)
                | Error error -> Assert.Fail($"Expected lab data for '{path.value}', got {error.describe}")
                Assert.Equal<string list>(before, folder.paths ())

        let assertLabFile (path : KitRelativePath) (storedText : KitFileText) (expectedText : KitFileText) : unit =
            folder.placeLabFile path.value storedText.value
            assertDataFile path storedText expectedText

        let assertKitMetadata (expectedKit : KitFile) : unit =
            let path = KitRelativePath ".kit-json"
            let expectedText = expectedKit.fileText |> Result.defaultWith (fun error -> failwith error.describe)
            assertDataFile path expectedText expectedText
            for reader in [ proxy; otherProxy; folder.openProxy () ] do
                Assert.Equal<Result<KitFile, KitError>>(Ok expectedKit, reader.readKitFile ())

        assertKitMetadata kit

        let runPath = KitRelativePath "data/raw/run 1.csv"
        let runText = KitFileText "phi,I\n0,0.5\n"
        assertLabFile runPath runText runText

        let notesPath = KitRelativePath "notes.txt"
        let rawPath = KitRelativePath "raw/run 2.csv"
        let unicodeText = KitFileText "λ,I\r\n600,0.5\r\n"
        assertLabFile notesPath unicodeText unicodeText
        assertLabFile rawPath unicodeText unicodeText

        let emptyPath = KitRelativePath "data/empty.csv"
        let emptyText = KitFileText ""
        assertLabFile emptyPath emptyText emptyText

        let bomPath = KitRelativePath "data/bom.csv"
        let bomText = KitFileText $"\uFEFF{unicodeText.value}"
        assertLabFile bomPath bomText unicodeText

        let embeddedBomPath = KitRelativePath "data/embedded-bom.csv"
        let embeddedBomText = KitFileText "a,\uFEFFb\n"
        assertLabFile embeddedBomPath embeddedBomText embeddedBomText

        let replacementText = KitFileText "phi,I\n0,0.75\n"
        assertLabFile runPath replacementText replacementText

        let before = folder.paths ()
        for missingPath in [ KitRelativePath "data/raw/none.csv"; KitRelativePath "absent/raw/none.csv" ] do
            match proxy.readDataFile missingPath with
            | Error (MissingKitFileErr reason) -> Assert.Contains(missingPath.value, reason)
            | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
            | Ok _ -> Assert.Fail("Reading a missing lab file must fail.")
            Assert.Equal<string list>(before, folder.paths ())

        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())

        let replacement =
            {
                kit with
                    name = KitName "Reviewed QZ/LR"
                    modified = clock.now ()
                    notes = KitNotes "Metadata: μ, λ\nReviewed"
                    sendExclude = [ SendExcludeGlob "**/*.dng" ]
            }
        let beforeReplacement = folder.paths ()
        Assert.Equal<Result<unit, KitError>>(Ok (), otherProxy.writeKitFile replacement)
        Assert.Equal<string list>(beforeReplacement, folder.paths ())
        assertKitMetadata replacement

    [<Fact>]
    member this.``a second proxy cannot take a held lock and is told who holds it, until it is released`` () : unit =
        let folder = this.freshFolder ()
        let proxyA = folder.openProxy ()
        let proxyB = folder.openProxy ()
        let proxyC = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.initialiseKit kit)
        let timeA = clock.now ()
        let holderA = takeKitLock proxyA timeA
        Assert.Equal<ClockTime>(timeA, holderA.acquiredAt)
        Assert.Equal<KitLockHolder>(holderA, storedKitLock proxyC)
        let before = folder.paths ()
        Assert.Equal<Result<KitLockHolder, KitError>>(Error (KitLockedErr holderA), proxyB.acquireLock (clock.now ()))
        Assert.Equal<string list>(before, folder.paths ())
        Assert.Equal<KitLockHolder>(holderA, storedKitLock proxyC)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.releaseLock ())

        let timeB = clock.now ()
        let holderB = takeKitLock proxyB timeB
        if holderA.owner.machine.value = "in-memory" then
            let expectedOwnerA : KitLockOwner =
                {
                    machine = MachineName "in-memory"
                    user = UserName "in-memory"
                    processId = ProcessId 1
                }
            let expectedOwnerB =
                {
                    expectedOwnerA with processId = ProcessId 2
                }
            Assert.Equal<KitLockOwner>(expectedOwnerA, holderA.owner)
            Assert.Equal<KitLockOwner>(expectedOwnerB, holderB.owner)
        else
            Assert.Equal<KitLockOwner>(currentKitLockOwner (), holderA.owner)
            Assert.Equal<KitLockOwner>(currentKitLockOwner (), holderB.owner)
        Assert.Equal<ClockTime>(timeB, holderB.acquiredAt)
        Assert.Equal<KitLockHolder>(holderB, storedKitLock proxyA)
        Assert.Equal<Result<KitLockHolder, KitError>>(Error (KitLockedErr holderB), proxyC.acquireLock (clock.now ()))
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.releaseLock ())
        Assert.Equal<KitLockHolder>(holderB, storedKitLock proxyC)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.releaseLock ())
        contendForKitLock folder clock

    [<Fact>]
    member this.``a proxy that holds the lock keeps it when it asks again, and another proxy's release leaves it`` () : unit =
        let folder = this.freshFolder ()
        let proxyA = folder.openProxy ()
        let proxyB = folder.openProxy ()
        let proxyC = folder.openProxy ()
        let clock = steppingClock ()
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.initialiseKit (newKitFile clock))
        let timeA = clock.now ()
        let holderA = takeKitLock proxyA timeA
        let before = folder.paths ()
        let lockPath = KitRelativePath ".oc/lock"
        let original = proxyC.readDataFile lockPath
        Assert.Equal<Result<KitLockHolder, KitError>>(Ok holderA, proxyA.acquireLock (clock.now ()))
        Assert.Equal<Result<KitDataFile, KitError>>(original, proxyC.readDataFile lockPath)
        // Equal process and timestamp do not transfer ownership to a different proxy (spec 0050 step 0129).
        Assert.Equal<Result<KitLockHolder, KitError>>(Error (KitLockedErr holderA), proxyB.acquireLock timeA)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.releaseLock ())
        Assert.Equal<Result<KitLockHolder, KitError>>(Error (KitLockedErr holderA), proxyC.acquireLock (clock.now ()))
        Assert.Equal<Result<KitDataFile, KitError>>(original, proxyC.readDataFile lockPath)
        Assert.Equal<string list>(before, folder.paths ())

        let replacement =
            {
                owner =
                    {
                        holderA.owner with machine = MachineName "another-machine"
                    }
                acquiredAt = clock.now ()
            }
        placeKitLock folder replacement
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.releaseLock ())
        Assert.Equal<KitLockHolder>(replacement, storedKitLock proxyC)
        Assert.Equal<Result<KitLockHolder, KitError>>(Error (KitLockedErr replacement), proxyA.acquireLock (clock.now ()))

    [<Fact>]
    member this.``the lock is written in the cache folder only when it is first taken, and its release keeps the folder`` () : unit =
        let folder = this.freshFolder ()
        let proxyA = folder.openProxy ()
        let proxyB = folder.openProxy ()
        let proxyC = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.initialiseKit kit)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.releaseLock ())
        Assert.Equal<string list>([ ".kit-json"; "data"; "library" ], folder.paths ())
        let holder = takeKitLock proxyA (clock.now ())
        Assert.Equal<string list>([ ".kit-json"; ".oc"; ".oc/lock"; "data"; "library" ], folder.paths ())
        Assert.Equal<KitLockHolder>(holder, storedKitLock proxyC)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.releaseLock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.releaseLock ())
        Assert.Equal<string list>([ ".kit-json"; ".oc"; "data"; "library" ], folder.paths ())
        let nextTime = clock.now ()
        let next = takeKitLock proxyA nextTime
        Assert.Equal<ClockTime>(nextTime, next.acquiredAt)
        Assert.Equal<KitLockOwner>(holder.owner, next.owner)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.releaseLock ())
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxyC.readKitFile ())

/// Runs the shared kit-file contract over one in-memory folder per fact (spec 0050 S4.1).
type KitProxyTests () =
    inherit KitProxyContract ()

    override _.freshFolder () : KitTestFolder =
        let folder = InMemoryKitFolder.create ()
        {
            openProxy = fun () -> KitProxy.createInMemory folder
            placeLabFile =
                fun path text ->
                    let fileText = KitFileText text
                    KitRelativePath.tryCreate path
                    |> Result.bind (fun relativePath -> folder.placeLabFile relativePath fileText)
                    |> Result.defaultWith (fun error -> failwith error.describe)
            paths = fun () -> folder.paths () |> List.map (fun path -> path.value)
        }

/// Runs the shared kit-file contract over real folders under the test output (spec 0050 S4.1).
type KitProxyFolderTests () =
    inherit KitProxyContract ()

    override _.freshFolder () : KitTestFolder =
        let folder = freshKitFolderPath ()
        Directory.CreateDirectory folder.value |> ignore
        kitFolderAdapter folder

    [<Fact>]
    member this.``the folder lock names this machine, this user and this process`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let clock = steppingClock ()
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit (newKitFile clock))
        let time = clock.now ()
        let holder = takeKitLock proxy time
        Assert.Equal<KitLockOwner>(currentKitLockOwner (), holder.owner)
        Assert.Equal<ClockTime>(time, holder.acquiredAt)
        Assert.Equal<KitLockHolder>(holder, storedKitLock (folder.openProxy ()))
        let path = KitRelativePath ".oc/lock"
        let data = proxy.readDataFile path |> Result.defaultWith (fun error -> failwith error.describe)
        let text = holder.lockFileText |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<KitFileText>(text, data.text)
        Assert.DoesNotContain("\r", text.value)
        Assert.EndsWith("\n", text.value)
        Assert.False(text.value.EndsWith("\n\n", StringComparison.Ordinal))
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.releaseLock ())

    [<Fact>]
    member this.``a lock left on this machine by a process that has ended is taken over`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let clock = steppingClock ()
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit (newKitFile clock))
        let stale =
            {
                owner =
                    {
                        machine = MachineName (Environment.MachineName.ToLowerInvariant())
                        user = UserName "previous-user"
                        processId = ProcessId Int32.MaxValue
                    }
                acquiredAt = clock.now ()
            }
        placeKitLock folder stale
        let time = clock.now ()
        let holder = takeKitLock proxy time
        Assert.Equal<KitLockOwner>(currentKitLockOwner (), holder.owner)
        Assert.Equal<ClockTime>(time, holder.acquiredAt)
        Assert.Equal<KitLockHolder>(holder, storedKitLock proxy)
        Assert.NotEqual<KitLockHolder>(stale, holder)
        Assert.Equal<string list>([ ".kit-json"; ".oc"; ".oc/lock"; "data"; "library" ], folder.paths ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.releaseLock ())
        placeKitLock folder stale
        contendForKitLock folder clock

    [<Fact>]
    member this.``a lock taken on another machine is live`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let clock = steppingClock ()
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit (newKitFile clock))
        let foreign =
            {
                owner =
                    {
                        currentKitLockOwner () with machine = MachineName "another-machine"
                    }
                acquiredAt = clock.now ()
            }
        placeKitLock folder foreign
        let before = folder.paths ()
        let lockPath = KitRelativePath ".oc/lock"
        let original = proxy.readDataFile lockPath
        Assert.Equal<Result<KitLockHolder, KitError>>(Error (KitLockedErr foreign), proxy.acquireLock (clock.now ()))
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.releaseLock ())
        Assert.Equal<KitLockHolder>(foreign, storedKitLock proxy)
        Assert.Equal<Result<KitDataFile, KitError>>(original, proxy.readDataFile lockPath)
        Assert.Equal<string list>(before, folder.paths ())

    [<Fact>]
    member _.``an unreadable lock is taken over`` () : unit =
        let folderPath = freshKitFolderPath ()
        let folder = kitFolderAdapter folderPath
        let proxy = folder.openProxy ()
        let clock = steppingClock ()
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit (newKitFile clock))
        for unreadable in [ "not a lock"; "null"; "{}"; "{\"owner\":null,\"acquiredAt\":\"2026-11-03T12:00:00Z\"}" ] do
            let text = KitFileText unreadable
            match text.parseLockHolder with
            | Error (OpticalConstructor.Storage.Errors.MalformedJsonErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"Expected MalformedJsonErr, got {error.describe}")
            | Ok _ -> Assert.Fail("An unreadable lock must not bind to a holder.")
            folder.placeLabFile ".oc/lock" text.value
            let time = clock.now ()
            let holder = takeKitLock proxy time
            Assert.Equal<KitLockOwner>(currentKitLockOwner (), holder.owner)
            Assert.Equal<ClockTime>(time, holder.acquiredAt)
            Assert.Equal<KitLockHolder>(holder, storedKitLock proxy)
            Assert.Equal<string list>([ ".kit-json"; ".oc"; ".oc/lock"; "data"; "library" ], folder.paths ())
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.releaseLock ())

        folder.placeLabFile ".oc/lock" "not a lock"
        let file = FileName (Path.Combine(folderPath.value, ".oc", "lock"))
        let originalBytes = File.ReadAllBytes file.value
        do
            use inaccessible = new FileStream(file.value, FileMode.Open, FileAccess.Read, FileShare.None)
            match proxy.acquireLock (clock.now ()) with
            | Error (KitIoErr reason) -> Assert.Contains(folderPath.value, reason)
            | Error error -> Assert.Fail($"Expected KitIoErr for inaccessible text, got {error.describe}")
            | Ok _ -> Assert.Fail("An IO failure must not be treated as malformed lock text.")
        Assert.Equal<byte>(originalBytes, File.ReadAllBytes file.value)
        contendForKitLock folder clock

        let holder = takeKitLock proxy (clock.now ())
        do
            use inaccessible = new FileStream(file.value, FileMode.Open, FileAccess.Read, FileShare.None)
            match proxy.releaseLock () with
            | Error (KitIoErr reason) -> Assert.Contains(folderPath.value, reason)
            | Error error -> Assert.Fail($"Expected KitIoErr for an inaccessible held lock, got {error.describe}")
            | Ok () -> Assert.Fail("An inaccessible held lock must report its IO failure.")
        Assert.Equal<KitLockHolder>(holder, storedKitLock proxy)
        Assert.Equal<Result<KitLockHolder, KitError>>(Ok holder, proxy.acquireLock (clock.now ()))
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.releaseLock ())
        Assert.Equal<string list>([ ".kit-json"; ".oc"; "data"; "library" ], folder.paths ())

    [<Fact>]
    member _.``a folder that does not exist is not created by its proxy, and initialising it creates it`` () : unit =
        let path = freshKitFolderPath ()
        let folder = kitFolderAdapter path
        Assert.False(Directory.Exists path.value)
        let proxy = folder.openProxy ()
        Assert.False(Directory.Exists path.value)
        let kit = newKitFile (steppingClock ())

        match proxy.readKitFile () with
        | Error (NotAKitFolderErr reason) -> Assert.Contains(path.value, reason)
        | Error error -> Assert.Fail($"Expected NotAKitFolderErr, got {error.describe}")
        | Ok _ -> Assert.Fail("Reading a missing folder must not create a kit.")

        Assert.False(Directory.Exists path.value)

        match proxy.writeKitFile kit with
        | Error (NotAKitFolderErr reason) -> Assert.Contains(path.value, reason)
        | Error error -> Assert.Fail($"Expected NotAKitFolderErr, got {error.describe}")
        | Ok () -> Assert.Fail("Replacing metadata must not create the missing folder.")

        Assert.False(Directory.Exists path.value)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        Assert.True(Directory.Exists path.value)
        Assert.Equal<string list>([ ".kit-json"; "data"; "library" ], folder.paths ())
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, (folder.openProxy ()).readKitFile ())

    [<Fact>]
    member _.``a kit file of another schema version or of broken text is refused, naming the kit file`` () : unit =
        let path = freshKitFolderPath ()
        let folder = kitFolderAdapter path
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let file = KitFile.fileName.combine path
        let originalBytes = File.ReadAllBytes file.value
        let originalText = Encoding.UTF8.GetString originalBytes
        Assert.False(originalText.StartsWith('\uFEFF'))

        File.WriteAllText(file.value, originalText, UTF8Encoding true)
        let withBom = File.ReadAllBytes file.value
        Assert.True((Encoding.UTF8.GetString withBom).StartsWith('\uFEFF'))
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxy.readKitFile ())
        Assert.Equal<byte>(withBom, File.ReadAllBytes file.value)

        let unsupported = JsonNode.Parse originalText
        unsupported["schemaVersion"] <- JsonValue.Create 2
        let unsupportedText = unsupported.ToJsonString()
        folder.placeLabFile KitFile.fileName.value unsupportedText

        match proxy.readKitFile () with
        | Error (KitFileRejectedErr reason) ->
            Assert.Contains(KitFile.fileName.value, reason)
            Assert.Contains(path.value, reason)
            Assert.Contains("schema version 2", reason)
            Assert.Contains("reads version 1", reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok _ -> Assert.Fail("An unsupported kit schema version must be refused.")

        Assert.Equal<byte>(Encoding.UTF8.GetBytes unsupportedText, File.ReadAllBytes file.value)
        folder.placeLabFile KitFile.fileName.value "{"

        match proxy.readKitFile () with
        | Error (KitFileRejectedErr reason) ->
            Assert.Contains(KitFile.fileName.value, reason)
            Assert.Contains(path.value, reason)
        | Error error -> Assert.Fail($"Expected KitFileRejectedErr, got {error.describe}")
        | Ok _ -> Assert.Fail("Malformed kit JSON must be refused.")

        Assert.Equal<byte>(Encoding.UTF8.GetBytes "{", File.ReadAllBytes file.value)
        Assert.Equal<string list>([ ".kit-json"; "data"; "library" ], folder.paths ())

    [<Fact>]
    member _.``a replace that fails keeps the old file whole, removes its temporary file and is KitIoErr`` () : unit =
        let path = freshKitFolderPath ()
        let folder = kitFolderAdapter path
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        let stem = FileStem.ofText "QZ-LR"
        let original = KitFileText "Original setup: μ, λ\r\n"
        let replacement = KitFileText "New setup: 測定\n"
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile SetupKitFile stem original)
        let file = (SetupKitFile.fileNameOf stem).combine path
        let originalBytes = File.ReadAllBytes file.value
        Assert.Equal<byte>(Encoding.UTF8.GetBytes original.value, originalBytes)
        let before = folder.paths ()

        do
            use lockedFile = new FileStream(file.value, FileMode.Open, FileAccess.Read, FileShare.None)
            match proxy.writeKindFile SetupKitFile stem replacement with
            | Error (KitIoErr reason) ->
                Assert.Contains(file.value, reason)
                Assert.Contains(path.value, reason)
            | Error error -> Assert.Fail($"Expected KitIoErr, got {error.describe}")
            | Ok () -> Assert.Fail("Replacing an exclusively held file must fail.")
            Assert.Equal<string list>(before, folder.paths ())

        Assert.Equal<byte>(originalBytes, File.ReadAllBytes file.value)
        Assert.Equal<Result<KitFileText, KitError>>(Ok original, proxy.readKindFile SetupKitFile stem)
        Assert.Equal<string list>(before, folder.paths ())
        Assert.False(folder.paths () |> List.exists (fun name -> name.EndsWith(".tmp", StringComparison.OrdinalIgnoreCase)))

        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile SetupKitFile stem replacement)
        Assert.Equal<byte>(Encoding.UTF8.GetBytes replacement.value, File.ReadAllBytes file.value)
        Assert.Equal<Result<KitFileText, KitError>>(Ok replacement, proxy.readKindFile SetupKitFile stem)
        Assert.Equal<string list>(before, folder.paths ())

    [<Fact>]
    member _.``a lab file with a byte-order mark reads without it and hashes its bytes as stored`` () : unit =
        let path = freshKitFolderPath ()
        let folder = kitFolderAdapter path
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let relativePath = KitRelativePath "data/run.csv"
        let text = KitFileText "a,b\n"
        let file = FileName (Path.GetFullPath(Path.Combine(path.value, relativePath.value)))
        let bytes = Array.append [| 0xEFuy; 0xBBuy; 0xBFuy |] (Encoding.UTF8.GetBytes text.value)
        let expectedHash = ContentHash (Convert.ToHexStringLower(SHA256.HashData bytes))
        let textHash = ContentHash (Convert.ToHexStringLower(SHA256.HashData(Encoding.UTF8.GetBytes text.value)))
        File.WriteAllBytes(file.value, bytes)
        let before = folder.paths ()

        match proxy.readDataFile relativePath with
        | Ok data ->
            Assert.Equal<KitRelativePath>(relativePath, data.path)
            Assert.Equal<KitFileText>(text, data.text)
            Assert.Equal<ContentHash>(expectedHash, data.contentHash)
            Assert.NotEqual<ContentHash>(textHash, data.contentHash)
        | Error error -> Assert.Fail($"Expected BOM-prefixed lab data, got {error.describe}")

        Assert.Equal<byte>(bytes, File.ReadAllBytes file.value)
        Assert.Equal<string list>(before, folder.paths ())

    [<Fact>]
    member _.``a lab file another process holds open is KitIoErr, never a throw`` () : unit =
        let path = freshKitFolderPath ()
        let folder = kitFolderAdapter path
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let relativePath = KitRelativePath "data/run.csv"
        let text = KitFileText "angle,intensity\n45,0.25\n"
        folder.placeLabFile relativePath.value text.value
        let file = FileName (Path.GetFullPath(Path.Combine(path.value, relativePath.value)))
        let bytes = File.ReadAllBytes file.value
        let expectedHash = ContentHash (Convert.ToHexStringLower(SHA256.HashData bytes))
        let before = folder.paths ()

        do
            use lockedFile = new FileStream(file.value, FileMode.Open, FileAccess.Read, FileShare.None)
            match proxy.readDataFile relativePath with
            | Error (KitIoErr reason) ->
                Assert.Contains(file.value, reason)
                Assert.Contains(path.value, reason)
            | Error error -> Assert.Fail($"Expected KitIoErr, got {error.describe}")
            | Ok _ -> Assert.Fail("Reading an exclusively held lab file must fail.")
            Assert.Equal<string list>(before, folder.paths ())

        Assert.Equal<byte>(bytes, File.ReadAllBytes file.value)
        match proxy.readDataFile relativePath with
        | Ok data ->
            Assert.Equal<KitRelativePath>(relativePath, data.path)
            Assert.Equal<KitFileText>(text, data.text)
            Assert.Equal<ContentHash>(expectedHash, data.contentHash)
        | Error error -> Assert.Fail($"Expected lab data after releasing the handle, got {error.describe}")
        Assert.Equal<string list>(before, folder.paths ())
