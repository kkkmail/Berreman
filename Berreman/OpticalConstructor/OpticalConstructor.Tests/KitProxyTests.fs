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
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.KitFolderStore

/// One folder viewed through fresh proxies and an outside writer (spec 0050 S4.1).
type KitTestFolder =
    {
        openProxy : unit -> KitProxy
        placeLabFile : string -> string -> unit
        paths : unit -> string list
        /// Resolves a kit-relative path to its absolute name (spec 0050 S4.2).
        absoluteName : string -> FileName
        /// Names a file outside this kit without creating its folder (spec 0050 S4.2).
        outsideName : string -> FileName
        /// Deletes a file as an outside writer (spec 0050 S4.2).
        removeLabFile : string -> unit
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
        let outside = FolderName (Path.Combine(Path.GetDirectoryName folder.value, $"outside-{Guid.NewGuid():N}"))
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
            absoluteName = fun path -> FileName (Path.Combine(folder.value, path))
            outsideName = fun path -> FileName (Path.Combine(outside.value, path))
            removeLabFile = fun path -> File.Delete(Path.Combine(folder.value, path))
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

    let externallyChangedKit (folder : KitTestFolder) : KitProxy =
        let proxyA = folder.openProxy ()
        let proxyB = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        let experiment = FileStem.ofText "X"
        let added = FileStem.ofText "Y"
        let own = FileStem.ofText "own"
        let original = KitFileText "Original experiment: μ\n"
        let changed = KitFileText "Changed experiment: λ\n"
        let setup = KitFileText "Setup\n"
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxyA.changedKitFiles ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.initialiseKit kit)
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxyA.changedKitFiles ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.writeKindFile ExperimentKitFile experiment original)
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxyA.changedKitFiles ())
        let before = folder.paths ()
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.recordStamps ())
        Assert.Equal<string list>(before, folder.paths ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.writeKindFile SetupKitFile own setup)
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxyA.changedKitFiles ())
        let initiallyPresent = [ KitFile.fileName; ExperimentKitFile.fileNameOf experiment; SetupKitFile.fileNameOf own ]
        Assert.Equal<Result<FileName list, KitError>>(Ok initiallyPresent, proxyB.changedKitFiles ())

        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.writeKindFile SetupKitFile added setup)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.writeKindFile ExperimentKitFile experiment changed)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.deleteKindFile SetupKitFile own)
        let reviewed =
            {
                kit with notes = KitNotes "Reviewed by another writer"
            }
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.writeKitFile reviewed)
        let expected = [ KitFile.fileName; ExperimentKitFile.fileNameOf experiment; SetupKitFile.fileNameOf added; SetupKitFile.fileNameOf own ]
        for _ in 1 .. 2 do
            Assert.Equal<Result<FileName list, KitError>>(Ok expected, proxyA.changedKitFiles ())
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxyB.changedKitFiles ())
        proxyA

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
        let completed = Task.WhenAll tasks
        Assert.True(completed.Wait(TimeSpan.FromSeconds 10.0), "Kit lock contention must finish within its bounded retry budget.")
        let results = completed.GetAwaiter().GetResult() |> Array.toList
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

    let assertKitGuardFailure (file : FileName) (cause : exn) (result : Result<'Value, KitError>) : unit =
        match result with
        | Error (KitIoErr reason) ->
            Assert.Contains(file.value, reason)
            Assert.Contains(cause.GetType().Name, reason)
            Assert.Contains($"0x{uint32 cause.HResult:X8}", reason)
            Assert.Contains(cause.Message, reason)
        | Error error -> Assert.Fail($"Expected a guard IO failure, got {error.describe}")
        | Ok _ -> Assert.Fail("An unavailable guard must report its original IO failure.")

    type FolderName with

        member this.assertKitGuardRetryPolicy () : unit =
            let file = FileName (Path.Combine(this.value, "policy-guard.tmp"))
            let actual = KitLockGuardProxy.create file
            let transientFailures : exn list =
                [
                    IOException("sharing", int 0x80070020u)
                    IOException("locked", int 0x80070021u)
                    UnauthorizedAccessException("pending deletion")
                ]
            for failures in [ []; transientFailures; List.replicate 100 (UnauthorizedAccessException("pending deletion") :> exn) ] do
                let pending = System.Collections.Generic.Queue<exn>(failures)
                let mutable openings = 0
                let mutable waits = 0
                let scripted =
                    {
                        actual with
                            tryOpenGuard =
                                fun () ->
                                    openings <- openings + 1
                                    if pending.Count = 0 then actual.tryOpenGuard ()
                                    else Error (KitGuardIoExn (file, pending.Dequeue ()))
                            waitForGuardRetry = fun () -> waits <- waits + 1; Ok ()
                    }
                do
                    use guard = scripted.acquireGuard () |> Result.defaultWith (fun error -> failwith error.describe)
                    Assert.True guard.CanWrite
                    Assert.Equal(failures.Length + 1, openings)
                    Assert.Equal(failures.Length, waits)
                Assert.False(File.Exists file.value)

            for failure in transientFailures do
                let mutable openings = 0
                let mutable waits = 0
                let permanent =
                    {
                        tryOpenGuard = fun () -> openings <- openings + 1; Error (KitGuardIoExn (file, failure))
                        waitForGuardRetry = fun () -> waits <- waits + 1; Ok ()
                    }
                permanent.acquireGuard () |> assertKitGuardFailure file failure
                Assert.Equal(101, openings)
                Assert.Equal(100, waits)

            let fatal = FileNotFoundException "guard parent missing"
            let mutable openings = 0
            let nonRetryable =
                {
                    tryOpenGuard = fun () -> openings <- openings + 1; Error (KitGuardIoExn (file, fatal))
                    waitForGuardRetry = fun () -> Assert.Fail("A non-retryable IO failure must not wait."); Ok ()
                }
            nonRetryable.acquireGuard () |> assertKitGuardFailure file fatal
            Assert.Equal(1, openings)

            let interrupted = ThreadInterruptedException "retry interrupted"
            let waitingFails =
                {
                    nonRetryable with
                        tryOpenGuard = fun () -> Error (KitGuardIoExn (file, UnauthorizedAccessException "pending deletion"))
                        waitForGuardRetry = fun () -> Error (KitGuardIoExn (file, interrupted))
                }
            waitingFails.acquireGuard () |> assertKitGuardFailure file interrupted
            Assert.False(File.Exists file.value)

    let assertBlockedKitGuard (folderPath : FolderName) (proxy : KitProxy) (clock : ClockProvider) : unit =
        let guard = FileName (Path.Combine(folderPath.value, ".oc", "lock-guard.tmp"))
        let folder = kitFolderAdapter folderPath
        let blockers : (string * ((unit -> unit) -> unit)) list =
            [
                "0x80070020", (fun action ->
                    use held = new FileStream(guard.value, FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None, 1, FileOptions.DeleteOnClose)
                    action ())
                "0x80070005", (fun action ->
                    File.WriteAllText(guard.value, "outside guard")
                    File.SetAttributes(guard.value, FileAttributes.ReadOnly)
                    try action ()
                    finally
                        File.SetAttributes(guard.value, FileAttributes.Normal)
                        Assert.Equal("outside guard", File.ReadAllText guard.value)
                        File.Delete guard.value)
            ]
        for nativeError, block in blockers do
            let checkFailure (operation : unit -> Result<'Value, KitError>) : unit =
                let running = Task.Run<Result<'Value, KitError>>(fun () -> operation ())
                Assert.True(running.Wait(TimeSpan.FromSeconds 10.0), "A permanently unavailable guard must finish.")
                match running.GetAwaiter().GetResult() with
                | Error (KitIoErr reason) ->
                    Assert.Contains(guard.value, reason)
                    Assert.Contains(nativeError, reason)
                | Error error -> Assert.Fail($"Expected guard IO failure, got {error.describe}")
                | Ok _ -> Assert.Fail("A blocked guard must not complete acquisition or release.")
            block (fun () -> checkFailure (fun () -> proxy.acquireLock (clock.now ())))
            Assert.Equal<string list>([ ".kit-json"; ".oc"; "data"; "library" ], folder.paths ())
            let holder = takeKitLock proxy (clock.now ())
            block (fun () -> checkFailure proxy.releaseLock)
            Assert.Equal<KitLockHolder>(holder, storedKitLock proxy)
            Assert.Equal<Result<KitLockHolder, KitError>>(Ok holder, proxy.acquireLock (clock.now ()))
            Assert.Equal<Result<KitLockHolder, KitError>>(Error (KitLockedErr holder), (folder.openProxy ()).acquireLock (clock.now ()))
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.releaseLock ())
            Assert.Equal<string list>([ ".kit-json"; ".oc"; "data"; "library" ], folder.paths ())

open KitProxyTestData

/// The kit-file facts shared by in-memory and filesystem folders (spec 0050 S4.1).
[<AbstractClass>]
type KitProxyContract () =

    /// Returns an existing folder that holds no files or subfolders (spec 0050 S4.1).
    abstract freshFolder : unit -> KitTestFolder

    [<Fact>]
    member this.``a copied-in file keeps its name and bytes, and a taken name gets the next free suffix`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let leaf = FileName "run 1.csv"
        let firstText = KitFileText "phi,I\n0,0.5\n"
        let otherText = KitFileText "phi,I\n0,0.75\n"
        let firstBytes = Encoding.UTF8.GetBytes firstText.value |> FileByteContent
        let otherBytes = Encoding.UTF8.GetBytes otherText.value |> FileByteContent
        let firstPath = KitRelativePath "data/run 1.csv"
        let secondPath = KitRelativePath "data/run 1 (2).csv"
        let thirdPath = KitRelativePath "data/run 1 (3).csv"
        let assertCopy (name : FileName) (bytes : FileByteContent) (path : KitRelativePath) (text : KitFileText) : KitDataFile =
            let copied =
                proxy.copyInFile KitDataFolder.root name bytes
                |> Result.defaultWith (fun error -> failwith error.describe)
            let expected : KitDataFile =
                {
                    path = path
                    text = text
                    contentHash = ContentHash.ofBytes bytes
                }
            Assert.Equal<KitDataFile>(expected, copied)
            for reader in [ proxy; folder.openProxy () ] do
                Assert.Equal<Result<KitDataFile, KitError>>(Ok copied, reader.readDataFile path)
            copied
        let first = assertCopy leaf firstBytes firstPath firstText
        assertCopy leaf otherBytes secondPath otherText |> ignore
        assertCopy leaf otherBytes thirdPath otherText |> ignore
        Assert.Equal<Result<KitDataFile, KitError>>(Ok first, proxy.readDataFile firstPath)
        Assert.Equal<string list>(
            [ ".kit-json"; "data"; "data/run 1 (2).csv"; "data/run 1 (3).csv"; "data/run 1.csv"; "library" ],
            folder.paths ())

        folder.placeLabFile "data/RUN 1 (4).csv/held.txt" "A folder also occupies a name"
        let outside = folder.outsideName "run 1.csv"
        let fifthPath = KitRelativePath "data/run 1 (5).csv"
        assertCopy outside otherBytes fifthPath otherText |> ignore
        Assert.Contains("data/RUN 1 (4).csv/held.txt", folder.paths ())
        Assert.Equal<Result<KitDataFile, KitError>>(Ok first, proxy.readDataFile firstPath)
        folder.removeLabFile secondPath.value
        assertCopy leaf firstBytes secondPath firstText |> ignore
        Assert.False(folder.paths () |> List.exists (fun name -> name.EndsWith(".tmp", StringComparison.OrdinalIgnoreCase)))

    [<Fact>]
    member this.``copying into a new folder inside data creates it`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let relativeFolder = KitRelativePath "data/raw/2026-11-03"
        let destination = KitDataFolder.tryCreate relativeFolder |> Result.defaultWith (fun error -> failwith error.describe)
        let leaf = FileName "run.csv"
        let text = KitFileText "phi,I\n0,0.5\n"
        let bytes = Encoding.UTF8.GetBytes text.value |> FileByteContent
        let expectedPath = KitRelativePath "data/raw/2026-11-03/run.csv"
        let copied = proxy.copyInFile destination leaf bytes |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<KitRelativePath>(expectedPath, copied.path)
        Assert.Equal<KitFileText>(text, copied.text)
        Assert.Equal<ContentHash>(ContentHash.ofBytes bytes, copied.contentHash)
        Assert.Equal<Result<KitDataFile, KitError>>(Ok copied, (folder.openProxy ()).readDataFile expectedPath)
        Assert.Equal<string list>(
            [ ".kit-json"; "data"; "data/raw"; "data/raw/2026-11-03"; "data/raw/2026-11-03/run.csv"; "library" ],
            folder.paths ())

    [<Fact>]
    member this.``a file inside the kit is located by its absolute name, and one outside it is not`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let path = KitRelativePath "data/run 1.csv"
        let inside = folder.absoluteName path.value
        let outside = folder.outsideName "run 1.csv"
        let before = folder.paths ()
        for reader in [ proxy; proxy.readOnlyView; folder.openProxy () ] do
            Assert.Equal<KitRelativePath option>(Some path, reader.tryKitPathOf inside)
            Assert.Equal<KitRelativePath option>(None, reader.tryKitPathOf outside)
        Assert.Equal<string list>(before, folder.paths ())
        folder.placeLabFile path.value "phi,I\n0,0.5\n"
        Assert.Equal<KitRelativePath option>(Some path, proxy.tryKitPathOf inside)
        folder.removeLabFile path.value
        Assert.Equal<KitRelativePath option>(Some path, proxy.tryKitPathOf inside)
        match proxy.readDataFile path with
        | Error (MissingKitFileErr reason) -> Assert.Contains(path.value, reason)
        | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
        | Ok _ -> Assert.Fail("An outside deletion must be visible to the kit proxy.")
        Assert.Equal<string list>(before, folder.paths ())

    [<Fact>]
    member this.``a read-only kit refuses a copy-in and writes nothing`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let path = KitRelativePath "data/run 1.csv"
        folder.placeLabFile path.value "Keep this lab file\n"
        let original = proxy.readDataFile path
        let before = folder.paths ()
        let reader = proxy.readOnlyView
        let leaf = FileName "run 1.csv"
        let bytes = Encoding.UTF8.GetBytes "Refused copy\n" |> FileByteContent
        let missingPath = KitRelativePath "data/new/raw"
        let missingFolder = KitDataFolder.tryCreate missingPath |> Result.defaultWith (fun error -> failwith error.describe)
        for destination in [ KitDataFolder.root; missingFolder ] do
            Assert.Equal<Result<KitDataFile, KitError>>(
                Error (KitReadOnlyErr "copyInFile"), reader.copyInFile destination leaf bytes)
            Assert.Equal<string list>(before, folder.paths ())
            Assert.Equal<Result<KitDataFile, KitError>>(original, proxy.readDataFile path)
        Assert.Equal<KitRelativePath option>(Some path, reader.tryKitPathOf (folder.absoluteName path.value))

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
    member this.``kit files another writer adds, changes or removes are reported, and this proxy's own writes are not`` () : unit =
        let folder = this.freshFolder ()
        let proxy = externallyChangedKit folder
        let experiment = FileStem.ofText "X"
        let added = FileStem.ofText "Y"
        let own = FileStem.ofText "own"
        let metadata = proxy.readKitFile () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKitFile metadata)
        let remaining = [ ExperimentKitFile.fileNameOf experiment; SetupKitFile.fileNameOf added; SetupKitFile.fileNameOf own ]
        Assert.Equal<Result<FileName list, KitError>>(Ok remaining, proxy.changedKitFiles ())
        let replacement = KitFileText "Own replacement\n"
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile ExperimentKitFile experiment replacement)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.deleteKindFile SetupKitFile added)
        match proxy.deleteKindFile SetupKitFile own with
        | Error (MissingKitFileErr _) -> ()
        | Error error -> Assert.Fail($"Expected MissingKitFileErr, got {error.describe}")
        | Ok () -> Assert.Fail("Deleting an already removed file must fail.")
        Assert.Equal<Result<FileName list, KitError>>(Ok [ SetupKitFile.fileNameOf own ], proxy.changedKitFiles ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile SetupKitFile own replacement)
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxy.changedKitFiles ())

        let observer = folder.openProxy ()
        Assert.Equal<Result<unit, KitError>>(Ok (), observer.recordStamps ())
        let stem = FileStem.ofText "Every kind"
        let expected = KitFileKind.all |> List.map (fun kind -> kind.fileNameOf stem) |> List.sortWith (fun a b -> String.CompareOrdinal(a.value, b.value))
        for text in [ KitFileText "First\n"; KitFileText "Changed\n" ] do
            for kind in KitFileKind.all do
                Assert.Equal<Result<unit, KitError>>(Ok (), proxy.writeKindFile kind stem text)
            Assert.Equal<Result<FileName list, KitError>>(Ok [], proxy.changedKitFiles ())
            Assert.Equal<Result<FileName list, KitError>>(Ok expected, observer.changedKitFiles ())
            Assert.Equal<Result<unit, KitError>>(Ok (), observer.recordStamps ())
        for kind in KitFileKind.all do
            Assert.Equal<Result<unit, KitError>>(Ok (), proxy.deleteKindFile kind stem)
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxy.changedKitFiles ())
        Assert.Equal<Result<FileName list, KitError>>(Ok expected, observer.changedKitFiles ())

    [<Fact>]
    member this.``recording the stamps again clears the report, and lab files are never reported`` () : unit =
        let folder = this.freshFolder ()
        let proxy = externallyChangedKit folder
        let before = folder.paths ()
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.recordStamps ())
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxy.changedKitFiles ())
        Assert.Equal<string list>(before, folder.paths ())
        let ignored =
            [ "data/new.csv"; "notes.txt"; ".oc/lock"; ".kit-json.tmp"; "library/.kit-json" ]
            @ [ for kind in KitFileKind.all do
                    $"data/hidden{kind.extension.value}"
                    $".oc/hidden{kind.extension.value}"
                    $"library/hidden{kind.extension.value}"
                    $"pending{kind.extension.value}.tmp" ]
        for text in [ "Lab content"; "Changed lab content" ] do
            for path in ignored do folder.placeLabFile path text
            Assert.Equal<Result<FileName list, KitError>>(Ok [], proxy.changedKitFiles ())
        let present = [ FileName ".kit-json"; FileName "X.experiment-json"; FileName "Y.setup-json" ]
        let beforeOpen = folder.paths ()
        let fresh = folder.openProxy ()
        Assert.Equal<Result<FileName list, KitError>>(Ok present, fresh.changedKitFiles ())
        Assert.Equal<Result<unit, KitError>>(Ok (), fresh.recordStamps ())
        Assert.Equal<Result<FileName list, KitError>>(Ok [], fresh.changedKitFiles ())
        Assert.Equal<string list>(beforeOpen, folder.paths ())

        let other = folder.openProxy ()
        let metadata = proxy.readKitFile () |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Result<unit, KitError>>(Ok (), other.writeKitFile metadata)
        let experiment = FileStem.ofText "X"
        let text = proxy.readKindFile ExperimentKitFile experiment |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Result<unit, KitError>>(Ok (), other.writeKindFile ExperimentKitFile experiment text)
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxy.changedKitFiles ())

        let stems = [ FileStem.ofText "Z"; FileStem.ofText "a"; FileStem.ofText "Å" ]
        let uppercaseFiles =
            [ for stem in stems do
                for kind in KitFileKind.all do
                    FileName $"{stem.value}{kind.extension.value.ToUpperInvariant()}" ]
            |> List.sortWith (fun a b -> String.CompareOrdinal(a.value, b.value))
        for file in uppercaseFiles do folder.placeLabFile file.value "Unicode: μ\n"
        Assert.Equal<Result<FileName list, KitError>>(Ok uppercaseFiles, proxy.changedKitFiles ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.recordStamps ())
        Assert.Equal<Result<unit, KitError>>(Ok (), other.recordStamps ())
        let sameText = KitFileText "Unicode: μ\n"
        for stem in stems do
            for kind in KitFileKind.all do
                Assert.Equal<Result<unit, KitError>>(Ok (), other.writeKindFile kind stem sameText)
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxy.changedKitFiles ())
        Assert.Equal<Result<FileName list, KitError>>(Ok [], other.changedKitFiles ())

        let bom = KitFileText $"\uFEFF{text.value}"
        folder.placeLabFile (ExperimentKitFile.fileNameOf experiment).value bom.value
        Assert.Equal<Result<FileName list, KitError>>(Ok [ ExperimentKitFile.fileNameOf experiment ], proxy.changedKitFiles ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.recordStamps ())
        folder.placeLabFile (ExperimentKitFile.fileNameOf experiment).value bom.value
        Assert.Equal<Result<FileName list, KitError>>(Ok [], proxy.changedKitFiles ())

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
        for _ in 1 .. 25 do
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

    [<Fact>]
    member this.``opening a kit read-write reads its kit file and takes the lock, and closing it releases the lock`` () : unit =
        let folder = this.freshFolder ()
        let proxyA = folder.openProxy ()
        let proxyB = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.initialiseKit kit)
        Assert.Equal<Result<FileName list, KitError>>(Ok [ KitFile.fileName ], proxyA.changedKitFiles ())

        let context =
            KitContext.openKit proxyA ReadWriteKitAccess clock
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<KitFile>(kit, context.kitFile)
        Assert.Equal<KitAccess>(ReadWriteKitAccess, context.access)
        Assert.Same(proxyA, context.proxy)
        Assert.Same(clock, context.clock)
        Assert.Contains(".oc/lock", folder.paths ())
        let lockTime = ClockTime (kit.created.value.AddSeconds 1.0)
        Assert.Equal<ClockTime>(lockTime, (storedKitLock proxyB).acquiredAt)
        Assert.Equal<Result<FileName list, KitError>>(Ok [], context.proxy.changedKitFiles ())
        Assert.Equal<Result<unit, KitError>>(Ok (), context.close ())
        Assert.DoesNotContain(".oc/lock", folder.paths ())
        Assert.Equal<Result<unit, KitError>>(Ok (), context.close ())

        let stampError = KitIoErr "cannot record kit-file stamps"
        let cleanupError = KitIoErr "lock cleanup reported an error"
        for cleanupResult in [ Ok (); Error cleanupError ] do
            let failingProxy =
                {
                    proxyA with
                        recordStamps =
                            fun () ->
                                Assert.Contains(".oc/lock", folder.paths ())
                                Error stampError
                        releaseLock =
                            fun () ->
                                Assert.Contains(".oc/lock", folder.paths ())
                                Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.releaseLock ())
                                cleanupResult
                }
            let result = KitContext.openKit failingProxy ReadWriteKitAccess clock |> Result.map (fun opened -> opened.kitFile)
            Assert.Equal<Result<KitFile, KitError>>(Error stampError, result)
            Assert.DoesNotContain(".oc/lock", folder.paths ())
            let reopened =
                KitContext.openKit proxyB ReadWriteKitAccess clock
                |> Result.defaultWith (fun error -> failwith error.describe)
            Assert.Equal<Result<unit, KitError>>(Ok (), reopened.close ())

    [<Fact>]
    member this.``a kit open elsewhere refuses a read-write open and allows a read-only one`` () : unit =
        let folder = this.freshFolder ()
        let proxyA = folder.openProxy ()
        let proxyB = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.initialiseKit kit)
        let owner =
            KitContext.openKit proxyA ReadWriteKitAccess clock
            |> Result.defaultWith (fun error -> failwith error.describe)
        let holder = storedKitLock proxyB
        let before = folder.paths ()
        let lockBefore = proxyB.readDataFile KitRelativePath.kitLockPath
        let refusedProxy =
            {
                proxyB with
                    recordStamps = fun () -> failwith "A lock refusal must not record stamps."
                    releaseLock = fun () -> failwith "A lock refusal must not release a lock."
            }
        let refused = KitContext.openKit refusedProxy ReadWriteKitAccess clock |> Result.map (fun opened -> opened.kitFile)
        Assert.Equal<Result<KitFile, KitError>>(Error (KitLockedErr holder), refused)
        Assert.Equal<string list>(before, folder.paths ())
        Assert.Equal<Result<KitDataFile, KitError>>(lockBefore, proxyB.readDataFile KitRelativePath.kitLockPath)

        let unusedClock : ClockProvider =
            {
                now = fun () -> failwith "A read-only open must not consult the lock clock."
            }
        for proxy in [ proxyB; proxyA ] do
            let reader =
                KitContext.openKit proxy ReadOnlyKitAccess unusedClock
                |> Result.defaultWith (fun error -> failwith error.describe)
            Assert.Equal<KitFile>(kit, reader.kitFile)
            Assert.Equal<KitAccess>(ReadOnlyKitAccess, reader.access)
            Assert.Same(unusedClock, reader.clock)
            Assert.Equal<Result<FileName list, KitError>>(Ok [], reader.proxy.changedKitFiles ())
            Assert.Equal<Result<unit, KitError>>(Ok (), reader.close ())
            Assert.Equal<string list>(before, folder.paths ())
            Assert.Equal<Result<KitDataFile, KitError>>(lockBefore, proxyB.readDataFile KitRelativePath.kitLockPath)

        Assert.Equal<Result<unit, KitError>>(Ok (), owner.close ())
        let nextOwner =
            KitContext.openKit proxyB ReadWriteKitAccess clock
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Result<unit, KitError>>(Ok (), nextOwner.close ())

    [<Fact>]
    member this.``a read-only kit context writes nothing, not even the cache folder`` () : unit =
        let folder = this.freshFolder ()
        let proxyA = folder.openProxy ()
        let proxyB = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock
        let experiment = FileStem.ofText "QZ-LR"
        let original = KitFileText "Original experiment: μ\n"
        let replacement = KitFileText "Refused replacement\n"
        let reviewed =
            {
                kit with notes = KitNotes "Refused review"
            }
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.initialiseKit kit)
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.writeKindFile ExperimentKitFile experiment original)
        let before = folder.paths ()
        let guardedProxy =
            {
                proxyA with
                    acquireLock = fun _ -> failwith "A read-only context must not acquire a lock."
                    releaseLock = fun () -> failwith "A read-only context must not release a lock."
            }
        let context =
            KitContext.openKit guardedProxy ReadOnlyKitAccess clock
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<KitAccess>(ReadOnlyKitAccess, context.access)
        Assert.Equal<KitFile>(kit, context.kitFile)
        Assert.Equal<string list>(before, folder.paths ())
        let assertReadOnly (verb : string) (result : Result<unit, KitError>) : unit =
            Assert.Equal<Result<unit, KitError>>(Error (KitReadOnlyErr verb), result)
            Assert.Equal<string list>(before, folder.paths ())
            Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxyB.readKitFile ())
            Assert.Equal<Result<KitFileText, KitError>>(Ok original, proxyB.readKindFile ExperimentKitFile experiment)
        assertReadOnly "initialiseKit" (context.proxy.initialiseKit reviewed)
        assertReadOnly "writeKitFile" (context.proxy.writeKitFile reviewed)
        assertReadOnly "writeKindFile" (context.proxy.writeKindFile ExperimentKitFile experiment replacement)
        assertReadOnly "deleteKindFile" (context.proxy.deleteKindFile ExperimentKitFile experiment)
        assertReadOnly "acquireLock" (context.proxy.acquireLock (clock.now ()) |> Result.map ignore)
        assertReadOnly "writeKitFile" (context.saveKitFile reviewed |> Result.map ignore)

        Assert.Equal<Result<KitFile, KitError>>(Ok kit, context.proxy.readKitFile ())
        Assert.Equal<Result<FileStem list, KitError>>(Ok [ experiment ], context.proxy.listKindFiles ExperimentKitFile)
        Assert.Equal<Result<KitFileText, KitError>>(Ok original, context.proxy.readKindFile ExperimentKitFile experiment)
        let experimentPath =
            KitRelativePath.tryCreate (ExperimentKitFile.fileNameOf experiment).value
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<Result<KitDataFile, KitError>>(proxyB.readDataFile experimentPath, context.proxy.readDataFile experimentPath)
        Assert.Equal<Result<FileName list, KitError>>(Ok [], context.proxy.changedKitFiles ())
        let changed = KitFileText "An outside writer changed the experiment\n"
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyB.writeKindFile ExperimentKitFile experiment changed)
        Assert.Equal<Result<FileName list, KitError>>(Ok [ ExperimentKitFile.fileNameOf experiment ], context.proxy.changedKitFiles ())
        Assert.Equal<Result<unit, KitError>>(Ok (), context.proxy.recordStamps ())
        Assert.Equal<Result<FileName list, KitError>>(Ok [], context.proxy.changedKitFiles ())
        Assert.Equal<Result<unit, KitError>>(Ok (), context.close ())
        Assert.Equal<string list>(before, folder.paths ())
        Assert.DoesNotContain(".oc", folder.paths ())

        let stampError = KitIoErr "read-only stamp scan failed"
        let failingReader =
            {
                guardedProxy with recordStamps = fun () -> Error stampError
            }
        let failed = KitContext.openKit failingReader ReadOnlyKitAccess clock |> Result.map (fun opened -> opened.kitFile)
        Assert.Equal<Result<KitFile, KitError>>(Error stampError, failed)
        Assert.Equal<string list>(before, folder.paths ())
        Assert.Equal<Result<KitFile, KitError>>(Ok kit, proxyB.readKitFile ())
        Assert.Equal<Result<KitFileText, KitError>>(Ok changed, proxyB.readKindFile ExperimentKitFile experiment)

    [<Fact>]
    member this.``saving the kit file through the context stamps its modified time`` () : unit =
        let folder = this.freshFolder ()
        let proxyA = folder.openProxy ()
        let proxyB = folder.openProxy ()
        let clock = steppingClock ()
        let kit = newKitFile clock
        Assert.Equal<Result<unit, KitError>>(Ok (), proxyA.initialiseKit kit)
        let context =
            KitContext.openKit proxyA ReadWriteKitAccess clock
            |> Result.defaultWith (fun error -> failwith error.describe)
        let reviewed =
            {
                context.kitFile with notes = KitNotes "Reviewed"
            }
        let expectedModified = ClockTime (kit.created.value.AddSeconds 2.0)
        let expected =
            {
                reviewed with modified = expectedModified
            }
        let saved = context.saveKitFile reviewed |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal<KitFile>(expected, saved.kitFile)
        Assert.Equal<ClockTime>(kit.created, saved.kitFile.created)
        Assert.Equal<ClockTime>(expectedModified, saved.kitFile.modified)
        Assert.Equal<KitFile>(kit, context.kitFile)
        Assert.Equal<KitAccess>(ReadWriteKitAccess, saved.access)
        Assert.Same(context.proxy, saved.proxy)
        Assert.Same(clock, saved.clock)
        Assert.Equal<Result<KitFile, KitError>>(Ok expected, proxyB.readKitFile ())
        Assert.Equal<Result<FileName list, KitError>>(Ok [], saved.proxy.changedKitFiles ())

        let metadataPath = KitRelativePath ".kit-json"
        let storedMetadata = proxyB.readDataFile metadataPath
        let experiment = FileStem.ofText "QZ-LR"
        let text = KitFileText "Experiment without a metadata edit\n"
        Assert.Equal<Result<unit, KitError>>(Ok (), saved.proxy.writeKindFile ExperimentKitFile experiment text)
        Assert.Equal<Result<KitDataFile, KitError>>(storedMetadata, proxyB.readDataFile metadataPath)
        Assert.Equal<Result<unit, KitError>>(Ok (), saved.proxy.deleteKindFile ExperimentKitFile experiment)
        Assert.Equal<Result<KitDataFile, KitError>>(storedMetadata, proxyB.readDataFile metadataPath)

        let writeError = KitIoErr "metadata replacement refused"
        let failingContext =
            {
                saved with
                    proxy =
                        {
                            saved.proxy with writeKitFile = fun _ -> Error writeError
                        }
            }
        let failed = failingContext.saveKitFile kit |> Result.map (fun opened -> opened.kitFile)
        Assert.Equal<Result<KitFile, KitError>>(Error writeError, failed)
        Assert.Equal<KitFile>(expected, saved.kitFile)
        Assert.Equal<Result<KitFile, KitError>>(Ok expected, proxyB.readKitFile ())
        Assert.Equal<Result<unit, KitError>>(Ok (), saved.close ())
        Assert.DoesNotContain(".oc/lock", folder.paths ())

    [<Fact>]
    member this.``opening a folder that is not a kit writes nothing`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let unusedClock : ClockProvider =
            {
                now = fun () -> failwith "A failed metadata read must not consult the clock."
            }
        let guardedProxy =
            {
                proxy with
                    acquireLock = fun _ -> failwith "A failed metadata read must not acquire a lock."
                    releaseLock = fun () -> failwith "A failed metadata read must not release a lock."
                    recordStamps = fun () -> failwith "A failed metadata read must not record stamps."
            }
        for access in [ ReadWriteKitAccess; ReadOnlyKitAccess ] do
            let result = KitContext.openKit guardedProxy access unusedClock |> Result.map (fun opened -> opened.kitFile)
            Assert.Equal<Result<KitFile, KitError>>(proxy.readKitFile (), result)
            match result with
            | Error (NotAKitFolderErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
            | Error error -> Assert.Fail($"Expected NotAKitFolderErr, got {error.describe}")
            | Ok _ -> Assert.Fail("Opening an empty folder must not create a kit.")
            Assert.Empty(folder.paths ())

            let readError = KitFileRejectedErr "invalid kit metadata"
            let rejectedProxy =
                {
                    guardedProxy with readKitFile = fun () -> Error readError
                }
            let rejected = KitContext.openKit rejectedProxy access unusedClock |> Result.map (fun opened -> opened.kitFile)
            Assert.Equal<Result<KitFile, KitError>>(Error readError, rejected)
            Assert.Empty(folder.paths ())

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
            absoluteName = fun path -> FileName $"{folder.kitFolder.value.value}/{path}"
            outsideName = fun path -> FileName $"C:/outside/{path}"
            removeLabFile =
                fun path ->
                    KitRelativePath.tryCreate path
                    |> Result.bind folder.removeLabFile
                    |> Result.defaultWith (fun error -> failwith error.describe)
        }

/// Runs the shared kit-file contract over real folders under the test output (spec 0050 S4.1).
type KitProxyFolderTests () =
    inherit KitProxyContract ()

    override _.freshFolder () : KitTestFolder =
        let folder = freshKitFolderPath ()
        Directory.CreateDirectory folder.value |> ignore
        kitFolderAdapter folder

    [<Fact>]
    member this.``a copied-in file holds exactly the bytes given, a byte-order mark included`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let leaf = FileName "bom.csv"
        let text = KitFileText "a,b\n"
        let bytes = Array.append [| 0xEFuy; 0xBBuy; 0xBFuy |] (Encoding.UTF8.GetBytes text.value) |> FileByteContent
        let expectedHash = ContentHash (Convert.ToHexStringLower(SHA256.HashData bytes.value))
        let textBytes = Encoding.UTF8.GetBytes text.value |> FileByteContent
        let copied = proxy.copyInFile KitDataFolder.root leaf bytes |> Result.defaultWith (fun error -> failwith error.describe)
        let expectedPath = KitRelativePath "data/bom.csv"
        Assert.Equal<KitRelativePath>(expectedPath, copied.path)
        Assert.Equal<KitFileText>(text, copied.text)
        Assert.Equal<ContentHash>(expectedHash, copied.contentHash)
        Assert.NotEqual<ContentHash>(ContentHash.ofBytes textBytes, copied.contentHash)
        let file = folder.absoluteName expectedPath.value
        Assert.Equal<byte>(bytes.value, File.ReadAllBytes file.value)
        Assert.Equal<Result<KitDataFile, KitError>>(Ok copied, (folder.openProxy ()).readDataFile expectedPath)
        Assert.Equal<string list>([ ".kit-json"; "data"; "data/bom.csv"; "library" ], folder.paths ())

    [<Fact>]
    member this.``the new-file writer refuses a file that exists, keeps it whole and leaves no temporary file`` () : unit =
        let folder = this.freshFolder ()
        let proxy = folder.openProxy ()
        let kit = newKitFile (steppingClock ())
        Assert.Equal<Result<unit, KitError>>(Ok (), proxy.initialiseKit kit)
        let file = folder.absoluteName "x.csv"
        let original = Encoding.UTF8.GetBytes "Keep these bytes: μ\r\n" |> FileByteContent
        let replacement = Encoding.UTF8.GetBytes "Refused replacement\n" |> FileByteContent
        File.WriteAllBytes(file.value, original.value)
        let before = folder.paths ()
        match file.writeNewBytesAtomically replacement with
        | Error (FileIoExn error) -> Assert.False(String.IsNullOrWhiteSpace error.Message)
        | Error error -> Assert.Fail($"Expected FileIoExn, got {error.describe}")
        | Ok () -> Assert.Fail("The new-file writer must never replace an existing file.")
        Assert.Equal<byte>(original.value, File.ReadAllBytes file.value)
        Assert.Equal<string list>(before, folder.paths ())
        Assert.False(folder.paths () |> List.exists (fun name -> name.EndsWith(".tmp", StringComparison.OrdinalIgnoreCase)))

        let destination = folder.absoluteName (Path.Combine("data", "run.csv"))
        let staging = folder.absoluteName (Path.Combine("data", "run.csv.tmp"))
        File.WriteAllBytes(staging.value, original.value)
        let occupiedPaths = folder.paths ()
        Assert.False(File.Exists destination.value)
        match destination.writeNewBytesAtomically replacement with
        | Error (FileIoExn error) -> Assert.False(String.IsNullOrWhiteSpace error.Message)
        | Error error -> Assert.Fail($"Expected FileIoExn, got {error.describe}")
        | Ok () -> Assert.Fail("The new-file writer must refuse an occupied staging path.")
        Assert.Equal<byte>(original.value, File.ReadAllBytes staging.value)
        Assert.False(File.Exists destination.value)
        Assert.Equal<string list>(occupiedPaths, folder.paths ())

        let leaf = FileName "run.csv"
        match proxy.copyInFile KitDataFolder.root leaf replacement with
        | Error (KitIoErr reason) -> Assert.Contains(destination.value, reason)
        | Error error -> Assert.Fail($"Expected KitIoErr, got {error.describe}")
        | Ok _ -> Assert.Fail("Copy-in must refuse an occupied staging path without retrying.")
        Assert.Equal<byte>(original.value, File.ReadAllBytes staging.value)
        Assert.False(File.Exists destination.value)
        Assert.Equal<string list>(occupiedPaths, folder.paths ())

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
        for _ in 1 .. 25 do
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
        for _ in 1 .. 25 do
            folder.placeLabFile ".oc/lock" "not a lock"
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

        folderPath.assertKitGuardRetryPolicy ()
        assertBlockedKitGuard folderPath proxy clock

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
