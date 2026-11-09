namespace OpticalConstructor.Tests

open System
open System.Text
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive

/// One disk seen through an archive proxy and an outside writer, every path relative to `root` (spec 0050 S6).
type KitArchiveTestDisk =
    {
        archive : KitArchiveProxy
        root : string
        placeFile : string -> byte[] -> unit
        placeFolder : string -> unit
        /// Every file under a folder with its bytes, kit-relative and forward-slashed, in ordinal order.
        files : string -> (string * byte[]) list
        /// Every file and folder under a folder, kit-relative and forward-slashed, in ordinal order.
        paths : string -> string list
    }

module private KitArchiveProxyTestData =

    /// Places the sample kit under `kit`: five files, one holding every byte value, and the empty folder `data/empty` (spec 0050 S6).
    let sampleKit (disk : KitArchiveTestDisk) : unit =
        disk.placeFile "kit/.kit-json" (Encoding.UTF8.GetBytes "{ \"name\": \"sample\" }")
        disk.placeFile "kit/data/run 1.csv" (Encoding.UTF8.GetBytes "phi,I\n0,0.5\n")
        disk.placeFile "kit/data/raw/IMG_0001.DNG" [| 0uy .. 255uy |]
        disk.placeFile "kit/library/x.material-json" (Encoding.UTF8.GetBytes "{ \"material\": \"x\" }")
        disk.placeFile "kit/notes.txt" (Encoding.UTF8.GetBytes "notes")
        disk.placeFolder "kit/data/empty"

    let kitPath (text : string) : KitRelativePath =
        match KitRelativePath.tryCreate text with
        | Ok path -> path
        | Error error -> failwith error.describe

    let folderUnder (disk : KitArchiveTestDisk) (relative : string) : KitFolder = KitFolder (FolderName $"{disk.root}/{relative}")

    /// The zip's name under `out`, placed first: a real disk writes a zip only into a folder that exists (spec 0050 S6).
    let zipUnderOut (disk : KitArchiveTestDisk) : FileName =
        disk.placeFolder "out"
        FileName $"{disk.root}/out/kit.zip"

    let expectOk<'Value> (result : Result<'Value, KitArchiveError>) : 'Value =
        match result with
        | Ok value -> value
        | Error error -> failwith error.describe

    let fiveFiles : KitArchiveFileCount = KitArchiveFileCount 5

    /// Cancels `source` on the first progress it is told.
    let cancelledOnFirstProgress (source : CancellationTokenSource) : KitArchiveObserver =
        {
            reportArchiveProgress = fun _ -> source.Cancel ()
            cancellation = source.Token
        }

    /// Zips the sample kit's whole tree to `out/kit.zip`, answering the tree and the zip.
    let zipSampleKit (disk : KitArchiveTestDisk) : KitTreeEntry list * FileName =
        sampleKit disk
        let kit = folderUnder disk "kit"
        let tree = expectOk (disk.archive.listKitTree kit)
        let zip = zipUnderOut disk
        let job : KitZipJob = { kit = kit; entries = tree; zip = zip }
        Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(Ok (KitArchiveFinished fiveFiles), disk.archive.writeKitZip job KitArchiveObserver.unobserved)
        tree, zip

    let comparableFiles (files : (string * byte[]) list) : (string * byte list) list =
        files |> List.map (fun (path, bytes) -> path, List.ofArray bytes)

    /// The sample kit with the app's cache in it: a lock held elsewhere and a planner file under `kit/.oc` (spec 0050 S6).
    let sampleKitWithAppCache (disk : KitArchiveTestDisk) : unit =
        sampleKit disk
        disk.placeFile "kit/.oc/lock" (Encoding.UTF8.GetBytes "held elsewhere")
        disk.placeFile "kit/.oc/planner/c.json" (Encoding.UTF8.GetBytes "{ }")

    /// The sample kit's file, leaving every `.dng` out of a send (spec 0050 S6).
    let dngExcludingKitFile : KitFile =
        let id = KitId (Guid.Parse "0b7e4f3a-9c2d-4a61-8e15-d3f6a2c90b47")
        let name = KitName "sample"
        let author = KitAuthor "Test author"
        let instant = ClockTime (DateTime(2026, 11, 9, 12, 0, 0, DateTimeKind.Utc))
        {
            KitFile.create id name author instant with
                sendExclude = [ SendExcludeGlob "**/*.dng" ]
        }

    let fourFiles : KitArchiveFileCount = KitArchiveFileCount 4

    /// Sends the sample kit, its app cache in it, to `out/kit.zip`, answering the zip.
    let sendSampleKit (disk : KitArchiveTestDisk) : FileName =
        sampleKitWithAppCache disk
        let zip = zipUnderOut disk
        Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(
            Ok (KitArchiveFinished fourFiles),
            disk.archive.sendKit dngExcludingKitFile (folderUnder disk "kit") zip KitArchiveObserver.unobserved)
        zip

    /// Zips these entries of the kit to `out/kit.zip`, asserting how many files it holds, and answers the zip.
    let zipKitEntries (disk : KitArchiveTestDisk) (entries : KitTreeEntry list) (files : KitArchiveFileCount) : FileName =
        let zip = zipUnderOut disk
        let job : KitZipJob = { kit = folderUnder disk "kit"; entries = entries; zip = zip }
        Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(Ok (KitArchiveFinished files), disk.archive.writeKitZip job KitArchiveObserver.unobserved)
        zip

open KitArchiveProxyTestData

/// The archive seam's contract, held by every implementation over a fresh disk of its own (spec 0050 S6).
[<AbstractClass>]
type KitArchiveProxyContract () =

    /// A fresh, empty disk with an archive proxy over it.
    abstract freshDisk : unit -> KitArchiveTestDisk

    [<Fact>]
    member this.``a kit folder is listed as every file and folder under it, kit-relative, in ordinal order`` () : unit =
        let disk = this.freshDisk ()
        sampleKit disk

        let expected =
            [
                KitTreeFile (kitPath ".kit-json")
                KitTreeFolder (kitPath "data")
                KitTreeFolder (kitPath "data/empty")
                KitTreeFolder (kitPath "data/raw")
                KitTreeFile (kitPath "data/raw/IMG_0001.DNG")
                KitTreeFile (kitPath "data/run 1.csv")
                KitTreeFolder (kitPath "library")
                KitTreeFile (kitPath "library/x.material-json")
                KitTreeFile (kitPath "notes.txt")
            ]

        Assert.Equal<Result<KitTreeEntry list, KitArchiveError>>(Ok expected, disk.archive.listKitTree (folderUnder disk "kit"))

    [<Fact>]
    member this.``a zip of the given entries extracts into an empty folder byte for byte, each file reported once`` () : unit =
        let disk = this.freshDisk ()
        sampleKit disk
        let kit = folderUnder disk "kit"
        let tree = expectOk (disk.archive.listKitTree kit)
        let zip = zipUnderOut disk
        let progress = ResizeArray<KitArchiveProgress> ()
        let recording : KitArchiveObserver = { reportArchiveProgress = progress.Add; cancellation = CancellationToken.None }
        let zipJob : KitZipJob = { kit = kit; entries = tree; zip = zip }

        Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(Ok (KitArchiveFinished fiveFiles), disk.archive.writeKitZip zipJob recording)
        let expectedProgress =
            [ 1 .. 5 ] |> List.map (fun filesDone -> { filesDone = KitArchiveFileCount filesDone; filesTotal = fiveFiles })
        Assert.Equal<KitArchiveProgress list>(expectedProgress, List.ofSeq progress)
        Assert.Equal<Result<KitTreeEntry list, KitArchiveError>>(Ok tree, disk.archive.listZipEntries zip)

        Assert.DoesNotContain<string>("received", disk.paths "")
        let unzipJob : KitUnzipJob = { zip = zip; entries = tree; target = folderUnder disk "received" }
        Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(
            Ok (KitArchiveFinished fiveFiles),
            disk.archive.extractKitZip unzipJob KitArchiveObserver.unobserved)
        Assert.Equal<string list>(disk.paths "kit", disk.paths "received")
        Assert.Equal<(string * byte list) list>(comparableFiles (disk.files "kit"), comparableFiles (disk.files "received"))

    [<Fact>]
    member this.``extracting into a folder that holds anything is refused and writes nothing`` () : unit =
        let disk = this.freshDisk ()
        let tree, zip = zipSampleKit disk
        disk.placeFile "received/notes.txt" (Encoding.UTF8.GetBytes "already here")
        let unzipJob : KitUnzipJob = { zip = zip; entries = tree; target = folderUnder disk "received" }

        match disk.archive.extractKitZip unzipJob KitArchiveObserver.unobserved with
        | Error (KitZipTargetNotEmptyErr reason) -> Assert.Contains("received", reason)
        | Ok _
        | Error _ -> Assert.Fail "extracted into a folder that already holds a file"

        Assert.Equal<string list>([ "notes.txt" ], disk.paths "received")

    [<Fact>]
    member this.``a zip job cancelled before its next file writes no zip`` () : unit =
        let disk = this.freshDisk ()
        sampleKit disk
        let kit = folderUnder disk "kit"
        let tree = expectOk (disk.archive.listKitTree kit)
        let zip = zipUnderOut disk
        use source = new CancellationTokenSource ()
        let zipJob : KitZipJob = { kit = kit; entries = tree; zip = zip }

        Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(Ok KitArchiveCancelled, disk.archive.writeKitZip zipJob (cancelledOnFirstProgress source))

        match disk.archive.listZipEntries zip with
        | Error (KitArchiveIoErr _) -> ()
        | Ok _
        | Error _ -> Assert.Fail "a cancelled zip job left a zip that lists"

        Assert.Empty(disk.paths "out")

    [<Fact>]
    member this.``an extraction cancelled before its next file removes everything it wrote`` () : unit =
        let disk = this.freshDisk ()
        let tree, zip = zipSampleKit disk
        let before = disk.paths ""
        use source = new CancellationTokenSource ()
        let unzipJob : KitUnzipJob = { zip = zip; entries = tree; target = folderUnder disk "received" }

        Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(
            Ok KitArchiveCancelled,
            disk.archive.extractKitZip unzipJob (cancelledOnFirstProgress source))
        Assert.DoesNotContain<string>("received", disk.paths "")
        Assert.Equal<string list>(before, disk.paths "")

    [<Fact>]
    member this.``a kit folder or a zip that is not there is KitArchiveIoErr`` () : unit =
        let disk = this.freshDisk ()
        sampleKit disk

        match disk.archive.listKitTree (folderUnder disk "none") with
        | Error (KitArchiveIoErr reason) -> Assert.Contains("none", reason)
        | Ok _
        | Error _ -> Assert.Fail "listed a kit folder that is not there"

        match disk.archive.listZipEntries (FileName $"{disk.root}/none.zip") with
        | Error (KitArchiveIoErr reason) -> Assert.Contains("none.zip", reason)
        | Ok _
        | Error _ -> Assert.Fail "listed a zip that is not there"

    [<Fact>]
    member this.``sending a kit zips every file and folder but the app cache and what its globs leave out`` () : unit =
        let disk = this.freshDisk ()
        let zip = sendSampleKit disk

        let expected =
            [
                KitTreeFile (kitPath ".kit-json")
                KitTreeFolder (kitPath "data")
                KitTreeFolder (kitPath "data/empty")
                KitTreeFolder (kitPath "data/raw")
                KitTreeFile (kitPath "data/run 1.csv")
                KitTreeFolder (kitPath "library")
                KitTreeFile (kitPath "library/x.material-json")
                KitTreeFile (kitPath "notes.txt")
            ]

        Assert.Equal<Result<KitTreeEntry list, KitArchiveError>>(Ok expected, disk.archive.listZipEntries zip)

    [<Fact>]
    member this.``a zip is never written inside the kit it holds`` () : unit =
        let disk = this.freshDisk ()
        sampleKitWithAppCache disk
        let before = disk.paths "kit"
        let zip = FileName $"{disk.root}/kit/out/kit.zip"

        match disk.archive.sendKit dngExcludingKitFile (folderUnder disk "kit") zip KitArchiveObserver.unobserved with
        | Error (KitZipInsideKitErr reason) -> Assert.Contains("kit/out/kit.zip", reason)
        | Ok _
        | Error _ -> Assert.Fail "sent a kit into a zip inside the kit"

        Assert.Equal<string list>(before, disk.paths "kit")

    [<Fact>]
    member this.``a received kit holds every sent file byte for byte`` () : unit =
        let disk = this.freshDisk ()
        let zip = sendSampleKit disk

        Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(
            Ok (KitArchiveFinished fourFiles),
            disk.archive.receiveKit zip (folderUnder disk "received") KitArchiveObserver.unobserved)

        let leftOut = [ ".oc/lock"; ".oc/planner/c.json"; "data/raw/IMG_0001.DNG" ]
        let sent = disk.files "kit" |> List.filter (fun (path, _) -> not (List.contains path leftOut))
        Assert.Equal<(string * byte list) list>(comparableFiles sent, comparableFiles (disk.files "received"))

    [<Fact>]
    member this.``a zip that holds no kit file is refused and nothing is extracted`` () : unit =
        let disk = this.freshDisk ()
        sampleKitWithAppCache disk
        let kitFileEntry = KitTreeFile (kitPath ".kit-json")
        let tree = expectOk (disk.archive.listKitTree (folderUnder disk "kit"))
        Assert.Contains(kitFileEntry, tree)
        let zip = zipKitEntries disk (tree |> List.filter (fun entry -> entry <> kitFileEntry)) (KitArchiveFileCount 6)

        match disk.archive.receiveKit zip (folderUnder disk "received") KitArchiveObserver.unobserved with
        | Error (KitZipHoldsNoKitErr reason) -> Assert.Contains("kit.zip", reason)
        | Ok _
        | Error _ -> Assert.Fail "received a zip that holds no kit file"

        Assert.DoesNotContain<string>("received", disk.paths "")

    [<Fact>]
    member this.``a zip's app cache is not received`` () : unit =
        let disk = this.freshDisk ()
        sampleKitWithAppCache disk
        let tree = expectOk (disk.archive.listKitTree (folderUnder disk "kit"))
        Assert.Contains(KitTreeFolder (kitPath ".oc"), tree)
        Assert.Contains(KitTreeFile (kitPath ".oc/lock"), tree)
        let zip = zipKitEntries disk tree (KitArchiveFileCount 7)

        Assert.Equal<Result<KitArchiveOutcome, KitArchiveError>>(
            Ok (KitArchiveFinished fiveFiles),
            disk.archive.receiveKit zip (folderUnder disk "received") KitArchiveObserver.unobserved)

        let received = disk.paths "received"
        Assert.Contains(".kit-json", received)
        Assert.DoesNotContain<string>(received, fun path -> path = ".oc" || path.StartsWith(".oc/", StringComparison.Ordinal))

/// The archive contract over the in-memory disk, rooted at `C:/disk` (spec 0050 S6).
type KitArchiveProxyTests () =
    inherit KitArchiveProxyContract ()

    override _.freshDisk () : KitArchiveTestDisk =
        let disk = InMemoryArchiveDisk.create ()
        let root = "C:/disk"
        let under (relative : string) : string = if relative = "" then root else $"{root}/{relative}"

        let namesUnder (folder : string) : string list =
            let prefix = $"{under folder}/"
            disk.paths ()
            |> List.choose (fun name ->
                if name.value.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then Some (name.value.Substring prefix.Length)
                else None)
            |> List.sortWith (fun first second -> String.CompareOrdinal(first, second))

        {
            archive = KitArchiveProxy.createInMemory disk
            root = root
            placeFile = fun path bytes -> disk.placeFile (FileName (under path)) (FileByteContent bytes)
            placeFolder = fun path -> disk.placeFolder (FolderName (under path))
            files =
                fun folder ->
                    namesUnder folder
                    |> List.choose (fun path ->
                        disk.tryDiskFile (FileName $"{under folder}/{path}") |> Option.map (fun content -> path, content.value))
            paths = namesUnder
        }
