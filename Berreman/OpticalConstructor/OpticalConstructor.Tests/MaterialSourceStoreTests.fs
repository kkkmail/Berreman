namespace OpticalConstructor.Tests

open System
open System.IO
open System.IO.Compression
open System.Text
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Storage.MaterialImport
open OpticalConstructor.Storage.MaterialSourceStore
open OpticalConstructor.Tests.MaterialSourceFixture

/// The refractiveindex.info adapter over fixture snapshots and archives under the test output: its reads, its update
/// through the archive seam, and what a failed or cancelled update keeps (spec 0050 S13.1).
module MaterialSourceStoreTests =

    let private owner : string = "MaterialSourceStoreTests"

    let private siRecord : MaterialSourceRecordId = MaterialSourceRecordId "main/Si/nk/Test.yml"

    let private glassRecord : MaterialSourceRecordId = MaterialSourceRecordId "glass/test/BK7/nk/Test.yml"

    let private heldVersion : MaterialSourceVersion = MaterialSourceVersion "2026-11-01"

    let private firstClockTime : DateTime = DateTime(2026, 11, 1, 9, 30, 0, DateTimeKind.Utc)

    let private laterClockTime : DateTime = DateTime(2026, 11, 4, 23, 59, 0, DateTimeKind.Utc)

    /// The snapshot folder an adapter over `sources` reads.
    let private snapshotIn (sources : FolderName) : string = Path.Combine(sources.value, "refractiveindex.info")

    /// A downloader for the facts that never update: asking it is a failure of its own.
    let private unusedDownloader : SourceArchiveDownloader =
        {
            downloadArchive = fun url _ _ -> Error (SourceDownloadErr $"'{url.value}' must not be downloaded here")
        }

    let private progressOf (filesDone : int) (filesTotal : int) : SourceSnapshotProgress =
        {
            filesDone = SourceSnapshotFileCount filesDone
            filesTotal = SourceSnapshotFileCount filesTotal
        }

    /// Every file under `folder`, relative and forward-slashed, with its bytes as hex, in ordinal order.
    let private filesUnder (folder : string) : (string * string) list =
        Directory.EnumerateFiles(folder, "*", SearchOption.AllDirectories)
        |> Seq.map (fun file -> Path.GetRelativePath(folder, file).Replace('\\', '/'), Convert.ToHexString(File.ReadAllBytes file))
        |> Seq.sortWith (fun (first, _) (second, _) -> String.CompareOrdinal(first, second))
        |> Seq.toList

    /// The names directly inside `folder`, in ordinal order.
    let private namesIn (folder : string) : string list =
        Directory.EnumerateFileSystemEntries folder
        |> Seq.map Path.GetFileName
        |> Seq.sortWith (fun first second -> String.CompareOrdinal(first, second))
        |> Seq.toList

    let private errorOf (what : string) (result : Result<'Value, MaterialSourceError>) : MaterialSourceError =
        match result with
        | Error error -> error
        | Ok _ -> failwith $"{what}: expected an error, got a result"

    [<Fact>]
    let ``a refractiveindex.info source without a snapshot answers no version and refuses search and fetch`` () =
        let sources = freshSources owner
        let proxy = MaterialSourceProxy.createRefractiveIndexInfo sources unusedDownloader (fixedClock firstClockTime)
        Assert.Equal(MaterialSourceName "refractiveindex.info", proxy.sourceName)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok None, proxy.snapshotVersion ())

        match errorOf "search" (proxy.searchRecords (MaterialSourceQuery "silicon")) with
        | SourceSnapshotMissingErr _ -> ()
        | error -> Assert.Fail $"search: expected SourceSnapshotMissingErr, got {error.describe}"

        match errorOf "fetch" (proxy.fetchRecord siRecord) with
        | SourceSnapshotMissingErr _ -> ()
        | error -> Assert.Fail $"fetch: expected SourceSnapshotMissingErr, got {error.describe}"

        Assert.Empty(namesIn sources.value)

    [<Fact>]
    let ``a snapshot's catalog is searched with the shared rule`` () =
        let sources = freshSources owner
        writeSnapshot (snapshotIn sources) (siPage ())
        let proxy = MaterialSourceProxy.createRefractiveIndexInfo sources unusedDownloader (fixedClock firstClockTime)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some heldVersion), proxy.snapshotVersion ())

        let catalogHits =
            match (RefractiveIndexInfoCatalogText (File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "fixtures", "catalog-nk.yml")))).hits with
            | Ok hits -> hits
            | Error error -> failwith error.describe

        for query in [ MaterialSourceQuery "silicon"; MaterialSourceQuery "  " ] do
            Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(Ok (query.filter catalogHits), proxy.searchRecords query)

        match proxy.searchRecords (MaterialSourceQuery "silicon") with
        | Ok hits -> Assert.Equal<MaterialSourceRecordId list>([ siRecord ], hits |> List.map _.record)
        | Error error -> Assert.Fail error.describe

        match proxy.searchRecords (MaterialSourceQuery "") with
        | Ok hits -> Assert.Equal<MaterialSourceRecordId list>([ siRecord; glassRecord ], hits |> List.map _.record)
        | Error error -> Assert.Fail error.describe

    [<Fact>]
    let ``a fetched page is MaterialImport's material under its hit's name, with the page's hash, the stamp's version, CC0 and its citation`` () =
        let sources = freshSources owner
        let snapshot = snapshotIn sources
        writeSnapshot snapshot (siPage ())
        let proxy = MaterialSourceProxy.createRefractiveIndexInfo sources unusedDownloader (fixedClock laterClockTime)

        match proxy.fetchRecord siRecord, (RefractiveIndexInfoText (siPage ())).importMaterial with
        | Ok fetched, Ok imported ->
            Assert.Equal(imported.physics, fetched.material.physics)
            Assert.Equal(EntryName "Si (Silicon), Test 2026: n,k 0.25–0.70 µm", fetched.material.name)
            let pageBytes = File.ReadAllBytes(Path.Combine(snapshot, "database", "data", "main", "Si", "nk", "Test.yml"))
            let expected : SourcePullInfo =
                {
                    source = MaterialSourceName "refractiveindex.info"
                    record = siRecord
                    recordHash = ContentHash.ofBytes (FileByteContent pageBytes)
                    sourceVersion = heldVersion
                    licence = EntryLicence "CC0-1.0"
                    citation =
                        EntryCitation
                            "refractiveindex.info, main/Si/nk/Test.yml: Test fixture — minimal refractiveindex.info-style tabulated page (NOT real Si data)."
                }
            Assert.Equal(expected, fetched.pull)
        | Error error, _ -> Assert.Fail $"fetch: {error.describe}"
        | _, Error error -> Assert.Fail $"import: {error.describe}"

        match proxy.fetchRecord glassRecord, (RefractiveIndexInfoText (glassPage ())).importMaterial with
        | Ok fetched, Ok imported ->
            Assert.Equal(imported.physics, fetched.material.physics)
            Assert.Equal(EntryName "BK7 (test), Test 2026: n 0.3–2.5 µm", fetched.material.name)
        | Error error, _ -> Assert.Fail $"fetch: {error.describe}"
        | _, Error error -> Assert.Fail $"import: {error.describe}"

        for unlisted in [ MaterialSourceRecordId "main/Ge/nk/Test.yml"; MaterialSourceRecordId "../x.yml" ] do
            match errorOf unlisted.value (proxy.fetchRecord unlisted) with
            | SourceRecordMissingErr _ -> ()
            | error -> Assert.Fail $"'{unlisted.value}': expected SourceRecordMissingErr, got {error.describe}"

    [<Fact>]
    let ``an update downloads the archive and replaces the snapshot with its catalog and data, file by file`` () =
        let sources = freshSources owner
        let snapshot = snapshotIn sources
        writeSnapshot snapshot (siPage ())
        let zip = $"{sources.value}.zip"
        writeArchive zip (changedSiPage ())
        let asked = ResizeArray<SourceArchiveUrl>()
        let proxy = MaterialSourceProxy.createRefractiveIndexInfo sources (copyingDownloader zip asked) (fixedClock laterClockTime)
        let progress = ResizeArray<SourceSnapshotProgress>()
        let observer : SourceSnapshotObserver = { reportSnapshotProgress = progress.Add; cancellation = CancellationToken.None }

        let updated : SourceSnapshotInfo = { version = MaterialSourceVersion "2026-11-04"; files = SourceSnapshotFileCount 3 }
        Assert.Equal<Result<SourceSnapshotOutcome, MaterialSourceError>>(Ok (SourceSnapshotUpdated updated), proxy.updateSnapshot observer)
        Assert.Equal<SourceSnapshotProgress list>([ progressOf 1 3; progressOf 2 3; progressOf 3 3 ], List.ofSeq progress)
        Assert.Equal<SourceArchiveUrl list>(
            [ SourceArchiveUrl "https://github.com/polyanskiy/refractiveindex.info-database/archive/refs/heads/main.zip" ],
            List.ofSeq asked)

        Assert.Equal<string list>(
            [ ".snapshot-json"; "database/catalog-nk.yml"; "database/data/glass/test/BK7/nk/Test.yml"; "database/data/main/Si/nk/Test.yml" ],
            filesUnder snapshot |> List.map fst)
        Assert.Equal<string list>([ ".snapshot-json"; "database" ], namesIn snapshot)
        Assert.Equal<string list>([ "catalog-nk.yml"; "data" ], namesIn (Path.Combine(snapshot, "database")))
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some updated.version), proxy.snapshotVersion ())

        match proxy.fetchRecord siRecord with
        | Ok fetched ->
            Assert.Equal(ContentHash.ofBytes (FileByteContent (Encoding.UTF8.GetBytes (changedSiPage ()))), fetched.pull.recordHash)
            Assert.Equal(updated.version, fetched.pull.sourceVersion)
        | Error error -> Assert.Fail error.describe

        Assert.Equal<string list>([ "refractiveindex.info" ], namesIn sources.value)

    [<Fact>]
    let ``an update cancelled before its next file keeps the held snapshot`` () =
        let sources = freshSources owner
        let snapshot = snapshotIn sources
        writeSnapshot snapshot (siPage ())
        let held = filesUnder snapshot
        let zip = $"{sources.value}.zip"
        writeArchive zip (changedSiPage ())
        let proxy = MaterialSourceProxy.createRefractiveIndexInfo sources (copyingDownloader zip (ResizeArray ())) (fixedClock laterClockTime)
        use cancelling = new CancellationTokenSource()
        let observer : SourceSnapshotObserver = { reportSnapshotProgress = (fun _ -> cancelling.Cancel()); cancellation = cancelling.Token }

        Assert.Equal<Result<SourceSnapshotOutcome, MaterialSourceError>>(Ok SourceSnapshotCancelled, proxy.updateSnapshot observer)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some heldVersion), proxy.snapshotVersion ())
        Assert.Equal<(string * string) list>(held, filesUnder snapshot)
        Assert.Equal<string list>([ "refractiveindex.info" ], namesIn sources.value)

    [<Fact>]
    let ``an archive without the database catalog is refused and the held snapshot kept`` () =
        let sources = freshSources owner
        let snapshot = snapshotIn sources
        writeSnapshot snapshot (siPage ())
        let held = filesUnder snapshot
        let zip = $"{sources.value}.zip"
        do
            use stream = new FileStream(zip, FileMode.Create, FileAccess.Write, FileShare.None)
            use archive = new ZipArchive(stream, ZipArchiveMode.Create)
            use content = (archive.CreateEntry "refractiveindex.info-database-main/README.md").Open ()
            let bytes = Encoding.UTF8.GetBytes "# refractiveindex.info database\n"
            content.Write(bytes, 0, bytes.Length)
        let proxy = MaterialSourceProxy.createRefractiveIndexInfo sources (copyingDownloader zip (ResizeArray ())) (fixedClock laterClockTime)

        match errorOf "update" (proxy.updateSnapshot SourceSnapshotObserver.unobserved) with
        | SourceArchiveInvalidErr _ -> ()
        | error -> Assert.Fail $"update: expected SourceArchiveInvalidErr, got {error.describe}"

        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some heldVersion), proxy.snapshotVersion ())
        Assert.Equal<(string * string) list>(held, filesUnder snapshot)
        Assert.Equal<string list>([ "refractiveindex.info" ], namesIn sources.value)

    [<Fact>]
    let ``a failed download keeps the held snapshot`` () =
        let sources = freshSources owner
        let snapshot = snapshotIn sources
        writeSnapshot snapshot (siPage ())
        let held = filesUnder snapshot
        let offline : SourceArchiveDownloader = { downloadArchive = fun _ _ _ -> Error (SourceDownloadErr "offline") }
        let proxy = MaterialSourceProxy.createRefractiveIndexInfo sources offline (fixedClock laterClockTime)

        Assert.Equal<Result<SourceSnapshotOutcome, MaterialSourceError>>(
            Error (SourceDownloadErr "offline"),
            proxy.updateSnapshot SourceSnapshotObserver.unobserved)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some heldVersion), proxy.snapshotVersion ())
        Assert.Equal<(string * string) list>(held, filesUnder snapshot)
        Assert.Equal<string list>([ "refractiveindex.info" ], namesIn sources.value)

    [<Fact>]
    let ``a snapshot stamp of another schema version is refused`` () =
        let sources = freshSources owner
        let snapshot = snapshotIn sources
        writeSnapshot snapshot (siPage ())
        let stamp = Path.Combine(snapshot, SourceSnapshotStamp.fileName.value)
        File.WriteAllText(stamp, "{\n  \"schemaVersion\": 2,\n  \"source\": \"refractiveindex.info\",\n  \"sourceVersion\": \"2026-11-01\"\n}\n")
        let proxy = MaterialSourceProxy.createRefractiveIndexInfo sources unusedDownloader (fixedClock firstClockTime)

        match errorOf "version" (proxy.snapshotVersion ()) with
        | UnsupportedSnapshotSchemaErr _ -> ()
        | error -> Assert.Fail $"version: expected UnsupportedSnapshotSchemaErr, got {error.describe}"

        match errorOf "search" (proxy.searchRecords (MaterialSourceQuery "silicon")) with
        | UnsupportedSnapshotSchemaErr _ -> ()
        | error -> Assert.Fail $"search: expected UnsupportedSnapshotSchemaErr, got {error.describe}"
