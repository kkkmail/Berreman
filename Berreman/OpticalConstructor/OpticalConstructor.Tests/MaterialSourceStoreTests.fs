namespace OpticalConstructor.Tests

open System
open System.IO
open System.IO.Compression
open System.Text
open System.Threading
open Xunit
open Softellect.Sys.Primitives
open Berreman.Dispersion
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Storage.MaterialImport
open OpticalConstructor.Storage.MaterialSourceStore
open OpticalConstructor.Tests.MaterialSourceFixture

/// The refractiveindex.info and PTB OCDB adapters over fixture snapshots and archives under the test output: their reads,
/// their update through the archive seam, and what a failed or cancelled update keeps (spec 0050 S13.1, S13.2).
module MaterialSourceStoreTests =

    let private owner : string = "MaterialSourceStoreTests"

    let private siRecord : MaterialSourceRecordId = MaterialSourceRecordId "main/Si/nk/Test.yml"

    let private glassRecord : MaterialSourceRecordId = MaterialSourceRecordId "glass/test/BK7/nk/Test.yml"

    let private heldVersion : MaterialSourceVersion = MaterialSourceVersion "2026-11-01"

    let private firstClockTime : DateTime = DateTime(2026, 11, 1, 9, 30, 0, DateTimeKind.Utc)

    let private laterClockTime : DateTime = DateTime(2026, 11, 4, 23, 59, 0, DateTimeKind.Utc)

    let private coRecord : MaterialSourceRecordId = MaterialSourceRecordId "Co.txt"

    let private ruRecord : MaterialSourceRecordId = MaterialSourceRecordId "Ru_2019.txt"

    /// The snapshot folder an adapter over `sources` reads.
    let private snapshotIn (sources : FolderName) : string = Path.Combine(sources.value, "refractiveindex.info")

    /// The snapshot folder the PTB OCDB adapter over `sources` reads.
    let private ptbSnapshotIn (sources : FolderName) : string = Path.Combine(sources.value, "ptb-ocdb")

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

    /// The uncertainty of a fetched material's one isotropic tabulated segment.
    let private tabulatedUncertaintyOf (material : MaterialEntry) : TabulatedNkUncertainty option =
        match material.physics with
        | ComplexityPhysics { eps = EpsWithDispValue (IsotropicDispersive [ { dispersion = TabulatedNK table } ]); magnetic = None; active = None } ->
            table.uncertaintyOpt
        | physics -> failwith $"expected one isotropic segment carrying a TabulatedNK table, got %A{physics}"

    [<Fact>]
    let ``a PTB OCDB source without a snapshot answers no version and refuses search and fetch`` () =
        let sources = freshSources owner
        let proxy = MaterialSourceProxy.createPtbOcdb sources unusedDownloader (fixedClock firstClockTime)
        Assert.Equal(MaterialSourceName "PTB OCDB", proxy.sourceName)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok None, proxy.snapshotVersion ())

        match errorOf "search" (proxy.searchRecords (MaterialSourceQuery "co")) with
        | SourceSnapshotMissingErr _ -> ()
        | error -> Assert.Fail $"search: expected SourceSnapshotMissingErr, got {error.describe}"

        match errorOf "fetch" (proxy.fetchRecord coRecord) with
        | SourceSnapshotMissingErr _ -> ()
        | error -> Assert.Fail $"fetch: expected SourceSnapshotMissingErr, got {error.describe}"

        Assert.Empty(namesIn sources.value)

    [<Fact>]
    let ``a PTB OCDB snapshot lists its data records, and a fetched record is the reader's material with CC BY 4.0 and its citation`` () =
        let sources = freshSources owner
        let snapshot = ptbSnapshotIn sources
        writePtbSnapshot snapshot (ptbRecord ())
        let proxy = MaterialSourceProxy.createPtbOcdb sources unusedDownloader (fixedClock laterClockTime)
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some heldVersion), proxy.snapshotVersion ())

        let coHit : MaterialSourceHit = { record = coRecord; name = EntryName "Co (PTB OCDB)" }
        let ruHit : MaterialSourceHit = { record = ruRecord; name = EntryName "Ru_2019 (PTB OCDB)" }
        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(Ok [ coHit; ruHit ], proxy.searchRecords (MaterialSourceQuery ""))
        Assert.Equal<Result<MaterialSourceHit list, MaterialSourceError>>(Ok [ coHit ], proxy.searchRecords (MaterialSourceQuery "co"))

        match proxy.fetchRecord coRecord, (PtbOcdbText (ptbRecord ())).importMaterial with
        | Ok fetched, Ok imported ->
            Assert.Equal(imported.physics, fetched.material.physics)
            match tabulatedUncertaintyOf fetched.material with
            | Some _ -> ()
            | None -> Assert.Fail "the Co record's 3-sigma bounds must be its uncertainties"
            Assert.Equal(coHit.name, fetched.material.name)
            let recordBytes = File.ReadAllBytes(Path.Combine(snapshot, "ocdb", "db", "data", "Co.txt")) |> FileByteContent
            let expected : SourcePullInfo =
                {
                    source = MaterialSourceName "PTB OCDB"
                    record = coRecord
                    recordHash = ContentHash.ofBytes recordBytes
                    sourceVersion = heldVersion
                    licence = EntryLicence "CC-BY-4.0"
                    citation = EntryCitation "PTB Optical Constants Database (OCDB), ocdb/db/data/Co.txt: Optical constants for Co created by PTB"
                }
            Assert.Equal(expected, fetched.pull)
        | Error error, _ -> Assert.Fail $"fetch: {error.describe}"
        | _, Error error -> Assert.Fail $"import: {error.describe}"

        match proxy.fetchRecord ruRecord with
        | Ok fetched ->
            Assert.Equal(ruHit.name, fetched.material.name)
            Assert.Equal<TabulatedNkUncertainty option>(None, tabulatedUncertaintyOf fetched.material)
        | Error error -> Assert.Fail $"fetch: {error.describe}"

        for unlisted in [ MaterialSourceRecordId "../Co.txt"; MaterialSourceRecordId "Ta.txt" ] do
            match errorOf unlisted.value (proxy.fetchRecord unlisted) with
            | SourceRecordMissingErr _ -> ()
            | error -> Assert.Fail $"'{unlisted.value}': expected SourceRecordMissingErr, got {error.describe}"

    [<Fact>]
    let ``a PTB OCDB update keeps the data records and the licence file of the repository archive`` () =
        let sources = freshSources owner
        let snapshot = ptbSnapshotIn sources
        writePtbSnapshot snapshot (ptbRecord ())
        let zip = $"{sources.value}.zip"
        writePtbArchive zip (changedPtbRecord ())
        let asked = ResizeArray<SourceArchiveUrl>()
        let proxy = MaterialSourceProxy.createPtbOcdb sources (copyingDownloader zip asked) (fixedClock laterClockTime)
        let progress = ResizeArray<SourceSnapshotProgress>()
        let observer : SourceSnapshotObserver = { reportSnapshotProgress = progress.Add; cancellation = CancellationToken.None }

        let updated : SourceSnapshotInfo = { version = MaterialSourceVersion "2026-11-04"; files = SourceSnapshotFileCount 3 }
        Assert.Equal<Result<SourceSnapshotOutcome, MaterialSourceError>>(Ok (SourceSnapshotUpdated updated), proxy.updateSnapshot observer)
        Assert.Equal<SourceSnapshotProgress list>([ progressOf 1 3; progressOf 2 3; progressOf 3 3 ], List.ofSeq progress)
        Assert.Equal<SourceArchiveUrl list>([ SourceArchiveUrl "https://github.com/PTB-SR/ocdb/archive/refs/heads/master.zip" ], List.ofSeq asked)

        Assert.Equal<string list>(
            [ ".snapshot-json"; "ocdb/db/COPYING"; "ocdb/db/data/Co.txt"; "ocdb/db/data/Ru_2019.txt" ],
            filesUnder snapshot |> List.map fst)
        Assert.Equal<string list>([ ".snapshot-json"; "ocdb" ], namesIn snapshot)
        Assert.Equal<string list>([ "COPYING"; "data" ], namesIn (Path.Combine(snapshot, "ocdb", "db")))
        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some updated.version), proxy.snapshotVersion ())

        match proxy.fetchRecord coRecord with
        | Ok fetched ->
            Assert.Equal(ContentHash.ofBytes (FileByteContent (Encoding.UTF8.GetBytes (changedPtbRecord ()))), fetched.pull.recordHash)
            Assert.Equal(updated.version, fetched.pull.sourceVersion)
        | Error error -> Assert.Fail error.describe

        Assert.Equal<string list>([ "ptb-ocdb" ], namesIn sources.value)

    [<Fact>]
    let ``a PTB OCDB archive without its licence file is refused and the held snapshot kept`` () =
        let sources = freshSources owner
        let snapshot = ptbSnapshotIn sources
        writePtbSnapshot snapshot (ptbRecord ())
        let held = filesUnder snapshot
        let zip = $"{sources.value}.zip"
        writePtbArchive zip (changedPtbRecord ())
        do
            use archive = ZipFile.Open(zip, ZipArchiveMode.Update)
            match archive.GetEntry "ocdb-master/ocdb/db/COPYING" with
            | null -> Assert.Fail "the fixture archive holds no licence file to remove"
            | licence -> licence.Delete()
        let proxy = MaterialSourceProxy.createPtbOcdb sources (copyingDownloader zip (ResizeArray ())) (fixedClock laterClockTime)

        match errorOf "update" (proxy.updateSnapshot SourceSnapshotObserver.unobserved) with
        | SourceArchiveInvalidErr _ -> ()
        | error -> Assert.Fail $"update: expected SourceArchiveInvalidErr, got {error.describe}"

        Assert.Equal<Result<MaterialSourceVersion option, MaterialSourceError>>(Ok (Some heldVersion), proxy.snapshotVersion ())
        Assert.Equal<(string * string) list>(held, filesUnder snapshot)
        Assert.Equal<string list>([ "ptb-ocdb" ], namesIn sources.value)
