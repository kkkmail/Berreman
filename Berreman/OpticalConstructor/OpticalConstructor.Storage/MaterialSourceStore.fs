namespace OpticalConstructor.Storage

open System
open System.Globalization
open System.IO
open System.Net.Http
open System.Text
open System.Threading
open Softellect.Sys
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.SchemaValidation
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.KitArchiveStore
open OpticalConstructor.Storage.MaterialImport

/// The refractiveindex.info adapter over an offline snapshot of its GitHub database. Every .NET IO and HTTP exception is
/// caught here and mapped to MaterialSourceError, and nothing above the proxy handles one (spec 0050 S13.1; .spec-md §4).
module MaterialSourceStore =

    /// The stamp beside a source's snapshot: the source and the version its last update installed (spec 0050 S13.1).
    type SourceSnapshotStamp =
        {
            schemaVersion : FileSchemaVersion
            source : MaterialSourceName
            sourceVersion : MaterialSourceVersion
        }

        static member currentSchemaVersion : FileSchemaVersion = FileSchemaVersion 1

        /// The one place `.snapshot-json` is written (spec 0050 S13.1; 001 §A).
        static member extension : FileExtension = FileExtension ".snapshot-json"

        /// Named by its extension alone, as a file that exists once per folder is (spec 0050 S13.1; 001 §A).
        static member fileName : FileName = (FileStem.ofText "").withExtension SourceSnapshotStamp.extension

    /// The text of a snapshot's stamp file (spec 0050 S13.1).
    type SourceSnapshotText =
        | SourceSnapshotText of string

        member this.value : string = let (SourceSnapshotText text) = this in text

    type SourceSnapshotStamp with

        /// Canonical stamp JSON: sorted keys, LF line ends and one final newline (spec 0050 S13.1).
        member this.fileText : Result<SourceSnapshotText, StorageError> =
            nodeOf this |> Result.map (fun node -> node.canonicalFileText |> SourceSnapshotText)

    type SourceSnapshotText with

        member private this.toJsonText : JsonText = JsonText this.value

        /// Validates the stamp's schema and refuses another schema version before binding; there is no up-converter and
        /// no fallback reader (spec 0050 S13.1; 001 §B).
        member this.parseStamp : Result<SourceSnapshotStamp, StorageError> =
            parseVersionedText validateSourceSnapshot SourceSnapshotStamp.extension SourceSnapshotStamp.currentSchemaVersion this.toJsonText

    /// Where a source's database archive is downloaded from (spec 0050 S13.1).
    type SourceArchiveUrl =
        | SourceArchiveUrl of string

        member this.value : string = let (SourceArchiveUrl url) = this in url

    /// Writes the archive at the URL into the file, reading the token while it does; `http` is the real one. The online
    /// sources' one wire carrier: a GET sends no body, and only its answer reaches the file (spec 0050 S13.1; ELEV022).
    [<ReferenceEquality>]
    [<WireCarrier>]
    type SourceArchiveDownloader =
        {
            downloadArchive : SourceArchiveUrl -> FileName -> CancellationToken -> Result<unit, MaterialSourceError>
        }

        /// A GET per call through its own `HttpClient`, the body streamed into the file (spec 0050 S13.1; 001 §D).
        [<ElevationBoundary(nameof IoBoundary, "the one HTTP seam of the online material sources (001 §D): a GET streamed into a file; every exception is SourceDownloadErr")>]
        static member http : SourceArchiveDownloader =
            let downloadArchive (url : SourceArchiveUrl) (file : FileName) (cancellation : CancellationToken) : Result<unit, MaterialSourceError> =
                try
                    use client = new HttpClient()
                    use request = new HttpRequestMessage(HttpMethod.Get, url.value)
                    use response = client.Send(request, HttpCompletionOption.ResponseHeadersRead, cancellation)
                    response.EnsureSuccessStatusCode() |> ignore
                    use body = response.Content.ReadAsStream cancellation
                    use target = new FileStream(file.value, FileMode.Create, FileAccess.Write, FileShare.None)
                    body.CopyToAsync(target, cancellation).GetAwaiter().GetResult()
                    Ok ()
                with error -> Error (SourceDownloadErr $"'{url.value}': {error.Message}")

            { downloadArchive = downloadArchive }

    type KitFileText with

        /// A stamp file as the kit's one decode reads it (spec 0050 S13.1).
        member private this.toSourceSnapshotText : SourceSnapshotText = SourceSnapshotText this.value

        /// A snapshot's catalog as the kit's one decode reads it (spec 0050 S13.1).
        member private this.toRefractiveIndexInfoCatalogText : RefractiveIndexInfoCatalogText = RefractiveIndexInfoCatalogText this.value

        /// A record's page as the kit's one decode reads it (spec 0050 S13.1).
        member private this.toRefractiveIndexInfoText : RefractiveIndexInfoText = RefractiveIndexInfoText this.value

    type KitArchiveFileCount with

        /// An extraction's file count as the snapshot update's (spec 0050 S13.1).
        member private this.toSourceSnapshotFileCount : SourceSnapshotFileCount = SourceSnapshotFileCount this.value

    type KitArchiveProgress with

        /// An extraction's progress as the snapshot update's, of the same counts (spec 0050 S13.1).
        member private this.toSourceSnapshotProgress : SourceSnapshotProgress =
            {
                filesDone = this.filesDone.toSourceSnapshotFileCount
                filesTotal = this.filesTotal.toSourceSnapshotFileCount
            }

    type ClockTime with

        /// The instant's UTC date as a snapshot's version, `yyyy-MM-dd` in the invariant culture (spec 0050 S13.1).
        member private this.sourceSnapshotVersion : MaterialSourceVersion =
            MaterialSourceVersion (this.value.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture))

    type MaterialSourceName with

        /// The source's snapshot folder in the sources folder, named by the source (spec 0050 S13.1).
        member private this.snapshotFolderIn (sources : FolderName) : FolderName = sources.combine (FolderName this.value)

    type MaterialSourceProxy with

        /// The refractiveindex.info adapter: its snapshot in `<sources>/refractiveindex.info`, updated from the database's
        /// archive through `downloader` and stamped with `clock`'s date. Building it touches nothing (spec 0050 S13.1; 001 §D).
        static member createRefractiveIndexInfo
            (sources : FolderName)
            (downloader : SourceArchiveDownloader)
            (clock : ClockProvider)
            : MaterialSourceProxy =
            let sourceName = MaterialSourceName "refractiveindex.info"
            // The database's README: CC0 1.0 Universal (spec 0050 S13.1; 001 §D).
            let licence = EntryLicence "CC0-1.0"
            let archive = SourceArchiveUrl "https://github.com/polyanskiy/refractiveindex.info-database/archive/refs/heads/main.zip"
            let catalogPath = KitRelativePath "database/catalog-nk.yml"
            let dataPath = KitRelativePath "database/data"
            let snapshot = sourceName.snapshotFolderIn sources
            let stampFile = SourceSnapshotStamp.fileName.combine snapshot
            let zip = FileName $"{snapshot.value}.zip.tmp"
            let unpacked = KitFolder (FolderName $"{snapshot.value}.tmp")
            let retired = FolderName $"{snapshot.value}.old"
            let archives = KitArchiveProxy.create ()

            let snapshotFileAt (path : KitRelativePath) : FileName =
                FileName (Path.Combine(Array.append [| snapshot.value |] (path.value.Split '/')))

            let readFileText (file : FileName) : KitFileText = (File.ReadAllBytes file.value |> FileByteContent).kitFileText

            let snapshotMissing = SourceSnapshotMissingErr $"'{sourceName.value}' holds no snapshot in '{snapshot.value}'"

            let readStamp () : Result<SourceSnapshotStamp option, MaterialSourceError> =
                try
                    if File.Exists stampFile.value then
                        match (readFileText stampFile).toSourceSnapshotText.parseStamp with
                        | Ok stamp -> Ok (Some stamp)
                        | Error (UnsupportedSchemaVersionErr _ as error) ->
                            Error (UnsupportedSnapshotSchemaErr $"'{stampFile.value}': {error.describe}")
                        | Error error -> Error (SourceSnapshotIoErr $"'{stampFile.value}': {error.describe}")
                    else
                        Ok None
                with error -> Error (SourceSnapshotIoErr $"'{stampFile.value}': {error.Message}")

            let heldStamp () : Result<SourceSnapshotStamp, MaterialSourceError> =
                readStamp ()
                |> Result.bind (fun stampOpt ->
                    match stampOpt with
                    | Some stamp -> Ok stamp
                    | None -> Error snapshotMissing)

            let catalogHits () : Result<MaterialSourceHit list, MaterialSourceError> =
                let catalogFile = snapshotFileAt catalogPath
                try
                    (readFileText catalogFile).toRefractiveIndexInfoCatalogText.hits
                    |> Result.mapError (fun error -> SourceSnapshotIoErr $"'{catalogFile.value}': {error.describe}")
                with error -> Error (SourceSnapshotIoErr $"'{catalogFile.value}': {error.Message}")

            let searchRecords (query : MaterialSourceQuery) : Result<MaterialSourceHit list, MaterialSourceError> =
                heldStamp () |> Result.bind (fun _ -> catalogHits ()) |> Result.map query.filter

            let fetchPage
                (stamp : SourceSnapshotStamp)
                (hit : MaterialSourceHit)
                (page : KitRelativePath)
                : Result<SourceMaterialRecord, MaterialSourceError> =
                let pageFile = snapshotFileAt page
                try
                    let bytes = File.ReadAllBytes pageFile.value |> FileByteContent
                    let pageText = bytes.kitFileText.toRefractiveIndexInfoText
                    match pageText.importMaterial with
                    | Error error -> Error (SourceRecordUnreadableErr $"'{hit.record.value}': {error.describe}")
                    | Ok material ->
                        let citation =
                            match pageText.referencesOpt with
                            | Some references -> EntryCitation $"{sourceName.value}, {hit.record.value}: {references.value}"
                            | None -> EntryCitation $"{sourceName.value}, {hit.record.value}"
                        let pull : SourcePullInfo =
                            {
                                source = sourceName
                                record = hit.record
                                recordHash = ContentHash.ofBytes bytes
                                sourceVersion = stamp.sourceVersion
                                licence = licence
                                citation = citation
                            }
                        let fetched : SourceMaterialRecord =
                            {
                                pull = pull
                                material = { material with name = hit.name }
                            }
                        Ok fetched
                with error -> Error (SourceSnapshotIoErr $"'{pageFile.value}': {error.Message}")

            let fetchRecord (record : MaterialSourceRecordId) : Result<SourceMaterialRecord, MaterialSourceError> =
                heldStamp ()
                |> Result.bind (fun stamp ->
                    catalogHits ()
                    |> Result.bind (fun hits ->
                        match hits |> List.tryFind (fun hit -> hit.record = record), KitRelativePath.tryCreate record.value with
                        | None, _ ->
                            Error (SourceRecordMissingErr $"'{sourceName.value}' {stamp.sourceVersion.value} does not list '{record.value}'")
                        | Some _, Error error -> Error (SourceRecordMissingErr $"'{record.value}': {error.describe}")
                        | Some hit, Ok recordPath -> fetchPage stamp hit (KitRelativePath $"{dataPath.value}/{recordPath.value}")))

            let snapshotIo (io : unit -> unit) : Result<unit, MaterialSourceError> =
                try
                    io ()
                    Ok ()
                with error -> Error (SourceSnapshotIoErr $"the snapshot folder '{snapshot.value}': {error.Message}")

            // Keep the run's own answer when a cleanup also fails (spec 0050 S13.1).
            let cleanQuietly (io : unit -> unit) : unit =
                try
                    io ()
                with _ -> ()

            let deleteFile (file : FileName) : unit = if File.Exists file.value then File.Delete file.value

            let deleteFolder (folder : FolderName) : unit = if Directory.Exists folder.value then Directory.Delete(folder.value, true)

            let clearLeftovers () : unit =
                Directory.CreateDirectory sources.value |> ignore
                deleteFile zip
                deleteFolder unpacked.value
                deleteFolder retired

            // The archive's one top folder holds the database; only its catalog and its data travel (spec 0050 S13.1).
            let snapshotEntriesOf (entries : KitTreeEntry list) : Result<KitRelativePath * KitTreeEntry list, MaterialSourceError> =
                let notTheDatabase = SourceArchiveInvalidErr $"the archive of '{archive.value}' is not one folder holding '{catalogPath.value}'"
                match entries |> List.map (fun entry -> KitRelativePath (entry.path.value.Split('/').[0])) |> List.distinct with
                | [ top ] ->
                    let catalogEntry = KitTreeFile (KitRelativePath $"{top.value}/{catalogPath.value}")
                    let dataPrefix = $"{top.value}/{dataPath.value}/"
                    if entries |> List.contains catalogEntry then
                        let kept =
                            entries
                            |> List.filter (fun entry -> entry = catalogEntry || entry.path.value.StartsWith(dataPrefix, StringComparison.Ordinal))
                        Ok (top, kept)
                    else
                        Error notTheDatabase
                | _ -> Error notTheDatabase

            // The held snapshot is replaced whole: moved to `.old`, the staged one moved in; `.old` exists here only
            // when this run moved it, so a failed move-in moves it back (spec 0050 S13.1).
            let installStaged (staged : FolderName) : Result<unit, MaterialSourceError> =
                snapshotIo (fun () -> if Directory.Exists snapshot.value then Directory.Move(snapshot.value, retired.value))
                |> Result.bind (fun () ->
                    let installed = snapshotIo (fun () -> Directory.Move(staged.value, snapshot.value))
                    match installed with
                    | Ok () -> ()
                    | Error _ -> cleanQuietly (fun () -> if Directory.Exists retired.value then Directory.Move(retired.value, snapshot.value))
                    installed)

            let stampAndInstall (top : KitRelativePath) (files : KitArchiveFileCount) : Result<SourceSnapshotOutcome, MaterialSourceError> =
                let staged = FolderName (Path.Combine(unpacked.value.value, top.value))
                let version = (clock.now ()).sourceSnapshotVersion
                let stamp : SourceSnapshotStamp =
                    {
                        schemaVersion = SourceSnapshotStamp.currentSchemaVersion
                        source = sourceName
                        sourceVersion = version
                    }
                stamp.fileText
                |> Result.bind (fun text ->
                    let bytes = Encoding.UTF8.GetBytes text.value |> FileByteContent
                    (SourceSnapshotStamp.fileName.combine staged).writeBytesAtomically bytes)
                |> Result.mapError (fun error -> SourceSnapshotIoErr $"the stamp in '{staged.value}': {error.describe}")
                |> Result.bind (fun () -> installStaged staged)
                |> Result.map (fun () -> SourceSnapshotUpdated { version = version; files = files.toSourceSnapshotFileCount })

            let extractAndInstall (observer : SourceSnapshotObserver) : Result<SourceSnapshotOutcome, MaterialSourceError> =
                let archiveObserver : KitArchiveObserver =
                    {
                        reportArchiveProgress = fun progress -> observer.reportSnapshotProgress progress.toSourceSnapshotProgress
                        cancellation = observer.cancellation
                    }

                archives.listZipEntries zip
                |> Result.mapError (fun error -> SourceArchiveInvalidErr $"the archive of '{archive.value}': {error.describe}")
                |> Result.bind snapshotEntriesOf
                |> Result.bind (fun (top, entries) ->
                    let job : KitUnzipJob = { zip = zip; entries = entries; target = unpacked }
                    archives.extractKitZip job archiveObserver
                    |> Result.mapError (fun error -> SourceSnapshotIoErr $"the snapshot folder '{snapshot.value}': {error.describe}")
                    |> Result.bind (fun outcome ->
                        match outcome with
                        | KitArchiveCancelled -> Ok SourceSnapshotCancelled
                        | KitArchiveFinished files -> stampAndInstall top files))

            let updateSnapshot (observer : SourceSnapshotObserver) : Result<SourceSnapshotOutcome, MaterialSourceError> =
                let outcome =
                    try
                        snapshotIo clearLeftovers
                        |> Result.bind (fun () ->
                            let downloaded = downloader.downloadArchive archive zip observer.cancellation
                            if observer.cancellation.IsCancellationRequested then
                                Ok SourceSnapshotCancelled
                            else
                                downloaded |> Result.bind (fun () -> extractAndInstall observer))
                    with error -> Error (SourceSnapshotIoErr $"the snapshot folder '{snapshot.value}': {error.Message}")

                cleanQuietly (fun () -> deleteFile zip)
                cleanQuietly (fun () -> deleteFolder unpacked.value)

                match outcome with
                | Ok (SourceSnapshotUpdated _) -> cleanQuietly (fun () -> deleteFolder retired)
                | Ok SourceSnapshotCancelled
                | Error _ -> ()

                outcome

            {
                sourceName = sourceName
                snapshotVersion = fun () -> readStamp () |> Result.map (Option.map _.sourceVersion)
                searchRecords = searchRecords
                fetchRecord = fetchRecord
                updateSnapshot = updateSnapshot
            }
