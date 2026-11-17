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
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Domain.HenkeCompounds
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.CanonicalJson
open OpticalConstructor.Storage.SchemaValidation
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.KitArchiveStore
open OpticalConstructor.Storage.MaterialImport

/// The online sources' adapters, each over an offline snapshot of its source: a GitHub repository's archive or CXRO's
/// element files. Every .NET IO and HTTP exception is caught here and mapped to MaterialSourceError, and nothing above the
/// proxy handles one (spec 0050 S13.1, S13.2; .spec-md §4).
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

        /// A PTB OCDB record as the kit's one decode reads it, a leading byte-order mark dropped (spec 0050 S13.2).
        member private this.toPtbOcdbText : PtbOcdbText = PtbOcdbText this.value

        /// A CXRO scattering-factor file as the kit's one decode reads it (spec 0050 S13.2).
        member private this.toHenkeScatteringText : HenkeScatteringText = HenkeScatteringText this.value

    type FileName with

        /// A snapshot file's bytes through the kit's one decode (spec 0050 S13.1).
        member private this.snapshotFileText : KitFileText = (File.ReadAllBytes this.value |> FileByteContent).kitFileText

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

    /// Whether a file of a source's archive travels into its snapshot (spec 0050 S13.2).
    type private SourceArchiveEntryChoice =
        | KeepArchiveEntry
        | DropArchiveEntry

    /// A source's archive: its URL, the file that makes it the source's, and the files its snapshot keeps, each path
    /// relative to the archive's one top folder (spec 0050 S13.2).
    [<ReferenceEquality>]
    type private SourceArchiveSpec =
        {
            url : SourceArchiveUrl
            requiredFile : KitRelativePath
            keep : KitRelativePath -> SourceArchiveEntryChoice
        }

    /// One file a file-by-file update downloads: its URL, its name in the snapshot and the check its downloaded copy passes
    /// (spec 0050 S13.2).
    [<ReferenceEquality>]
    type private SourceSnapshotDownload =
        {
            fileUrl : SourceArchiveUrl
            fileName : FileName
            checkFile : FileName -> Result<unit, MaterialSourceError>
        }

    /// A source's snapshot folder, the one body every adapter's version, install and update run; an update works in its
    /// siblings `<folder>.zip.tmp`, `<folder>.tmp` and `<folder>.old` (spec 0050 S13.1, S13.2).
    type private SourceSnapshotLayout =
        {
            source : MaterialSourceName
            folder : FolderName
        }

        member private this.stampFile : FileName = SourceSnapshotStamp.fileName.combine this.folder

        member private this.downloadedZip : FileName = FileName $"{this.folder.value}.zip.tmp"

        member private this.unpackedFolder : KitFolder = KitFolder (FolderName $"{this.folder.value}.tmp")

        member private this.retiredFolder : FolderName = FolderName $"{this.folder.value}.old"

        /// The snapshot's folder at a path relative to it (spec 0050 S13.2).
        member this.folderAt (path : KitRelativePath) : FolderName =
            FolderName (Path.Combine(Array.append [| this.folder.value |] (path.value.Split '/')))

        /// The snapshot's file at a path relative to it (spec 0050 S13.1).
        member this.fileAt (path : KitRelativePath) : FileName =
            match path.parentOpt with
            | Some parent -> path.leafName.combine (this.folderAt parent)
            | None -> path.leafName.combine this.folder

        /// The stamp's version, None without a stamp; a stamp of another schema version is UnsupportedSnapshotSchemaErr
        /// (spec 0050 S13.1).
        member this.snapshotVersion : Result<MaterialSourceVersion option, MaterialSourceError> =
            let stampFile = this.stampFile
            try
                if File.Exists stampFile.value then
                    match stampFile.snapshotFileText.toSourceSnapshotText.parseStamp with
                    | Ok stamp -> Ok (Some stamp.sourceVersion)
                    | Error (UnsupportedSchemaVersionErr _ as error) ->
                        Error (UnsupportedSnapshotSchemaErr $"'{stampFile.value}': {error.describe}")
                    | Error error -> Error (SourceSnapshotIoErr $"'{stampFile.value}': {error.describe}")
                else
                    Ok None
            with error -> Error (SourceSnapshotIoErr $"'{stampFile.value}': {error.Message}")

        /// The stamp's version; SourceSnapshotMissingErr without a stamp (spec 0050 S13.2).
        member this.heldVersion : Result<MaterialSourceVersion, MaterialSourceError> =
            this.snapshotVersion
            |> Result.bind (fun versionOpt ->
                match versionOpt with
                | Some version -> Ok version
                | None -> Error (SourceSnapshotMissingErr $"'{this.source.value}' holds no snapshot in '{this.folder.value}'"))

        member private this.snapshotIo (io : unit -> unit) : Result<unit, MaterialSourceError> =
            try
                io ()
                Ok ()
            with error -> Error (SourceSnapshotIoErr $"the snapshot folder '{this.folder.value}': {error.Message}")

        // Keep the run's own answer when a cleanup also fails (spec 0050 S13.1).
        static member private cleanQuietly (io : unit -> unit) : unit =
            try
                io ()
            with _ -> ()

        static member private deleteFile (file : FileName) : unit = if File.Exists file.value then File.Delete file.value

        static member private deleteFolder (folder : FolderName) : unit =
            if Directory.Exists folder.value then Directory.Delete(folder.value, true)

        /// Deletes what an interrupted update left beside the snapshot, creating the sources folder when it is missing
        /// (spec 0050 S13.1, S13.2).
        member private this.clearLeftovers () : unit =
            Directory.CreateDirectory(Path.GetDirectoryName this.folder.value) |> ignore
            SourceSnapshotLayout.deleteFile this.downloadedZip
            SourceSnapshotLayout.deleteFolder this.unpackedFolder.value
            SourceSnapshotLayout.deleteFolder this.retiredFolder

        /// Stamps `staged` with the clock's date and installs it whole: the held snapshot moved to `.old`, the staged one
        /// moved in, then `.old` deleted. `.old` exists only when this run moved it, so a failed move-in moves it back
        /// (spec 0050 S13.1, S13.2).
        member this.installFrom
            (staged : FolderName)
            (clock : ClockProvider)
            (files : SourceSnapshotFileCount)
            : Result<SourceSnapshotOutcome, MaterialSourceError> =
            let snapshot = this.folder
            let retired = this.retiredFolder
            let version = (clock.now ()).sourceSnapshotVersion
            let stamp : SourceSnapshotStamp =
                {
                    schemaVersion = SourceSnapshotStamp.currentSchemaVersion
                    source = this.source
                    sourceVersion = version
                }

            let moveIn () : Result<unit, MaterialSourceError> =
                let installed = this.snapshotIo (fun () -> Directory.Move(staged.value, snapshot.value))
                match installed with
                | Ok () -> SourceSnapshotLayout.cleanQuietly (fun () -> SourceSnapshotLayout.deleteFolder retired)
                | Error _ ->
                    SourceSnapshotLayout.cleanQuietly (fun () ->
                        if Directory.Exists retired.value then Directory.Move(retired.value, snapshot.value))
                installed

            stamp.fileText
            |> Result.bind (fun text ->
                let bytes = Encoding.UTF8.GetBytes text.value |> FileByteContent
                (SourceSnapshotStamp.fileName.combine staged).writeBytesAtomically bytes)
            |> Result.mapError (fun error -> SourceSnapshotIoErr $"the stamp in '{staged.value}': {error.describe}")
            |> Result.bind (fun () -> this.snapshotIo (fun () -> if Directory.Exists snapshot.value then Directory.Move(snapshot.value, retired.value)))
            |> Result.bind moveIn
            |> Result.map (fun () -> SourceSnapshotUpdated { version = version; files = files })

        /// Downloads `spec`'s archive and installs the file entries it keeps, through the app's one zip reader, telling
        /// `observer` per file. An archive that is not one top folder holding `spec.requiredFile` is SourceArchiveInvalidErr;
        /// a failed or cancelled update keeps the held snapshot and leaves nothing behind (spec 0050 S13.1, S13.2).
        member this.updateFromArchive
            (spec : SourceArchiveSpec)
            (downloader : SourceArchiveDownloader)
            (clock : ClockProvider)
            (observer : SourceSnapshotObserver)
            : Result<SourceSnapshotOutcome, MaterialSourceError> =
            let zip = this.downloadedZip
            let unpacked = this.unpackedFolder
            let archives = KitArchiveProxy.create ()

            let keptEntriesOf (entries : KitTreeEntry list) : Result<KitRelativePath * KitTreeEntry list, MaterialSourceError> =
                let notTheSource =
                    SourceArchiveInvalidErr $"the archive of '{spec.url.value}' is not one folder holding '{spec.requiredFile.value}'"
                match entries |> List.map (fun entry -> KitRelativePath (entry.path.value.Split('/').[0])) |> List.distinct with
                | [ top ] ->
                    let topPrefix = $"{top.value}/"
                    let topFiles =
                        entries
                        |> List.choose (fun entry ->
                            match entry with
                            | KitTreeFile path when path.value.StartsWith(topPrefix, StringComparison.Ordinal) ->
                                Some (KitRelativePath (path.value.Substring topPrefix.Length), entry)
                            | KitTreeFile _
                            | KitTreeFolder _ -> None)
                    if topFiles |> List.exists (fun (relative, _) -> relative = spec.requiredFile) then
                        let kept =
                            topFiles
                            |> List.choose (fun (relative, entry) ->
                                match spec.keep relative with
                                | KeepArchiveEntry -> Some entry
                                | DropArchiveEntry -> None)
                        Ok (top, kept)
                    else
                        Error notTheSource
                | _ -> Error notTheSource

            let extractAndInstall () : Result<SourceSnapshotOutcome, MaterialSourceError> =
                let archiveObserver : KitArchiveObserver =
                    {
                        reportArchiveProgress = fun progress -> observer.reportSnapshotProgress progress.toSourceSnapshotProgress
                        cancellation = observer.cancellation
                    }

                archives.listZipEntries zip
                |> Result.mapError (fun error -> SourceArchiveInvalidErr $"the archive of '{spec.url.value}': {error.describe}")
                |> Result.bind keptEntriesOf
                |> Result.bind (fun (top, entries) ->
                    let job : KitUnzipJob = { zip = zip; entries = entries; target = unpacked }
                    archives.extractKitZip job archiveObserver
                    |> Result.mapError (fun error -> SourceSnapshotIoErr $"the snapshot folder '{this.folder.value}': {error.describe}")
                    |> Result.bind (fun outcome ->
                        match outcome with
                        | KitArchiveCancelled -> Ok SourceSnapshotCancelled
                        | KitArchiveFinished files ->
                            let staged = FolderName (Path.Combine(unpacked.value.value, top.value))
                            this.installFrom staged clock files.toSourceSnapshotFileCount))

            let outcome =
                try
                    this.snapshotIo this.clearLeftovers
                    |> Result.bind (fun () ->
                        let downloaded = downloader.downloadArchive spec.url zip observer.cancellation
                        if observer.cancellation.IsCancellationRequested then
                            Ok SourceSnapshotCancelled
                        else
                            downloaded |> Result.bind extractAndInstall)
                with error -> Error (SourceSnapshotIoErr $"the snapshot folder '{this.folder.value}': {error.Message}")

            SourceSnapshotLayout.cleanQuietly (fun () -> SourceSnapshotLayout.deleteFile zip)
            SourceSnapshotLayout.cleanQuietly (fun () -> SourceSnapshotLayout.deleteFolder unpacked.value)
            outcome

        /// Downloads each file in order into `<folder>.tmp` through `downloader`, reading the cancellation before each and
        /// after its download and telling `observer` after each, then installs them whole; a file its check refuses is that
        /// error. A failed or cancelled update keeps the held snapshot and leaves nothing behind (spec 0050 S13.2).
        member this.updateFileByFile
            (downloads : SourceSnapshotDownload list)
            (downloader : SourceArchiveDownloader)
            (clock : ClockProvider)
            (observer : SourceSnapshotObserver)
            : Result<SourceSnapshotOutcome, MaterialSourceError> =
            let staged = this.unpackedFolder.value
            let filesTotal = downloads |> List.length |> SourceSnapshotFileCount

            let rec fetchFrom (pending : SourceSnapshotDownload list) (filesDone : int) : Result<SourceSnapshotOutcome, MaterialSourceError> =
                match pending with
                | [] -> this.installFrom staged clock filesTotal
                | download :: rest ->
                    if observer.cancellation.IsCancellationRequested then
                        Ok SourceSnapshotCancelled
                    else
                        let target = download.fileName.combine staged
                        let downloaded = downloader.downloadArchive download.fileUrl target observer.cancellation
                        if observer.cancellation.IsCancellationRequested then
                            Ok SourceSnapshotCancelled
                        else
                            match downloaded |> Result.bind (fun () -> download.checkFile target) with
                            | Error error -> Error error
                            | Ok () ->
                                let progress : SourceSnapshotProgress =
                                    {
                                        filesDone = SourceSnapshotFileCount (filesDone + 1)
                                        filesTotal = filesTotal
                                    }
                                observer.reportSnapshotProgress progress
                                fetchFrom rest (filesDone + 1)

            let outcome =
                try
                    this.snapshotIo (fun () ->
                        this.clearLeftovers ()
                        Directory.CreateDirectory staged.value |> ignore)
                    |> Result.bind (fun () -> fetchFrom downloads 0)
                with error -> Error (SourceSnapshotIoErr $"the snapshot folder '{this.folder.value}': {error.Message}")

            // An install moved the staged folder in; every other end leaves it to delete (spec 0050 S13.2).
            SourceSnapshotLayout.cleanQuietly (fun () -> SourceSnapshotLayout.deleteFolder staged)
            outcome

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
            let catalogPath = KitRelativePath "database/catalog-nk.yml"
            let dataPath = KitRelativePath "database/data"
            let layout : SourceSnapshotLayout = { source = sourceName; folder = sourceName.snapshotFolderIn sources }

            // The archive's one top folder holds the database; only its catalog and its data travel (spec 0050 S13.1).
            let archive : SourceArchiveSpec =
                {
                    url = SourceArchiveUrl "https://github.com/polyanskiy/refractiveindex.info-database/archive/refs/heads/main.zip"
                    requiredFile = catalogPath
                    keep =
                        fun path ->
                            if path = catalogPath || path.value.StartsWith($"{dataPath.value}/", StringComparison.Ordinal) then
                                KeepArchiveEntry
                            else
                                DropArchiveEntry
                }

            let catalogHits () : Result<MaterialSourceHit list, MaterialSourceError> =
                let catalogFile = layout.fileAt catalogPath
                try
                    catalogFile.snapshotFileText.toRefractiveIndexInfoCatalogText.hits
                    |> Result.mapError (fun error -> SourceSnapshotIoErr $"'{catalogFile.value}': {error.describe}")
                with error -> Error (SourceSnapshotIoErr $"'{catalogFile.value}': {error.Message}")

            let searchRecords (query : MaterialSourceQuery) : Result<MaterialSourceHit list, MaterialSourceError> =
                layout.heldVersion |> Result.bind (fun _ -> catalogHits ()) |> Result.map query.filter

            let fetchPage
                (version : MaterialSourceVersion)
                (hit : MaterialSourceHit)
                (page : KitRelativePath)
                : Result<SourceMaterialRecord, MaterialSourceError> =
                let pageFile = layout.fileAt page
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
                                sourceVersion = version
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
                layout.heldVersion
                |> Result.bind (fun version ->
                    catalogHits ()
                    |> Result.bind (fun hits ->
                        match hits |> List.tryFind (fun hit -> hit.record = record), KitRelativePath.tryCreate record.value with
                        | None, _ ->
                            Error (SourceRecordMissingErr $"'{sourceName.value}' {version.value} does not list '{record.value}'")
                        | Some _, Error error -> Error (SourceRecordMissingErr $"'{record.value}': {error.describe}")
                        | Some hit, Ok recordPath -> fetchPage version hit (KitRelativePath $"{dataPath.value}/{recordPath.value}")))

            {
                sourceName = sourceName
                snapshotVersion = fun () -> layout.snapshotVersion
                searchRecords = searchRecords
                fetchRecord = fetchRecord
                updateSnapshot = layout.updateFromArchive archive downloader clock
            }

        /// The PTB OCDB adapter: its snapshot in `<sources>/ptb-ocdb`, the data records and licence file of PTB's
        /// repository archive, updated by refractiveindex.info's body. Building it touches nothing (spec 0050 S13.2; 001 §D).
        static member createPtbOcdb
            (sources : FolderName)
            (downloader : SourceArchiveDownloader)
            (clock : ClockProvider)
            : MaterialSourceProxy =
            let sourceName = MaterialSourceName "PTB OCDB"
            // `ocdb/db/COPYING`: the data files in `data` are licensed under CC BY 4.0 (spec 0050 S13.2; 001 §D).
            let licence = EntryLicence "CC-BY-4.0"
            let licencePath = KitRelativePath "ocdb/db/COPYING"
            let dataPath = KitRelativePath "ocdb/db/data"
            let recordExtension = FileExtension ".txt"
            let layout : SourceSnapshotLayout = { source = sourceName; folder = sources.combine (FolderName "ptb-ocdb") }

            // The repository has no index: a record is a `.txt` file directly in `ocdb/db/data/` (spec 0050 S13.2).
            let archive : SourceArchiveSpec =
                {
                    url = SourceArchiveUrl "https://github.com/PTB-SR/ocdb/archive/refs/heads/master.zip"
                    requiredFile = licencePath
                    keep =
                        fun path ->
                            if path = licencePath
                               || (path.parentOpt = Some dataPath
                                   && path.value.EndsWith(recordExtension.value, StringComparison.OrdinalIgnoreCase)) then
                                KeepArchiveEntry
                            else
                                DropArchiveEntry
                }

            let dataHits () : Result<MaterialSourceHit list, MaterialSourceError> =
                let dataFolder = layout.folderAt dataPath
                try
                    Directory.EnumerateFiles dataFolder.value
                    |> Seq.map (fun file -> MaterialSourceRecordId (Path.GetFileName file))
                    |> Seq.filter (fun record -> record.value.EndsWith(recordExtension.value, StringComparison.OrdinalIgnoreCase))
                    |> Seq.sortWith (fun first second -> String.CompareOrdinal(first.value, second.value))
                    |> Seq.map (fun record ->
                        let hit : MaterialSourceHit =
                            {
                                record = record
                                name = EntryName $"{Path.GetFileNameWithoutExtension record.value} ({sourceName.value})"
                            }
                        hit)
                    |> Seq.toList
                    |> Ok
                with error -> Error (SourceSnapshotIoErr $"'{dataFolder.value}': {error.Message}")

            let searchRecords (query : MaterialSourceQuery) : Result<MaterialSourceHit list, MaterialSourceError> =
                layout.heldVersion |> Result.bind (fun _ -> dataHits ()) |> Result.map query.filter

            let fetchData (version : MaterialSourceVersion) (hit : MaterialSourceHit) : Result<SourceMaterialRecord, MaterialSourceError> =
                let recordPath = KitRelativePath $"{dataPath.value}/{hit.record.value}"
                let recordFile = layout.fileAt recordPath
                try
                    let bytes = File.ReadAllBytes recordFile.value |> FileByteContent
                    let recordText = bytes.kitFileText.toPtbOcdbText
                    match recordText.importMaterial with
                    | Error error -> Error (SourceRecordUnreadableErr $"'{hit.record.value}': {error.describe}")
                    | Ok material ->
                        let cited = $"PTB Optical Constants Database (OCDB), {recordPath.value}"
                        let citation =
                            match recordText.headlineOpt with
                            | Some headline -> EntryCitation $"{cited}: {headline.value}"
                            | None -> EntryCitation cited
                        let pull : SourcePullInfo =
                            {
                                source = sourceName
                                record = hit.record
                                recordHash = ContentHash.ofBytes bytes
                                sourceVersion = version
                                licence = licence
                                citation = citation
                            }
                        let fetched : SourceMaterialRecord =
                            {
                                pull = pull
                                material = { material with name = hit.name }
                            }
                        Ok fetched
                with error -> Error (SourceSnapshotIoErr $"'{recordFile.value}': {error.Message}")

            let fetchRecord (record : MaterialSourceRecordId) : Result<SourceMaterialRecord, MaterialSourceError> =
                layout.heldVersion
                |> Result.bind (fun version ->
                    dataHits ()
                    |> Result.bind (fun hits ->
                        match hits |> List.tryFind (fun hit -> hit.record = record) with
                        | Some hit -> fetchData version hit
                        | None ->
                            Error (SourceRecordMissingErr $"'{sourceName.value}' {version.value} does not list '{record.value}'")))

            {
                sourceName = sourceName
                snapshotVersion = fun () -> layout.snapshotVersion
                searchRecords = searchRecords
                fetchRecord = fetchRecord
                updateSnapshot = layout.updateFromArchive archive downloader clock
            }

        /// The CXRO Henke compound calculator: its snapshot in `<sources>/cxro-henke` holds CXRO's atomic scattering
        /// factors of H to U, fetched file by file through `downloader`; a record is a compound it computes. Building it
        /// touches nothing (spec 0050 S13.2; 001 §D).
        static member createHenkeCompounds
            (sources : FolderName)
            (downloader : SourceArchiveDownloader)
            (clock : ClockProvider)
            : MaterialSourceProxy =
            let sourceName = MaterialSourceName "CXRO Henke"
            // CXRO states no licence; `LicenseRef-` is SPDX's form for one it does not list (spec 0050 S13.2).
            let licence = EntryLicence "LicenseRef-CXRO"
            let baseUrl = "https://henke.lbl.gov/optical_constants/sf/"
            let layout : SourceSnapshotLayout = { source = sourceName; folder = sources.combine (FolderName "cxro-henke") }

            let fileOf (element : ChemicalElement) : FileName = FileName $"{element.symbol.value.ToLowerInvariant()}.nff"

            let elementFileOf (held : FormulaCount) : FileName = (fileOf held.element).combine layout.folder

            let missingFactors (version : MaterialSourceVersion) (held : FormulaCount) : MaterialSourceError =
                SourceRecordMissingErr $"'{sourceName.value}' {version.value} holds no factors for '{held.element.symbol.value}'"

            // `CompoundSpec`'s own density text is private to Domain; this is its rule, G6 in the invariant culture.
            let compoundText (spec : CompoundSpec) : string =
                let density = spec.density.value.ToString("G6", CultureInfo.InvariantCulture)
                $"{spec.formula.text} at {density} g/cm³"

            let downloadOf (element : ChemicalElement) : SourceSnapshotDownload =
                let file = fileOf element
                let url = SourceArchiveUrl $"{baseUrl}{file.value}"
                {
                    fileUrl = url
                    fileName = file
                    checkFile =
                        fun downloaded ->
                            match downloaded.snapshotFileText.toHenkeScatteringText.factors with
                            | Ok _ -> Ok ()
                            | Error error ->
                                Error (SourceArchiveInvalidErr $"'{element.symbol.value}' from '{url.value}': {error.describe}")
                }

            // The source has no catalog: its one hit is the compound the query writes (spec 0050 S13.2).
            let searchRecords (query : MaterialSourceQuery) : Result<MaterialSourceHit list, MaterialSourceError> =
                layout.heldVersion
                |> Result.bind (fun version ->
                    match CompoundSpec.tryOfQuery query with
                    | Error error -> Error (SourceRecordUnreadableErr $"'{query.value}': {error.describe}")
                    | Ok spec ->
                        match spec.formula.counts |> List.tryFind (fun held -> not (File.Exists (elementFileOf held).value)) with
                        | Some held -> Error (missingFactors version held)
                        | None ->
                            let hit : MaterialSourceHit = { record = spec.recordId; name = spec.entryName }
                            Ok [ hit ])

            let fetchCompound
                (version : MaterialSourceVersion)
                (record : MaterialSourceRecordId)
                (spec : CompoundSpec)
                : Result<SourceMaterialRecord, MaterialSourceError> =
                let readElement (held : FormulaCount) : Result<FileByteContent, MaterialSourceError> =
                    let file = elementFileOf held
                    if File.Exists file.value then Ok (File.ReadAllBytes file.value |> FileByteContent) else Error (missingFactors version held)

                let factorsOf (held : FormulaCount) (bytes : FileByteContent) : Result<ElementSymbol * AtomicScatteringFactors, MaterialSourceError> =
                    match bytes.kitFileText.toHenkeScatteringText.factors with
                    | Ok factors -> Ok (held.element.symbol, factors)
                    | Error error -> Error (SourceRecordUnreadableErr $"'{held.element.symbol.value}' of '{record.value}': {error.describe}")

                let compound = compoundText spec

                spec.formula.counts
                |> List.map readElement
                |> OpticalConstructor.Domain.ItemEvaluation.sequenced
                |> Result.bind (fun files ->
                    List.map2 factorsOf spec.formula.counts files
                    |> OpticalConstructor.Domain.ItemEvaluation.sequenced
                    |> Result.bind (fun factors ->
                        spec.indexTableWith (Map.ofList factors)
                        |> Result.mapError (fun error -> SourceRecordUnreadableErr $"'{record.value}': {error.describe}"))
                    |> Result.map (fun table ->
                        // The record's text, then each element's file in formula order, so an update that changes one
                        // element's factors changes the hash (spec 0050 S13.1, S13.2).
                        let hashed = Array.concat (Encoding.UTF8.GetBytes record.value :: (files |> List.map _.value)) |> FileByteContent
                        let pull : SourcePullInfo =
                            {
                                source = sourceName
                                record = record
                                recordHash = ContentHash.ofBytes hashed
                                sourceVersion = version
                                licence = licence
                                citation =
                                    EntryCitation
                                        $"B.L. Henke, E.M. Gullikson and J.C. Davis, Atomic Data and Nuclear Data Tables 54, 181-342 (1993); CXRO atomic scattering factors (henke.lbl.gov/optical_constants): {compound}"
                            }
                        let description =
                            EntryDescription $"Computed from CXRO atomic scattering factors (Henke, Gullikson and Davis 1993) for {compound}."
                        let fetched : SourceMaterialRecord =
                            {
                                pull = pull
                                material = table.isotropicEntry spec.entryName CategoryIds.semiconductor description
                            }
                        fetched))

            let fetchRecord (record : MaterialSourceRecordId) : Result<SourceMaterialRecord, MaterialSourceError> =
                layout.heldVersion
                |> Result.bind (fun version ->
                    match CompoundSpec.tryOfRecord record with
                    | Error error -> Error (SourceRecordMissingErr $"'{record.value}': {error.describe}")
                    | Ok spec ->
                        try
                            fetchCompound version record spec
                        with error -> Error (SourceSnapshotIoErr $"'{layout.folder.value}': {error.Message}"))

            {
                sourceName = sourceName
                snapshotVersion = fun () -> layout.snapshotVersion
                searchRecords = searchRecords
                fetchRecord = fetchRecord
                updateSnapshot = layout.updateFileByFile (ChemicalElement.all |> List.map downloadOf) downloader clock
            }
