namespace OpticalConstructor.Domain

open System.Threading
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles

/// The online-source seam: one `MaterialSourceProxy` per adapter searches, fetches and updates its source's snapshot,
/// doing IO only; what a pull writes is the import's (spec 0050 S13.1; 001 §D, §P).
module MaterialSources =

    /// Why a source could not be searched, fetched from or updated; the reason names the source, record or file (spec 0050 S13.1).
    type MaterialSourceError =
        /// The source holds no snapshot yet (spec 0050 S13.1).
        | SourceSnapshotMissingErr of reason : string
        /// The held snapshot does not list the record (spec 0050 S13.1).
        | SourceRecordMissingErr of reason : string
        /// The record's page does not parse (spec 0050 S13.1).
        | SourceRecordUnreadableErr of reason : string
        /// The snapshot folder could not be read or written (spec 0050 S13.1).
        | SourceSnapshotIoErr of reason : string
        /// The source's archive could not be downloaded (spec 0050 S13.1).
        | SourceDownloadErr of reason : string
        /// The downloaded archive is not the source's database (spec 0050 S13.1).
        | SourceArchiveInvalidErr of reason : string
        /// A snapshot stamp of another schema version (spec 0050 S13.1; 001 §B: never up-converted).
        | UnsupportedSnapshotSchemaErr of reason : string

        member this.describe : string =
            match this with
            | SourceSnapshotMissingErr reason -> $"the source holds no snapshot: {reason}"
            | SourceRecordMissingErr reason -> $"the snapshot does not list the record: {reason}"
            | SourceRecordUnreadableErr reason -> $"the record could not be read: {reason}"
            | SourceSnapshotIoErr reason -> $"the snapshot could not be read or written: {reason}"
            | SourceDownloadErr reason -> $"the source's archive could not be downloaded: {reason}"
            | SourceArchiveInvalidErr reason -> $"the archive is not the source's database: {reason}"
            | UnsupportedSnapshotSchemaErr reason -> $"the snapshot stamp has another schema version: {reason}"

    /// One record a search finds, and the name a material pulled from it takes (spec 0050 S13.1).
    type MaterialSourceHit =
        {
            record : MaterialSourceRecordId
            name : EntryName
        }

    /// A search's text; `filter` is the one search rule every adapter applies (spec 0050 S13.1).
    type MaterialSourceQuery =
        | MaterialSourceQuery of string

        member this.value : string = let (MaterialSourceQuery s) = this in s

        /// Keeps, in order, the hits whose name or record holds each white-space-separated word, ignoring case; a blank
        /// query keeps every hit (spec 0050 S13.1).
        member this.filter (hits : MaterialSourceHit list) : MaterialSourceHit list =
            let words = this.value.Split(Array.empty<char>, System.StringSplitOptions.RemoveEmptyEntries)
            hits
            |> List.filter (fun hit ->
                words
                |> Array.forall (fun word ->
                    hit.name.value.Contains(word, System.StringComparison.OrdinalIgnoreCase)
                    || hit.record.value.Contains(word, System.StringComparison.OrdinalIgnoreCase)))

    /// A fetched record: its pull provenance and the material its page parses into, named by its hit, under an id the
    /// pull replaces (spec 0050 S13.1).
    type SourceMaterialRecord =
        {
            pull : SourcePullInfo
            material : MaterialEntry
        }

    /// How many files a snapshot update writes (spec 0050 S13.1).
    type SourceSnapshotFileCount =
        | SourceSnapshotFileCount of int

        member this.value : int = let (SourceSnapshotFileCount count) = this in count

    /// Told after each file a snapshot update writes (spec 0050 S13.1, .spec-md §10.2).
    type SourceSnapshotProgress =
        {
            filesDone : SourceSnapshotFileCount
            filesTotal : SourceSnapshotFileCount
        }

    /// Told after each file and asked before each whether the update is cancelled, as `JobRunner.runLoop` and
    /// `KitArchiveObserver` (spec 0050 S13.1, .spec-md §10.2).
    [<ReferenceEquality>]
    type SourceSnapshotObserver =
        {
            reportSnapshotProgress : SourceSnapshotProgress -> unit
            cancellation : CancellationToken
        }

        /// Tells nobody and is never cancelled (spec 0050 S13.1).
        static member unobserved : SourceSnapshotObserver =
            {
                reportSnapshotProgress = ignore
                cancellation = CancellationToken.None
            }

    /// The snapshot an update installed: the source's version and the files it wrote (spec 0050 S13.1).
    type SourceSnapshotInfo =
        {
            version : MaterialSourceVersion
            files : SourceSnapshotFileCount
        }

    /// How a snapshot update ended (spec 0050 S13.1).
    type SourceSnapshotOutcome =
        /// The source's current snapshot is now the held one (spec 0050 S13.1).
        | SourceSnapshotUpdated of snapshot : SourceSnapshotInfo
        /// The update stopped before its next file and left the held snapshot as it was (spec 0050 S13.1).
        | SourceSnapshotCancelled

    /// One adapter's seam to its online source; it does IO only (spec 0050 S13.1; 001 §D, §P).
    /// Storage's `createRefractiveIndexInfo` is the refractiveindex.info adapter, `createInMemory` the double.
    [<ReferenceEquality>]
    type MaterialSourceProxy =
        {
            /// The source the adapter reads: data, not IO (spec 0050 S13.1).
            sourceName : MaterialSourceName
            /// The held snapshot's version, None before the first update (spec 0050 S13.1).
            snapshotVersion : unit -> Result<MaterialSourceVersion option, MaterialSourceError>
            /// The held snapshot's catalog through the query's `filter`; SourceSnapshotMissingErr without a snapshot (spec 0050 S13.1).
            searchRecords : MaterialSourceQuery -> Result<MaterialSourceHit list, MaterialSourceError>
            /// One listed record, parsed, under the held version; SourceSnapshotMissingErr without a snapshot, SourceRecordMissingErr unlisted (spec 0050 S13.1).
            fetchRecord : MaterialSourceRecordId -> Result<SourceMaterialRecord, MaterialSourceError>
            /// Replaces the held snapshot by the source's current one, reporting and reading the cancellation per file; a cancelled or failed update keeps the held snapshot (spec 0050 S13.1).
            updateSnapshot : SourceSnapshotObserver -> Result<SourceSnapshotOutcome, MaterialSourceError>
        }

    /// One record as a test publishes it to the double (spec 0050 S13.1).
    type InMemorySourceRecord =
        {
            hit : MaterialSourceHit
            recordHash : ContentHash
            citation : EntryCitation
            material : MaterialEntry
        }

    /// The double's source: the held snapshot and the one its next update installs. Its state is mutated only inside its
    /// members and `createInMemory`'s closures, as `InMemoryArchiveDisk` keeps its own (spec 0050 S13.1).
    [<Sealed>]
    type InMemoryMaterialSource private (name : MaterialSourceName, licence : EntryLicence) =
        let sourceSync = obj ()
        let mutable heldOpt : (MaterialSourceVersion * InMemorySourceRecord list) option = None
        let mutable publishedOpt : (MaterialSourceVersion * InMemorySourceRecord list) option = None

        /// A source that holds no snapshot and publishes none (spec 0050 S13.1).
        static member create (name : MaterialSourceName) (licence : EntryLicence) : InMemoryMaterialSource =
            InMemoryMaterialSource (name, licence)

        /// Sets what the next update installs (spec 0050 S13.1).
        member _.publish (version : MaterialSourceVersion) (records : InMemorySourceRecord list) : unit =
            lock sourceSync (fun () -> publishedOpt <- Some (version, records))

        member internal _.sourceName : MaterialSourceName = name

        member internal _.sourceLicence : EntryLicence = licence

        member internal _.heldSnapshotOpt : (MaterialSourceVersion * InMemorySourceRecord list) option =
            lock sourceSync (fun () -> heldOpt)

        member internal _.publishedSnapshotOpt : (MaterialSourceVersion * InMemorySourceRecord list) option =
            lock sourceSync (fun () -> publishedOpt)

        member internal _.holdSnapshot (version : MaterialSourceVersion) (records : InMemorySourceRecord list) : unit =
            lock sourceSync (fun () -> heldOpt <- Some (version, records))

    type MaterialSourceProxy with

        /// The double: the five fields over `source`, so every proxy over one source shares its snapshot (spec 0050 S13.1).
        static member createInMemory (source : InMemoryMaterialSource) : MaterialSourceProxy =
            let snapshotMissing = SourceSnapshotMissingErr $"'{source.sourceName.value}' has not been updated"

            let snapshotVersion () : Result<MaterialSourceVersion option, MaterialSourceError> =
                source.heldSnapshotOpt |> Option.map fst |> Ok

            let searchRecords (query : MaterialSourceQuery) : Result<MaterialSourceHit list, MaterialSourceError> =
                match source.heldSnapshotOpt with
                | Some (_, records) -> records |> List.map _.hit |> query.filter |> Ok
                | None -> Error snapshotMissing

            let fetchRecord (record : MaterialSourceRecordId) : Result<SourceMaterialRecord, MaterialSourceError> =
                match source.heldSnapshotOpt with
                | Some (version, records) ->
                    match records |> List.tryFind (fun held -> held.hit.record = record) with
                    | Some held ->
                        let pull : SourcePullInfo =
                            {
                                source = source.sourceName
                                record = record
                                recordHash = held.recordHash
                                sourceVersion = version
                                licence = source.sourceLicence
                                citation = held.citation
                            }
                        let fetched : SourceMaterialRecord =
                            {
                                pull = pull
                                material = { held.material with name = held.hit.name }
                            }
                        Ok fetched
                    | None ->
                        Error (SourceRecordMissingErr $"'{source.sourceName.value}' {version.value} does not list '{record.value}'")
                | None -> Error snapshotMissing

            let updateSnapshot (observer : SourceSnapshotObserver) : Result<SourceSnapshotOutcome, MaterialSourceError> =
                match source.publishedSnapshotOpt with
                | None -> Error (SourceDownloadErr $"'{source.sourceName.value}' publishes no snapshot")
                | Some (version, records) ->
                    let filesTotal = records |> List.length |> SourceSnapshotFileCount

                    let rec installFrom (pending : InMemorySourceRecord list) (written : InMemorySourceRecord list) : SourceSnapshotOutcome =
                        match pending with
                        | [] ->
                            source.holdSnapshot version records
                            SourceSnapshotUpdated { version = version; files = filesTotal }
                        | record :: rest ->
                            if observer.cancellation.IsCancellationRequested then
                                SourceSnapshotCancelled
                            else
                                let writtenNow = record :: written
                                observer.reportSnapshotProgress
                                    {
                                        filesDone = writtenNow |> List.length |> SourceSnapshotFileCount
                                        filesTotal = filesTotal
                                    }
                                installFrom rest writtenNow

                    installFrom records [] |> Ok

            {
                sourceName = source.sourceName
                snapshotVersion = snapshotVersion
                searchRecords = searchRecords
                fetchRecord = fetchRecord
                updateSnapshot = updateSnapshot
            }
