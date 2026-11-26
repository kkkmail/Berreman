namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Domain.CatalogueImport

/// What a pull writes: a pulled record becomes a user material version under its pull provenance, and an update mints
/// the next version only where a record changed, never mutating one (spec 0050 S13.1; 001 §C, §D).
module MaterialSourcePulls =

    /// Why a pull or an update stopped; each case wraps the refusing seam's error (spec 0050 S13.1).
    type MaterialSourcePullError =
        /// The source refused to fetch the record or to update its snapshot (spec 0050 S13.1).
        | PullSourceErr of error : MaterialSourceError
        /// The library refused to read or to store a pulled version (spec 0050 S13.1).
        | PullImportErr of error : CatalogueImportError
        /// The entry's latest version could not be read back as a material (spec 0050 S13.1).
        | PullEntryErr of error : LibraryEntryError

        member this.describe : string =
            match this with
            | PullSourceErr error -> $"the source refused: {error.describe}"
            | PullImportErr error -> $"the library refused the pulled version: {error.describe}"
            | PullEntryErr error -> $"the pulled material could not be built: {error.describe}"

    /// What a pull or an update did for one pulled entry (spec 0050 S13.1).
    type SourcePullChange =
        /// The record became a new user material at version 1 (spec 0050 S13.1).
        | PullAddedEntry
        /// The record changed: its entry's next version holds the record's new physics (spec 0050 S13.1; 001 §C).
        | PullMintedVersion
        /// The record's hash equals its entry's newest pulled version's; nothing was written (spec 0050 S13.1).
        | PullFoundUnchanged
        /// The snapshot no longer lists the record; nothing was written (spec 0050 S13.1).
        | PullRecordAbsent

    /// One pulled entry's outcome: its record, its name and the version added, minted, or left as it was (spec 0050 S13.1).
    type SourcePullResult =
        {
            record : MaterialSourceRecordId
            name : EntryName
            version : LibraryVersionRef
            change : SourcePullChange
        }

    /// An installed snapshot and what refreshing the source's pulled entries over it did (spec 0050 S13.1).
    type SourceRefreshInfo =
        {
            snapshot : SourceSnapshotInfo
            pulls : SourcePullResult list
        }

    /// How a source's snapshot update ended (spec 0050 S13.1).
    type SourceRefresh =
        /// The source's snapshot was installed and its pulled entries refreshed over it (spec 0050 S13.1).
        | SourceRefreshed of SourceRefreshInfo
        /// The update was cancelled: the held snapshot and every entry stay as they were (spec 0050 S13.1).
        | SourceRefreshCancelled

    /// Seals the content and stores it as its entry's latest version, through the import's one write (spec 0050 S13.1).
    let private putPulledVersion (context : CatalogueImportContext) (content : LibraryEntryContent) : Result<LibraryVersionRef, MaterialSourcePullError> =
        context.sealEntry content
        |> Result.bind (fun envelope ->
            let put : EntryVersionPut =
                {
                    envelope = envelope
                    latest = MoveLatestVersion
                }
            context.proxy.putEntryVersion put |> Result.map (fun () -> envelope.libraryVersionRef))
        |> Result.mapError PullImportErr

    /// Each entry's newest row, in record order, with its held entry, leaving out a retired entry so a pull never revives
    /// it (spec 0050 S13.1); a row whose entry the library does not hold is refused.
    let private activePulls (context : CatalogueImportContext) (rows : PulledRecord list) : Result<(PulledRecord * HeldEntry) list, MaterialSourcePullError> =
        rows
        |> List.groupBy _.version.entryId
        |> List.map (fun (_, entryRows) -> entryRows |> List.maxBy _.version.version)
        |> PulledRecord.inRecordOrder
        |> List.fold (fun result row ->
            result |> Result.bind (fun pulls ->
                context.proxy.tryHeldEntry row.version.entryId
                |> Result.mapError PullImportErr
                |> Result.bind (function
                    | Some held ->
                        match held.lifecycle with
                        | Lifecycle.ActiveEntry -> Ok ((row, held) :: pulls)
                        | Lifecycle.InactiveEntry -> Ok pulls
                    | None ->
                        Error (PullImportErr (ImportRejectedErr $"record '{row.pull.record.value}' was pulled into entry {row.version.entryId.value}, which the library does not hold"))))) (Ok [])
        |> Result.map List.rev

    /// Re-fetches one entry's record: absent or unchanged writes nothing; a changed hash mints the next version of the
    /// entry's latest with the record's physics, every held version kept (spec 0050 S13.1; 001 §C).
    let private refreshPull (context : CatalogueImportContext) (source : MaterialSourceProxy) (row : PulledRecord) (held : HeldEntry) : Result<SourcePullResult, MaterialSourcePullError> =
        let resultAt (version : LibraryVersionRef) (name : EntryName) (change : SourcePullChange) : SourcePullResult =
            {
                record = row.pull.record
                name = name
                version = version
                change = change
            }
        match source.fetchRecord row.pull.record with
        | Error (SourceRecordMissingErr _) -> Ok (resultAt row.version held.name PullRecordAbsent)
        | Error error -> Error (PullSourceErr error)
        | Ok fetched when fetched.pull.recordHash = row.pull.recordHash -> Ok (resultAt row.version held.name PullFoundUnchanged)
        | Ok fetched ->
            context.proxy.tryLatestEnvelope row.version.entryId
            |> Result.mapError PullImportErr
            |> Result.bind (function
                | None -> Error (PullImportErr (ImportRejectedErr $"entry {row.version.entryId.value} holds no latest version to refresh"))
                | Some latest ->
                    latest.content.toMaterialEntry
                    |> Result.mapError PullEntryErr
                    |> Result.bind (fun material ->
                        let refreshed =
                            {
                                material with
                                    physics = fetched.material.physics
                            }
                        LibraryEntryContent.ofMaterialEntry latest.content.version.next (SourcePullProvenance fetched.pull) refreshed
                        |> putPulledVersion context
                        |> Result.map (fun version -> resultAt version refreshed.name PullMintedVersion)))

    /// Refreshes each pulled entry in order; the first error stops it, the writes before it staying (spec 0050 S13.1).
    let private refreshEach (context : CatalogueImportContext) (source : MaterialSourceProxy) (pulls : (PulledRecord * HeldEntry) list) : Result<SourcePullResult list, MaterialSourcePullError> =
        pulls
        |> List.fold (fun result (row, held) ->
            result |> Result.bind (fun results ->
                refreshPull context source row held |> Result.map (fun pulled -> pulled :: results))) (Ok [])
        |> Result.map List.rev

    type CatalogueImportContext with

        /// Adds an unheld record as a user material at version 1 under its pull provenance, else refreshes the entries it
        /// was pulled into as `refreshPulls` does; a record held only by retired entries is refused (spec 0050 S13.1).
        member this.pullRecord (source : MaterialSourceProxy) (record : MaterialSourceRecordId) : Result<SourcePullResult list, MaterialSourcePullError> =
            this.proxy.pulledRecordsOf source.sourceName
            |> Result.mapError PullImportErr
            |> Result.bind (fun rows ->
                match rows |> List.filter (fun row -> row.pull.record = record) with
                | [] ->
                    source.fetchRecord record
                    |> Result.mapError PullSourceErr
                    |> Result.bind (fun fetched ->
                        let content =
                            {
                                LibraryEntryContent.ofMaterialEntry VersionNumber.first (SourcePullProvenance fetched.pull) fetched.material with
                                    id = this.newEntryId ()
                            }
                        putPulledVersion this content
                        |> Result.map (fun version ->
                            let added : SourcePullResult =
                                {
                                    record = record
                                    name = content.name
                                    version = version
                                    change = PullAddedEntry
                                }
                            [ added ]))
                | recordRows ->
                    activePulls this recordRows
                    |> Result.bind (function
                        | [] ->
                            let retired = recordRows |> List.map (fun row -> $"{row.version.entryId.value}") |> List.distinct |> String.concat ", "
                            Error (PullImportErr (ImportRejectedErr $"record '{record.value}' was pulled into retired entry {retired}; a retired material is marked active in the Materials window, never revived by a pull"))
                        | active -> refreshEach this source active))

        /// Re-fetches the newest pulled row of every active entry pulled from the source, minting a version only where the
        /// record's hash changed; a retired entry is never refreshed (spec 0050 S13.1; 001 §C, §D).
        member this.refreshPulls (source : MaterialSourceProxy) : Result<SourcePullResult list, MaterialSourcePullError> =
            this.proxy.pulledRecordsOf source.sourceName
            |> Result.mapError PullImportErr
            |> Result.bind (activePulls this)
            |> Result.bind (refreshEach this source)

        /// Updates the source's snapshot, then refreshes its pulled entries over it; a cancelled update refreshes nothing
        /// (spec 0050 S13.1). It is the job Update snapshot runs.
        member this.updateSourceSnapshot (source : MaterialSourceProxy) (observer : SourceSnapshotObserver) : Result<SourceRefresh, MaterialSourcePullError> =
            source.updateSnapshot observer
            |> Result.mapError PullSourceErr
            |> Result.bind (function
                | SourceSnapshotCancelled -> Ok SourceRefreshCancelled
                | SourceSnapshotUpdated snapshot ->
                    this.refreshPulls source
                    |> Result.map (fun pulls ->
                        SourceRefreshed
                            {
                                snapshot = snapshot
                                pulls = pulls
                            }))
