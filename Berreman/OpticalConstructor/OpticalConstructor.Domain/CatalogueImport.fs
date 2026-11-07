namespace OpticalConstructor.Domain

open System
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles

/// The library-import seam, in-memory stores and pure per-entry decision table (spec 0050 §3.4; S3.1, 001 §B).
/// Identifies every row before confirmed choices write through the version seam.
module CatalogueImport =

    /// Import read, validation, sealing or write failures with diagnostic payloads (spec 0050 §3.4; S3.1).
    type CatalogueImportError =
        | ImportRejectedErr of reason : string
        | ImportStoreUnavailableErr of reason : string
        | UndecodableEntryErr of LibraryEntryError
        | CatalogueUnreadableErr of CatalogueError
        | CatalogueAlreadyRegisteredErr of reason : string
        | InvalidImportChoiceErr of reason : string
        | DanglingReferenceErr of reason : string
        | EntryHashingErr of reason : string
        | DroppedFileRefusedErr of reason : string
        | UnresolvedReferenceErr of reason : string

        member this.describe : string =
            match this with
            | ImportStoreUnavailableErr reason -> $"the import store is unavailable: {reason}"
            | ImportRejectedErr reason -> $"a catalogue entry was rejected: {reason}"
            | UndecodableEntryErr error -> $"a catalogue entry could not be decoded: {error.describe}"
            | CatalogueUnreadableErr error -> $"the catalogue could not be read: {error.describe}"
            | CatalogueAlreadyRegisteredErr reason -> $"the catalogue is already registered: {reason}"
            | InvalidImportChoiceErr reason -> $"an import choice is invalid: {reason}"
            | DanglingReferenceErr reason -> $"an import would leave a dangling reference: {reason}"
            | EntryHashingErr reason -> $"a catalogue entry could not be hashed: {reason}"
            | DroppedFileRefusedErr reason -> $"a dropped file was refused: {reason}"
            | UnresolvedReferenceErr reason -> $"a dropped entry has an unresolved reference: {reason}"

    /// One version and content hash held by the library (spec 0050 001 §B).
    type HeldEntryVersion =
        {
            version : VersionNumber
            contentHash : ContentHash
        }

    /// An id's latest metadata and every held version in ascending order (spec 0050 001 §B).
    type HeldEntry =
        {
            id : LibraryEntryId
            kind : LibraryEntryKind
            name : EntryName
            latest : VersionNumber
            lifecycle : EntryLifecycle
            provenance : EntryProvenance
            versions : HeldEntryVersion list
        }

    /// The origin of incoming library entries (spec 0050 001 §B).
    type ImportOrigin =
        /// The catalogue's identity and display name (spec 0050 001 §B).
        | CatalogueImportOrigin of id : CatalogueId * name : CatalogueName
        /// An entry file dropped onto Library or Materials, held as FileName.leafName (spec 0050 001 §H).
        | DroppedFileOrigin of file : FileName

        member this.describe : string =
            match this with
            | CatalogueImportOrigin (_, name) -> name.value
            | DroppedFileOrigin file -> file.value

    /// Whether adding a version moves the library's latest pointer (spec 0050 001 §B).
    type LatestMove =
        /// Move latest to the newer protected built-in version from its own catalogue (spec 0050 001 §B).
        | MoveLatestVersion
        /// Preserve the library's current latest version (spec 0050 001 §B).
        | KeepLatestVersion

    /// An action offered for explicit confirmation on one import row (spec 0050 001 §B).
    type ImportAction =
        /// Add the incoming identity, retaining its own name (spec 0050 001 §B).
        | AddIncomingEntry
        /// Reuse the identical version already held (spec 0050 001 §B).
        | ReuseHeldEntry
        /// Add an unheld version with the stated latest-pointer policy (spec 0050 001 §B).
        | AddIncomingVersion of LatestMove
        /// Preserve divergent content under a new identity with fork provenance (spec 0050 001 §B).
        | ForkIncomingEntry
        /// Resolve incoming references to the selected held identity (spec 0050 001 §B).
        | RebindToHeldEntry of LibraryEntryId
        /// Leave the incoming entry unimported (spec 0050 001 §B).
        | SkipIncomingEntry
        /// Mark an unlisted entry inactive while keeping it resolvable (spec 0050 001 §B).
        | RetireHeldEntry

    /// The six per-entry decision-table rows in specification order (spec 0050 001 §B).
    type ImportStatus =
        /// The incoming identity is new to the library (spec 0050 001 §B).
        | EntryIdNotHeld of incoming : LibraryEntryEnvelope
        /// The library holds the same identity, version and hash (spec 0050 001 §B).
        | SameContentHash of incoming : LibraryEntryEnvelope * held : HeldEntry
        /// The identity is held but the incoming version is not (spec 0050 001 §B).
        | VersionNotHeld of incoming : LibraryEntryEnvelope * held : HeldEntry
        /// The held kind or same-version hash conflicts with the incoming entry (spec 0050 001 §B).
        | DivergentContent of incoming : LibraryEntryEnvelope * held : HeldEntry
        /// A new identity shares its kind and name with held entries (spec 0050 001 §B).
        | NameCollision of incoming : LibraryEntryEnvelope * sameName : HeldEntry list
        /// A held entry is no longer listed by its catalogue (spec 0050 001 §B).
        | EntryUnlisted of held : HeldEntry

    /// One import row with its preselected default and offered alternatives (spec 0050 001 §B).
    type ImportDecision =
        {
            origin : ImportOrigin
            status : ImportStatus
            defaultAction : ImportAction
            alternatives : ImportAction list
        }

    /// A confirmed row and the selected action (spec 0050 001 §B).
    type ImportChoice =
        {
            decision : ImportDecision
            action : ImportAction
        }

    /// Every import row awaiting explicit confirmation, in input order (spec 0050 001 §B).
    type ImportSplit =
        {
            awaiting : ImportDecision list
        }

    type ImportDecision with

        /// A dropped file's unheld version preserves the library's latest pointer (spec 0050 001 §B, §H).
        static member decide (origin : ImportOrigin) (incoming : LibraryEntryEnvelope) (heldOpt : HeldEntry option) (sameName : HeldEntry list) : ImportDecision =
            let status, defaultAction, alternatives =
                match heldOpt with
                | Some held when held.kind <> incoming.content.schema.kind ->
                    DivergentContent (incoming, held), ForkIncomingEntry, [ SkipIncomingEntry ]
                | Some held ->
                    match held.versions |> List.tryFind (fun version -> version.version = incoming.content.version) with
                    | Some version when version.contentHash = incoming.contentHash ->
                        SameContentHash (incoming, held), ReuseHeldEntry, []
                    | Some _ ->
                        DivergentContent (incoming, held), ForkIncomingEntry, [ SkipIncomingEntry ]
                    | None ->
                        let move =
                            match incoming.content.provenance, origin with
                            | BuiltInProvenance catalogueId, CatalogueImportOrigin (originId, _)
                                when catalogueId = originId && incoming.content.version > held.latest -> MoveLatestVersion
                            | _ -> KeepLatestVersion
                        VersionNotHeld (incoming, held), AddIncomingVersion move, [ SkipIncomingEntry ]
                | None ->
                    match sameName with
                    | [] -> EntryIdNotHeld incoming, AddIncomingEntry, [ SkipIncomingEntry ]
                    | _ ->
                        let alternatives = (sameName |> List.map (fun held -> RebindToHeldEntry held.id)) @ [ SkipIncomingEntry ]
                        NameCollision (incoming, sameName), AddIncomingEntry, alternatives

            {
                origin = origin
                status = status
                defaultAction = defaultAction
                alternatives = alternatives
            }

        static member unlisted (origin : ImportOrigin) (held : HeldEntry) : ImportDecision =
            {
                origin = origin
                status = EntryUnlisted held
                defaultAction = RetireHeldEntry
                alternatives = []
            }

        member this.entryId : LibraryEntryId =
            match this.status with
            | EntryIdNotHeld incoming | SameContentHash (incoming, _) | VersionNotHeld (incoming, _)
            | DivergentContent (incoming, _) | NameCollision (incoming, _) -> incoming.content.id
            | EntryUnlisted held -> held.id

        member this.kind : LibraryEntryKind =
            match this.status with
            | EntryIdNotHeld incoming | SameContentHash (incoming, _) | VersionNotHeld (incoming, _)
            | DivergentContent (incoming, _) | NameCollision (incoming, _) -> incoming.content.schema.kind
            | EntryUnlisted held -> held.kind

        member this.incomingOpt : LibraryEntryEnvelope option =
            match this.status with
            | EntryIdNotHeld incoming | SameContentHash (incoming, _) | VersionNotHeld (incoming, _)
            | DivergentContent (incoming, _) | NameCollision (incoming, _) -> Some incoming
            | EntryUnlisted _ -> None

        member this.byDefault : ImportChoice =
            {
                decision = this
                action = this.defaultAction
            }

        member this.rowName : EntryName =
            match this.status with
            | NameCollision (incoming, _) ->
                EntryName $"{incoming.content.name.value} — from {this.origin.describe}"
            | EntryIdNotHeld incoming | SameContentHash (incoming, _) | VersionNotHeld (incoming, _)
            | DivergentContent (incoming, _) -> incoming.content.name
            | EntryUnlisted held -> held.name

        static member split (decisions : ImportDecision list) : ImportSplit =
            {
                awaiting = decisions
            }

    type EntryVersionPut =
        {
            envelope : LibraryEntryEnvelope
            latest : LatestMove
        }

    /// The UTC time at which a catalogue was imported (spec 0050 S3.1).
    type CatalogueImportTime =
        | CatalogueImportTime of DateTime

        member this.value : DateTime = let (CatalogueImportTime time) = this in time

    /// The number of entries absorbed from a catalogue (spec 0050 S3.1).
    type CatalogueEntryCount =
        | CatalogueEntryCount of int

        member this.value : int = let (CatalogueEntryCount count) = this in count

    /// A catalogue's registered folder and last completed import (spec 0050 S3.1, 001 §B, §C).
    type CatalogueImportLogEntry =
        {
            catalogueId : CatalogueId
            folder : FolderName
            manifestHash : ContentHash
            importedAt : CatalogueImportTime
            entryCount : CatalogueEntryCount
        }

    /// Every row of a changed catalogue, with metadata to log only after confirmation (spec 0050 §3.7).
    type PendingCatalogueImport =
        {
            entry : CatalogueImportLogEntry
            decisions : ImportDecision list
        }

    type FolderImportResult =
        | CatalogueUnchanged of CatalogueImportLogEntry
        | CatalogueImported of CatalogueImportLogEntry
        | CatalogueAwaitsChoices of PendingCatalogueImport

    type FolderImportReport =
        {
            folder : FolderName
            outcome : Result<FolderImportResult, CatalogueImportError>
        }

    /// Startup results retain folder order and individual failures (spec 0050 S3.1).
    type StartupImportReport =
        {
            folders : FolderImportReport list
        }

        member this.pending : PendingCatalogueImport list =
            this.folders |> List.choose (fun report ->
                match report.outcome with
                | Ok (CatalogueAwaitsChoices pending) -> Some pending
                | Ok (CatalogueUnchanged _ | CatalogueImported _) | Error _ -> None)

    /// Reads held versions and puts active, unpinned envelopes without overwriting (spec 0050 S3.1).
    /// A first put or MoveLatestVersion sets latest; duplicate versions and kind conflicts are refused.
    /// Retires held entries and maintains the catalogue import log; equality is by reference.
    [<ReferenceEquality>]
    type CatalogueImportProxy =
        {
            tryHeldEntry : LibraryEntryId -> Result<HeldEntry option, CatalogueImportError>
            /// Returns one held version's envelope regardless of lifecycle, or None when not held (spec 0050 S3.2).
            tryHeldEnvelope : LibraryEntryId -> VersionNumber -> Result<LibraryEntryEnvelope option, CatalogueImportError>
            heldEntriesNamed : LibraryEntryKind -> EntryName -> Result<HeldEntry list, CatalogueImportError>
            heldEntriesOf : CatalogueId -> Result<HeldEntry list, CatalogueImportError>
            putEntryVersion : EntryVersionPut -> Result<unit, CatalogueImportError>
            retireEntry : LibraryEntryId -> Result<unit, CatalogueImportError>
            loggedImports : unit -> Result<CatalogueImportLogEntry list, CatalogueImportError>
            logImport : CatalogueImportLogEntry -> Result<unit, CatalogueImportError>
            forgetImport : CatalogueId -> Result<unit, CatalogueImportError>
        }

        /// Lists registered extra folders in log order, excluding the shipped catalogue (spec 0050 S3.1).
        member this.registeredFolders () : Result<CatalogueImportLogEntry list, CatalogueImportError> =
            this.loggedImports ()
            |> Result.map (List.filter (fun entry -> entry.catalogueId <> CatalogueIds.builtInCatalogue))

        member this.decideEntry (origin : ImportOrigin) (incoming : LibraryEntryEnvelope) : Result<ImportDecision, CatalogueImportError> =
            this.tryHeldEntry incoming.content.id
            |> Result.bind (function
                | Some held -> Ok (ImportDecision.decide origin incoming (Some held) [])
                | None ->
                    this.heldEntriesNamed incoming.content.schema.kind incoming.content.name
                    |> Result.map (ImportDecision.decide origin incoming None))

        member this.decideCatalogue (catalogue : CatalogueData) : Result<ImportDecision list, CatalogueImportError> =
            let origin = CatalogueImportOrigin (catalogue.manifest.id, catalogue.manifest.name)
            catalogue.envelopes
            |> List.fold (fun result envelope ->
                result |> Result.bind (fun rows -> this.decideEntry origin envelope |> Result.map (fun row -> row :: rows))) (Ok [])
            |> Result.bind (fun reversed ->
                this.heldEntriesOf catalogue.manifest.id
                |> Result.map (fun heldEntries ->
                    let listed = catalogue.manifest.entries |> List.map _.id |> Set.ofList
                    let unlisted =
                        heldEntries
                        |> List.choose (fun held ->
                            match held.lifecycle with
                            | ActiveEntry when not (Set.contains held.id listed) -> Some (ImportDecision.unlisted origin held)
                            | _ -> None)
                    List.rev reversed @ unlisted))

    type EntryRebinding =
        {
            incoming : LibraryEntryId
            held : LibraryEntryId
        }

    type EntryFork =
        {
            original : LibraryEntryId
            fork : LibraryEntryId
        }

    type ImportOutcome =
        {
            rebindings : EntryRebinding list
            forks : EntryFork list
        }

    [<ReferenceEquality>]
    type CatalogueImportContext =
        {
            proxy : CatalogueImportProxy
            sealEntry : LibraryEntryContent -> Result<LibraryEntryEnvelope, CatalogueImportError>
            newEntryId : unit -> LibraryEntryId
            hashManifest : CatalogueManifest -> Result<ContentHash, CatalogueImportError>
            now : unit -> CatalogueImportTime
            openCatalogue : FolderName -> CatalogueProxy
            /// Reads one entry file, retaining its file name in a refusal (spec 0050 S3.2).
            readEntryFile : LibraryEntryKind -> FileName -> Result<LibraryEntryEnvelope, CatalogueImportError>
        }

        /// Validates the complete drop and returns decisions in file order without writing (spec 0050 001 §B, §H; S3.2).
        member this.decideDroppedFiles (files : FileName list) : Result<ImportDecision list, CatalogueImportError> =
            let readFile (file : FileName) : Result<FileName * LibraryEntryEnvelope, CatalogueImportError> =
                match LibraryEntryKind.tryOfFileName file with
                | None -> Error (DroppedFileRefusedErr $"{file.value}: not a library entry file")
                | Some kind ->
                    this.readEntryFile kind file
                    |> Result.map (fun envelope -> file, envelope)
                    |> Result.mapError (function
                        | DroppedFileRefusedErr _ as error -> error
                        | error -> DroppedFileRefusedErr $"{file.value}: {error.describe}")

            let readResults = files |> List.map readFile
            let refusals =
                readResults |> List.choose (function
                    | Error (DroppedFileRefusedErr reason) -> Some reason
                    | Error error -> Some error.describe
                    | Ok _ -> None)

            match refusals with
            | _ :: _ -> Error (DroppedFileRefusedErr (String.concat "\n" refusals))
            | [] ->
                let entries = readResults |> List.choose (function Ok entry -> Some entry | Error _ -> None)
                entries
                |> List.fold (fun result (file, envelope) ->
                    result |> Result.bind (fun ((seen : Map<LibraryEntryId * VersionNumber, FileName * LibraryEntryEnvelope>), unique) ->
                        let key = envelope.content.id, envelope.content.version
                        match Map.tryFind key seen with
                        | Some (_, first) when first.contentHash = envelope.contentHash -> Ok (seen, unique)
                        | Some (firstFile, _) ->
                            Error (DroppedFileRefusedErr $"{firstFile.value} and {file.value}: different content for entry {envelope.content.describe}")
                        | None -> Ok (Map.add key (file, envelope) seen, (file, envelope) :: unique))) (Ok (Map.empty, []))
                |> Result.bind (fun (_, reversed) ->
                    let unique = List.rev reversed
                    let droppedIds = unique |> List.map (fun (_, envelope) -> envelope.content.id) |> Set.ofList
                    let validateReferences ((file, envelope) : FileName * LibraryEntryEnvelope) : Result<unit, CatalogueImportError> =
                        envelope.content.referencedEntryIds
                        |> Set.fold (fun result referenced ->
                            result |> Result.bind (fun () ->
                                if Set.contains referenced droppedIds then Ok ()
                                else
                                    this.proxy.tryHeldEntry referenced
                                    |> Result.bind (function
                                        | Some _ -> Ok ()
                                        | None -> Error (UnresolvedReferenceErr $"entry {envelope.content.describe} in {file.value} references missing entry {referenced.value}")))) (Ok ())

                    unique
                    |> List.fold (fun result entry -> result |> Result.bind (fun () -> validateReferences entry)) (Ok ())
                    |> Result.bind (fun () ->
                        unique
                        |> List.fold (fun result (file, envelope) ->
                            result |> Result.bind (fun rows ->
                                let origin = DroppedFileOrigin file.leafName
                                this.proxy.decideEntry origin envelope |> Result.map (fun row -> row :: rows))) (Ok [])
                        |> Result.map List.rev))

        /// Validates every confirmed choice before writing in reference order, retaining writes before a failure (spec 0050 S3.1).
        member this.apply (choices : ImportChoice list) : Result<ImportOutcome, CatalogueImportError> =
            let validateAction (choice : ImportChoice) : Result<unit, CatalogueImportError> =
                if choice.action = choice.decision.defaultAction || List.contains choice.action choice.decision.alternatives then Ok ()
                else Error (InvalidImportChoiceErr $"entry '{choice.decision.rowName.value}' ({choice.decision.entryId.value}) does not offer the chosen action")

            let omitted =
                choices |> List.choose (fun choice ->
                    match choice.action with
                    | SkipIncomingEntry | RebindToHeldEntry _ -> Some choice.decision.entryId
                    | _ -> None) |> Set.ofList

            let validateReferences (choice : ImportChoice) : Result<unit, CatalogueImportError> =
                match choice.action, choice.decision.incomingOpt with
                | (AddIncomingEntry | AddIncomingVersion _ | ForkIncomingEntry), Some incoming ->
                    match Set.intersect incoming.content.referencedEntryIds omitted |> Set.toList with
                    | referenced :: _ -> Error (DanglingReferenceErr $"entry '{incoming.content.name.value}' ({incoming.content.id.value}) references skipped or rebound entry {referenced.value}")
                    | [] -> Ok ()
                | _ -> Ok ()

            let applyChoice (outcome : ImportOutcome) (choice : ImportChoice) : Result<ImportOutcome, CatalogueImportError> =
                match choice.action with
                | ReuseHeldEntry | SkipIncomingEntry -> Ok outcome
                | RebindToHeldEntry held ->
                    let binding =
                        {
                            incoming = choice.decision.entryId
                            held = held
                        }
                    Ok
                        {
                            outcome with
                                rebindings = binding :: outcome.rebindings
                        }
                | RetireHeldEntry -> this.proxy.retireEntry choice.decision.entryId |> Result.map (fun () -> outcome)
                | AddIncomingEntry | AddIncomingVersion _ | ForkIncomingEntry ->
                    match choice.decision.incomingOpt with
                    | None -> Error (InvalidImportChoiceErr $"entry {choice.decision.entryId.value} has no incoming envelope")
                    | Some incoming ->
                        match choice.action with
                        | ForkIncomingEntry ->
                            let forkContent =
                                {
                                    incoming.content with
                                        id = this.newEntryId ()
                                        version = VersionNumber.first
                                        provenance = ForkProvenance
                                            {
                                                originalId = incoming.content.id
                                                originalVersion = incoming.content.version
                                                originalProvenance = incoming.content.provenance
                                            }
                                }
                            this.sealEntry forkContent
                            |> Result.bind (fun envelope ->
                                let put =
                                    {
                                        envelope = envelope
                                        latest = MoveLatestVersion
                                    }
                                this.proxy.putEntryVersion put)
                            |> Result.map (fun () ->
                                let fork =
                                    {
                                        original = incoming.content.id
                                        fork = forkContent.id
                                    }
                                {
                                    outcome with
                                        forks = fork :: outcome.forks
                                })
                        | action ->
                            let put =
                                {
                                    envelope = incoming
                                    latest = match action with AddIncomingVersion move -> move | _ -> MoveLatestVersion
                                }
                            this.proxy.putEntryVersion put |> Result.map (fun () -> outcome)

            choices
            |> List.fold (fun result choice -> result |> Result.bind (fun () -> validateAction choice)) (Ok ())
            |> Result.bind (fun () -> choices |> List.fold (fun result choice -> result |> Result.bind (fun () -> validateReferences choice)) (Ok ()))
            |> Result.bind (fun () ->
                let ordered =
                    [ CategoryEntryKind; MaterialEntryKind; SampleEntryKind; SourceEntryKind; PolarizerEntryKind; DetectorEntryKind ]
                    |> List.collect (fun kind -> choices |> List.filter (fun choice -> choice.decision.kind = kind && choice.action <> RetireHeldEntry))
                let retired = choices |> List.filter (fun choice -> choice.action = RetireHeldEntry)
                let initial =
                    {
                        rebindings = []
                        forks = []
                    }
                ordered @ retired
                |> List.fold (fun result choice -> result |> Result.bind (fun outcome -> applyChoice outcome choice)) (Ok initial)
                |> Result.map (fun outcome ->
                    {
                        rebindings = List.rev outcome.rebindings
                        forks = List.rev outcome.forks
                    }))

        /// Requires exactly one choice per pending row, applies its offered action and logs only after success (spec 0050 S3.1).
        member this.settle (pending : PendingCatalogueImport) (choices : ImportChoice list) : Result<ImportOutcome, CatalogueImportError> =
            let pendingIds = pending.decisions |> List.map _.entryId |> Set.ofList
            let choicesById = choices |> List.groupBy (fun choice -> choice.decision.entryId) |> Map.ofList
            match choices |> List.tryFind (fun choice -> not (Set.contains choice.decision.entryId pendingIds)) with
            | Some choice -> Error (InvalidImportChoiceErr $"entry {choice.decision.entryId.value} is not pending for catalogue {pending.entry.catalogueId.value}")
            | None ->
                pending.decisions
                |> List.fold (fun result decision ->
                    result |> Result.bind (fun confirmed ->
                        match Map.tryFind decision.entryId choicesById with
                        | Some [ choice ] ->
                            let canonical =
                                {
                                    decision = decision
                                    action = choice.action
                                }
                            Ok (canonical :: confirmed)
                        | Some _ -> Error (InvalidImportChoiceErr $"entry {decision.entryId.value} has multiple choices for catalogue {pending.entry.catalogueId.value}")
                        | None -> Error (InvalidImportChoiceErr $"entry {decision.entryId.value} has no choice for catalogue {pending.entry.catalogueId.value}"))) (Ok [])
                |> Result.bind (List.rev >> this.apply)
                |> Result.bind (fun outcome ->
                    let entry =
                        {
                            pending.entry with
                                importedAt = this.now ()
                        }
                    this.proxy.logImport entry |> Result.map (fun () -> outcome))

        /// Returns every decision without writes; the caller must confirm the complete list before apply (spec 0050 §3.7).
        member this.importCatalogue (catalogue : CatalogueData) : Result<ImportDecision list, CatalogueImportError> =
            this.proxy.decideCatalogue catalogue

        member this.importFrom (catalogue : CatalogueProxy) : Result<ImportDecision list, CatalogueImportError> =
            catalogue.readCatalogue ()
            |> Result.mapError CatalogueUnreadableErr
            |> Result.bind this.importCatalogue

        member private this.inspectImportFolder (imports : CatalogueImportLogEntry list) (folder : FolderName) : Result<FolderImportResult, CatalogueImportError> =
            let catalogue = this.openCatalogue folder
            catalogue.readManifest ()
            |> Result.mapError CatalogueUnreadableErr
            |> Result.bind (fun manifest ->
                this.hashManifest manifest
                |> Result.bind (fun hash ->
                    match imports |> List.tryFind (fun entry -> entry.catalogueId = manifest.id) with
                    | Some entry when entry.manifestHash = hash -> Ok (CatalogueUnchanged entry)
                    | _ ->
                        this.importFrom catalogue
                        |> Result.map (fun decisions ->
                            CatalogueAwaitsChoices
                                {
                                    entry =
                                        {
                                            catalogueId = manifest.id
                                            folder = folder
                                            manifestHash = hash
                                            importedAt = this.now ()
                                            entryCount = CatalogueEntryCount manifest.entries.Length
                                        }
                                    decisions = decisions
                                })))

        /// An unchanged manifest skips entry reads; otherwise every row awaits confirmation without writes (spec 0050 §3.7).
        member this.importFolder (folder : FolderName) : Result<FolderImportResult, CatalogueImportError> =
            this.proxy.loggedImports ()
            |> Result.bind (fun imports -> this.inspectImportFolder imports folder)

        /// Refuses reserved or registered identities; every new folder row awaits settlement without writes (spec 0050 S3.1).
        member this.registerFolder (folder : FolderName) : Result<FolderImportResult, CatalogueImportError> =
            (this.openCatalogue folder).readManifest ()
            |> Result.mapError CatalogueUnreadableErr
            |> Result.bind (fun manifest ->
                this.proxy.loggedImports ()
                |> Result.bind (fun imports ->
                    match imports |> List.tryFind (fun entry -> entry.catalogueId = manifest.id) with
                    | Some entry ->
                        Error (CatalogueAlreadyRegisteredErr $"folder '{folder.value}' contains catalogue '{manifest.name.value}' ({manifest.id.value}), already registered under '{entry.folder.value}'")
                    | None when manifest.id = CatalogueIds.builtInCatalogue ->
                        Error (CatalogueAlreadyRegisteredErr $"folder '{folder.value}' contains catalogue '{manifest.name.value}' ({manifest.id.value}), reserved for the shipped Built-in catalogue")
                    | None -> this.importFolder folder))

        /// Inspects the shipped folder first, then each logged non-built-in folder; folder failures remain local (spec 0050 S3.1).
        member this.importOnStart (builtInFolder : FolderName) : Result<StartupImportReport, CatalogueImportError> =
            this.proxy.loggedImports ()
            |> Result.map (fun imports ->
                let registered =
                    imports
                    |> List.filter (fun entry -> entry.catalogueId <> CatalogueIds.builtInCatalogue)
                    |> List.map _.folder
                {
                    folders = builtInFolder :: registered |> List.map (fun folder ->
                        {
                            folder = folder
                            outcome = this.inspectImportFolder imports folder
                        })
                })

    /// Empty stores bundled with the CatalogueImportProxy that fills them (spec 0050 §3.4).
    /// Read the imported catalogue through the four store proxies; equality is by reference.
    [<ReferenceEquality>]
    type InMemoryCatalogueStores =
        {
            proxy : CatalogueImportProxy
            categories : CategoryProxy
            materials : MaterialProxy
            samples : SampleProxy
            libraryEntries : LibraryProxy
        }

    type private ImportEntryState =
        {
            envelopes : LibraryEntryEnvelope list
            latestEnvelope : LibraryEntryEnvelope
            lifecycle : EntryLifecycle
        }

        member this.heldEntry : HeldEntry =
            {
                id = this.latestEnvelope.content.id
                kind = this.latestEnvelope.content.schema.kind
                name = this.latestEnvelope.content.name
                latest = this.latestEnvelope.content.version
                lifecycle = this.lifecycle
                provenance = this.latestEnvelope.content.provenance
                versions = this.envelopes |> List.map (fun envelope ->
                    {
                        version = envelope.content.version
                        contentHash = envelope.contentHash
                    })
            }

    type CatalogueImportProxy with

        /// Keeps import history in memory, writing each new id through its empty stores (spec 0050 S2.3; S3.1).
        /// Later versions and retirement belong to this seam; failed decodes or store writes leave its map unchanged.
        static member createInMemory () : InMemoryCatalogueStores =
            let samples = SampleProxy.createInMemory ()
            let materials = MaterialProxy.createInMemory (samplesReferencing samples)
            let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
            let presets = Library.createInMemory ()

            let categoryReason (e : CategoryError) : string =
                match e with
                | UnknownCategoryIdErr r | DuplicateCategoryIdErr r | CategoryStillReferencedErr r | BuiltInNotRemovableErr r | InvalidCategoryErr r | CategoryStoreUnavailableErr r -> r
            let materialReason (e : MaterialError) : string =
                match e with
                | UnknownMaterialIdErr r | DuplicateMaterialIdErr r | MaterialStillReferencedErr r | InvalidMaterialErr r | MaterialVersionPinnedErr r | MaterialStoreUnavailableErr r -> r
            let sampleReason (e : SampleError) : string =
                match e with
                | UnknownSampleIdErr r | DuplicateSampleIdErr r | InvalidSampleErr r | SampleVersionPinnedErr r | SampleStoreUnavailableErr r -> r
            let libraryReason (e : LibraryError) : string =
                match e with
                | UnknownEntryIdErr r | NoEntriesForKindErr r | LibraryUnavailableErr r | PresetRefusedErr r -> r

            let entries = ref (Map.empty<LibraryEntryId, ImportEntryState>)
            let imports = ref (Map.empty<CatalogueId, CatalogueImportLogEntry>)

            let pushDecoded (save : 'Entry -> Result<unit, CatalogueImportError>) (decoded : Result<'Entry, LibraryEntryError>) : Result<unit, CatalogueImportError> =
                decoded |> Result.mapError UndecodableEntryErr |> Result.bind save

            let pushFirst (envelope : LibraryEntryEnvelope) : Result<unit, CatalogueImportError> =
                let content = envelope.content
                let savePreset (entry : LibraryEntry) : Result<unit, CatalogueImportError> =
                    presets.addEntry entry |> Result.mapError (libraryReason >> ImportRejectedErr)
                match content.schema.kind with
                | CategoryEntryKind -> content.toMaterialCategory |> pushDecoded (fun entry -> categories.addCategory entry |> Result.mapError (categoryReason >> ImportRejectedErr))
                | MaterialEntryKind -> content.toMaterialEntry |> pushDecoded (fun entry -> materials.saveMaterial entry |> Result.mapError (materialReason >> ImportRejectedErr))
                | SampleEntryKind -> content.toSample |> pushDecoded (fun entry -> samples.saveSample entry |> Result.mapError (sampleReason >> ImportRejectedErr))
                | SourceEntryKind -> content.toSourcePreset |> Result.map SourceItem |> pushDecoded savePreset
                | PolarizerEntryKind -> content.toPolarizerPreset |> Result.map PolarizerItem |> pushDecoded savePreset
                | DetectorEntryKind -> content.toDetectorPreset |> Result.map DetectorItem |> pushDecoded savePreset

            let proxy : CatalogueImportProxy =
                {
                    tryHeldEntry = fun id -> Ok (entries.Value |> Map.tryFind id |> Option.map _.heldEntry)
                    tryHeldEnvelope = fun id version ->
                        entries.Value |> Map.tryFind id
                        |> Option.bind (fun state -> state.envelopes |> List.tryFind (fun envelope -> envelope.content.version = version))
                        |> Ok
                    heldEntriesNamed = fun kind name ->
                        entries.Value |> Map.toList |> List.map (fun (_, state) -> state.heldEntry)
                        |> List.filter (fun held -> held.kind = kind && held.name = name) |> Ok
                    heldEntriesOf = fun catalogueId ->
                        entries.Value |> Map.toList |> List.map (fun (_, state) -> state.heldEntry)
                        |> List.filter (fun held -> held.provenance = BuiltInProvenance catalogueId) |> Ok
                    putEntryVersion = fun put ->
                        let envelope = put.envelope
                        let content = envelope.content
                        let next =
                            match entries.Value |> Map.tryFind content.id with
                            | Some state when state.latestEnvelope.content.schema.kind <> content.schema.kind ->
                                Error (ImportRejectedErr $"entry '{content.name.value}' ({content.id.value}) is held as {state.latestEnvelope.content.schema.kind.extension.value}, not {content.schema.kind.extension.value}")
                            | Some state when state.envelopes |> List.exists (fun held -> held.content.version = content.version) ->
                                Error (ImportRejectedErr $"entry '{content.name.value}' ({content.id.value}) already holds version {content.version.value}")
                            | Some state ->
                                let latestEnvelope, lifecycle =
                                    match put.latest with
                                    | MoveLatestVersion -> envelope, ActiveEntry
                                    | KeepLatestVersion -> state.latestEnvelope, state.lifecycle
                                Ok
                                    {
                                        envelopes = envelope :: state.envelopes |> List.sortBy _.content.version
                                        latestEnvelope = latestEnvelope
                                        lifecycle = lifecycle
                                    }
                            | None ->
                                pushFirst envelope |> Result.map (fun () ->
                                    {
                                        envelopes = [ envelope ]
                                        latestEnvelope = envelope
                                        lifecycle = ActiveEntry
                                    })
                        next |> Result.map (fun state -> entries.Value <- Map.add content.id state entries.Value)
                    retireEntry = fun id ->
                        match entries.Value |> Map.tryFind id with
                        | None -> Error (ImportRejectedErr $"entry {id.value} is not held and cannot be retired")
                        | Some state ->
                            let retired =
                                {
                                    state with
                                        lifecycle = InactiveEntry
                                }
                            entries.Value <- Map.add id retired entries.Value
                            Ok ()
                    loggedImports = fun () ->
                        imports.Value |> Map.toList |> List.map snd
                        |> List.sortWith (fun first second -> StringComparer.Ordinal.Compare(first.folder.value, second.folder.value))
                        |> Ok
                    logImport = fun entry ->
                        imports.Value <- Map.add entry.catalogueId entry imports.Value
                        Ok ()
                    forgetImport = fun id ->
                        imports.Value <- Map.remove id imports.Value
                        Ok ()
                }

            {
                proxy = proxy
                categories = categories
                materials = materials
                samples = samples
                libraryEntries = presets.proxy
            }
