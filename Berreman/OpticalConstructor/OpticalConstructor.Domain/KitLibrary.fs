namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary

/// Snapshots referenced versions and overlays the session's library with kit entries (spec 0050 S4.2).
module KitLibrary =

    /// A kit snapshot failure, preserving the storage/import error or missing reference (spec 0050 S4.2).
    type KitLibraryError =
        | KitLibraryKitErr of KitError
        | KitLibraryImportErr of CatalogueImportError
        | UnheldEntryErr of reason : string

        /// Describes the failed condition and its underlying diagnostic payload (spec 0050 S4.2).
        member this.describe : string =
            match this with
            | KitLibraryKitErr error -> $"the kit refused the library snapshot: {error.describe}"
            | KitLibraryImportErr error -> $"the library refused the kit snapshot: {error.describe}"
            | UnheldEntryErr reason -> $"neither the library nor the kit holds the reference: {reason}"

    /// A kit identity explicitly rebound to a held library envelope (spec 0050 S4.2).
    type EntryReplacement =
        {
            incoming : LibraryEntryId
            held : LibraryEntryEnvelope
        }

    /// Read-only snapshots and confirmed replacements for one constructor session (spec 0050 S4.2).
    type KitLibraryOverlay =
        {
            snapshots : LibraryEntryEnvelope list
            replacements : EntryReplacement list
        }

    /// The four library stores read by a constructor session (spec 0050 S4.2).
    [<ReferenceEquality>]
    type KitLibraryStores =
        {
            library : LibraryProxy
            materials : MaterialProxy
            samples : SampleProxy
            categories : CategoryProxy
        }

    /// One opened kit and the library seam supplying and pinning its versions (spec 0050 S4.2).
    [<ReferenceEquality>]
    type KitLibraryContext =
        {
            kit : KitContext
            imports : CatalogueImportContext
        }

        /// Opens snapshots and reads each rebinding's latest held envelope without writes (spec 0050 S4.2).
        /// A missing held entry refuses the overlay with UnheldEntryErr.
        member this.openOverlay (rebindings : EntryRebinding list) : Result<KitLibraryOverlay, KitLibraryError> =
            this.kit.proxy.listSnapshots ()
            |> Result.mapError KitLibraryKitErr
            |> Result.bind (fun snapshots ->
                rebindings
                |> List.fold (fun result rebinding ->
                    result |> Result.bind (fun replacements ->
                        this.imports.proxy.tryLatestEnvelope rebinding.held
                        |> Result.mapError KitLibraryImportErr
                        |> Result.bind (function
                            | None -> Error (UnheldEntryErr $"re-binding {rebinding.incoming.value} to {rebinding.held.value}")
                            | Some held ->
                                let replacement : EntryReplacement =
                                    {
                                        incoming = rebinding.incoming
                                        held = held
                                    }
                                Ok (replacement :: replacements)))) (Ok [])
                |> Result.map (fun replacements ->
                    {
                        snapshots = snapshots
                        replacements = List.rev replacements
                    }))

        /// Reads snapshot decisions in listing order without writes, including with read-only access (spec 0050 S4.2).
        /// After KitContext.openKit, the opener must call this and show every row in the import dialog, even an all-Reuse list.
        /// Only a kit with no snapshot rows may open without that dialog.
        member this.resolveSnapshots () : Result<ImportDecision list, KitLibraryError> =
            this.kit.proxy.listSnapshots ()
            |> Result.mapError KitLibraryKitErr
            |> Result.bind (fun snapshots ->
                let origin = KitSnapshotOrigin this.kit.kitFile.name
                snapshots
                |> List.fold (fun result envelope ->
                    result |> Result.bind (fun rows ->
                        this.imports.proxy.decideEntry origin envelope
                        |> Result.mapError KitLibraryImportErr
                        |> Result.map (fun row -> row :: rows))) (Ok [])
                |> Result.map List.rev)

        /// Applies confirmed rows, then pins added incoming versions in choice order (spec 0050 S4.2).
        /// Returns the import outcome or first failure; forks remain unpinned and the kit is unchanged.
        member this.confirm (choices : ImportChoice list) : Result<ImportOutcome, CatalogueImportError> =
            this.imports.apply choices
            |> Result.bind (fun outcome ->
                choices
                |> List.fold (fun result choice ->
                    result |> Result.bind (fun () ->
                        match choice.action, choice.decision.incomingOpt with
                        | (AddIncomingEntry | AddIncomingVersion _), Some incoming ->
                            this.imports.proxy.pinEntryVersion incoming.content.id incoming.content.version
                        | _ -> Ok ())) (Ok ())
                |> Result.map (fun () -> outcome))

        /// Pins and writes missing versions depth first, retaining held snapshots and returning writes in visit order (spec 0050 S4.2).
        /// Read-only access performs no IO; the first failure stops traversal and keeps earlier writes for the next call.
        member this.snapshotReferences (references : EntryReference list) : Result<LibraryVersionRef list, KitLibraryError> =
            match this.kit.access with
            | ReadOnlyKitAccess -> Error (KitLibraryKitErr (KitReadOnlyErr "snapshotReferences"))
            | ReadWriteKitAccess ->
                this.kit.proxy.listSnapshots ()
                |> Result.mapError KitLibraryKitErr
                |> Result.bind (fun envelopes ->
                    let snapshots = envelopes |> List.map (fun envelope -> envelope.libraryVersionRef, envelope) |> Map.ofList

                    let resolveReference (reference : EntryReference) : Result<LibraryEntryEnvelope, KitLibraryError> =
                        let resolved =
                            match reference with
                            | EntryVersionReference version ->
                                match Map.tryFind version snapshots with
                                | Some envelope -> Ok (Some envelope)
                                | None -> this.imports.proxy.tryHeldEnvelope version.entryId version.version
                            | EntryLatestReference id ->
                                this.imports.proxy.tryLatestEnvelope id
                                |> Result.map (function
                                    | Some envelope -> Some envelope
                                    | None ->
                                        envelopes
                                        |> List.filter (fun envelope -> envelope.content.id = id)
                                        |> List.sortByDescending (fun envelope -> envelope.content.version)
                                        |> List.tryHead)
                        resolved
                        |> Result.mapError KitLibraryImportErr
                        |> Result.bind (function
                            | None -> Error (UnheldEntryErr reference.describe)
                            | Some envelope ->
                                Map.tryFind envelope.libraryVersionRef snapshots
                                |> Option.defaultValue envelope
                                |> Ok)

                    let rec visitReferences
                        (visited : Set<LibraryVersionRef>)
                        (written : LibraryVersionRef list)
                        (pending : EntryReference list)
                        : Result<LibraryVersionRef list, KitLibraryError> =
                        match pending with
                        | [] -> Ok (List.rev written)
                        | reference :: rest ->
                            resolveReference reference
                            |> Result.bind (fun envelope ->
                                let version = envelope.libraryVersionRef
                                if Set.contains version visited then
                                    visitReferences visited written rest
                                else
                                    let writeResult =
                                        if Map.containsKey version snapshots then
                                            Ok written
                                        else
                                            this.imports.proxy.pinEntryVersion version.entryId version.version
                                            |> Result.mapError KitLibraryImportErr
                                            |> Result.bind (fun () -> this.kit.proxy.writeSnapshot envelope |> Result.mapError KitLibraryKitErr)
                                            |> Result.map (fun () -> version :: written)
                                    writeResult
                                    |> Result.bind (fun nextWritten ->
                                        visitReferences (Set.add version visited) nextWritten (envelope.content.references @ rest)))

                    visitReferences Set.empty [] references)

    let private tryOverlayReplacement (overlay : KitLibraryOverlay) (id : LibraryEntryId) : LibraryEntryEnvelope option =
        overlay.replacements
        |> List.tryFind (fun replacement -> replacement.incoming = id)
        |> Option.map (fun replacement -> replacement.held)

    let private resolveOverlayReference
        (overlay : KitLibraryOverlay)
        (reference : EntryReference)
        (readStore : unit -> Result<'Entry option, 'Error>)
        (decode : LibraryEntryEnvelope -> Result<'Entry option, 'Error>)
        : Result<'Entry option, 'Error> =
        let readSnapshot () : Result<'Entry option, 'Error> =
            overlay.snapshots
            |> List.filter (fun envelope ->
                match reference with
                | EntryVersionReference version -> envelope.libraryVersionRef = version
                | EntryLatestReference id -> envelope.content.id = id)
            |> List.sortByDescending (fun envelope -> envelope.content.version)
            |> List.tryHead
            |> function
                | Some envelope -> decode envelope
                | None -> Ok None

        match tryOverlayReplacement overlay reference.entryId with
        | Some held -> decode held
        | None ->
            let readThen
                (firstRead : unit -> Result<'Entry option, 'Error>)
                (fallbackRead : unit -> Result<'Entry option, 'Error>)
                : Result<'Entry option, 'Error> =
                firstRead ()
                |> Result.bind (function
                    | Some entry -> Ok (Some entry)
                    | None -> fallbackRead ())
            match reference with
            | EntryVersionReference _ -> readThen readSnapshot readStore
            | EntryLatestReference _ -> readThen readStore readSnapshot

    let private overlayListingSnapshots
        (overlay : KitLibraryOverlay)
        (kinds : LibraryEntryKind list)
        : LibraryEntryEnvelope list =
        overlay.snapshots
        |> List.groupBy (fun envelope -> envelope.content.id)
        |> List.map (fun (_, versions) -> versions |> List.maxBy (fun envelope -> envelope.content.version))
        |> List.filter (fun envelope ->
            List.contains envelope.content.schema.kind kinds
            && not (overlay.replacements |> List.exists (fun replacement -> replacement.incoming = envelope.content.id)))

    /// The session's library projection over read-only kit entries (spec 0050 S4.2).
    type LibraryProxy with

        /// Resolves replacements first and offers only snapshots absent from the store, deriving one By kind tree (spec 0050 S4.2).
        member this.overlaidWith (overlay : KitLibraryOverlay) : LibraryProxy =
            let decode (envelope : LibraryEntryEnvelope) : Result<LibraryEntry option, LibraryError> =
                let content = envelope.content
                match content.schema.kind with
                | SampleEntryKind -> content.toSample |> Result.map (SampleItem >> Some)
                | SourceEntryKind -> content.toSourcePreset |> Result.map (SourceItem >> Some)
                | PolarizerEntryKind -> content.toPolarizerPreset |> Result.map (PolarizerItem >> Some)
                | DetectorEntryKind -> content.toDetectorPreset |> Result.map (DetectorItem >> Some)
                | MaterialEntryKind | CategoryEntryKind -> Ok None
                |> Result.mapError (fun error -> LibraryUnavailableErr $"snapshot {content.describe}: {error.describe}")

            let entriesForKind (kind : CatalogueKind) : Result<LibraryEntry list, LibraryError> =
                this.entriesForKind kind
                |> Result.bind (fun stored ->
                    overlayListingSnapshots overlay [ SampleEntryKind; SourceEntryKind; PolarizerEntryKind; DetectorEntryKind ]
                    |> List.fold (fun result envelope ->
                        result |> Result.bind (fun entries ->
                            match stored |> List.exists (fun entry -> entry.entryId.tryToLibraryEntryId = Some envelope.content.id) with
                            | true -> Ok entries
                            | false ->
                                decode envelope
                                |> Result.bind (function
                                    | Some entry when List.contains kind entry.forKinds ->
                                        this.tryGetEntry entry.entryId
                                        |> Result.map (function
                                            | Some _ -> entries
                                            | None -> entry :: entries)
                                    | _ -> Ok entries))) (Ok [])
                    |> Result.map (fun snapshots -> stored @ List.rev snapshots))

            {
                entriesForKind = entriesForKind
                libraryTrees = fun () ->
                    [ CatalogueKind.Sample; LightSource; Detector; LinearPolarizer; CircularPolarizer ]
                    |> List.fold (fun result kind ->
                        result |> Result.bind (fun entries ->
                            entriesForKind kind |> Result.map (fun offered -> entries @ offered))) (Ok [])
                    |> Result.map (fun entries -> [ LibraryTree.byKind (entries |> List.distinctBy (fun entry -> entry.entryId)) ])
                tryGetEntry = fun id ->
                    match id.tryToLibraryEntryId with
                    | None -> this.tryGetEntry id
                    | Some entryId ->
                        let reference = EntryLatestReference entryId
                        resolveOverlayReference overlay reference (fun () -> this.tryGetEntry id) decode
            }

    /// The session's material resolution over read-only kit versions (spec 0050 S4.2).
    type MaterialProxy with

        /// Prefers snapshots for exact versions and the store for latest reads; listings and writes pass through (spec 0050 S4.2).
        member this.overlaidWith (overlay : KitLibraryOverlay) : MaterialProxy =
            let decode (envelope : LibraryEntryEnvelope) : Result<MaterialEntry option, MaterialError> =
                envelope.content.toMaterialEntry
                |> Result.map Some
                |> Result.mapError (fun error -> MaterialStoreUnavailableErr $"snapshot {envelope.content.describe}: {error.describe}")
            {
                this with
                    tryGetMaterial = fun id ->
                        let reference = EntryLatestReference id.toLibraryEntryId
                        resolveOverlayReference overlay reference (fun () -> this.tryGetMaterial id) decode
                    resolveVersion = fun version ->
                        let reference =
                            EntryVersionReference
                                {
                                    entryId = version.materialId.toLibraryEntryId
                                    version = version.version
                                }
                        resolveOverlayReference overlay reference (fun () -> this.resolveVersion version) decode
            }

    /// The session's sample resolution over read-only kit versions (spec 0050 S4.2).
    type SampleProxy with

        /// Prefers snapshots for exact versions and the store for latest reads; listings and writes pass through (spec 0050 S4.2).
        member this.overlaidWith (overlay : KitLibraryOverlay) : SampleProxy =
            let decode (envelope : LibraryEntryEnvelope) : Result<Sample option, SampleError> =
                envelope.content.toSample
                |> Result.map Some
                |> Result.mapError (fun error -> SampleStoreUnavailableErr $"snapshot {envelope.content.describe}: {error.describe}")
            {
                this with
                    tryGetSample = fun id ->
                        let reference = EntryLatestReference id.toLibraryEntryId
                        resolveOverlayReference overlay reference (fun () -> this.tryGetSample id) decode
                    resolveVersion = fun version ->
                        let reference =
                            EntryVersionReference
                                {
                                    entryId = version.sampleId.toLibraryEntryId
                                    version = version.version
                                }
                        resolveOverlayReference overlay reference (fun () -> this.resolveVersion version) decode
            }

    /// The session's categories, including categories held only by its kit (spec 0050 S4.2).
    type CategoryProxy with

        /// Appends the highest snapshot-only categories while preserving every write field (spec 0050 S4.2).
        member this.overlaidWith (overlay : KitLibraryOverlay) : CategoryProxy =
            {
                this with
                    listCategories = fun () ->
                        this.listCategories ()
                        |> Result.bind (fun stored ->
                            overlayListingSnapshots overlay [ CategoryEntryKind ]
                            |> List.filter (fun envelope ->
                                not (stored |> List.exists (fun category -> category.id.toLibraryEntryId = envelope.content.id)))
                            |> List.fold (fun result envelope ->
                                result |> Result.bind (fun categories ->
                                    envelope.content.toMaterialCategory
                                    |> Result.mapError (fun error -> CategoryStoreUnavailableErr $"snapshot {envelope.content.describe}: {error.describe}")
                                    |> Result.map (fun category -> category :: categories))) (Ok [])
                            |> Result.map (fun snapshots -> stored @ List.rev snapshots))
            }

    /// Composes the four stores used by a kit session (spec 0050 S4.2).
    type KitLibraryStores with

        /// Overlays each store's reads without caching entries or writing snapshots back (spec 0050 S4.2).
        member this.overlaidWith (overlay : KitLibraryOverlay) : KitLibraryStores =
            {
                library = this.library.overlaidWith overlay
                materials = this.materials.overlaidWith overlay
                samples = this.samples.overlaidWith overlay
                categories = this.categories.overlaidWith overlay
            }
