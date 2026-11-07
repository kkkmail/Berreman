namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles

/// The library-import seam, in-memory stores and pure per-entry decision table (spec 0050 §3.4; S3.1, 001 §B).
/// Decodes parsed catalogue entries through Result-returning save functions.
module CatalogueImport =

    /// A store refusal, undecodable entry or unreadable catalogue with its diagnostic payload (spec 0050 §3.4).
    type CatalogueImportError =
        | ImportRejectedErr of reason : string
        | UndecodableEntryErr of LibraryEntryError
        | CatalogueUnreadableErr of CatalogueError

        member this.describe : string =
            match this with
            | ImportRejectedErr reason -> $"a catalogue entry was rejected: {reason}"
            | UndecodableEntryErr error -> $"a catalogue entry could not be decoded: {error.describe}"
            | CatalogueUnreadableErr error -> $"the catalogue could not be read: {error.describe}"

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

        member this.byDefault : ImportChoice =
            {
                decision = this
                action = this.defaultAction
            }

        member this.rowName : EntryName =
            match this.status with
            | NameCollision (incoming, _) ->
                let (CatalogueImportOrigin (_, name)) = this.origin
                EntryName $"{incoming.content.name.value} — from {name.value}"
            | EntryIdNotHeld incoming | SameContentHash (incoming, _) | VersionNotHeld (incoming, _)
            | DivergentContent (incoming, _) -> incoming.content.name
            | EntryUnlisted held -> held.name

        static member split (decisions : ImportDecision list) : ImportSplit =
            {
                awaiting = decisions
            }

    /// Saves each catalogue kind, returning CatalogueImportError on refusal (spec 0050 §3.4).
    /// Function-valued fields make proxy equality reference-based.
    [<ReferenceEquality>]
    type CatalogueImportProxy =
        {
            saveCategory : MaterialCategory -> Result<unit, CatalogueImportError>
            saveMaterial : MaterialEntry -> Result<unit, CatalogueImportError>
            saveSample : Sample -> Result<unit, CatalogueImportError>
            saveLibraryEntry : LibraryEntry -> Result<unit, CatalogueImportError>
        }

        /// Imports categories, materials, samples, then source, polarizer and detector presets (spec 0050 §3.4).
        /// Preserves row order within each kind; the first decode or save failure stops further writes, retaining prior saves.
        member this.importCatalogue (catalogue : CatalogueData) : Result<unit, CatalogueImportError> =
            let pushDecoded (save : 'Entry -> Result<unit, CatalogueImportError>) (decoded : Result<'Entry, LibraryEntryError>) : Result<unit, CatalogueImportError> =
                decoded |> Result.mapError UndecodableEntryErr |> Result.bind save

            let pushEnvelope (envelope : LibraryEntryEnvelope) : Result<unit, CatalogueImportError> =
                let content = envelope.content
                match content.schema.kind with
                | CategoryEntryKind -> content.toMaterialCategory |> pushDecoded this.saveCategory
                | MaterialEntryKind -> content.toMaterialEntry |> pushDecoded this.saveMaterial
                | SampleEntryKind -> content.toSample |> pushDecoded this.saveSample
                | SourceEntryKind -> content.toSourcePreset |> Result.map SourceItem |> pushDecoded this.saveLibraryEntry
                | PolarizerEntryKind -> content.toPolarizerPreset |> Result.map PolarizerItem |> pushDecoded this.saveLibraryEntry
                | DetectorEntryKind -> content.toDetectorPreset |> Result.map DetectorItem |> pushDecoded this.saveLibraryEntry

            [ CategoryEntryKind; MaterialEntryKind; SampleEntryKind; SourceEntryKind; PolarizerEntryKind; DetectorEntryKind ]
            |> List.collect (fun kind -> catalogue.envelopes |> List.filter (fun envelope -> envelope.content.schema.kind = kind))
            |> List.fold (fun imported envelope -> imported |> Result.bind (fun () -> pushEnvelope envelope)) (Ok ())

        /// Reads and imports a catalogue, preserving the read failure's payload (spec 0050 §3.4).
        member this.importFrom (catalogue : CatalogueProxy) : Result<unit, CatalogueImportError> =
            catalogue.readCatalogue ()
            |> Result.mapError CatalogueUnreadableErr
            |> Result.bind this.importCatalogue

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

    type CatalogueImportProxy with

        /// Builds an import proxy over any four stores, in-memory doubles or database stores (spec 0050 S2.3).
        /// Maps each store's refusal to ImportRejectedErr with its diagnostic reason.
        static member ofStores (categories : CategoryProxy) (materials : MaterialProxy) (samples : SampleProxy) (presets : LibraryEntryStore) : CatalogueImportProxy =
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

            {
                saveCategory = fun c -> categories.addCategory c |> Result.mapError (categoryReason >> ImportRejectedErr)
                saveMaterial = fun m -> materials.saveMaterial m |> Result.mapError (materialReason >> ImportRejectedErr)
                saveSample = fun s -> samples.saveSample s |> Result.mapError (sampleReason >> ImportRejectedErr)
                saveLibraryEntry = fun e -> presets.addEntry e |> Result.mapError (libraryReason >> ImportRejectedErr)
            }

        /// Builds a CatalogueImportProxy over empty in-memory stores, mapping refusals to ImportRejectedErr (spec 0050 §3.4).
        /// Returns InMemoryCatalogueStores with live reference lookups and no database or file IO.
        static member createInMemory () : InMemoryCatalogueStores =
            let samples = SampleProxy.createInMemory ()
            let materials = MaterialProxy.createInMemory (samplesReferencing samples)
            let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
            let libraryEntries = Library.createInMemory ()

            {
                proxy = CatalogueImportProxy.ofStores categories materials samples libraryEntries
                categories = categories
                materials = materials
                samples = samples
                libraryEntries = libraryEntries.proxy
            }
