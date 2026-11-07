namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.MaterialLibrary   // MaterialId (spec 0033 step 002)
open OpticalConstructor.Domain.Library           // SampleId (spec 0033 step 002)
open OpticalConstructor.Domain.LibraryEntryFiles

/// Shared lifecycle, version references and pin-based version decisions for both stores
/// (spec 0038 Part H; spec 0050 S2.2).
module Lifecycle =

    /// Whether a library entry (a specific version) is live or retired (§H). A named
    /// two-case DU, never a naked bool: an `InactiveEntry` is superseded / soft-deleted —
    /// hidden from the pickers but still RESOLVABLE, so a scene or experiment that already
    /// binds it keeps rendering. A future third state (e.g. archived) is a non-breaking
    /// addition of a case, not a signature change.
    type EntryLifecycle =
        | ActiveEntry
        | InactiveEntry

        /// The 1-based database tag (spec 0050).
        member this.value : int =
            match this with
            | ActiveEntry -> 1
            | InactiveEntry -> 2

        /// Reads a database tag, rejecting unknown values (spec 0050).
        static member tryOfDb (v : int) : EntryLifecycle option =
            match v with
            | 1 -> Some ActiveEntry
            | 2 -> Some InactiveEntry
            | _ -> None

    /// Whether a library version is pinned (spec 0050, 001 §C): a one-way pin is set
    /// when a kit first references the version or it is exported; a pinned version's physics never mutates in place.
    type PinState =
        | VersionPinned
        | VersionUnpinned

        /// The 1-based database tag (spec 0050).
        member this.value : int =
            match this with
            | VersionPinned -> 1
            | VersionUnpinned -> 2

        /// Reads a database tag, rejecting unknown values (spec 0050).
        static member tryOfDb (v : int) : PinState option =
            match v with
            | 1 -> Some VersionPinned
            | 2 -> Some VersionUnpinned
            | _ -> None

    /// A reference to a specific material or sample version (spec 0038 Part H).
    type VersionRef =
        | MaterialVersionRef of MaterialVersionId
        | SampleVersionRef of SampleVersionId

        /// The exact material or sample version a kit snapshots (spec 0050 S4.2).
        member this.entryReference : EntryReference =
            match this with
            | MaterialVersionRef materialVersion ->
                EntryVersionReference
                    {
                        entryId = materialVersion.materialId.toLibraryEntryId
                        version = materialVersion.version
                    }
            | SampleVersionRef sampleVersion ->
                EntryVersionReference
                    {
                        entryId = sampleVersion.sampleId.toLibraryEntryId
                        version = sampleVersion.version
                    }

    /// Identical saves keep the version; metadata edits and unpinned physics edits mutate it.
    /// Pinned physics edits mint a successor (spec 0038 Part H; spec 0050 S2.2).
    type VersionDecision =
        | MutateInPlace
        | MintNextVersion
        | KeepCurrent

    /// A versioned entry's payload as the rule sees it (§H): the physics tree
    /// (`MaterialComplexity` for the material store, `SampleStructure` for the sample store)
    /// and the metadata (the store's own name / description / category record). Generic so
    /// ONE rule serves both stores; the same shape carries both the stored latest version and
    /// the incoming save.
    type VersionPayload<'Physics, 'Metadata> =
        {
            physics : 'Physics
            metadata : 'Metadata
        }

    /// Compares stored and incoming payloads using structural equality (spec 0038 Part H;
    /// spec 0048 E018; spec 0050 S2.2). The stored latest version supplies the pin:
    ///
    /// | Physics | Metadata | Pin | Decision |
    /// | Identical | Identical | Either | KeepCurrent |
    /// | Identical | Changed | Either | MutateInPlace |
    /// | Changed | Either | Unpinned | MutateInPlace |
    /// | Changed | Either | Pinned | MintNextVersion |
    let decideVersioning<'Physics, 'Metadata when 'Physics : equality and 'Metadata : equality>
        (stored : VersionPayload<'Physics, 'Metadata>)
        (incoming : VersionPayload<'Physics, 'Metadata>)
        (pin : PinState)
        : VersionDecision =
        let physicsChanged = not (stored.physics = incoming.physics)
        let metadataChanged = not (stored.metadata = incoming.metadata)

        match physicsChanged, metadataChanged, pin with
        | false, false, _ -> KeepCurrent
        | false, true, _ -> MutateInPlace
        | true, _, VersionUnpinned -> MutateInPlace
        | true, _, VersionPinned -> MintNextVersion
