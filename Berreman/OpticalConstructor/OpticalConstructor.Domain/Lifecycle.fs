namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.MaterialLibrary   // MaterialId (spec 0033 step 002)
open OpticalConstructor.Domain.Library           // SampleId (spec 0033 step 002)

/// Spec 0038 Part H (step 020, AC-H1) — the entry-lifecycle foundation both library
/// stores share: version numbering, the active/inactive state, a cross-store versioned
/// reference, the "which versions are currently in use" seam, and — the load-bearing
/// piece — the ONE pure version-creation decision function the material store (step 21)
/// and the sample store (step 22) both call. Everything here is pure and Avalonia-free;
/// nothing wires a store yet (the seam is truthful for the live session and becomes fully
/// truthful when persistence arrives). Every primitive is elevated — no naked int, no
/// naked bool — so a versioned reference and the decision inputs carry meaning, not
/// primitives.
module Lifecycle =

    /// A monotonically increasing version number of a library entry (§H). A single-case
    /// `int` DU — no naked int stands in for a version. `first` is the version every entry
    /// starts at; `.next` mints the successor the store freezes onto when a USED version's
    /// physics changes; `.value` is the wire/disk integer, reached only at the IO boundary.
    type VersionNumber =
        | VersionNumber of int

        member this.value = let (VersionNumber v) = this in v

        /// The successor version — one past this one. Pure; `VersionNumber.first.next` is
        /// version 2.
        member this.next : VersionNumber = VersionNumber (this.value + 1)

        /// The version every entry starts at (1). New entries and the re-expressed seeds
        /// begin here; the store never fabricates a version number by hand.
        static member first : VersionNumber = VersionNumber 1

    /// A specific version of a material entry (§H): the material identity plus the version
    /// number. This is the exact key a versioned experiment binding pins (step 25) — a
    /// binding references the precise version it was built against, not merely the material,
    /// so a later mint of the material's `.next` version does not silently rewrite an
    /// existing experiment's physics.
    type MaterialVersionId =
        {
            materialId : MaterialId
            version : VersionNumber
        }

    /// A specific version of a sample entry (§H): the sample identity plus the version
    /// number. Mirrors `MaterialVersionId` — a versioned sample binding pins the exact
    /// version it was built against.
    type SampleVersionId =
        {
            sampleId : SampleId
            version : VersionNumber
        }

    /// Whether a library entry (a specific version) is live or retired (§H). A named
    /// two-case DU, never a naked bool: an `InactiveEntry` is superseded / soft-deleted —
    /// hidden from the pickers but still RESOLVABLE, so a scene or experiment that already
    /// binds it keeps rendering. A future third state (e.g. archived) is a non-breaking
    /// addition of a case, not a signature change.
    type EntryLifecycle =
        | ActiveEntry
        | InactiveEntry

    /// A reference to a specific versioned entry across BOTH stores (§H): a material version
    /// or a sample version. The set of these that the live experiments currently bind is
    /// what makes a version "in use" (see `VersionsInUse`) and therefore immutable — a used
    /// version's physics change mints the next version rather than mutating the bound one.
    type VersionRef =
        | MaterialVersionRef of MaterialVersionId
        | SampleVersionRef of SampleVersionId

    /// The "which versions are currently in use" seam (§H): a record of one function that
    /// yields the set of versioned references the live experiment store binds. Step 25
    /// constructs the real one over the in-memory experiment descriptors; until persistence
    /// arrives it is truthful for the live session (an in-use version survives only as long
    /// as the session that references it). Injected into the stores' saves so the
    /// version-creation rule can ask "is this version used?" without the Domain reaching
    /// into the experiment store. Function-valued, so it compares by reference (the proxy
    /// convention, `Library.LibraryProxy`).
    [<ReferenceEquality>]
    type VersionsInUse =
        {
            versionsInUse : unit -> Set<VersionRef>
        }

    /// Whether the version being saved over is currently bound by a live experiment (§H). A
    /// named two-case DU, never a naked bool, so the decision match reads as prose and the
    /// signature says what the flag means.
    type VersionUsage =
        | VersionUsed
        | VersionUnused

        /// Elevate a raw "is used" bool at its computation site (the seam lookup below).
        static member ofBool (used : bool) : VersionUsage =
            match used with
            | true -> VersionUsed
            | false -> VersionUnused

    /// The usage of a specific versioned reference, resolved through the in-use seam (§H):
    /// the reference is `VersionUsed` iff the seam's current set contains it. The single
    /// place a store turns a `VersionRef` + the seam into the `VersionUsage` the decision
    /// rule consumes.
    let usageOf (inUse : VersionsInUse) (reference : VersionRef) : VersionUsage =
        inUse.versionsInUse () |> Set.contains reference |> VersionUsage.ofBool

    /// The version-creation decision (§H): the ONE outcome the shared rule yields.
    /// `MutateInPlace` overwrites the current version's stored payload without minting a new
    /// version (either an unused version's edit, or a metadata-only edit of any version);
    /// `MintNextVersion` freezes the used current version and starts its `.next` carrying the
    /// incoming payload; `KeepCurrent` is a no-op — the incoming save is identical (physics
    /// AND metadata) to what is stored.
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

    /// THE version-creation rule (§H), expressed ONCE and shared by both stores. Inputs: the
    /// stored latest version's payload, the incoming save, and whether the stored version is
    /// used. Structural comparison is INJECTED (`physicsEqual` / `metadataEqual`) rather than
    /// imposed as an equality constraint, so the rule never dictates how a store defines
    /// "structurally different" and stays trivially testable; the real stores pass `(=)` over
    /// their value-tree physics and metadata. The decision table:
    ///
    /// - physics identical, metadata identical         -> `KeepCurrent`   (a no-op save)
    /// - physics identical, metadata changed           -> `MutateInPlace` (metadata-only edits
    ///                                                    NEVER version, regardless of use)
    /// - physics changed, version UNused               -> `MutateInPlace` (an unused version
    ///                                                    is edited in place)
    /// - physics changed, version USED                 -> `MintNextVersion` (a used version with
    ///                                                    structurally different physics mints
    ///                                                    the next; the new metadata rides along)
    let decideVersioning
        (physicsEqual : 'Physics -> 'Physics -> bool)
        (metadataEqual : 'Metadata -> 'Metadata -> bool)
        (stored : VersionPayload<'Physics, 'Metadata>)
        (incoming : VersionPayload<'Physics, 'Metadata>)
        (usage : VersionUsage)
        : VersionDecision =
        let physicsChanged = not (physicsEqual stored.physics incoming.physics)
        let metadataChanged = not (metadataEqual stored.metadata incoming.metadata)

        match physicsChanged, metadataChanged, usage with
        | false, false, _ -> KeepCurrent
        | false, true, _ -> MutateInPlace
        | true, _, VersionUnused -> MutateInPlace
        | true, _, VersionUsed -> MintNextVersion
