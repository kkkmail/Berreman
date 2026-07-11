namespace OpticalConstructor.Domain

/// Spec 0038 (028, ADD_CONTRACT STORE_XDUO_0005) — the experiment-collection persistence seam.
/// Declares, in the Domain, the DECLARED-lifecycle `ExperimentCollectionProxy`: the elevated
/// `CollectionName` key, the pure `ExperimentCollectionSnapshot` that captures a NAMED collection of
/// step-25 full-setup `Experiments.Experiment` values (each carrying its optional `DataFilePath`
/// attachment) as DATA, the `ExperimentCollectionError` channel, and the `[<ReferenceEquality>]`
/// `ExperimentCollectionProxy` functional-proxy record — the sibling `Scene.SceneProxy` convention
/// (`Scene.fs`): a record of camelCase `Result`-returning functions a test substitutes with an
/// in-memory stub. This is ONLY the seam: NO disk format and NO `.ocproj` wiring. A future storage
/// cycle (`IMPLEMENT_CONTRACT STORE_XDUO_0005`) swaps in a real, file-backed `create` in
/// `OpticalConstructor.Storage`, and wires the live `ExperimentCollection` (the editable in-progress
/// collection, `Experiments.fs`) as the app-scope `VersionsInUse` source (`AppContext.fs`, §0.2),
/// leaving the bay / host logic that holds the proxy unchanged. Every primitive is elevated: the key
/// is a single-case DU with a blank-rejecting `tryCreate`, and the error carries a diagnostic reason.
///
/// The module is `ExperimentCollectionStore` (NOT `ExperimentCollection`) so it never collides with
/// the existing `Experiments.ExperimentCollection` TYPE (the editable draft-plus-list collection).
/// It compiles AFTER `Experiments.fs` because `ExperimentCollectionSnapshot` reuses that module's
/// `Experiment` (the ordered `ElementDescriptor` setup + `DataFilePath` attachment introduced at
/// step 025).
module ExperimentCollectionStore =

    /// The experiment-collection persistence error channel (errors as values; the case carries a
    /// diagnostic `reason` — a bare error case is useless in a log). One case at the declared surface:
    /// a collection rejected as malformed (a blank name at `CollectionName.tryCreate`, or a snapshot a
    /// future store deems invalid). The later real store MAY grow the channel (missing file, IO
    /// failure) without breaking this declared shape — a new case is a compiler-guided addition.
    type ExperimentCollectionError =
        | InvalidCollection of reason : string

    /// A saved experiment-collection's name — the persistence key `tryLoadCollection` addresses,
    /// `saveCollection` carries inside its snapshot, and `listCollections` enumerates. Elevated
    /// single-case string DU; `.value` is the raw key (reached only at the storage boundary), and
    /// `tryCreate` rejects a blank name at the parse boundary (a collection with no name has no
    /// addressable key), returning the typed `InvalidCollection`.
    type CollectionName =
        | CollectionName of string

        member this.value = let (CollectionName s) = this in s

        static member tryCreate (name : string) : Result<CollectionName, ExperimentCollectionError> =
            if System.String.IsNullOrWhiteSpace name
            then Error (InvalidCollection "an experiment-collection name must not be blank")
            else Ok (CollectionName name)

    /// A named collection of experiments captured as pure DATA (the seam a future storage cycle
    /// serializes): the `name` key and the ordered `experiments` — the step-25 full-setup
    /// `Experiments.Experiment` values, each with its captured `ElementDescriptor` setup and its
    /// optional `DataFilePath` attachment (`dataFileOpt`). Holds no live element reference and no
    /// Avalonia handle — every field is a pure Domain value, so a saved collection survives the
    /// session that authored it and a save-load round-trip.
    type ExperimentCollectionSnapshot =
        {
            name : CollectionName
            experiments : Experiments.Experiment list
        }

    /// The experiment-collection persistence seam (the functional-proxy convention,
    /// `Scene.SceneProxy`): a record of camelCase `Result`-returning functions a future disk-backed
    /// store fills; a test substitutes a stub of the SAME shape over a fixed map. Function-valued
    /// fields have no structural equality, so the proxy compares by reference — a host model that
    /// holds one keeps its (Elmish-required) equality, comparing the proxy by identity.
    ///
    /// - `saveCollection snapshot` — persist (insert or overwrite) the collection under its own
    ///   `snapshot.name`;
    /// - `tryLoadCollection name` — the collection stored under `name`, or `None` when none is stored;
    /// - `listCollections ()` — the names of every stored collection.
    ///
    /// DECLARED lifecycle: this is the seam only — no disk format, no `.ocproj` wiring. The real
    /// store lands in a later `IMPLEMENT_CONTRACT` cycle in `OpticalConstructor.Storage`.
    [<ReferenceEquality>]
    type ExperimentCollectionProxy =
        {
            saveCollection : ExperimentCollectionSnapshot -> Result<unit, ExperimentCollectionError>
            tryLoadCollection : CollectionName -> Result<ExperimentCollectionSnapshot option, ExperimentCollectionError>
            listCollections : unit -> Result<CollectionName list, ExperimentCollectionError>
        }
