namespace OpticalConstructor.Domain

/// Stores named collections of ordered experiment setups and their optional DataAttachment values
/// (spec 0038 steps 028/029; spec 0050 S4.2).
module ExperimentCollectionStore =

    /// The experiment-collection persistence error channel (errors as values; the case carries a
    /// diagnostic `reason` — a bare error case is useless in a log). One case at the declared surface:
    /// a collection rejected as malformed (a blank name at `CollectionName.tryCreate`, or a snapshot a
    /// future store deems invalid). The later real store MAY grow the channel (missing file, IO
    /// failure) without breaking this declared shape — a new case is a compiler-guided addition.
    type ExperimentCollectionError =
        | InvalidCollectionErr of reason : string

    /// A saved experiment-collection's name — the persistence key `tryLoadCollection` addresses,
    /// `saveCollection` carries inside its snapshot, and `listCollections` enumerates. Elevated
    /// single-case string DU; `.value` is the raw key (reached only at the storage boundary), and
    /// `tryCreate` rejects a blank name at the parse boundary (a collection with no name has no
    /// addressable key), returning the typed `InvalidCollectionErr`.
    type CollectionName =
        | CollectionName of string

        member this.value = let (CollectionName s) = this in s

        static member tryCreate (name : string) : Result<CollectionName, ExperimentCollectionError> =
            if System.String.IsNullOrWhiteSpace name
            then Error (InvalidCollectionErr "an experiment-collection name must not be blank")
            else Ok (CollectionName name)

    /// A named, ordered collection preserving each experiment's setup and optional DataAttachment
    /// (spec 0038 step 028; spec 0050 S4.2).
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

    type ExperimentCollectionProxy with

        /// Spec 0038 (029, IMPLEMENT_CONTRACT STORE_XDUO_0005) — the real, stateful IN-MEMORY
        /// experiment-collection store behind the DECLARED `ExperimentCollectionProxy` seam (the
        /// `SceneProxy.createInMemory` precedent, `Scene.fs`): a `ref` `Map<CollectionName,
        /// ExperimentCollectionSnapshot>` captured INSIDE the closure (the IO boundary), so logic
        /// holding the proxy stays pure and a test substitutes this whole `createInMemory` (or a stub
        /// of the same shape). Starts EMPTY — a fresh session has no saved collections.
        /// `saveCollection` upserts (insert or overwrite under the snapshot's OWN `name`), RE-validating
        /// that name through `CollectionName.tryCreate` so a directly-constructed blank key is rejected
        /// as `InvalidCollectionErr` rather than silently stored; `tryLoadCollection` / `listCollections`
        /// read the map. Unlike `SceneProxy.saveScene` the name lives INSIDE the snapshot (the slice
        /// pins that signature), so there is no separate `name` argument. This is an INTRINSIC
        /// augmentation staying in this file (the `SceneProxy.createInMemory` precedent): it needs no
        /// type compiled after this module. A future disk-backed `create` in `OpticalConstructor.Storage`
        /// swaps this out with no change to the host that holds the proxy.
        static member createInMemory () : ExperimentCollectionProxy =
            let store = ref (Map.empty : Map<CollectionName, ExperimentCollectionSnapshot>)
            {
                saveCollection =
                    fun (snapshot : ExperimentCollectionSnapshot) ->
                        match CollectionName.tryCreate snapshot.name.value with
                        | Ok validName ->
                            store.Value <- store.Value |> Map.add validName snapshot
                            Ok ()
                        | Error e -> Error e

                tryLoadCollection = fun (name : CollectionName) -> Ok (store.Value |> Map.tryFind name)

                listCollections = fun () -> Ok (store.Value |> Map.toList |> List.map fst)
            }
