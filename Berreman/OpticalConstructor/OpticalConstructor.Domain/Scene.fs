namespace OpticalConstructor.Domain

/// Spec 0038 (026, ADD_CONTRACT STORE_XDUO_0004) — the scene persistence seam. Declares, in the
/// Domain, the DECLARED-lifecycle `SceneProxy`: the elevated `SceneName` key, the pure
/// `SceneSnapshot` that captures the constructor scene as DATA (the placed elements, the table
/// view state, the snap mode), the `SceneStoreError` channel, and the `[<ReferenceEquality>]`
/// `SceneProxy` functional-proxy record — the `Library.LibraryProxy` convention (`ElementId.fs`):
/// a record of camelCase `Result`-returning functions a test substitutes with an in-memory stub.
/// This is ONLY the seam: NO disk format and NO `.ocproj` wiring. A future storage cycle
/// (`IMPLEMENT_CONTRACT STORE_XDUO_0004`) swaps in a real, file-backed `create` in
/// `OpticalConstructor.Storage`, leaving the bay / host logic that holds the proxy unchanged.
/// Every primitive is elevated: the key is a single-case DU with a blank-rejecting `tryCreate`,
/// the snap flag is a two-case DU (never a naked bool), and the error carries a diagnostic reason.
module Scene =

    /// The scene persistence error channel (errors as values; the case carries a diagnostic
    /// `reason` — a bare error case is useless in a log). One case at the declared surface: a
    /// scene rejected as malformed (a blank name at `SceneName.tryCreate`, or a snapshot a future
    /// store deems invalid). The later real store MAY grow the channel (missing file, IO failure)
    /// without breaking this declared shape — a new case is a compiler-guided addition.
    type SceneStoreError =
        | InvalidScene of reason : string

    /// A saved scene's name — the persistence key `saveScene` / `tryLoadScene` address and
    /// `listScenes` enumerates. Elevated single-case string DU; `.value` is the raw key (reached
    /// only at the storage boundary), and `tryCreate` rejects a blank name at the parse boundary
    /// (a scene with no name has no addressable key), returning the typed `InvalidScene`.
    type SceneName =
        | SceneName of string

        member this.value = let (SceneName s) = this in s

        static member tryCreate (name : string) : Result<SceneName, SceneStoreError> =
            if System.String.IsNullOrWhiteSpace name
            then Error (InvalidScene "a scene name must not be blank")
            else Ok (SceneName name)

    /// Whether the scene's elements SNAP onto the beam chain (the Main "Lego constructor": the
    /// light source is the ray root and downstream elements snap onto the beam, reflecting at
    /// mirrors) or sit at FREE placement (the static test scenes). A two-case named DU, never a
    /// naked bool (the elevate-every-primitive discipline) so a match site reads as prose and a
    /// third mode is a non-breaking addition; `.value` is the wire form the host model's
    /// `snapChain : bool` maps to, reached only at the IO boundary — the `SelectWindowModality`
    /// precedent (`WorkbenchSettings.fs`).
    type SnapMode =
        | SnapToBeam
        | FreePlacement

        /// The host / wire form: true = snap onto the beam (IO boundary only).
        member this.value =
            match this with
            | SnapToBeam -> true
            | FreePlacement -> false

        static member create (snap : bool) : SnapMode =
            match snap with
            | true -> SnapToBeam
            | false -> FreePlacement

    /// One captured element of a constructor scene: its stable, serializable identity
    /// (`Library.ElementId` — distinct from the Library binding), its full placement, and its
    /// per-element draw zoom. The `placement` (`Placement.ElementPlacement`, already persisted by
    /// `Project.placements`) carries the element's `catalogueKind` tag and its `valueId` Library
    /// binding, so the snapshot captures the id, the CatalogueKind, the placement, and the valueId
    /// binding through ONE reused domain type rather than duplicating derivable fields. Mirrors the
    /// live Main-scene `TestElement` (`TableAndElementRotationView`, Ui) over Domain types — the
    /// Domain cannot reference the Ui, so the snapshot re-declares the same shape here.
    type SceneElement =
        {
            id : Library.ElementId
            placement : Placement.ElementPlacement
            zoom : float
        }

    /// The constructor scene captured as pure DATA (the seam a future storage cycle serializes):
    /// the placed `elements`, the table `view` state (screen rotation / pan / zoom), and the
    /// `snap` mode. Holds no live element reference and no Avalonia handle — every field is a pure
    /// Domain value, so a saved scene survives the session that authored it. The table PLATE and
    /// the other project aggregates stay outside this snapshot — this slice captures exactly the
    /// scene fields the how-to enumerates.
    type SceneSnapshot =
        {
            elements : SceneElement list
            view : Table.TableViewState
            snap : SnapMode
        }

    /// The scene persistence seam (the functional-proxy convention, `Library.LibraryProxy`): a
    /// record of camelCase `Result`-returning functions a future disk-backed store fills; a test
    /// substitutes a stub of the SAME shape over a fixed map. Function-valued fields have no
    /// structural equality, so the proxy compares by reference — a host model that holds one keeps
    /// its (Elmish-required) equality, comparing the proxy by identity.
    ///
    /// - `saveScene name snapshot` — persist (insert or overwrite) the scene under `name`;
    /// - `tryLoadScene name` — the scene stored under `name`, or `None` when none is stored;
    /// - `listScenes ()` — the names of every stored scene.
    ///
    /// DECLARED lifecycle: this is the seam only — no disk format, no `.ocproj` wiring. The real
    /// store lands in a later `IMPLEMENT_CONTRACT` cycle in `OpticalConstructor.Storage`.
    [<ReferenceEquality>]
    type SceneProxy =
        {
            saveScene : SceneName -> SceneSnapshot -> Result<unit, SceneStoreError>
            tryLoadScene : SceneName -> Result<SceneSnapshot option, SceneStoreError>
            listScenes : unit -> Result<SceneName list, SceneStoreError>
        }

    type SceneProxy with

        /// Spec 0038 (027, IMPLEMENT_CONTRACT STORE_XDUO_0004) — the real, stateful IN-MEMORY scene
        /// store behind the DECLARED `SceneProxy` seam (the `SampleProxy.createInMemory` precedent,
        /// `SampleStore.fs`): a `ref` `Map<SceneName, SceneSnapshot>` captured INSIDE the closure
        /// (the IO boundary), so logic holding the proxy stays pure and a test substitutes this whole
        /// `create` (or a stub of the same shape). Starts EMPTY — a fresh session has no saved scenes.
        /// `saveScene` upserts (insert or overwrite under the name), RE-validating the name through
        /// `SceneName.tryCreate` so a directly-constructed blank key is rejected as `InvalidScene`
        /// rather than silently stored; `tryLoadScene` / `listScenes` read the map. This is an
        /// INTRINSIC augmentation staying in this file (the `CategoryProxy.createInMemory` precedent,
        /// `MaterialLibrary.fs`): it needs no type compiled after `Scene.fs`, unlike the versioned
        /// `SampleProxy` store whose `decideVersioning` rule forces it into `SampleStore.fs`. A future
        /// disk-backed `create` in `OpticalConstructor.Storage` swaps this out with no change to the
        /// host that holds the proxy.
        static member createInMemory () : SceneProxy =
            let store = ref (Map.empty : Map<SceneName, SceneSnapshot>)
            {
                saveScene =
                    fun (name : SceneName) (snapshot : SceneSnapshot) ->
                        match SceneName.tryCreate name.value with
                        | Ok validName ->
                            store.Value <- store.Value |> Map.add validName snapshot
                            Ok ()
                        | Error e -> Error e

                tryLoadScene = fun (name : SceneName) -> Ok (store.Value |> Map.tryFind name)

                listScenes = fun () -> Ok (store.Value |> Map.toList |> List.map fst)
            }
