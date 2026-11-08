namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.LibraryEntryFiles

/// Pure scene snapshots and their persistence seam (spec 0038, STORE_XDUO_0004),
/// with a named, versioned .setup-json envelope for kit storage (spec 0050 S4.3).
module Scene =

    /// The scene persistence error channel (errors as values; the case carries a diagnostic
    /// `reason` — a bare error case is useless in a log). One case at the declared surface: a
    /// scene rejected as malformed (a blank name at `SceneName.tryCreate`, or a snapshot a future
    /// store deems invalid). The later real store MAY grow the channel (missing file, IO failure)
    /// without breaking this declared shape — a new case is a compiler-guided addition.
    type SceneStoreError =
        | InvalidSceneErr of reason : string

    /// A saved scene's key; tryCreate rejects blank names (spec 0038, STORE_XDUO_0004).
    /// The original name is retained in the file while its stem is filesystem-safe (spec 0050 S4.3).
    type SceneName =
        | SceneName of string

        member this.value = let (SceneName s) = this in s

        /// The scene's file stem preserves its name except for forbidden characters (spec 0050 S4.3).
        member this.fileStem : FileStem = FileStem.ofText this.value

        static member tryCreate (name : string) : Result<SceneName, SceneStoreError> =
            if System.String.IsNullOrWhiteSpace name
            then Error (InvalidSceneErr "a scene name must not be blank")
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
    /// Domain cannot reference the Ui, so the snapshot re-declares the same shape here. Spec 0046:
    /// `beam` is the beam the element sits on (the main beam, or the reflected ray of a partially-
    /// reflecting element, by that element's id), so a saved scene keeps its reflected-ray attachments.
    type SceneElement =
        {
            id : Library.ElementId
            placement : Placement.ElementPlacement
            zoom : BeamLayout.ElementZoom
            beam : BeamLayout.BeamHost
        }

    /// The placed elements, view, snap mode and pivot as pure data, without UI handles
    /// (specs 0038, 0046). SceneFile preserves this snapshot in a kit (spec 0050 S4.3).
    type SceneSnapshot =
        {
            elements : SceneElement list
            view : Table.TableViewState
            snap : SnapMode
            /// Spec 0046: the global rotation pivot — elements rotate about their centre, or about the
            /// middle of their incident face. It moves elements and rays, so a saved scene keeps it.
            pivot : BeamLayout.RotationPivot
        }

        /// Each bound library entry at its latest, once in element order (spec 0050 S4.3).
        member this.libraryReferences : EntryReference list =
            this.elements
            |> List.choose (fun element ->
                element.placement.valueId
                |> Option.bind (fun valueId -> valueId.tryToLibraryEntryId))
            |> List.distinct
            |> List.map EntryLatestReference

    /// A .setup-json keeps the original scene name alongside its snapshot (spec 0050 S4.3).
    type SceneFile =
        {
            schemaVersion : FileSchemaVersion
            name : SceneName
            scene : SceneSnapshot
        }

        /// The only scene-file schema version this app reads and writes (spec 0050 S4.3).
        static member currentSchemaVersion : FileSchemaVersion = FileSchemaVersion 1

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
        /// `SceneName.tryCreate` so a directly-constructed blank key is rejected as `InvalidSceneErr`
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
