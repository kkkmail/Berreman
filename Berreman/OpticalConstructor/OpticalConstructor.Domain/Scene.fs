namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.LibraryEntryFiles

/// The constructor scene as pure data, kept in a kit as <scene name>.setup-json
/// (spec 0038; spec 0050 S4.3).
module Scene =

    /// A malformed scene carries a diagnostic reason, including a rejected blank name
    /// (spec 0038; spec 0050 S4.3).
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

    /// Elements snap onto the beam chain in the constructor or use free placement in static
    /// test scenes; the boolean wire form is used only at the IO boundary (spec 0038).
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

    /// Captures an element's identity, placement, draw zoom and beam attachment (specs 0038, 0046).
    /// Placement retains the catalogue kind and library binding; beam retains reflected-ray
    /// attachments by their host element's identity.
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
