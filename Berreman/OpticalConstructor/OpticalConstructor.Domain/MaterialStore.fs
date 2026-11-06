namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle

/// Versioned in-memory material storage with one-way per-version pins
/// (spec 0038 step 021; spec 0050 §3.4, S2.2). Stores start empty and catalogue import supplies built-ins.
module MaterialStore =

    /// A stored material version; only the latest is editable, while older versions remain resolvable.
    type private VersionRecord =
        {
            versionNo : VersionNumber
            lifecycle : EntryLifecycle
            pin : PinState
            entry : MaterialEntry
        }

    /// Material display fields whose edits never mint a version (spec 0038 Part H; spec 0050 S2.2).
    type MaterialMetadata =
        {
            name : EntryName
            category : CategoryId
            description : EntryDescription option
        }

    type MaterialEntry with

        /// Comparable physics and display metadata for the shared versioning rule (spec 0050 S2.2).
        member this.versionPayload : VersionPayload<MaterialComplexity, MaterialMetadata> =
            {
                physics = this.complexity
                metadata =
                    {
                        name = this.name
                        category = this.category
                        description = this.description
                    }
            }

    type MaterialProxy with

        static member private createInMemoryStore
            (initialEntries : MaterialEntry list)
            (samplesReferencing : MaterialId -> Sample list)
            : MaterialProxy =

            let initialVersions =
                initialEntries
                |> List.map (fun e ->
                    let version =
                        {
                            versionNo = VersionNumber.first
                            lifecycle = ActiveEntry
                            pin = VersionUnpinned
                            entry = e
                        }
                    e.id, [ version ])
                |> Map.ofList
            let store = ref initialVersions

            /// The latest (highest-numbered) version of a non-empty history.
            let latestOf (versions : VersionRecord list) : VersionRecord =
                versions |> List.maxBy (fun v -> v.versionNo.value)

            /// The LATEST version of each material, filtered by the show-inactive scope.
            let currentEntries (scope : InactiveVisibility) : MaterialEntry list =
                store.Value
                |> Map.toList
                |> List.choose (fun (_, versions) ->
                    let latest = latestOf versions
                    match scope, latest.lifecycle with
                    | IncludeInactive, _ -> Some latest.entry
                    | ActiveOnly, ActiveEntry -> Some latest.entry
                    | ActiveOnly, InactiveEntry -> None)

            /// Retire / revive the LATEST version of a known material (the shared body of the three
            /// lifecycle verbs). Unknown id → `UnknownMaterialId`; older versions are untouched.
            let setLatestLifecycle (id : MaterialId) (lifecycle : EntryLifecycle) : Result<unit, MaterialError> =
                match store.Value |> Map.tryFind id with
                | Some versions ->
                    let latest = latestOf versions
                    let updated =
                        versions
                        |> List.map (fun v -> if v.versionNo = latest.versionNo then { v with lifecycle = lifecycle } else v)
                    store.Value <- store.Value |> Map.add id updated
                    Ok ()
                | None -> Error (MaterialError.unknownMaterial id)

            {
                listMaterials = fun scope -> Ok (currentEntries scope)

                searchMaterials = fun q -> Ok (byQuery q { entries = currentEntries ActiveOnly })

                tryGetMaterial =
                    fun id -> Ok (store.Value |> Map.tryFind id |> Option.map (fun versions -> (latestOf versions).entry))

                resolveVersion =
                    fun mvid ->
                        Ok (
                            store.Value
                            |> Map.tryFind mvid.materialId
                            |> Option.bind (fun versions -> versions |> List.tryFind (fun v -> v.versionNo = mvid.version))
                            |> Option.map (fun v -> v.entry))

                saveMaterial =
                    fun entry ->
                        entry.validate
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind entry.id with
                            | None ->
                                let first =
                                    {
                                        versionNo = VersionNumber.first
                                        lifecycle = ActiveEntry
                                        pin = VersionUnpinned
                                        entry = entry
                                    }
                                store.Value <- store.Value |> Map.add entry.id [ first ]
                                Ok ()
                            | Some versions ->
                                let latest = latestOf versions
                                match decideVersioning latest.entry.versionPayload entry.versionPayload latest.pin with
                                | KeepCurrent -> Ok ()
                                | MutateInPlace ->
                                    let updated =
                                        versions
                                        |> List.map (fun v -> if v.versionNo = latest.versionNo then { v with entry = entry } else v)
                                    store.Value <- store.Value |> Map.add entry.id updated
                                    Ok ()
                                | MintNextVersion ->
                                    let minted =
                                        {
                                            versionNo = latest.versionNo.next
                                            lifecycle = ActiveEntry
                                            pin = VersionUnpinned
                                            entry = entry
                                        }
                                    store.Value <- store.Value |> Map.add entry.id (versions @ [ minted ])
                                    Ok ())

                markMaterialInactive = fun id -> setLatestLifecycle id InactiveEntry
                markMaterialActive = fun id -> setLatestLifecycle id ActiveEntry
                // Supersede retires the latest version the same way — step 020 defines `InactiveEntry`
                // as "superseded / soft-deleted", so a superseded version behaves as inactive
                // automatically (hidden from offers, still resolvable by version). A distinct verb so
                // the UI's "Supersede" intent binds separately from the reversible "Mark inactive".
                supersedeMaterial = fun id -> setLatestLifecycle id InactiveEntry

                pinMaterialVersion =
                    fun version ->
                        match store.Value |> Map.tryFind version.materialId with
                        | Some versions when versions |> List.exists (fun v -> v.versionNo = version.version) ->
                            let updated =
                                versions
                                |> List.map (fun v ->
                                    if v.versionNo = version.version then
                                        {
                                            v with pin = VersionPinned
                                        }
                                    else v)
                            store.Value <- store.Value |> Map.add version.materialId updated
                            Ok ()
                        | _ -> Error (MaterialError.unknownMaterialVersion version)

                removeMaterial =
                    fun id ->
                        match store.Value |> Map.tryFind id with
                        | Some versions ->
                            match samplesReferencing id with
                            | referencing when not (List.isEmpty referencing) ->
                                let names = referencing |> List.map (fun s -> s.name)
                                Error (MaterialError.stillReferenced (latestOf versions).entry names)
                            | _ ->
                                let pinnedVersions =
                                    versions
                                    |> List.filter (fun v -> v.pin = VersionPinned)
                                    |> List.map (fun v -> v.versionNo)
                                match pinnedVersions with
                                | [] ->
                                    store.Value <- store.Value |> Map.remove id
                                    Ok ()
                                | _ ->
                                    Error (MaterialError.versionsPinned (latestOf versions).entry pinnedVersions)
                        | None -> Error (MaterialError.unknownMaterial id)
            }

        /// Empty material store using the shared versioning rule (spec 0038 step 042; spec 0050 §3.4).
        /// Sample references and pinned versions block removal (spec 0050 S2.2).
        static member createInMemory
            (samplesReferencing : MaterialId -> Sample list)
            : MaterialProxy =
            MaterialProxy.createInMemoryStore [] samplesReferencing
