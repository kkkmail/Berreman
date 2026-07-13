namespace OpticalConstructor.Domain

open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle

/// Spec 0038 Part H (step 021, AC-H2) — the real, stateful in-memory MATERIAL store behind the
/// registered `MaterialProxy` write-seam (STORE_XDUO_0001), now VERSIONED. This is the type
/// augmentation `MaterialProxy.createInMemory`, moved here from `ElementId.fs` because the store
/// applies the step-20 shared `decideVersioning` rule (in `Lifecycle.fs`, which compiles AFTER
/// `ElementId.fs`); the `MaterialProxy` record itself stays re-typed IN PLACE in `MaterialLibrary.fs`.
/// Mutation stays INSIDE the closure (the IO boundary), so logic holding the proxy stays pure. A
/// test substitutes a stub of the SAME `MaterialProxy` shape or a fresh `createInMemory` over a
/// stubbed `VersionsInUse`.
module MaterialStore =

    /// One stored version of a material: its version number, its live/retired lifecycle, and the
    /// entry payload frozen at that version. The store keeps a per-`MaterialId` history of these,
    /// ascending by version number and non-empty; the LATEST (highest-numbered) version is the one
    /// the library edits and the offers surface, older versions are view-only resolution targets.
    type private VersionRecord =
        {
            versionNo : VersionNumber
            lifecycle : EntryLifecycle
            entry : MaterialEntry
        }

    /// The metadata half of a material payload (spec 0038 Part H): the display fields the rule
    /// treats as never-versioning (a name / category / description edit mutates in place, never
    /// mints). The physics half is `MaterialComplexity option` — the ONLY `(=)`-comparable view of
    /// a material's optics (the engine `properties` carries function cases and cannot be compared),
    /// and for an editable entry `properties` is `complexity.toProperties`, so equal complexity ⇒
    /// equal physics. A coded preset (`complexity = None`) has physics that editing cannot change,
    /// so its `None = None` never mints.
    type private MaterialMetadata =
        {
            name : string
            category : CategoryId
            description : string option
        }

    type MaterialProxy with

        /// Build the versioned in-memory materials store. `samplesReferencing` is the
        /// composition-root lookup naming the samples that bind a material (the existing
        /// `MaterialStillReferenced` hard-block); `versionsInUse` is the step-20 seam answering
        /// whether a specific version is currently bound by a live experiment — injected empty
        /// until step 25 wires the real one. The store seeds `builtInEntries` as version 1 active.
        /// The shared versioned in-memory material-store body (spec 0038 step 042 —
        /// IMPLEMENT_CONTRACT STORE_XDUO_0008), parameterised by the INITIAL entries so the
        /// SELF-SEEDING `createInMemory` and the EMPTY-store `createInMemoryEmpty` (which the
        /// seeding pipeline fills) never drift — everything below the seed line is identical. Each
        /// initial entry seeds as version 1 active.
        static member private createInMemoryStore
            (initialEntries : MaterialEntry list)
            (samplesReferencing : MaterialId -> Sample list)
            (versionsInUse : VersionsInUse)
            : MaterialProxy =

            let seeded =
                initialEntries
                |> List.map (fun e ->
                    e.id, [ { versionNo = VersionNumber.first; lifecycle = ActiveEntry; entry = e } ])
                |> Map.ofList
            let store = ref seeded

            let unknown (id : MaterialId) : MaterialError =
                UnknownMaterialId $"unknown material id '%s{string id.value}'"

            /// The latest (highest-numbered) version of a non-empty history.
            let latestOf (versions : VersionRecord list) : VersionRecord =
                versions |> List.maxBy (fun v -> v.versionNo.value)

            /// The payload the shared rule sees: physics = the complexity tree, metadata = the
            /// display fields.
            let payloadOf (e : MaterialEntry) : VersionPayload<MaterialComplexity option, MaterialMetadata> =
                {
                    physics = e.complexity
                    metadata = { name = e.name; category = e.category; description = e.description }
                }

            /// Whether the versioned reference is bound by a live experiment, through the seam.
            let usageAt (id : MaterialId) (versionNo : VersionNumber) : VersionUsage =
                usageOf versionsInUse (MaterialVersionRef { materialId = id; version = versionNo })

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
                | None -> Error (unknown id)

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
                        validateEntry entry
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind entry.id with
                            | None ->
                                // A brand-new material starts at version 1, active.
                                store.Value <-
                                    store.Value
                                    |> Map.add entry.id [ { versionNo = VersionNumber.first; lifecycle = ActiveEntry; entry = entry } ]
                                Ok ()
                            | Some versions ->
                                let latest = latestOf versions
                                let usage = usageAt entry.id latest.versionNo
                                match decideVersioning (=) (=) (payloadOf latest.entry) (payloadOf entry) usage with
                                | KeepCurrent -> Ok ()
                                | MutateInPlace ->
                                    let updated =
                                        versions
                                        |> List.map (fun v -> if v.versionNo = latest.versionNo then { v with entry = entry } else v)
                                    store.Value <- store.Value |> Map.add entry.id updated
                                    Ok ()
                                | MintNextVersion ->
                                    let minted = { versionNo = latest.versionNo.next; lifecycle = ActiveEntry; entry = entry }
                                    store.Value <- store.Value |> Map.add entry.id (versions @ [ minted ])
                                    Ok ())

                markMaterialInactive = fun id -> setLatestLifecycle id InactiveEntry
                markMaterialActive = fun id -> setLatestLifecycle id ActiveEntry
                // Supersede retires the latest version the same way — step 020 defines `InactiveEntry`
                // as "superseded / soft-deleted", so a superseded version behaves as inactive
                // automatically (hidden from offers, still resolvable by version). A distinct verb so
                // the UI's "Supersede" intent binds separately from the reversible "Mark inactive".
                supersedeMaterial = fun id -> setLatestLifecycle id InactiveEntry

                removeMaterial =
                    fun id ->
                        match store.Value |> Map.tryFind id with
                        | Some versions ->
                            match samplesReferencing id with
                            | referencing when not (List.isEmpty referencing) ->
                                let names =
                                    referencing
                                    |> List.map (fun s -> $"'%s{s.name}'")
                                    |> List.sort
                                    |> String.concat ", "
                                Error (MaterialStillReferenced $"material '%s{(latestOf versions).entry.name}' ('%s{string id.value}') is still referenced by %d{List.length referencing} sample(s): %s{names}")
                            | _ ->
                                let usedVersions =
                                    versions
                                    |> List.filter (fun v -> usageAt id v.versionNo = VersionUsed)
                                    |> List.map (fun v -> v.versionNo.value)
                                    |> List.sort
                                match usedVersions with
                                | [] ->
                                    store.Value <- store.Value |> Map.remove id
                                    Ok ()
                                | _ ->
                                    let versionList = usedVersions |> List.map (fun n -> $"v%d{n}") |> String.concat ", "
                                    Error (MaterialVersionInUse $"material '%s{(latestOf versions).entry.name}' ('%s{string id.value}') has %d{List.length usedVersions} version(s) bound by a live experiment: %s{versionList}")
                        | None -> Error (unknown id)
            }

        /// The SELF-SEEDING versioned in-memory material store (spec 0038 step 021; the app
        /// composition root keeps calling this unchanged): seeds `builtInEntries` as version 1 active.
        static member createInMemory
            (samplesReferencing : MaterialId -> Sample list)
            (versionsInUse : VersionsInUse)
            : MaterialProxy =
            MaterialProxy.createInMemoryStore builtInEntries samplesReferencing versionsInUse

        /// The EMPTY versioned in-memory material store (spec 0038 step 042): a BLANK store the
        /// seeding pipeline (`SeedingProxy`) fills through `saveMaterial`. Same versioning/remove
        /// behaviour as the seeded store — only the initial entries differ.
        static member createInMemoryEmpty
            (samplesReferencing : MaterialId -> Sample list)
            (versionsInUse : VersionsInUse)
            : MaterialProxy =
            MaterialProxy.createInMemoryStore [] samplesReferencing versionsInUse
