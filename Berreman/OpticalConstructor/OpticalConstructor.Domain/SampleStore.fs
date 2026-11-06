namespace OpticalConstructor.Domain

open System
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle

/// Versioned in-memory sample storage with one-way per-version pins
/// (spec 0038 step 022; spec 0050 §3.4, S2.2). Stores start empty and catalogue import supplies built-ins.
module SampleStore =

    /// A stored sample version; only the latest is editable, while older versions remain resolvable.
    type private SampleVersionRecord =
        {
            versionNo : VersionNumber
            lifecycle : EntryLifecycle
            pin : PinState
            sample : Sample
        }

    /// The metadata half of a sample payload (spec 0038 Part H): the fields the rule treats as
    /// never-versioning — a name / description edit, and the `SubstrateKind` display/search facet
    /// (which the engine mapping never reads: `Propagation.resolveSampleMaterials` / `sampleToSystem`
    /// consume only the `SampleStructure`). The physics half is the `(=)`-comparable `SampleStructure`
    /// itself — the material/geometry tree the engine expands; unlike a material's engine `properties`
    /// it carries no function case, so structural equality is direct.
    type private SampleMetadata =
        {
            name : EntryName
            description : EntryDescription
            substrate : SubstrateKind
        }

    type SampleProxy with

        static member private createInMemoryStore (initialSamples : Sample list) : SampleProxy =

            let initialVersions =
                initialSamples
                |> List.map (fun s ->
                    let version =
                        {
                            versionNo = VersionNumber.first
                            lifecycle = ActiveEntry
                            pin = VersionUnpinned
                            sample = s
                        }
                    s.id, [ version ])
                |> Map.ofList
            let store = ref initialVersions

            let unknown (id : SampleId) : SampleError =
                UnknownSampleIdErr $"unknown sample id '%s{string id.value}'"

            /// The latest (highest-numbered) version of a non-empty history.
            let latestOf (versions : SampleVersionRecord list) : SampleVersionRecord =
                versions |> List.maxBy (fun v -> v.versionNo.value)

            /// The payload the shared rule sees: physics = the structure tree, metadata = the display
            /// fields (name / description / substrate facet).
            let payloadOf (s : Sample) : VersionPayload<SampleStructure, SampleMetadata> =
                {
                    physics = s.structure
                    metadata = { name = s.name; description = s.description; substrate = s.substrate }
                }

            /// The LATEST version of each sample, filtered by the show-inactive scope.
            let currentSamples (scope : InactiveVisibility) : Sample list =
                store.Value
                |> Map.toList
                |> List.choose (fun (_, versions) ->
                    let latest = latestOf versions
                    match scope, latest.lifecycle with
                    | IncludeInactive, _ -> Some latest.sample
                    | ActiveOnly, ActiveEntry -> Some latest.sample
                    | ActiveOnly, InactiveEntry -> None)

            /// Retire / revive the LATEST version of a known sample (the shared body of the three
            /// lifecycle verbs). Unknown id → `UnknownSampleId`; older versions are untouched.
            let setLatestLifecycle (id : SampleId) (lifecycle : EntryLifecycle) : Result<unit, SampleError> =
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
                listSamples = fun scope -> Ok (currentSamples scope)

                searchSamples =
                    fun q ->
                        let byText =
                            currentSamples ActiveOnly
                            |> List.filter (fun s -> s.name.value.IndexOf(q.text.value, StringComparison.OrdinalIgnoreCase) >= 0)
                        match q.substrate with
                        | Some kind -> Ok (byText |> List.filter (fun s -> s.substrate = kind))
                        | None -> Ok byText

                tryGetSample =
                    fun id -> Ok (store.Value |> Map.tryFind id |> Option.map (fun versions -> (latestOf versions).sample))

                resolveVersion =
                    fun svid ->
                        Ok (
                            store.Value
                            |> Map.tryFind svid.sampleId
                            |> Option.bind (fun versions -> versions |> List.tryFind (fun v -> v.versionNo = svid.version))
                            |> Option.map (fun v -> v.sample))

                saveSample =
                    fun sample ->
                        sample.validate
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind sample.id with
                            | None ->
                                let first =
                                    {
                                        versionNo = VersionNumber.first
                                        lifecycle = ActiveEntry
                                        pin = VersionUnpinned
                                        sample = sample
                                    }
                                store.Value <- store.Value |> Map.add sample.id [ first ]
                                Ok ()
                            | Some versions ->
                                let latest = latestOf versions
                                match decideVersioning (payloadOf latest.sample) (payloadOf sample) latest.pin with
                                | KeepCurrent -> Ok ()
                                | MutateInPlace ->
                                    let updated =
                                        versions
                                        |> List.map (fun v -> if v.versionNo = latest.versionNo then { v with sample = sample } else v)
                                    store.Value <- store.Value |> Map.add sample.id updated
                                    Ok ()
                                | MintNextVersion ->
                                    let minted =
                                        {
                                            versionNo = latest.versionNo.next
                                            lifecycle = ActiveEntry
                                            pin = VersionUnpinned
                                            sample = sample
                                        }
                                    store.Value <- store.Value |> Map.add sample.id (versions @ [ minted ])
                                    Ok ())

                markSampleInactive = fun id -> setLatestLifecycle id InactiveEntry
                markSampleActive = fun id -> setLatestLifecycle id ActiveEntry
                // Supersede retires the latest version the same way — step 020 defines `InactiveEntry`
                // as "superseded / soft-deleted", so a superseded version behaves as inactive
                // automatically (hidden from offers, still resolvable by version). A distinct verb so
                // the UI's "Supersede" intent binds separately from the reversible "Mark inactive".
                supersedeSample = fun id -> setLatestLifecycle id InactiveEntry

                pinSampleVersion =
                    fun version ->
                        match store.Value |> Map.tryFind version.sampleId with
                        | Some versions when versions |> List.exists (fun v -> v.versionNo = version.version) ->
                            let updated =
                                versions
                                |> List.map (fun v ->
                                    if v.versionNo = version.version then
                                        {
                                            v with pin = VersionPinned
                                        }
                                    else v)
                            store.Value <- store.Value |> Map.add version.sampleId updated
                            Ok ()
                        | _ -> Error (UnknownSampleIdErr $"unknown sample version '%s{string version.sampleId.value}' v%d{version.version.value}")

                removeSample =
                    fun id ->
                        match store.Value |> Map.tryFind id with
                        | Some versions ->
                            let pinnedVersions =
                                versions
                                |> List.filter (fun v -> v.pin = VersionPinned)
                                |> List.map (fun v -> v.versionNo.value)
                                |> List.sort
                            match pinnedVersions with
                            | [] ->
                                store.Value <- store.Value |> Map.remove id
                                Ok ()
                            | _ ->
                                let versionList = pinnedVersions |> List.map (fun n -> $"v%d{n}") |> String.concat ", "
                                Error (SampleVersionPinnedErr $"sample '%s{(latestOf versions).sample.name.value}' ('%s{string id.value}') has %d{List.length pinnedVersions} version(s) pinned: %s{versionList}")
                        | None -> Error (unknown id)
            }

        /// Empty sample store using the shared versioning rule (spec 0038 step 042; spec 0050 §3.4).
        /// Pinned versions block removal (spec 0050 S2.2).
        static member createInMemory () : SampleProxy =
            SampleProxy.createInMemoryStore []
