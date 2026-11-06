namespace OpticalConstructor.Domain

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

    /// Sample display fields whose edits never mint a version (spec 0038 Part H; spec 0050 S2.3).
    type SampleMetadata =
        {
            name : EntryName
            description : EntryDescription
            substrate : SubstrateKind
        }

    type Sample with

        /// Comparable structure physics and display metadata for the shared versioning rule (spec 0050 S2.3).
        member this.versionPayload : VersionPayload<SampleStructure, SampleMetadata> =
            {
                physics = this.structure
                metadata =
                    {
                        name = this.name
                        description = this.description
                        substrate = this.substrate
                    }
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

            /// The latest (highest-numbered) version of a non-empty history.
            let latestOf (versions : SampleVersionRecord list) : SampleVersionRecord =
                versions |> List.maxBy (fun v -> v.versionNo.value)

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
                | None -> Error (SampleError.unknownSample id)

            {
                listSamples = fun scope -> Ok (currentSamples scope)

                searchSamples = fun q -> currentSamples ActiveOnly |> q.selectFrom |> Ok

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
                                match decideVersioning latest.sample.versionPayload sample.versionPayload latest.pin with
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
                        | _ -> Error (SampleError.unknownSampleVersion version)

                removeSample =
                    fun id ->
                        match store.Value |> Map.tryFind id with
                        | Some versions ->
                            let pinnedVersions =
                                versions
                                |> List.filter (fun v -> v.pin = VersionPinned)
                                |> List.map (fun v -> v.versionNo)
                            match pinnedVersions with
                            | [] ->
                                store.Value <- store.Value |> Map.remove id
                                Ok ()
                            | _ ->
                                Error (SampleError.versionsPinned (latestOf versions).sample pinnedVersions)
                        | None -> Error (SampleError.unknownSample id)
            }

        /// Empty sample store using the shared versioning rule (spec 0038 step 042; spec 0050 §3.4).
        /// Pinned versions block removal (spec 0050 S2.2).
        static member createInMemory () : SampleProxy =
            SampleProxy.createInMemoryStore []
