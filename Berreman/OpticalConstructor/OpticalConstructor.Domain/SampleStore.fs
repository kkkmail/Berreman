namespace OpticalConstructor.Domain

open System
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle

/// Spec 0038 Part H (step 022, AC-H2) — the real, stateful in-memory SAMPLE store behind the
/// registered `SampleProxy` write-seam (STORE_XDUO_0002), now VERSIONED. This is the type
/// augmentation `SampleProxy.createInMemory`, moved here from `ElementId.fs` because the store
/// applies the step-20 shared `decideVersioning` rule (in `Lifecycle.fs`, which compiles AFTER
/// `ElementId.fs`); the `SampleProxy` record itself stays re-typed IN PLACE in `ElementId.fs`. The
/// exact mirror of step 021's `MaterialStore.fs`. Mutation stays INSIDE the closure (the IO
/// boundary), so logic holding the proxy stays pure. A test substitutes a stub of the SAME
/// `SampleProxy` shape or a fresh `createInMemory` over a stubbed `VersionsInUse`.
module SampleStore =

    /// One stored version of a sample: its version number, its live/retired lifecycle, and the
    /// sample frozen at that version. The store keeps a per-`SampleId` history of these, ascending
    /// by version number and non-empty; the LATEST (highest-numbered) version is the one the library
    /// edits and the offers surface, older versions are view-only resolution targets.
    type private VersionRecord =
        {
            versionNo : VersionNumber
            lifecycle : EntryLifecycle
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
            name : string
            description : string
            substrate : SubstrateKind
        }

    type SampleProxy with

        /// Build the versioned in-memory samples store. `versionsInUse` is the step-20 seam answering
        /// whether a specific version is currently bound by a live experiment — injected empty until
        /// step 25 wires the real one. The store seeds every sample in `seedEntries` as version 1
        /// active. Unlike the material store there is NO `samplesReferencing`-analogue: nothing
        /// structural references a sample; a sample is "used" only when a live experiment binds one of
        /// its versions (the `VersionsInUse` seam), which the used-version removal block guards.
        /// The shared versioned in-memory sample-store body (spec 0038 step 042 —
        /// IMPLEMENT_CONTRACT STORE_XDUO_0008), parameterised by the INITIAL samples so the
        /// SELF-SEEDING `createInMemory` and the EMPTY-store `createInMemoryEmpty` (which the
        /// seeding pipeline fills) never drift — everything below the seed line is identical. Each
        /// initial sample seeds as version 1 active.
        static member private createInMemoryStore (initialSamples : Sample list) (versionsInUse : VersionsInUse) : SampleProxy =

            let seeded =
                initialSamples
                |> List.map (fun s -> s.id, [ { versionNo = VersionNumber.first; lifecycle = ActiveEntry; sample = s } ])
                |> Map.ofList
            let store = ref seeded

            let unknown (id : SampleId) : SampleError =
                UnknownSampleId $"unknown sample id '%s{string id.value}'"

            /// The latest (highest-numbered) version of a non-empty history.
            let latestOf (versions : VersionRecord list) : VersionRecord =
                versions |> List.maxBy (fun v -> v.versionNo.value)

            /// The payload the shared rule sees: physics = the structure tree, metadata = the display
            /// fields (name / description / substrate facet).
            let payloadOf (s : Sample) : VersionPayload<SampleStructure, SampleMetadata> =
                {
                    physics = s.structure
                    metadata = { name = s.name; description = s.description; substrate = s.substrate }
                }

            /// Whether the versioned reference is bound by a live experiment, through the seam.
            let usageAt (id : SampleId) (versionNo : VersionNumber) : VersionUsage =
                usageOf versionsInUse (SampleVersionRef { sampleId = id; version = versionNo })

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
                            |> List.filter (fun s -> s.name.IndexOf(q.text, StringComparison.OrdinalIgnoreCase) >= 0)
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
                        validateSample sample
                        |> Result.bind (fun () ->
                            match store.Value |> Map.tryFind sample.id with
                            | None ->
                                // A brand-new sample starts at version 1, active.
                                store.Value <-
                                    store.Value
                                    |> Map.add sample.id [ { versionNo = VersionNumber.first; lifecycle = ActiveEntry; sample = sample } ]
                                Ok ()
                            | Some versions ->
                                let latest = latestOf versions
                                let usage = usageAt sample.id latest.versionNo
                                match decideVersioning (=) (=) (payloadOf latest.sample) (payloadOf sample) usage with
                                | KeepCurrent -> Ok ()
                                | MutateInPlace ->
                                    let updated =
                                        versions
                                        |> List.map (fun v -> if v.versionNo = latest.versionNo then { v with sample = sample } else v)
                                    store.Value <- store.Value |> Map.add sample.id updated
                                    Ok ()
                                | MintNextVersion ->
                                    let minted = { versionNo = latest.versionNo.next; lifecycle = ActiveEntry; sample = sample }
                                    store.Value <- store.Value |> Map.add sample.id (versions @ [ minted ])
                                    Ok ())

                markSampleInactive = fun id -> setLatestLifecycle id InactiveEntry
                markSampleActive = fun id -> setLatestLifecycle id ActiveEntry
                // Supersede retires the latest version the same way — step 020 defines `InactiveEntry`
                // as "superseded / soft-deleted", so a superseded version behaves as inactive
                // automatically (hidden from offers, still resolvable by version). A distinct verb so
                // the UI's "Supersede" intent binds separately from the reversible "Mark inactive".
                supersedeSample = fun id -> setLatestLifecycle id InactiveEntry

                removeSample =
                    fun id ->
                        match store.Value |> Map.tryFind id with
                        | Some versions ->
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
                                Error (SampleVersionInUse $"sample '%s{(latestOf versions).sample.name}' ('%s{string id.value}') has %d{List.length usedVersions} version(s) bound by a live experiment: %s{versionList}")
                        | None -> Error (unknown id)
            }

        /// The SELF-SEEDING versioned in-memory sample store (spec 0038 step 022; the app
        /// composition root keeps calling this unchanged): seeds `SeedSamples.all` (the SampleItem
        /// entries of `seedEntries`) as version 1 active.
        static member createInMemory (versionsInUse : VersionsInUse) : SampleProxy =
            SampleProxy.createInMemoryStore SeedSamples.all versionsInUse

        /// The EMPTY versioned in-memory sample store (spec 0038 step 042): a BLANK store the seeding
        /// pipeline (`SeedingProxy`) fills through `saveSample`. Same versioning/remove behaviour as
        /// the seeded store — only the initial samples differ.
        static member createInMemoryEmpty (versionsInUse : VersionsInUse) : SampleProxy =
            SampleProxy.createInMemoryStore [] versionsInUse
