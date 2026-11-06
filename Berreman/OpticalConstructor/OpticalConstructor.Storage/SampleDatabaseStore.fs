namespace OpticalConstructor.Storage

open OpticalConstructor.Database
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.SampleStore
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open Softellect.Sys

module SampleDatabaseStore =

    type SampleProxy with

        [<ElevationBoundary(nameof StorageBoundary, "the SQLite samples store (001 §C): every field is one LibraryDatabase.run over the entry-version rows")>]
        static member createInDatabase (database : LibraryDatabase) : SampleProxy =
            let storageResult (result : Result<'Outcome, StorageError>) : Result<'Outcome, SampleError> =
                result |> Result.mapError (fun error -> SampleStoreUnavailableErr error.describe)

            let runSample (work : LibraryDbContext -> Result<'Outcome, SampleError>) : Result<'Outcome, SampleError> =
                database.run work |> storageResult |> Result.bind id

            let decodeSample (stored : StoredEntryVersion) : Result<Sample, SampleError> =
                stored.envelope.content.toSample
                |> Result.mapError (fun error -> SampleStoreUnavailableErr error.describe)

            let decodeSampleOpt (storedOpt : StoredEntryVersion option) : Result<Sample option, SampleError> =
                match storedOpt with
                | Some stored -> decodeSample stored |> Result.map Some
                | None -> Ok None

            let tryLatestSample (context : LibraryDbContext) (sampleId : SampleId) : Result<StoredEntryVersion option, SampleError> =
                context.tryLatestStored sampleId.toLibraryEntryId
                |> storageResult
                |> Result.map (Option.filter (fun stored -> stored.envelope.content.schema.kind = SampleEntryKind))

            let trySampleVersion (context : LibraryDbContext) (version : SampleVersionId) : Result<StoredEntryVersion option, SampleError> =
                tryLatestSample context version.sampleId
                |> Result.bind (fun latestOpt ->
                    match latestOpt with
                    | None -> Ok None
                    | Some _ -> context.tryStoredVersion version.sampleId.toLibraryEntryId version.version |> storageResult)

            let commitSample (context : LibraryDbContext) (staged : Result<unit, StorageError>) : Result<unit, SampleError> =
                staged |> storageResult |> Result.map (fun () -> context.SaveChanges() |> ignore)

            let listSamples (scope : InactiveVisibility) : Result<Sample list, SampleError> =
                runSample (fun context ->
                    context.latestStoredOfKind SampleEntryKind
                    |> storageResult
                    |> Result.bind (fun versions ->
                        versions
                        |> List.filter (fun stored ->
                            match scope, stored.state.lifecycle with
                            | IncludeInactive, _
                            | ActiveOnly, ActiveEntry -> true
                            | ActiveOnly, InactiveEntry -> false)
                        |> List.fold (fun samples stored ->
                            samples |> Result.bind (fun samples -> decodeSample stored |> Result.map (fun sample -> sample :: samples))) (Ok [])
                        |> Result.map (List.sortBy (fun sample -> sample.id))))

            let setSampleLifecycle (sampleId : SampleId) (lifecycle : EntryLifecycle) : Result<unit, SampleError> =
                runSample (fun context ->
                    tryLatestSample context sampleId
                    |> Result.bind (fun latestOpt ->
                        match latestOpt with
                        | None -> Error (SampleError.unknownSample sampleId)
                        | Some latest ->
                            let updated =
                                {
                                    latest with
                                        state =
                                            {
                                                latest.state with lifecycle = lifecycle
                                            }
                                }
                            context.stageVersion updated |> commitSample context))

            {
                listSamples = listSamples
                searchSamples = fun query -> listSamples ActiveOnly |> Result.map query.selectFrom
                tryGetSample =
                    fun sampleId ->
                        runSample (fun context -> tryLatestSample context sampleId |> Result.bind decodeSampleOpt)
                resolveVersion =
                    fun version ->
                        runSample (fun context -> trySampleVersion context version |> Result.bind decodeSampleOpt)
                saveSample =
                    fun sample ->
                        sample.validate
                        |> Result.bind (fun () ->
                            runSample (fun context ->
                                context.tryLatestStored sample.id.toLibraryEntryId
                                |> storageResult
                                |> Result.bind (fun latestOpt ->
                                    match latestOpt with
                                    | None ->
                                        LibraryEntryContent.ofSample VersionNumber.first UserProvenance sample
                                        |> context.stageNewEntry
                                        |> commitSample context
                                    | Some latest when latest.envelope.content.schema.kind <> SampleEntryKind ->
                                        let kind = latest.envelope.content.schema.kind
                                        Error (DuplicateSampleIdErr $"sample id '{sample.id.value}' is held by a {kind.extension.value} entry")
                                    | Some latest ->
                                        decodeSample latest
                                        |> Result.bind (fun previous ->
                                            let decision = decideVersioning previous.versionPayload sample.versionPayload latest.state.pin
                                            let content = LibraryEntryContent.ofSample latest.envelope.content.version latest.envelope.content.provenance sample
                                            context.stageEdit latest decision content |> commitSample context))))
                markSampleInactive = fun sampleId -> setSampleLifecycle sampleId InactiveEntry
                markSampleActive = fun sampleId -> setSampleLifecycle sampleId ActiveEntry
                supersedeSample = fun sampleId -> setSampleLifecycle sampleId InactiveEntry
                pinSampleVersion =
                    fun version ->
                        runSample (fun context ->
                            trySampleVersion context version
                            |> Result.bind (fun storedOpt ->
                                match storedOpt with
                                | None -> Error (SampleError.unknownSampleVersion version)
                                | Some stored ->
                                    let updated =
                                        {
                                            stored with
                                                state =
                                                    {
                                                        stored.state with pin = VersionPinned
                                                    }
                                        }
                                    context.stageVersion updated |> commitSample context))
                removeSample =
                    fun sampleId ->
                        runSample (fun context ->
                            tryLatestSample context sampleId
                            |> Result.bind (fun latestOpt ->
                                match latestOpt with
                                | None -> Error (SampleError.unknownSample sampleId)
                                | Some latest ->
                                    decodeSample latest
                                    |> Result.bind (fun sample ->
                                        context.storedVersionsOf sampleId.toLibraryEntryId
                                        |> storageResult
                                        |> Result.bind (fun versions ->
                                            let pinned =
                                                versions
                                                |> List.filter (fun stored -> stored.state.pin = VersionPinned)
                                                |> List.map (fun stored -> stored.envelope.content.version)
                                            match pinned with
                                            | _ :: _ -> Error (SampleError.versionsPinned sample pinned)
                                            | [] ->
                                                context.stageRemoval sampleId.toLibraryEntryId
                                                context.SaveChanges() |> ignore
                                                Ok ()))))
            }
