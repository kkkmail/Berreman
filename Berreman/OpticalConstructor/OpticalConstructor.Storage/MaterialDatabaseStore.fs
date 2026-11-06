namespace OpticalConstructor.Storage

open OpticalConstructor.Database
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open Softellect.Sys

module MaterialDatabaseStore =

    type MaterialProxy with

        [<ElevationBoundary(nameof StorageBoundary, "the SQLite materials store (001 §C): every field is one LibraryDatabase.run over the entry-version rows")>]
        static member createInDatabase (database : LibraryDatabase) : MaterialProxy =
            let storageResult (result : Result<'Outcome, StorageError>) : Result<'Outcome, MaterialError> =
                result |> Result.mapError (fun error -> MaterialStoreUnavailableErr error.describe)

            let runMaterial (work : LibraryDbContext -> Result<'Outcome, MaterialError>) : Result<'Outcome, MaterialError> =
                database.run work |> storageResult |> Result.bind id

            let decodeMaterial (stored : StoredEntryVersion) : Result<MaterialEntry, MaterialError> =
                stored.envelope.content.toMaterialEntry
                |> Result.mapError (fun error -> MaterialStoreUnavailableErr error.describe)

            let decodeMaterialOpt (storedOpt : StoredEntryVersion option) : Result<MaterialEntry option, MaterialError> =
                match storedOpt with
                | Some stored -> decodeMaterial stored |> Result.map Some
                | None -> Ok None

            let tryLatestMaterial (context : LibraryDbContext) (materialId : MaterialId) : Result<StoredEntryVersion option, MaterialError> =
                context.tryLatestStored materialId.toLibraryEntryId
                |> storageResult
                |> Result.map (Option.filter (fun stored -> stored.envelope.content.schema.kind = MaterialEntryKind))

            let tryMaterialVersion (context : LibraryDbContext) (version : MaterialVersionId) : Result<StoredEntryVersion option, MaterialError> =
                tryLatestMaterial context version.materialId
                |> Result.bind (fun latestOpt ->
                    match latestOpt with
                    | None -> Ok None
                    | Some _ -> context.tryStoredVersion version.materialId.toLibraryEntryId version.version |> storageResult)

            let commitMaterial (context : LibraryDbContext) (staged : Result<unit, StorageError>) : Result<unit, MaterialError> =
                staged |> storageResult |> Result.map (fun () -> context.SaveChanges() |> ignore)

            let listMaterials (scope : InactiveVisibility) : Result<MaterialEntry list, MaterialError> =
                runMaterial (fun context ->
                    context.latestStoredOfKind MaterialEntryKind
                    |> storageResult
                    |> Result.bind (fun versions ->
                        versions
                        |> List.filter (fun stored ->
                            match scope, stored.state.lifecycle with
                            | IncludeInactive, _
                            | ActiveOnly, ActiveEntry -> true
                            | ActiveOnly, InactiveEntry -> false)
                        |> List.fold (fun entries stored ->
                            entries |> Result.bind (fun entries -> decodeMaterial stored |> Result.map (fun entry -> entry :: entries))) (Ok [])
                        |> Result.map (List.sortBy (fun entry -> entry.id))))

            let setMaterialLifecycle (materialId : MaterialId) (lifecycle : EntryLifecycle) : Result<unit, MaterialError> =
                runMaterial (fun context ->
                    tryLatestMaterial context materialId
                    |> Result.bind (fun latestOpt ->
                        match latestOpt with
                        | None -> Error (MaterialError.unknownMaterial materialId)
                        | Some latest ->
                            let updated =
                                {
                                    latest with
                                        state =
                                            {
                                                latest.state with lifecycle = lifecycle
                                            }
                                }
                            context.stageVersion updated |> commitMaterial context))

            let referencingSampleNames (context : LibraryDbContext) (materialId : MaterialId) : Result<EntryName list, MaterialError> =
                query
                    {
                        for latest in context.LatestVersions do
                        join row in context.EntryVersions on (latest.EntryId = row.EntryId)
                        join reference in context.SampleMaterials on (row.EntryId = reference.SampleId)
                        where (row.Version = latest.Version
                               && row.Kind = SampleEntryKind.value
                               && row.Lifecycle = ActiveEntry.value
                               && reference.SampleVersion = row.Version
                               && reference.MaterialId = materialId.value)
                        select row
                        distinct
                    }
                |> Seq.fold (fun names row ->
                    names
                    |> Result.bind (fun names ->
                        row.toStoredVersion
                        |> storageResult
                        |> Result.bind (fun stored ->
                            stored.envelope.content.toSample
                            |> Result.mapError (fun error -> MaterialStoreUnavailableErr error.describe))
                        |> Result.map (fun sample -> sample.name :: names))) (Ok [])

            {
                listMaterials = listMaterials
                searchMaterials =
                    fun query ->
                        listMaterials ActiveOnly
                        |> Result.map (fun entries ->
                            let library : MaterialLibrary =
                                {
                                    entries = entries
                                }
                            byQuery query library)
                tryGetMaterial =
                    fun materialId ->
                        runMaterial (fun context -> tryLatestMaterial context materialId |> Result.bind decodeMaterialOpt)
                resolveVersion =
                    fun version ->
                        runMaterial (fun context -> tryMaterialVersion context version |> Result.bind decodeMaterialOpt)
                saveMaterial =
                    fun entry ->
                        entry.validate
                        |> Result.bind (fun () ->
                            runMaterial (fun context ->
                                context.tryLatestStored entry.id.toLibraryEntryId
                                |> storageResult
                                |> Result.bind (fun latestOpt ->
                                    match latestOpt with
                                    | None ->
                                        LibraryEntryContent.ofMaterialEntry VersionNumber.first UserProvenance entry
                                        |> context.stageNewEntry
                                        |> commitMaterial context
                                    | Some latest when latest.envelope.content.schema.kind <> MaterialEntryKind ->
                                        let kind = latest.envelope.content.schema.kind
                                        Error (DuplicateMaterialIdErr $"material id '{entry.id.value}' is held by a {kind.extension.value} entry")
                                    | Some latest ->
                                        decodeMaterial latest
                                        |> Result.bind (fun previous ->
                                            let decision = decideVersioning previous.versionPayload entry.versionPayload latest.state.pin
                                            let content = LibraryEntryContent.ofMaterialEntry latest.envelope.content.version latest.envelope.content.provenance entry
                                            context.stageEdit latest decision content |> commitMaterial context))))
                markMaterialInactive = fun materialId -> setMaterialLifecycle materialId InactiveEntry
                markMaterialActive = fun materialId -> setMaterialLifecycle materialId ActiveEntry
                supersedeMaterial = fun materialId -> setMaterialLifecycle materialId InactiveEntry
                pinMaterialVersion =
                    fun version ->
                        runMaterial (fun context ->
                            tryMaterialVersion context version
                            |> Result.bind (fun storedOpt ->
                                match storedOpt with
                                | None -> Error (MaterialError.unknownMaterialVersion version)
                                | Some stored ->
                                    let updated =
                                        {
                                            stored with
                                                state =
                                                    {
                                                        stored.state with pin = VersionPinned
                                                    }
                                        }
                                    context.stageVersion updated |> commitMaterial context))
                removeMaterial =
                    fun materialId ->
                        runMaterial (fun context ->
                            tryLatestMaterial context materialId
                            |> Result.bind (fun latestOpt ->
                                match latestOpt with
                                | None -> Error (MaterialError.unknownMaterial materialId)
                                | Some latest ->
                                    decodeMaterial latest
                                    |> Result.bind (fun entry ->
                                        referencingSampleNames context materialId
                                        |> Result.bind (fun names ->
                                            match names with
                                            | _ :: _ -> Error (MaterialError.stillReferenced entry names)
                                            | [] ->
                                                context.storedVersionsOf materialId.toLibraryEntryId
                                                |> storageResult
                                                |> Result.bind (fun versions ->
                                                    let pinned =
                                                        versions
                                                        |> List.filter (fun stored -> stored.state.pin = VersionPinned)
                                                        |> List.map (fun stored -> stored.envelope.content.version)
                                                    match pinned with
                                                    | _ :: _ -> Error (MaterialError.versionsPinned entry pinned)
                                                    | [] ->
                                                        context.stageRemoval materialId.toLibraryEntryId
                                                        context.SaveChanges() |> ignore
                                                        Ok ())))))
            }
