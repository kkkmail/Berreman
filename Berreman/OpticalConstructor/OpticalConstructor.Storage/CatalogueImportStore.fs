namespace OpticalConstructor.Storage

open System
open OpticalConstructor.Database
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open Softellect.Sys

module CatalogueImportStore =

    type CatalogueImportProxy with

        [<ElevationBoundary(nameof StorageBoundary, "the SQLite import seam (001 §B, §C): every field is one LibraryDatabase.run over the entry-version rows")>]
        static member createInDatabase (database : LibraryDatabase) : CatalogueImportProxy =
            let storageResult (result : Result<'Outcome, StorageError>) : Result<'Outcome, CatalogueImportError> =
                result |> Result.mapError (fun error -> ImportStoreUnavailableErr error.describe)

            let runImport (work : LibraryDbContext -> Result<'Outcome, CatalogueImportError>) : Result<'Outcome, CatalogueImportError> =
                database.run work |> storageResult |> Result.bind id

            let tryHeld (context : LibraryDbContext) (id : LibraryEntryId) : Result<HeldEntry option, CatalogueImportError> =
                context.tryLatestStored id
                |> storageResult
                |> Result.map (Option.map (fun latest ->
                    let content = latest.envelope.content
                    let versions =
                        query
                            {
                                for row in context.EntryVersions do
                                where (row.EntryId = id.value)
                                sortBy row.Version
                                select (row.Version, row.ContentHash)
                            }
                        |> Seq.map (fun (version, hash) ->
                            {
                                version = VersionNumber version
                                contentHash = ContentHash hash
                            })
                        |> Seq.toList
                    {
                        id = id
                        kind = content.schema.kind
                        name = content.name
                        latest = content.version
                        lifecycle = latest.state.lifecycle
                        provenance = content.provenance
                        versions = versions
                    }))

            let collectHeld (context : LibraryDbContext) (ids : LibraryEntryId list) : Result<HeldEntry list, CatalogueImportError> =
                ids
                |> List.fold (fun result id ->
                    result |> Result.bind (fun entries ->
                        tryHeld context id
                        |> Result.map (function Some held -> held :: entries | None -> entries))) (Ok [])
                |> Result.map List.rev

            {
                tryHeldEntry = fun id -> runImport (fun context -> tryHeld context id)
                heldEntriesNamed = fun kind name ->
                    runImport (fun context ->
                        query
                            {
                                for latest in context.LatestVersions do
                                join row in context.EntryVersions on (latest.EntryId = row.EntryId)
                                where (row.Version = latest.Version && row.Kind = kind.value && row.Name = name.value)
                                select latest.EntryId
                            }
                        |> Seq.map LibraryEntryId |> Seq.toList |> collectHeld context)
                heldEntriesOf = fun catalogueId ->
                    runImport (fun context ->
                        query
                            {
                                for latest in context.LatestVersions do
                                join row in context.EntryVersions on (latest.EntryId = row.EntryId)
                                where (row.Version = latest.Version && row.CatalogueId = Nullable catalogueId.value)
                                select latest.EntryId
                            }
                        |> Seq.map LibraryEntryId |> Seq.toList |> collectHeld context)
                putEntryVersion = fun put ->
                    runImport (fun context ->
                        let content = put.envelope.content
                        context.tryStoredVersion content.id content.version
                        |> storageResult
                        |> Result.bind (function
                            | Some _ ->
                                Error (ImportRejectedErr $"entry '{content.name.value}' ({content.id.value}) already holds version {content.version.value}")
                            | None ->
                                context.tryLatestStored content.id
                                |> storageResult
                                |> Result.bind (fun latestOpt ->
                                    match latestOpt with
                                    | Some latest when latest.envelope.content.schema.kind <> content.schema.kind ->
                                        Error (ImportRejectedErr $"entry '{content.name.value}' ({content.id.value}) is held as {latest.envelope.content.schema.kind.extension.value}, not {content.schema.kind.extension.value}")
                                    | _ ->
                                        let stored =
                                            {
                                                envelope = put.envelope
                                                state =
                                                    {
                                                        lifecycle = ActiveEntry
                                                        pin = VersionUnpinned
                                                    }
                                            }
                                        context.stageVersion stored
                                        |> storageResult
                                        |> Result.map (fun () ->
                                            match put.latest, latestOpt with
                                            | MoveLatestVersion, _ | _, None -> context.stageLatest content.id content.version
                                            | KeepLatestVersion, Some _ -> ()
                                            context.SaveChanges() |> ignore))))
                retireEntry = fun id ->
                    runImport (fun context ->
                        context.tryLatestStored id
                        |> storageResult
                        |> Result.bind (function
                            | None -> Error (ImportRejectedErr $"entry {id.value} is not held and cannot be retired")
                            | Some latest ->
                                let retired =
                                    {
                                        latest with
                                            state =
                                                {
                                                    latest.state with
                                                        lifecycle = InactiveEntry
                                                }
                                    }
                                context.stageVersion retired
                                |> storageResult
                                |> Result.map (fun () -> context.SaveChanges() |> ignore)))
            }

    type CatalogueImportContext with

        /// Supplies canonical sealing and fresh entry identities at the IO boundary (spec 0050 S3.1).
        static member create (proxy : CatalogueImportProxy) : CatalogueImportContext =
            {
                proxy = proxy
                sealEntry = fun content ->
                    content.``sealed``
                    |> Result.mapError (fun error -> EntryHashingErr $"entry '{content.name.value}' ({content.id.value}): {error.describe}")
                newEntryId = fun () -> LibraryEntryId (Guid.NewGuid())
            }
