namespace OpticalConstructor.Storage

open OpticalConstructor.Database
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open Softellect.Sys

module SourceDatabaseStore =

    type SourceProxy with

        [<ElevationBoundary(nameof StorageBoundary, "the SQLite user-sources store (001 §C): every field is one LibraryDatabase.run over the entry-version rows")>]
        static member createInDatabase (database : LibraryDatabase) : SourceProxy =
            let storageResult (result : Result<'Outcome, StorageError>) : Result<'Outcome, SourceError> =
                result |> Result.mapError (fun error -> SourceStoreUnavailableErr error.describe)

            let runSource (work : LibraryDbContext -> Result<'Outcome, SourceError>) : Result<'Outcome, SourceError> =
                database.run work |> storageResult |> Result.bind id

            let decodeSource (stored : StoredEntryVersion) : Result<SourcePreset, SourceError> =
                stored.envelope.content.toSourcePreset
                |> Result.mapError (fun error -> SourceStoreUnavailableErr error.describe)

            let decodeSourceOpt (storedOpt : StoredEntryVersion option) : Result<SourcePreset option, SourceError> =
                match storedOpt with
                | Some stored -> decodeSource stored |> Result.map Some
                | None -> Ok None

            let tryUserSource (context : LibraryDbContext) (sourceId : ElementValueId) : Result<StoredEntryVersion option, SourceError> =
                match sourceId.tryToLibraryEntryId with
                | None -> Ok None
                | Some entryId ->
                    context.tryLatestStored entryId
                    |> storageResult
                    |> Result.map (Option.filter (fun stored ->
                        stored.envelope.content.schema.kind = SourceEntryKind
                        && stored.envelope.content.provenance.protection = UserManaged))

            let commitSource (context : LibraryDbContext) (staged : Result<unit, StorageError>) : Result<unit, SourceError> =
                staged |> storageResult |> Result.map (fun () -> context.SaveChanges() |> ignore)

            {
                listSources =
                    fun () ->
                        runSource (fun context ->
                            context.latestStoredOfKind SourceEntryKind
                            |> storageResult
                            |> Result.bind (fun versions ->
                                versions
                                |> List.filter (fun stored -> stored.envelope.content.provenance.protection = UserManaged)
                                |> List.fold (fun sources stored ->
                                    sources |> Result.bind (fun sources -> decodeSource stored |> Result.map (fun source -> source :: sources))) (Ok [])
                                |> Result.map (List.sortBy (fun (source : SourcePreset) -> source.name, source.id))))
                tryGetSource =
                    fun sourceId ->
                        runSource (fun context -> tryUserSource context sourceId |> Result.bind decodeSourceOpt)
                saveSource =
                    fun source ->
                        source.validate
                        |> Result.bind (fun () ->
                            runSource (fun context ->
                                match source.id.tryToLibraryEntryId with
                                | None -> Error (InvalidSourceErr $"source id '{source.id.value}' is not a canonical GUID")
                                | Some entryId ->
                                    context.tryLatestStored entryId
                                    |> storageResult
                                    |> Result.bind (fun latestOpt ->
                                        match latestOpt with
                                        | None ->
                                            LibraryEntryContent.ofSourcePreset VersionNumber.first UserProvenance source
                                            |> Result.mapError (fun error -> InvalidSourceErr error.describe)
                                            |> Result.bind (fun content -> context.stageNewEntry content |> commitSource context)
                                        | Some latest when latest.envelope.content.schema.kind <> SourceEntryKind ->
                                            let kind = latest.envelope.content.schema.kind
                                            Error (InvalidSourceErr $"source id '{source.id.value}' is held by a {kind.extension.value} entry")
                                        | Some latest ->
                                            let previous = latest.envelope.content
                                            match previous.provenance.protection with
                                            | ProtectedBuiltIn ->
                                                Error (ProtectedSourceRefusedErr $"source '{previous.name.value}' ({source.id.value}) is a protected built-in")
                                            | UserManaged ->
                                                LibraryEntryContent.ofSourcePreset previous.version previous.provenance source
                                                |> Result.mapError (fun error -> InvalidSourceErr error.describe)
                                                |> Result.bind (fun content ->
                                                    let storedPayload : VersionPayload<LibraryEntryPayload, EntryName> =
                                                        {
                                                            physics = previous.payload
                                                            metadata = previous.name
                                                        }
                                                    let incomingPayload : VersionPayload<LibraryEntryPayload, EntryName> =
                                                        {
                                                            physics = content.payload
                                                            metadata = content.name
                                                        }
                                                    let decision = decideVersioning storedPayload incomingPayload latest.state.pin
                                                    context.stageEdit latest decision content |> commitSource context))))
                removeSource =
                    fun sourceId ->
                        runSource (fun context ->
                            tryUserSource context sourceId
                            |> Result.bind (fun latestOpt ->
                                match latestOpt with
                                | None -> Error (SourceError.unknownSource sourceId)
                                | Some latest ->
                                    let content = latest.envelope.content
                                    context.storedVersionsOf content.id
                                    |> storageResult
                                    |> Result.bind (fun versions ->
                                        let pinned =
                                            versions
                                            |> List.filter (fun stored -> stored.state.pin = VersionPinned)
                                            |> List.map (fun stored -> stored.envelope.content.version)
                                        match pinned with
                                        | _ :: _ ->
                                            let listed = pinned |> List.map (fun version -> $"v{version.value}") |> String.concat ", "
                                            Error (SourceVersionPinnedErr $"source '{content.name.value}' ({sourceId.value}) has pinned versions: {listed}")
                                        | [] ->
                                            context.stageRemoval content.id
                                            context.SaveChanges() |> ignore
                                            Ok ())))
            }
