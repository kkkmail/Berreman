namespace OpticalConstructor.Storage

open OpticalConstructor.Database
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.SampleDatabaseStore
open OpticalConstructor.Storage.SourceDatabaseStore
open Softellect.Sys

module PresetDatabaseStore =

    type LibraryEntryStore with

        [<ElevationBoundary(nameof StorageBoundary, "the SQLite preset store (001 §C): every field is one LibraryDatabase.run over the entry-version rows")>]
        static member createInDatabase (database : LibraryDatabase) : LibraryEntryStore =
            let storageResult (result : Result<'Outcome, StorageError>) : Result<'Outcome, LibraryError> =
                result |> Result.mapError (fun error -> LibraryUnavailableErr error.describe)

            let runPreset (work : LibraryDbContext -> Result<'Outcome, LibraryError>) : Result<'Outcome, LibraryError> =
                database.run work |> storageResult |> Result.bind id

            let decodePreset (stored : StoredEntryVersion) : Result<LibraryEntry option, LibraryError> =
                let content = stored.envelope.content
                let decoded : Result<LibraryEntry option, LibraryEntryError> =
                    match content.schema.kind with
                    | DetectorEntryKind -> content.toDetectorPreset |> Result.map (DetectorItem >> Some)
                    | PolarizerEntryKind -> content.toPolarizerPreset |> Result.map (PolarizerItem >> Some)
                    | SourceEntryKind ->
                        match content.provenance.protection with
                        | ProtectedBuiltIn -> content.toSourcePreset |> Result.map (SourceItem >> Some)
                        | UserManaged -> Ok None
                    | MaterialEntryKind | CategoryEntryKind | SampleEntryKind -> Ok None
                decoded |> Result.mapError (fun error -> LibraryUnavailableErr error.describe)

            let offeredPresets (context : LibraryDbContext) : Result<LibraryEntry list, LibraryError> =
                [ DetectorEntryKind; PolarizerEntryKind; SourceEntryKind ]
                |> List.fold (fun entries kind ->
                    entries |> Result.bind (fun entries ->
                        context.latestStoredOfKind kind
                        |> storageResult
                        |> Result.bind (fun versions ->
                            versions
                            |> List.filter (fun stored -> stored.state.lifecycle = ActiveEntry)
                            |> List.fold (fun collected stored ->
                                collected |> Result.bind (fun collected ->
                                    decodePreset stored
                                    |> Result.map (fun entryOpt ->
                                        match entryOpt with
                                        | Some entry -> entry :: collected
                                        | None -> collected))) (Ok entries)))) (Ok [])
                |> Result.map (List.sortBy (fun entry -> entry.entryName, entry.entryId))

            let proxy : LibraryProxy =
                {
                    entriesForKind =
                        fun kind ->
                            runPreset (fun context ->
                                offeredPresets context
                                |> Result.map (List.filter (fun entry -> entry.forKinds |> List.contains kind)))
                    libraryTrees =
                        fun () ->
                            runPreset (fun context ->
                                offeredPresets context |> Result.map (fun entries -> [ LibraryTree.byKind entries ]))
                    tryGetEntry =
                        fun entryId ->
                            runPreset (fun context ->
                                match entryId.tryToLibraryEntryId with
                                | None -> Ok None
                                | Some id ->
                                    context.tryLatestStored id
                                    |> storageResult
                                    |> Result.bind (fun storedOpt ->
                                        match storedOpt with
                                        | Some stored -> decodePreset stored
                                        | None -> Ok None))
                }

            {
                proxy = proxy
                addEntry =
                    fun entry ->
                        runPreset (fun context ->
                            let builder : Result<EntryProvenance -> Result<LibraryEntryContent, LibraryEntryError>, LibraryError> =
                                match entry with
                                | SampleItem sample ->
                                    Error (PresetRefusedErr $"sample '{sample.name.value}' ({entry.entryId.value}) belongs to the samples store")
                                | SourceItem source ->
                                    match source.protection with
                                    | UserManaged ->
                                        Error (PresetRefusedErr $"source '{source.name.value}' ({source.id.value}) belongs to the user-sources store")
                                    | ProtectedBuiltIn ->
                                        Ok (fun provenance -> LibraryEntryContent.ofSourcePreset VersionNumber.first provenance source)
                                | DetectorItem detector ->
                                    Ok (fun provenance -> LibraryEntryContent.ofDetectorPreset VersionNumber.first provenance detector)
                                | PolarizerItem polarizer ->
                                    Ok (fun provenance -> LibraryEntryContent.ofPolarizerPreset VersionNumber.first provenance polarizer)
                            builder |> Result.bind (fun buildContent ->
                                let existing =
                                    match entry.entryId.tryToLibraryEntryId with
                                    | None -> Ok None
                                    | Some id -> context.tryLatestStored id |> storageResult
                                existing |> Result.bind (fun storedOpt ->
                                    match storedOpt with
                                    | Some _ -> Error (LibraryError.entryAlreadyHeld entry)
                                    | None ->
                                        let provenance =
                                            match entry.protection with
                                            | ProtectedBuiltIn -> BuiltInProvenance CatalogueIds.builtInCatalogue
                                            | UserManaged -> UserProvenance
                                        buildContent provenance
                                        |> Result.mapError (fun error -> PresetRefusedErr error.describe)
                                        |> Result.bind (fun content -> context.stageNewEntry content |> storageResult)
                                        |> Result.map (fun () -> context.SaveChanges() |> ignore))))
            }

    type LibraryProxy with

        static member createInDatabase (database : LibraryDatabase) : LibraryProxy =
            liveLibrary
                (LibraryEntryStore.createInDatabase database).proxy
                (SampleProxy.createInDatabase database)
                (SourceProxy.createInDatabase database)
