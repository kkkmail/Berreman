namespace OpticalConstructor.Storage

open OpticalConstructor.Database
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open Softellect.Sys

module CategoryDatabaseStore =

    type CategoryProxy with

        [<ElevationBoundary(nameof StorageBoundary, "the SQLite category store (001 §C): every field is one LibraryDatabase.run over the entry-version rows")>]
        static member createInDatabase (database : LibraryDatabase) : CategoryProxy =
            let storageResult (result : Result<'Outcome, StorageError>) : Result<'Outcome, CategoryError> =
                result |> Result.mapError (fun error -> CategoryStoreUnavailableErr error.describe)

            let runCategory (work : LibraryDbContext -> Result<'Outcome, CategoryError>) : Result<'Outcome, CategoryError> =
                database.run work |> storageResult |> Result.bind id

            let decodeCategory (stored : StoredEntryVersion) : Result<MaterialCategory, CategoryError> =
                stored.envelope.content.toMaterialCategory
                |> Result.mapError (fun error -> CategoryStoreUnavailableErr error.describe)

            let tryLatestCategory (context : LibraryDbContext) (categoryId : CategoryId) : Result<StoredEntryVersion option, CategoryError> =
                context.tryLatestStored categoryId.toLibraryEntryId
                |> storageResult
                |> Result.map (Option.filter (fun stored -> stored.envelope.content.schema.kind = CategoryEntryKind))

            let commitCategory (context : LibraryDbContext) (staged : Result<unit, StorageError>) : Result<unit, CategoryError> =
                staged |> storageResult |> Result.map (fun () -> context.SaveChanges() |> ignore)

            let referencingMaterialNames (context : LibraryDbContext) (categoryId : CategoryId) : Result<EntryName list, CategoryError> =
                context.latestStoredOfKind MaterialEntryKind
                |> storageResult
                |> Result.bind (fun versions ->
                    versions
                    |> List.fold (fun names stored ->
                        names |> Result.bind (fun names ->
                            stored.envelope.content.toMaterialEntry
                            |> Result.mapError (fun error -> CategoryStoreUnavailableErr error.describe)
                            |> Result.map (fun entry ->
                                if entry.category = categoryId then entry.name :: names else names))) (Ok []))

            {
                listCategories =
                    fun () ->
                        runCategory (fun context ->
                            context.latestStoredOfKind CategoryEntryKind
                            |> storageResult
                            |> Result.bind (fun versions ->
                                versions
                                |> List.fold (fun categories stored ->
                                    categories |> Result.bind (fun categories ->
                                        decodeCategory stored |> Result.map (fun category -> category :: categories))) (Ok [])
                                |> Result.map (List.sortBy (fun category -> category.id))))
                addCategory =
                    fun category ->
                        category.validate
                        |> Result.bind (fun () ->
                            runCategory (fun context ->
                                context.tryLatestStored category.id.toLibraryEntryId
                                |> storageResult
                                |> Result.bind (fun latestOpt ->
                                    match latestOpt with
                                    | Some latest when latest.envelope.content.schema.kind = CategoryEntryKind ->
                                        decodeCategory latest
                                        |> Result.bind (fun existing -> Error (CategoryError.duplicateCategory existing))
                                    | Some latest ->
                                        let kind = latest.envelope.content.schema.kind
                                        Error (DuplicateCategoryIdErr $"category id '{category.id.value}' is held by a {kind.extension.value} entry")
                                    | None ->
                                        let provenance =
                                            match category.origin with
                                            | BuiltInCategory -> BuiltInProvenance CatalogueIds.builtInCatalogue
                                            | UserCategory -> UserProvenance
                                        LibraryEntryContent.ofMaterialCategory VersionNumber.first provenance category
                                        |> Result.mapError (fun error -> InvalidCategoryErr error.describe)
                                        |> Result.bind (fun content -> context.stageNewEntry content |> commitCategory context))))
                updateCategory =
                    fun category ->
                        category.validate
                        |> Result.bind (fun () ->
                            runCategory (fun context ->
                                tryLatestCategory context category.id
                                |> Result.bind (fun latestOpt ->
                                    match latestOpt with
                                    | None -> Error (CategoryError.unknownCategory category.id)
                                    | Some latest ->
                                        decodeCategory latest
                                        |> Result.bind (fun previous ->
                                            LibraryEntryContent.ofMaterialCategory latest.envelope.content.version latest.envelope.content.provenance category
                                            |> Result.mapError (fun error -> InvalidCategoryErr error.describe)
                                            |> Result.bind (fun content ->
                                                let storedPayload : VersionPayload<CategoryVisibility, CategoryName> =
                                                    {
                                                        physics = previous.visibility
                                                        metadata = previous.name
                                                    }
                                                let incomingPayload : VersionPayload<CategoryVisibility, CategoryName> =
                                                    {
                                                        physics = category.visibility
                                                        metadata = category.name
                                                    }
                                                let decision = decideVersioning storedPayload incomingPayload latest.state.pin
                                                context.stageEdit latest decision content |> commitCategory context)))))
                removeCategory =
                    fun categoryId ->
                        runCategory (fun context ->
                            tryLatestCategory context categoryId
                            |> Result.bind (fun latestOpt ->
                                match latestOpt with
                                | None -> Error (CategoryError.unknownCategory categoryId)
                                | Some latest ->
                                    decodeCategory latest
                                    |> Result.bind (fun category ->
                                        match category.origin with
                                        | BuiltInCategory -> Error (CategoryError.builtInNotRemovable category)
                                        | UserCategory ->
                                            referencingMaterialNames context categoryId
                                            |> Result.bind (fun names ->
                                                match names with
                                                | _ :: _ -> Error (CategoryError.stillReferenced category names)
                                                | [] ->
                                                    context.stageRemoval categoryId.toLibraryEntryId
                                                    context.SaveChanges() |> ignore
                                                    Ok ()))))
            }
