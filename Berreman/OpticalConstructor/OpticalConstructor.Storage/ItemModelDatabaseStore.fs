namespace OpticalConstructor.Storage

open OpticalConstructor.Database
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open Softellect.Sys

module ItemModelDatabaseStore =

    type ItemModelProxy with

        /// The item-model store over the library database (spec 0050 S7.1): it serves built-in and user item models
        /// alike and saves user item models only, because a built-in arrives through the import seam (step 95).
        [<ElevationBoundary(nameof StorageBoundary, "the SQLite item-model store (001 §C): every field is one LibraryDatabase.run over the entry-version rows")>]
        static member createInDatabase (database : LibraryDatabase) : ItemModelProxy =
            let storageResult (result : Result<'Outcome, StorageError>) : Result<'Outcome, ItemModelError> =
                result |> Result.mapError (fun error -> ItemModelStoreUnavailableErr error.describe)

            let runItemModel (work : LibraryDbContext -> Result<'Outcome, ItemModelError>) : Result<'Outcome, ItemModelError> =
                database.run work |> storageResult |> Result.bind id

            let decodeItemModel (stored : StoredEntryVersion) : Result<ItemModelVersion, ItemModelError> =
                let content = stored.envelope.content
                content.toItemModel
                |> Result.mapError (fun error -> ItemModelStoreUnavailableErr error.describe)
                |> Result.map (fun entry ->
                    let itemModel : ItemModelVersion =
                        {
                            version = content.version
                            entry = entry
                        }
                    itemModel)

            {
                listItemModels =
                    fun () ->
                        runItemModel (fun context ->
                            context.latestStoredOfKind ItemModelEntryKind
                            |> storageResult
                            |> Result.bind (fun versions ->
                                versions
                                |> List.fold (fun itemModels stored ->
                                    itemModels |> Result.bind (fun itemModels ->
                                        decodeItemModel stored |> Result.map (fun itemModel -> itemModel :: itemModels))) (Ok [])
                                |> Result.map (List.sortBy (fun (itemModel : ItemModelVersion) -> itemModel.entry.name, itemModel.entry.id))))
                resolveItemModelVersion =
                    fun versionId ->
                        runItemModel (fun context ->
                            context.tryStoredVersion versionId.itemModelId.toLibraryEntryId versionId.version
                            |> storageResult
                            |> Result.bind (fun storedOpt ->
                                match storedOpt with
                                | Some stored when stored.envelope.content.schema.kind = ItemModelEntryKind ->
                                    decodeItemModel stored |> Result.map Some
                                | Some _
                                | None -> Ok None))
                saveItemModel =
                    fun itemModel ->
                        let entry = itemModel.entry
                        runItemModel (fun context ->
                            context.tryLatestStored entry.id.toLibraryEntryId
                            |> storageResult
                            |> Result.bind (fun latestOpt ->
                                match latestOpt with
                                | Some latest when latest.envelope.content.schema.kind = ItemModelEntryKind ->
                                    let held = latest.envelope.content
                                    Error (DuplicateItemModelErr $"'{held.name.value}' ({entry.id.value}) at version {held.version.value}")
                                | Some latest ->
                                    let kind = latest.envelope.content.schema.kind
                                    Error (InvalidItemModelErr $"item model id '{entry.id.value}' is held by a {kind.extension.value} entry")
                                | None ->
                                    LibraryEntryContent.ofItemModel itemModel.version UserProvenance entry
                                    |> Result.mapError (fun error -> InvalidItemModelErr error.describe)
                                    |> Result.bind (fun content ->
                                        context.stageNewEntry content
                                        |> storageResult
                                        |> Result.map (fun () -> context.SaveChanges() |> ignore))))
            }
