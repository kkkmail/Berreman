namespace OpticalConstructor.Storage

open System
open System.IO
open Softellect.Sys.Primitives
open OpticalConstructor.Database
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open Softellect.Sys

module CatalogueImportStore =

    type CatalogueExportTarget with

        /// Names a new catalogue export after its destination folder's last segment (spec 0050 S3.3).
        static member ofFolder (folder : FolderName) : CatalogueExportTarget =
            {
                folder = folder
                name = CatalogueName (Path.GetFileName(folder.value.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)))
            }

    type CatalogueImportLogEntry with

        [<ElevationBoundary(nameof StorageBoundary, "the catalogue import log seam (spec 0050 001 §B, §C): elevated import metadata becomes database columns")>]
        member this.importRow : LibraryCatalogueImportRow =
            LibraryCatalogueImportRow(
                CatalogueId = this.catalogueId.value,
                Folder = this.folder.value,
                ManifestHash = this.manifestHash.value,
                ImportedAtUtc = this.importedAt.value,
                EntryCount = this.entryCount.value)

    type LibraryCatalogueImportRow with

        [<ElevationBoundary(nameof StorageBoundary, "the catalogue import log seam (spec 0050 001 §B, §C): database columns become elevated metadata, restoring SQLite's unspecified time kind to UTC")>]
        member this.toLogEntry : CatalogueImportLogEntry =
            {
                catalogueId = CatalogueId this.CatalogueId
                folder = FolderName this.Folder
                manifestHash = ContentHash this.ManifestHash
                importedAt = CatalogueImportTime (DateTime.SpecifyKind(this.ImportedAtUtc, DateTimeKind.Utc))
                entryCount = CatalogueEntryCount this.EntryCount
            }

    type CatalogueImportProxy with

        [<ElevationBoundary(nameof StorageBoundary, "the SQLite import seam (001 §B, §C): every field is one LibraryDatabase.run over the entry-version rows or catalogue import log")>]
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
                tryHeldEnvelope = fun id version ->
                    runImport (fun context ->
                        context.tryStoredVersion id version
                        |> storageResult
                        |> Result.map (Option.map _.envelope))
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
                heldUserEntries = fun () ->
                    runImport (fun context ->
                        query
                            {
                                for latest in context.LatestVersions do
                                join row in context.EntryVersions on (latest.EntryId = row.EntryId)
                                where (row.Version = latest.Version && row.Protection = UserManaged.value)
                                select latest.EntryId
                            }
                        |> Seq.map LibraryEntryId |> Seq.toList |> collectHeld context)
                pinEntryVersion = fun id version ->
                    runImport (fun context ->
                        context.stagePin id version
                        |> storageResult
                        |> Result.bind (function
                            | None -> Error (ImportRejectedErr $"entry {id.value} version {version.value} is not held and cannot be pinned")
                            | Some _ ->
                                context.SaveChanges() |> ignore
                                Ok ()))
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
                loggedImports = fun () ->
                    runImport (fun context ->
                        context.CatalogueImports
                        |> Seq.map _.toLogEntry
                        |> Seq.sortWith (fun first second -> StringComparer.Ordinal.Compare(first.folder.value, second.folder.value))
                        |> Seq.toList
                        |> Ok)
                logImport = fun entry ->
                    runImport (fun context ->
                        let incoming = entry.importRow
                        match context.CatalogueImports.Find(incoming.CatalogueId) with
                        | null -> context.CatalogueImports.Add incoming |> ignore
                        | row -> context.Entry(row).CurrentValues.SetValues incoming
                        context.SaveChanges() |> ignore
                        Ok ())
                forgetImport = fun id ->
                    runImport (fun context ->
                        match context.CatalogueImports.Find(id.value) with
                        | null -> ()
                        | row ->
                            context.CatalogueImports.Remove row |> ignore
                            context.SaveChanges() |> ignore
                        Ok ())
            }

    type CatalogueImportContext with

        /// Supplies canonical hashes, fresh identities, the UTC clock and catalogue/entry-file IO (spec 0050 S3.1; S3.2; S3.3).
        [<ElevationBoundary(nameof IoBoundary, "the catalogue import composition root: canonical hashing, ambient clock, fresh identities and filesystem proxies")>]
        static member create (proxy : CatalogueImportProxy) : CatalogueImportContext =
            {
                proxy = proxy
                sealEntry = fun content ->
                    content.``sealed``
                    |> Result.mapError (fun error -> EntryHashingErr $"entry '{content.name.value}' ({content.id.value}): {error.describe}")
                newEntryId = fun () -> LibraryEntryId (Guid.NewGuid())
                newCatalogueId = fun () -> CatalogueId (Guid.NewGuid())
                hashManifest = fun manifest ->
                    manifest.manifestHash
                    |> Result.mapError (fun error ->
                        CatalogueUnreadableErr (CatalogueFileRejectedErr $"manifest '{manifest.name.value}' ({manifest.id.value}): {error.describe}"))
                now = fun () -> CatalogueImportTime DateTime.UtcNow
                openCatalogue = CatalogueProxy.create
                readEntryFile = fun kind file ->
                    LibraryFileText.readEntryFile kind file
                    |> Result.mapError (fun error -> DroppedFileRefusedErr $"{file.value}: {error.describe}")
                writeEntryFile = fun file envelope ->
                    envelope.fileText
                    |> Result.bind (fun text -> text.writeAtomically file)
                    |> Result.mapError (fun error -> ExportFileWriteErr $"{file.value}: {error.describe}")
                exportFolderOpt = None
            }
