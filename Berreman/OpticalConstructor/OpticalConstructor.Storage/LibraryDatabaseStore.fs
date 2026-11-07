namespace OpticalConstructor.Storage

open System
open System.Data
open System.IO
open System.Linq
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open OpticalConstructor.Database
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Facets
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.Errors
open Softellect.Sys
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary

module LibraryDatabaseStore =

    /// The lifecycle and pin state held beside an entry version's file (spec 0050 001 §C).
    type EntryVersionState =
        {
            lifecycle : EntryLifecycle
            pin : PinState
        }

    /// One stored entry file and the database state held beside it (spec 0050 001 §C).
    type StoredEntryVersion =
        {
            envelope : LibraryEntryEnvelope
            state : EntryVersionState
        }

    type EntryProvenance with

        member this.kindValue : int =
            match this with
            | BuiltInProvenance _ -> 1
            | UserProvenance -> 2
            | KitImportProvenance _ -> 3
            | SourcePullProvenance _ -> 4
            | ForkProvenance _ -> 5

        member this.catalogueIdOpt : CatalogueId option =
            match this with
            | BuiltInProvenance catalogueId -> Some catalogueId
            | UserProvenance
            | KitImportProvenance _
            | SourcePullProvenance _
            | ForkProvenance _ -> None

    type StoredEntryVersion with

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): the row's columns and payload are written from its entry file and state")>]
        member this.versionRow : Result<LibraryEntryVersionRow, StorageError> =
            this.envelope.fileText
            |> Result.map (fun text ->
                let content = this.envelope.content
                let catalogueId =
                    match content.provenance.catalogueIdOpt with
                    | Some id -> Nullable id.value
                    | None -> Nullable<Guid>()

                LibraryEntryVersionRow(
                    EntryId = content.id.value,
                    Version = content.version.value,
                    Kind = content.schema.kind.value,
                    Name = content.name.value,
                    Lifecycle = this.state.lifecycle.value,
                    Protection = content.provenance.protection.value,
                    PinState = this.state.pin.value,
                    ProvenanceKind = content.provenance.kindValue,
                    CatalogueId = catalogueId,
                    PayloadText = text.value,
                    ContentHash = this.envelope.contentHash.value))

    type LibraryEntryEnvelope with

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (spec 0050 001 §C): an entry version's own discrete facet keys become relational rows")>]
        member this.facetRows : Result<LibraryEntryFacetRow list, StorageError> =
            let content = this.content

            let discretePairs (attributes : AttributeDef<'Item> list) (entry : 'Item) : (AttributeKey * DiscreteKey) list =
                attributes
                |> List.collect (fun attribute ->
                    match attribute.appliesTo entry with
                    | InapplicableAttribute -> []
                    | ApplicableAttribute ->
                        attribute.extract entry
                        |> List.choose (fun value ->
                            match value with
                            | DiscreteValue key -> Some (attribute.key, key)
                            | NumericValue _ -> None))
                |> List.distinct

            let entryPairs (entry : LibraryEntry) : (AttributeKey * DiscreteKey) list =
                discretePairs (LibraryFacets.libraryFacets [] []) entry

            let pairs : Result<(AttributeKey * DiscreteKey) list, LibraryEntryError> =
                match content.schema.kind with
                | MaterialEntryKind -> content.toMaterialEntry |> Result.map (discretePairs (LibraryFacets.materialFacets []))
                | SampleEntryKind -> content.toSample |> Result.map (SampleItem >> entryPairs)
                | SourceEntryKind -> content.toSourcePreset |> Result.map (SourceItem >> entryPairs)
                | PolarizerEntryKind -> content.toPolarizerPreset |> Result.map (PolarizerItem >> entryPairs)
                | DetectorEntryKind -> content.toDetectorPreset |> Result.map (DetectorItem >> entryPairs)
                | CategoryEntryKind -> Ok []

            pairs
            |> Result.mapError (fun error -> LibraryRowInvalidErr error.describe)
            |> Result.map (List.map (fun (key, value) ->
                LibraryEntryFacetRow(
                    EntryId = content.id.value,
                    Version = content.version.value,
                    FacetKey = key.value,
                    FacetValue = value.value)))

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (spec 0050 001 §C): a sample version's referenced material versions become relational keys")>]
        member this.sampleMaterialRows : Result<LibrarySampleMaterialRow list, StorageError> =
            let content = this.content
            match content.schema.kind with
            | SampleEntryKind ->
                content.toSample
                |> Result.mapError (fun error -> LibraryRowInvalidErr error.describe)
                |> Result.map (fun sample ->
                    sample.structure.referencedMaterialVersions
                    |> Set.toList
                    |> List.map (fun materialVersion ->
                        LibrarySampleMaterialRow(
                            SampleId = content.id.value,
                            SampleVersion = content.version.value,
                            MaterialId = materialVersion.materialId.value,
                            MaterialVersion = materialVersion.version.value)))
            | MaterialEntryKind
            | SourceEntryKind
            | PolarizerEntryKind
            | DetectorEntryKind
            | CategoryEntryKind -> Ok []

    type LibraryEntryVersionRow with

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): row tags, the entry file and every projected column are validated on read")>]
        member this.toStoredVersion : Result<StoredEntryVersion, StorageError> =
            match LibraryEntryKind.tryOfWire this.Kind, EntryLifecycle.tryOfDb this.Lifecycle, PinState.tryOfDb this.PinState with
            | None, _, _ ->
                Error (LibraryRowInvalidErr $"entry {this.EntryId} v{this.Version}: column Kind has unknown tag {this.Kind}")
            | _, None, _ ->
                Error (LibraryRowInvalidErr $"entry {this.EntryId} v{this.Version}: column Lifecycle has unknown tag {this.Lifecycle}")
            | _, _, None ->
                Error (LibraryRowInvalidErr $"entry {this.EntryId} v{this.Version}: column PinState has unknown tag {this.PinState}")
            | Some kind, Some lifecycle, Some pin ->
                let text = LibraryFileText this.PayloadText
                text.parseEntry kind
                |> Result.bind (fun envelope ->
                    let stored : StoredEntryVersion =
                        {
                            envelope = envelope
                            state =
                                {
                                    lifecycle = lifecycle
                                    pin = pin
                                }
                        }

                    stored.versionRow
                    |> Result.bind (fun expected ->
                        let columns =
                            [
                                nameof this.EntryId, this.EntryId = expected.EntryId
                                nameof this.Version, this.Version = expected.Version
                                nameof this.Kind, this.Kind = expected.Kind
                                nameof this.Name, this.Name = expected.Name
                                nameof this.Lifecycle, this.Lifecycle = expected.Lifecycle
                                nameof this.Protection, this.Protection = expected.Protection
                                nameof this.PinState, this.PinState = expected.PinState
                                nameof this.ProvenanceKind, this.ProvenanceKind = expected.ProvenanceKind
                                nameof this.CatalogueId, this.CatalogueId = expected.CatalogueId
                                nameof this.PayloadText, this.PayloadText = expected.PayloadText
                                nameof this.ContentHash, this.ContentHash = expected.ContentHash
                            ]

                        match columns |> List.tryPick (fun (column, agrees) -> if agrees then None else Some column) with
                        | Some column ->
                            Error (LibraryRowInvalidErr $"entry {this.EntryId} v{this.Version}: column {column} differs from its entry file")
                        | None -> Ok stored))

    /// Whether openOrCreate created and stamped the schema or found it stamped.
    type LibraryDatabaseOrigin =
        | CreatedLibraryDatabase
        | OpenedLibraryDatabase

    /// Owns one open SQLite connection and serializes context use for its lifetime (spec 0050 001 §C).
    [<Sealed>]
    type LibraryDatabase private (connection : SqliteConnection, origin : LibraryDatabaseOrigin) =
        let syncRoot = obj ()

        /// Reports whether this handle created or opened the stamped schema (001 §C; spec 0050 S2.4).
        member _.origin : LibraryDatabaseOrigin = origin

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught here and mapped to a typed error")>]
        member _.run<'Outcome> (work : LibraryDbContext -> 'Outcome) : Result<'Outcome, StorageError> =
            try
                lock syncRoot (fun () ->
                    // Spec 0050 S2.1: EF would reopen a closed connection, reviving a disposed handle.
                    if connection.State <> ConnectionState.Open then
                        Error (LibraryDatabaseExn (ObjectDisposedException(nameof LibraryDatabase)))
                    else
                        let options = DbContextOptionsBuilder<LibraryDbContext>().UseSqlite(connection).Options
                        use context = new LibraryDbContext(options)
                        Ok (work context))
            with error ->
                Error (LibraryDatabaseExn error)

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): schema reads are shared by open and restore, whose boundaries catch every exception")>]
        static member private verifyStamp (context : LibraryDbContext) : Result<unit, StorageError> =
            use command = context.Database.GetDbConnection().CreateCommand()
            command.CommandText <- "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'SchemaStamp';"

            let foundOpt =
                if unbox<int64> (command.ExecuteScalar()) = 0L then
                    None
                else
                    context.SchemaStamps.AsNoTracking()
                    |> Seq.tryFind (fun row -> row.Id = 1)
                    |> Option.map (fun row -> LibrarySchemaVersion row.SchemaVersion)

            match foundOpt with
            | Some found when found = LibrarySchemaVersion.current -> Ok ()
            | _ ->
                Error (LibrarySchemaMismatchErr
                    {
                        foundOpt = foundOpt
                        expected = LibrarySchemaVersion.current
                    })

        /// Writes a stamped, compacted copy to a new file without creating its directory (001 §C; spec 0050 S2.4).
        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught here and mapped to a typed error")>]
        member _.vacuumInto (target : FileName) : Result<unit, StorageError> =
            try
                lock syncRoot (fun () ->
                    let targetPath = Path.GetFullPath(target.value)
                    // Spec 0050 S2.4: SQLite accepts empty existing targets, but a backup must preserve them.
                    if File.Exists(targetPath) then
                        Error (LibraryDatabaseExn (IOException($"The backup target already exists: {targetPath}")))
                    else
                        use command = connection.CreateCommand()
                        command.CommandText <- "VACUUM INTO $target;"
                        command.Parameters.AddWithValue("$target", targetPath) |> ignore
                        command.ExecuteNonQuery() |> ignore
                        Ok ())
            with error ->
                Error (LibraryDatabaseExn error)

        /// Validates the backup, saves the live library, then restores into this handle (001 §C; spec 0050 S2.4).
        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught here and mapped to a typed error")>]
        member this.restoreFrom (backup : FileName) (beforeRestore : FileName) : Result<unit, StorageError> =
            try
                lock syncRoot (fun () ->
                    let builder = SqliteConnectionStringBuilder()
                    builder.DataSource <- Path.GetFullPath(backup.value)
                    builder.Mode <- SqliteOpenMode.ReadOnly
                    builder.Pooling <- false
                    use backupConnection = new SqliteConnection(builder.ConnectionString)
                    backupConnection.Open()
                    let options = DbContextOptionsBuilder<LibraryDbContext>().UseSqlite(backupConnection).Options
                    use context = new LibraryDbContext(options)

                    LibraryDatabase.verifyStamp context
                    |> Result.bind (fun () -> this.vacuumInto beforeRestore)
                    |> Result.map (fun () -> backupConnection.BackupDatabase(connection)))
            with error ->
                Error (LibraryDatabaseExn error)

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught here and mapped to a typed error")>]
        static member openOrCreate (connectionString : ConnectionString) : Result<LibraryDatabase, StorageError> =
            try
                let builder = SqliteConnectionStringBuilder(connectionString.value)
                builder.Pooling <- false
                let isFileDatabase = builder.DataSource <> ":memory:" && builder.Mode <> SqliteOpenMode.Memory

                if isFileDatabase then
                    let directory = Path.GetDirectoryName(builder.DataSource)
                    if not (String.IsNullOrEmpty directory) then
                        Directory.CreateDirectory(directory) |> ignore

                let connection = new SqliteConnection(builder.ConnectionString)

                try
                    connection.Open()
                    let options = DbContextOptionsBuilder<LibraryDbContext>().UseSqlite(connection).Options
                    use context = new LibraryDbContext(options)

                    let initialization =
                        use command = connection.CreateCommand()
                        command.CommandText <- "PRAGMA foreign_keys = ON;"
                        command.ExecuteNonQuery() |> ignore

                        // Spec 0050 S21-OPEN-STAMP: WAL changes the file header, so reject foreign schemas first.
                        command.CommandText <- "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name NOT GLOB 'sqlite_*';"
                        let preflight =
                            if unbox<int64> (command.ExecuteScalar()) = 0L then Ok ()
                            else LibraryDatabase.verifyStamp context

                        preflight
                        |> Result.bind (fun () ->
                            if isFileDatabase then
                                command.CommandText <- "PRAGMA journal_mode = WAL;"
                                command.ExecuteNonQuery() |> ignore

                            if context.Database.EnsureCreated() then
                                let stamp = LibrarySchemaStampRow(Id = 1, SchemaVersion = LibrarySchemaVersion.current.value)
                                context.SchemaStamps.Add(stamp) |> ignore
                                context.SaveChanges() |> ignore
                                Ok CreatedLibraryDatabase
                            else
                                LibraryDatabase.verifyStamp context |> Result.map (fun () -> OpenedLibraryDatabase))

                    match initialization with
                    | Ok origin -> Ok (new LibraryDatabase(connection, origin))
                    | Error error ->
                        connection.Dispose()
                        Error error
                with error ->
                    connection.Dispose()
                    Error (LibraryDatabaseExn error)
            with error ->
                Error (LibraryDatabaseExn error)

        interface IDisposable with
            member _.Dispose () : unit =
                lock syncRoot (fun () -> connection.Dispose())

    type LibraryDbContext with

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught by LibraryDatabase.run")>]
        member this.tryStoredVersion (id : LibraryEntryId) (version : VersionNumber) : Result<StoredEntryVersion option, StorageError> =
            match this.EntryVersions.Find(id.value, version.value) with
            | null -> Ok None
            | row -> row.toStoredVersion |> Result.map Some

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught by LibraryDatabase.run")>]
        member this.tryLatestStored (id : LibraryEntryId) : Result<StoredEntryVersion option, StorageError> =
            match this.LatestVersions.Find(id.value) with
            | null -> Ok None
            | latest ->
                let version = VersionNumber latest.Version
                this.tryStoredVersion id version

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught by LibraryDatabase.run")>]
        member this.storedVersionsOf (id : LibraryEntryId) : Result<StoredEntryVersion list, StorageError> =
            query
                {
                    for row in this.EntryVersions do
                    where (row.EntryId = id.value)
                    sortBy row.Version
                    select row
                }
            |> Seq.fold (fun versions row ->
                versions |> Result.bind (fun versions -> row.toStoredVersion |> Result.map (fun stored -> stored :: versions))) (Ok [])
            |> Result.map List.rev

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught by LibraryDatabase.run")>]
        member this.latestStoredOfKind (kind : LibraryEntryKind) : Result<StoredEntryVersion list, StorageError> =
            query
                {
                    for latest in this.LatestVersions do
                    join row in this.EntryVersions on (latest.EntryId = row.EntryId)
                    where (row.Version = latest.Version && row.Kind = kind.value)
                    select row
                }
            |> Seq.fold (fun versions row ->
                versions |> Result.bind (fun versions -> row.toStoredVersion |> Result.map (fun stored -> stored :: versions))) (Ok [])
            |> Result.map List.rev

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught by LibraryDatabase.run")>]
        member this.stageVersion (stored : StoredEntryVersion) : Result<unit, StorageError> =
            match stored.versionRow, stored.envelope.facetRows, stored.envelope.sampleMaterialRows with
            | Error error, _, _
            | _, Error error, _
            | _, _, Error error -> Error error
            | Ok row, Ok facets, Ok materials ->
                match this.EntryVersions.Find(row.EntryId, row.Version) with
                | null -> this.EntryVersions.Add(row) |> ignore
                | tracked -> this.Entry(tracked).CurrentValues.SetValues(row)

                // Spec 0050 S2.2: retain shared keys so repeated staging never tracks two entities for one key.
                this.EntryFacets.Where(fun facet -> facet.EntryId = row.EntryId && facet.Version = row.Version).Load()
                let previousFacets =
                    this.EntryFacets.Local
                    |> Seq.filter (fun facet -> facet.EntryId = row.EntryId && facet.Version = row.Version)
                    |> Seq.toList
                for previous in previousFacets do
                    if not (facets |> List.exists (fun facet -> facet.FacetKey = previous.FacetKey && facet.FacetValue = previous.FacetValue)) then
                        this.EntryFacets.Remove(previous) |> ignore
                for facet in facets do
                    match this.EntryFacets.Find(facet.EntryId, facet.Version, facet.FacetKey, facet.FacetValue) with
                    | null -> this.EntryFacets.Add(facet) |> ignore
                    | tracked when this.Entry(tracked).State = EntityState.Deleted -> this.Entry(tracked).State <- EntityState.Unchanged
                    | _ -> ()

                this.SampleMaterials.Where(fun material -> material.SampleId = row.EntryId && material.SampleVersion = row.Version).Load()
                let previousMaterials =
                    this.SampleMaterials.Local
                    |> Seq.filter (fun material -> material.SampleId = row.EntryId && material.SampleVersion = row.Version)
                    |> Seq.toList
                for previous in previousMaterials do
                    if not (materials |> List.exists (fun material -> material.MaterialId = previous.MaterialId && material.MaterialVersion = previous.MaterialVersion)) then
                        this.SampleMaterials.Remove(previous) |> ignore
                for material in materials do
                    match this.SampleMaterials.Find(material.SampleId, material.SampleVersion, material.MaterialId, material.MaterialVersion) with
                    | null -> this.SampleMaterials.Add(material) |> ignore
                    | tracked when this.Entry(tracked).State = EntityState.Deleted -> this.Entry(tracked).State <- EntityState.Unchanged
                    | _ -> ()
                Ok ()

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught by LibraryDatabase.run")>]
        member this.stagePin (id : LibraryEntryId) (version : VersionNumber) : Result<StoredEntryVersion option, StorageError> =
            this.tryStoredVersion id version
            |> Result.bind (function
                | None -> Ok None
                | Some stored ->
                    let pinned =
                        {
                            stored with
                                state =
                                    {
                                        stored.state with
                                            pin = VersionPinned
                                    }
                        }
                    this.stageVersion pinned |> Result.map (fun () -> Some stored))

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught by LibraryDatabase.run")>]
        member this.stageLatest (id : LibraryEntryId) (version : VersionNumber) : unit =
            match this.LatestVersions.Find(id.value) with
            | null -> this.LatestVersions.Add(LibraryLatestVersionRow(EntryId = id.value, Version = version.value)) |> ignore
            | row -> row.Version <- version.value

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught by LibraryDatabase.run")>]
        member this.stageRemoval (id : LibraryEntryId) : unit =
            match this.LatestVersions.Find(id.value) with
            | null -> ()
            | row -> this.LatestVersions.Remove(row) |> ignore
            this.EntryVersions.Where(fun row -> row.EntryId = id.value).Load()
            let versions = this.EntryVersions.Local |> Seq.filter (fun row -> row.EntryId = id.value) |> Seq.toList
            this.EntryVersions.RemoveRange(versions)

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught by LibraryDatabase.run")>]
        member this.stageNewEntry (content : LibraryEntryContent) : Result<unit, StorageError> =
            content.``sealed``
            |> Result.bind (fun envelope ->
                let stored : StoredEntryVersion =
                    {
                        envelope = envelope
                        state =
                            {
                                lifecycle = ActiveEntry
                                pin = VersionUnpinned
                            }
                    }
                this.stageVersion stored
                |> Result.map (fun () -> this.stageLatest content.id content.version))

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught by LibraryDatabase.run")>]
        member this.stageEdit (latest : StoredEntryVersion) (decision : VersionDecision) (content : LibraryEntryContent) : Result<unit, StorageError> =
            match decision with
            | KeepCurrent -> Ok ()
            | MutateInPlace ->
                let edited =
                    {
                        content with version = latest.envelope.content.version
                    }
                edited.``sealed``
                |> Result.bind (fun envelope ->
                    let stored =
                        {
                            envelope = envelope
                            state = latest.state
                        }
                    this.stageVersion stored)
            | MintNextVersion ->
                let edited =
                    {
                        content with version = latest.envelope.content.version.next
                    }
                this.stageNewEntry edited
