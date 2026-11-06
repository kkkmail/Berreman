namespace OpticalConstructor.Storage

open System
open System.Data
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open OpticalConstructor.Database
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.Errors
open Softellect.Sys
open Softellect.Sys.Primitives

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

        member this.catalogueIdOpt : CatalogueId option =
            match this with
            | BuiltInProvenance catalogueId -> Some catalogueId
            | UserProvenance
            | KitImportProvenance _
            | SourcePullProvenance _ -> None

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

    /// Owns one open SQLite connection and serializes context use for its lifetime (spec 0050 001 §C).
    [<Sealed>]
    type LibraryDatabase private (connection : SqliteConnection) =
        let syncRoot = obj ()

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

        [<ElevationBoundary(nameof StorageBoundary, "the library database seam (001 §C): every EF Core and SQLite call is caught here and mapped to a typed error")>]
        static member openOrCreate (connectionString : ConnectionString) : Result<LibraryDatabase, StorageError> =
            try
                let builder = SqliteConnectionStringBuilder(connectionString.value)
                builder.Pooling <- false
                let connection = new SqliteConnection(builder.ConnectionString)

                try
                    connection.Open()
                    let database = new LibraryDatabase(connection)

                    let initialization =
                        database.run (fun context ->
                            use command = connection.CreateCommand()
                            command.CommandText <- "PRAGMA foreign_keys = ON;"
                            command.ExecuteNonQuery() |> ignore

                            let verifyStamp () : Result<unit, StorageError> =
                                use stampCommand = connection.CreateCommand()
                                stampCommand.CommandText <- "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name = 'SchemaStamp';"

                                let foundOpt =
                                    if unbox<int64> (stampCommand.ExecuteScalar()) = 0L then
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

                            // Spec 0050 S21-OPEN-STAMP: WAL changes the file header, so reject foreign schemas first.
                            command.CommandText <- "SELECT COUNT(*) FROM sqlite_master WHERE type = 'table' AND name NOT GLOB 'sqlite_*';"
                            let preflight =
                                if unbox<int64> (command.ExecuteScalar()) = 0L then Ok ()
                                else verifyStamp ()

                            preflight
                            |> Result.bind (fun () ->
                                if builder.DataSource <> ":memory:" && builder.Mode <> SqliteOpenMode.Memory then
                                    command.CommandText <- "PRAGMA journal_mode = WAL;"
                                    command.ExecuteNonQuery() |> ignore

                                if context.Database.EnsureCreated() then
                                    let stamp = LibrarySchemaStampRow(Id = 1, SchemaVersion = LibrarySchemaVersion.current.value)
                                    context.SchemaStamps.Add(stamp) |> ignore
                                    context.SaveChanges() |> ignore
                                    Ok ()
                                else
                                    verifyStamp ()))
                        |> Result.bind id

                    match initialization with
                    | Ok () -> Ok database
                    | Error error ->
                        (database :> IDisposable).Dispose()
                        Error error
                with error ->
                    connection.Dispose()
                    Error (LibraryDatabaseExn error)
            with error ->
                Error (LibraryDatabaseExn error)

        interface IDisposable with
            member _.Dispose () : unit =
                lock syncRoot (fun () -> connection.Dispose())
