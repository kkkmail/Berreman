namespace OpticalConstructor.Storage

open System
open System.Data
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open OpticalConstructor.Database
open OpticalConstructor.Storage.Errors
open Softellect.Sys
open Softellect.Sys.Primitives

module LibraryDatabaseStore =

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
