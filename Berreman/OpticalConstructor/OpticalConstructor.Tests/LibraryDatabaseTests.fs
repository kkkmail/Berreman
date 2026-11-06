namespace OpticalConstructor.Tests

open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open OpticalConstructor.Database
open Xunit

/// Pins the library's SQLite table and column contract (spec 0050 S2.1).
module LibraryDatabaseTests =

    [<Fact>]
    let ``the library model creates exactly its seven tables and their columns`` () : unit =
        use connection = new SqliteConnection("Data Source=:memory:")
        connection.Open()

        let options = DbContextOptionsBuilder<LibraryDbContext>().UseSqlite(connection).Options
        use context = new LibraryDbContext(options)

        Assert.True(context.Database.EnsureCreated())

        let expectedColumns : Map<string, Set<string>> =
            [
                "EntryVersion",
                set [ "EntryId"; "Version"; "Kind"; "Name"; "Lifecycle"; "Protection"
                      "PinState"; "ProvenanceKind"; "CatalogueId"; "PayloadText"; "ContentHash" ]
                "LatestVersion", set [ "EntryId"; "Version" ]
                "EntryFacet", set [ "EntryId"; "Version"; "FacetKey"; "FacetValue" ]
                "SampleMaterial", set [ "SampleId"; "SampleVersion"; "MaterialId"; "MaterialVersion" ]
                "CatalogueImport", set [ "CatalogueId"; "Folder"; "ManifestHash"; "ImportedAtUtc"; "EntryCount" ]
                "PulledSource", set [ "EntryId"; "Version"; "Source"; "RecordPath"; "SourceVersion"; "Licence"; "Citation" ]
                "SchemaStamp", set [ "Id"; "SchemaVersion" ]
            ]
            |> Map.ofList

        let readNames (command : SqliteCommand) : Set<string> =
            use reader = command.ExecuteReader()
            seq
                {
                    while reader.Read() do
                        yield reader.GetString(0)
                }
            |> Set.ofSeq

        use tablesCommand = connection.CreateCommand()
        tablesCommand.CommandText <-
            "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT GLOB 'sqlite_*';"

        let expectedTables = expectedColumns |> Map.keys |> Set.ofSeq
        Assert.Equal<Set<string>>(expectedTables, readNames tablesCommand)

        for KeyValue(tableName, columns) in expectedColumns do
            use columnsCommand = connection.CreateCommand()
            columnsCommand.CommandText <- "SELECT name FROM pragma_table_info($table);"
            columnsCommand.Parameters.AddWithValue("$table", tableName) |> ignore
            Assert.Equal<Set<string>>(columns, readNames columnsCommand)

        Assert.False(context.Database.EnsureCreated())
