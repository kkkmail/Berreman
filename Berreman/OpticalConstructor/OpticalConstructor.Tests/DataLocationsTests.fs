namespace OpticalConstructor.Tests

open System.IO
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Ui
open OpticalConstructor.Ui.DataLocationsView

/// The Data locations screen's rows and its Open folder over a recording explorer (spec 0050 S5.3).
module DataLocationsTests =

    /// The facts' folder under the test output; no fact creates it or anything in it.
    let private factFolder : FolderName = FolderName (Path.Combine(System.AppContext.BaseDirectory, "DataLocationsTests"))

    /// Every location under `factFolder`, the library at `connection`.
    let private locationsOver (connection : ConnectionString) : DataLocations =
        {
            libraryConnection = connection
            backupsFolder = factFolder.combine (FolderName "Backups")
            preferencesFolder = factFolder.combine (FolderName "Preferences")
            kitsFolder = factFolder.combine (FolderName "Kits")
            exportsFolder = factFolder.combine (FolderName "Exports")
            cataloguesFolder = factFolder.combine (FolderName "Catalogues")
            shippedCatalogueFolder = factFolder.combine (FolderName "Catalogue")
        }

    let private fileLibrary : ConnectionString = ConnectionString $"Data Source={factFolder.value}/library.db"

    let private inMemoryLibrary : ConnectionString = ConnectionString "Data Source=:memory:"

    let private rowOf (kind : DataLocationKind) (rows : DataLocationRow list) : DataLocationRow =
        rows |> List.find (fun row -> row.kind = kind)

    /// The screen over `locations`, its explorer recording every folder it is asked to show into `shown`.
    let private screenOver (locations : DataLocations) (shown : ResizeArray<FolderName>) : DataLocationsModel =
        let context : DataLocationsContext =
            {
                locations = locations
                explorer = ExplorerProxy.createInMemory shown.Add
            }
        context.initDataLocations

    [<Fact>]
    let ``the rows list every location in order, the library by its file`` () : unit =
        let locations = locationsOver fileLibrary
        let rows = locations.rows
        let libraryFile = Path.GetFullPath(Path.Combine(factFolder.value, "library.db"))

        Assert.Equal<DataLocationKind list>(DataLocationKind.all, rows |> List.map _.kind)

        let library = rowOf LibraryDatabaseLocation rows
        Assert.Equal(libraryFile, library.path.value)
        Assert.Equal<FolderName option>(Some factFolder, library.folderOpt)

        let kits = rowOf KitsLocation rows
        Assert.Equal(locations.kitsFolder.value, kits.path.value)
        Assert.Equal<FolderName option>(Some locations.kitsFolder, kits.folderOpt)

        // Spec 0050 S5.3: the rows read no disk, so a folder not made yet is still listed.
        Assert.False(Directory.Exists locations.kitsFolder.value)
        Assert.False(File.Exists libraryFile)

        let screen = screenOver locations (ResizeArray<FolderName> ())
        Assert.Equal<DataLocationRow list>(rows, screen.rows)
        Assert.Equal<ControlCaption option>(None, screen.status)

    [<Fact>]
    let ``an in-memory library is listed without a folder`` () : unit =
        let rows = (locationsOver inMemoryLibrary).rows
        let library = rowOf LibraryDatabaseLocation rows

        Assert.Equal<DataLocationKind list>(DataLocationKind.all, rows |> List.map _.kind)
        Assert.Equal("In memory (Data Source=:memory:)", library.path.value)
        Assert.Equal<FolderName option>(None, library.folderOpt)

        for row in rows |> List.filter (fun row -> row.kind <> LibraryDatabaseLocation) do
            Assert.True(Option.isSome row.folderOpt, $"{row.kind.caption.value} has no folder.")

    [<Fact>]
    let ``Open folder hands the row's folder to the explorer`` () : unit =
        let locations = locationsOver inMemoryLibrary
        let shown = ResizeArray<FolderName> ()
        let screen = screenOver locations shown

        let opened = screen |> updateDataLocations (OpenDataLocationFolder KitsLocation)
        Assert.Equal<FolderName list>([ locations.kitsFolder ], List.ofSeq shown)
        Assert.Equal<ControlCaption option>(None, opened.status)

        let told = { screen with status = Some (ControlCaption "an earlier refusal") }
        let cleared = told |> updateDataLocations (OpenDataLocationFolder KitsLocation)
        Assert.Equal<ControlCaption option>(None, cleared.status)

        shown.Clear()
        let unchanged = screen |> updateDataLocations (OpenDataLocationFolder LibraryDatabaseLocation)
        Assert.Empty(shown)
        Assert.Equal<DataLocationsModel>(screen, unchanged)

    [<Fact>]
    let ``a folder the explorer cannot open is told`` () : unit =
        let locations = locationsOver fileLibrary
        let refusing : ExplorerProxy =
            {
                showFolder = fun _ -> Error (MissingFolderErr "gone")
            }
        let context : DataLocationsContext =
            {
                locations = locations
                explorer = refusing
            }
        let screen = context.initDataLocations

        let answer = screen |> updateDataLocations (OpenDataLocationFolder ExportsLocation)

        match answer.status with
        | Some status ->
            Assert.Contains("gone", status.value)
            Assert.StartsWith("The folder could not be opened — ", status.value)
        | None -> Assert.Fail "the refusal was not told"

        Assert.Equal<DataLocationRow list>(screen.rows, answer.rows)
