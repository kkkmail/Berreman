namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.DataLocationsView
open OpticalConstructor.Ui.WindowLauncher
open OpticalConstructor.Ui.Tests.UiDriver.ById

/// The Data locations screen mounted headless: its rows, their paths and Open folder (spec 0050 S5.3).
module DataLocationsWindowTests =

    /// The test scope's locations: an in-memory library and every folder under this class's own test output.
    let private testLocations : DataLocations = TestAppScope.testLocations (FolderName "DataLocationsWindowTests")

    /// The screen over the test locations, its explorer recording every folder it is asked to show into `shown`.
    let private screenOver (shown : ResizeArray<FolderName>) : DataLocationsModel =
        let context : DataLocationsContext =
            {
                locations = testLocations
                explorer = ExplorerProxy.createInMemory shown.Add
            }
        context.initDataLocations

    /// Mounts the screen over `model0` as `ScrollExtentTests` mounts a view, shown headless at 800 × 600, and closes it
    /// after `check`. Call it on the headless UI thread.
    let private withDataLocationsScreen (model0 : DataLocationsModel) (check : Window -> unit) : unit =
        let host = HostWindow()
        Program.mkSimple (fun () -> model0) updateDataLocations dataLocationsView |> Program.withHost host |> Program.run
        let window = host :> Window
        window.Width <- 800.0
        window.Height <- 600.0
        window.Show()
        Dispatcher.UIThread.RunJobs()

        try
            check window
        finally
            window.Close()

    [<Fact>]
    let ``the Data locations UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("DataLocationsStatusText", ConstructorUiId.dataLocationsStatusText.value)
        Assert.Equal("DataLocationRow_Kits", (ConstructorUiId.dataLocationRow KitsLocation).value)
        Assert.Equal("DataLocationPath_Kits", (ConstructorUiId.dataLocationPathText KitsLocation).value)
        Assert.Equal("DataLocationOpenFolder_Kits", (ConstructorUiId.dataLocationOpenFolderButton KitsLocation).value)

    [<Fact>]
    let ``every location is listed with its path, and Open folder only where there is a folder`` () : unit =
        HeadlessSession.run (fun () ->
            let screen = screenOver (ResizeArray<FolderName> ())

            withDataLocationsScreen screen (fun window ->
                for row in screen.rows do
                    findControl window (ConstructorUiId.dataLocationRow row.kind) |> ignore
                    Assert.Equal(row.path.value, textOf window (ConstructorUiId.dataLocationPathText row.kind))

                for kind in DataLocationKind.all do
                    findControl window (ConstructorUiId.dataLocationRow kind) |> ignore
                    findControl window (ConstructorUiId.dataLocationPathText kind) |> ignore

                Assert.Equal(testLocations.kitsFolder.value, textOf window (ConstructorUiId.dataLocationPathText KitsLocation))
                Assert.Equal("In memory (Data Source=:memory:)", textOf window (ConstructorUiId.dataLocationPathText LibraryDatabaseLocation))

                assertAbsent window (ConstructorUiId.dataLocationOpenFolderButton LibraryDatabaseLocation)

                for kind in DataLocationKind.all |> List.filter (fun kind -> kind <> LibraryDatabaseLocation) do
                    findControl window (ConstructorUiId.dataLocationOpenFolderButton kind) |> ignore

                assertAbsent window ConstructorUiId.dataLocationsStatusText))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Open folder hands that location's folder to the explorer`` () : unit =
        HeadlessSession.run (fun () ->
            let shown = ResizeArray<FolderName> ()

            withDataLocationsScreen (screenOver shown) (fun window ->
                clickOn window (ConstructorUiId.dataLocationOpenFolderButton ExportsLocation)

                Assert.Equal<FolderName list>([ testLocations.exportsFolder ], List.ofSeq shown)
                assertAbsent window ConstructorUiId.dataLocationsStatusText))

    [<Fact>]
    let ``the Data locations window is titled and carries its stable id`` () : unit =
        HeadlessSession.run (fun () ->
            let window = DataLocationsWindow(testLocations, ExplorerProxy.createInMemory ignore)

            try
                Assert.Equal("Data locations", window.Title)
                Assert.Equal(ConstructorUiId.dataLocationsWindow.value, window.Name)
                Assert.Equal(ConstructorUiId.dataLocationsWindow.value, AutomationProperties.GetAutomationId window)
            finally
                window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Data locations window opens once through the registry`` () : unit =
        HeadlessSession.run (fun () ->
            let factory (key : WindowKey) : Result<Window, WindowLauncherError> =
                match key with
                | DataLocationsWindowKey -> DataLocationsWindow(testLocations, ExplorerProxy.createInMemory ignore) :> Window |> Ok
                | _ -> Error (WindowFactoryFailedErr (key, "only the Data locations window is built here"))

            let launcher = WindowLauncher.create factory ModelessSelectWindows BrowseOpen

            match launcher.openOrActivate DataLocationsWindowKey with
            | Ok (CreatedWindow created) ->
                try
                    Assert.IsType<DataLocationsWindow>(created) |> ignore
                    Assert.True(created.IsVisible, "the first open must show the Data locations window")

                    match launcher.openOrActivate DataLocationsWindowKey with
                    | Ok (ActivatedWindow activated) ->
                        Assert.True(obj.ReferenceEquals(activated, created), "the second open must activate the same window")
                    | other -> Assert.Fail $"the second open must activate, got %A{other}"
                finally
                    created.Close()
                    Dispatcher.UIThread.RunJobs()
            | other -> Assert.Fail $"the first open must create, got %A{other}")
