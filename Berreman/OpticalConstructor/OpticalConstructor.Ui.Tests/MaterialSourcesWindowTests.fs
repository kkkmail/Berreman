namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.MaterialSources
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.ConstructorUiIdOfDomain
open OpticalConstructor.Ui.WindowLauncher
open OpticalConstructor.Ui.Tests.UiDriver.ById

/// The Online sources window mounted headless: its title and id, its source and snapshot, Search then Pull, and its
/// single instance (spec 0050 S13.1).
module MaterialSourcesWindowTests =

    /// The import context offering the double that holds Silicon's record at 2026-11-01.
    let private siliconImports () : CatalogueImportContext =
        MaterialSourcesWindowFixture.importsOffering [ MaterialSourcesWindowFixture.siliconSource () ]

    /// Shows the window over `imports` headless at 800 × 600, recording each caption it tells its opener into `applied`,
    /// and closes it after `check`. Call it on the headless UI thread.
    let private withSourcesWindow (imports : CatalogueImportContext) (applied : ResizeArray<ControlCaption>) (check : Window -> unit) : unit =
        let window = MaterialSourcesWindow(imports, applied.Add)
        window.Width <- 800.0
        window.Height <- 600.0
        window.Show()
        Dispatcher.UIThread.RunJobs()

        try
            check window
        finally
            window.Close()
            Dispatcher.UIThread.RunJobs()

    [<Fact>]
    let ``the Online sources UiIds are the stable intent-named ids`` () : unit =
        Assert.Equal("MaterialSourcesWindow", ConstructorUiId.materialSourcesWindow.value)
        Assert.Equal("MaterialSourceSnapshotText", ConstructorUiId.materialSourceSnapshotText.value)
        Assert.Equal("MaterialSourceUpdateButton", ConstructorUiId.materialSourceUpdateButton.value)
        Assert.Equal("MaterialSourceUpdateProgressBar", ConstructorUiId.materialSourceUpdateProgressBar.value)
        Assert.Equal("MaterialSourceUpdateProgressText", ConstructorUiId.materialSourceUpdateProgressText.value)
        Assert.Equal("MaterialSourceUpdateCancelButton", ConstructorUiId.materialSourceUpdateCancelButton.value)
        Assert.Equal("MaterialSourceQueryBox", ConstructorUiId.materialSourceQueryBox.value)
        Assert.Equal("MaterialSourceSearchButton", ConstructorUiId.materialSourceSearchButton.value)
        Assert.Equal("MaterialSourceHitList", ConstructorUiId.materialSourceHitList.value)
        Assert.Equal("MaterialSourcesStatusText", ConstructorUiId.materialSourcesStatusText.value)
        Assert.Equal("MaterialSourceChoice_Test source", (ConstructorUiId.materialSourceChoice MaterialSourcesWindowFixture.testSourceName).value)
        Assert.Equal("MaterialSourceHit_main/Si/nk/Aspnes.yml", (ConstructorUiId.materialSourceHitRow MaterialSourcesWindowFixture.siliconHit.record).value)
        Assert.Equal("MaterialSourcePull_main/Si/nk/Aspnes.yml", (ConstructorUiId.materialSourcePullButton MaterialSourcesWindowFixture.siliconHit.record).value)

    [<Fact>]
    let ``the Online sources window is titled, carries its id and lists its source with its snapshot`` () : unit =
        HeadlessSession.run (fun () ->
            withSourcesWindow (siliconImports ()) (ResizeArray<ControlCaption> ()) (fun window ->
                Assert.Equal("Online sources", window.Title)
                Assert.Equal(ConstructorUiId.materialSourcesWindow.value, window.Name)
                Assert.Equal(ConstructorUiId.materialSourcesWindow.value, AutomationProperties.GetAutomationId window)
                findControl window (ConstructorUiId.materialSourceChoice MaterialSourcesWindowFixture.testSourceName) |> ignore
                Assert.Equal("Snapshot 2026-11-01", textOf window ConstructorUiId.materialSourceSnapshotText)))

    [<Fact>]
    let ``a window with no source says so and offers nothing`` () : unit =
        HeadlessSession.run (fun () ->
            withSourcesWindow (MaterialSourcesWindowFixture.importsOffering []) (ResizeArray<ControlCaption> ()) (fun window ->
                Assert.Equal("No online material source is configured.", textOf window ConstructorUiId.materialSourcesStatusText)
                assertAbsent window ConstructorUiId.materialSourceUpdateButton
                assertAbsent window ConstructorUiId.materialSourceQueryBox))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``pressing Search then Pull pulls the record and tells the opener`` () : unit =
        HeadlessSession.run (fun () ->
            let applied = ResizeArray<ControlCaption> ()
            let record = MaterialSourcesWindowFixture.siliconHit.record

            withSourcesWindow (siliconImports ()) applied (fun window ->
                match findControl window ConstructorUiId.materialSourceQueryBox with
                | :? TextBox as queryBox -> queryBox.Text <- "silicon"
                | other -> Assert.Fail $"the query box is a {other.GetType().Name}, not a TextBox"
                Dispatcher.UIThread.RunJobs()
                assertAbsent window (ConstructorUiId.materialSourceHitRow record)

                clickOn window ConstructorUiId.materialSourceSearchButton
                findControl window (ConstructorUiId.materialSourceHitRow record) |> ignore

                clickOn window (ConstructorUiId.materialSourcePullButton record)
                let pulled = "Pulled Si (Silicon), Aspnes as version 1."
                Assert.Equal<ControlCaption list>([ ControlCaption pulled ], List.ofSeq applied)
                Assert.Equal(pulled, textOf window ConstructorUiId.materialSourcesStatusText)))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Online sources window opens once through the registry`` () : unit =
        HeadlessSession.run (fun () ->
            let factory (key : WindowKey) : Result<Window, WindowLauncherError> =
                match key with
                | MaterialSourcesWindowKey -> MaterialSourcesWindow(siliconImports (), ignore) :> Window |> Ok
                | _ -> Error (WindowFactoryFailedErr (key, "only the Online sources window is built here"))

            let launcher = WindowLauncher.create factory ModelessSelectWindows BrowseOpen

            match launcher.openOrActivate MaterialSourcesWindowKey with
            | Ok (CreatedWindow created) ->
                try
                    Assert.IsType<MaterialSourcesWindow>(created) |> ignore
                    Assert.True(created.IsVisible, "the first open must show the Online sources window")

                    match launcher.openOrActivate MaterialSourcesWindowKey with
                    | Ok (ActivatedWindow activated) ->
                        Assert.True(obj.ReferenceEquals(activated, created), "the second open must activate the same window")
                    | other -> Assert.Fail $"the second open must activate, got %A{other}"
                finally
                    created.Close()
                    Dispatcher.UIThread.RunJobs()
            | other -> Assert.Fail $"the first open must create, got %A{other}")
