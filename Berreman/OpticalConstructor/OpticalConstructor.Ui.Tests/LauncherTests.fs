/// Spec 0038 Part B.1 (step 002) / Part C (step 004) / step 045: the PRODUCT launcher contract.
/// After the Elmish shell's retirement the launcher IS the app's startup window. Step 045 finalizes
/// it to FOUR surface buttons — Main / Inverse / Materials / Library, in that order, each by a stable
/// `Name` + `AutomationId`: Main opens the forward constructor, Inverse the step-037 inverse
/// constructor, and Materials / Library the single-instance step-013/015 windows through the SAME
/// app-scoped launcher seam the constructor strip uses (`EditorLaunchers.defaults`), so a window
/// opened from the launcher and the same window opened from the constructor strip meet in ONE
/// open-or-activate space. Structure (gate `ui-tests`) pins the four buttons + their ids; behaviour
/// (gate `ui-smoke`) clicks each button headlessly and observes the surface it opens through the
/// public `Window.WindowOpenedEvent` seam (the `WireUiCompositionTests` seam), and proves the
/// launcher/constructor single-instance sharing. `SmokeTests` separately proves the REAL `App`
/// lifetime makes this window the startup `MainWindow` with the persisted theme.
namespace OpticalConstructor.Ui.Tests

open System.IO
open Microsoft.Data.Sqlite
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Interactivity
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Controls
open OpticalConstructor.Domain.CatalogueFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Storage.CatalogueFolderStore
open OpticalConstructor.Storage.EntryFileJson
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Storage.KitContentFiles
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.App

module LauncherTests =

    let private owner = FolderName "LauncherTests"

    let private buttons (w : Window) : Button list =
        w.GetVisualDescendants()
        |> Seq.choose (function :? Button as b -> Some b | _ -> None)
        |> List.ofSeq

    let private tryFindButton (name : string) (w : Window) : Button option =
        buttons w |> List.tryFind (fun b -> b.Name = name)

    let private tryFindLibraryError (window : Window) : TextBlock option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function
            | :? TextBlock as text when text.Name = ConstructorUiId.launcherLibraryErrorText.value -> Some text
            | _ -> None)

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the windows
    /// the launcher opens set both — the `WireUiCompositionTests` precedent).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || AutomationProperties.GetAutomationId c = id

    /// Confirms the fixture catalogue before handing the launcher its remaining work (spec 0050 S3.2).
    let private freshContext () : AppContext =
        let context = TestAppScope.freshAppContext owner
        { context with pendingImports = [] }

    let private tryFindLauncherControl (window : Window) (id : ConstructorUiId) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function
            | :? Control as control when AutomationProperties.GetAutomationId control = id.value -> Some control
            | _ -> None)

    let private launcherControl (window : Window) (id : ConstructorUiId) : Control =
        tryFindLauncherControl window id
        |> Option.defaultWith (fun () -> failwith $"Missing control {id.value}.")

    /// Exercises the real launcher with a substituted folder picker (spec 0050 S4.3).
    let private withLauncherPicker (picker : KitFolderPicker) (check : LauncherWindow -> unit) : unit =
        HeadlessSession.run (fun () ->
            let launcher = LauncherWindow(TestAppScope.testLocations owner, WorkbenchSettings.defaults, Ok (freshContext ()), fun _ -> picker)
            launcher.Show()
            Dispatcher.UIThread.RunJobs()
            try check launcher
            finally launcher.Close())

    /// Open a fresh launcher with an isolated picked folder (spec 0050 S4.3).
    let private withLauncher (check : LauncherWindow -> unit) : unit =
        withLauncherPicker (TestAppScope.kitPickerOf (TestAppScope.freshKitFolder owner)) check

    /// Click the launcher button `id` and return every NEW window opened (via the public
    /// `Window.WindowOpenedEvent`), excluding the launcher itself.
    let private clickAndObserve (launcher : LauncherWindow) (id : string) : Window list =
        let opened = ResizeArray<Window>()
        use _sub =
            Window.WindowOpenedEvent.Raised
            |> Observable.subscribe (fun (struct (sender, _args)) ->
                match sender with
                | :? Window as w when not (obj.ReferenceEquals(w, launcher)) -> opened.Add w
                | _ -> ())
        (match tryFindButton id launcher with
         | Some button -> button.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
         | None -> Assert.Fail($"launcher button '%s{id}' was not found"))
        Dispatcher.UIThread.RunJobs()
        List.ofSeq opened

    /// Click the centre of the clickable Border carrying `id` (by Name or AutomationId) — the
    /// constructor strip's Materials button is a Border, not a Button (the `WireUiCompositionTests`
    /// `clickOn` seam).
    let private clickBorder (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Border as b when matchesId id b && b.IsEffectivelyVisible -> Some b | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some b ->
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    // ------------------------- structure (gate `ui-tests`) -------------------------

    [<Fact>]
    let ``the launcher carries its title and fixed, non-resizable size`` () =
        withLauncher (fun launcher ->
            Assert.Equal("Optical Constructor — Launcher", launcher.Title)
            Assert.Equal(380.0, launcher.Width)
            Assert.Equal(300.0, launcher.Height)
            Assert.False(launcher.CanResize, "the launcher is a fixed-size form"))

    [<Fact>]
    let ``the launcher hosts Main / Inverse / Materials / Library in that order by stable name`` () =
        withLauncher (fun launcher ->
            let names = buttons launcher |> List.map (fun b -> b.Name)
            Assert.Equal<string list>(
                [ ConstructorUiId.openMainButton.value
                  ConstructorUiId.openInverseButton.value
                  ConstructorUiId.openMaterialsButton.value
                  ConstructorUiId.openLibraryButton.value ],
                names))

    [<Fact>]
    let ``a launcher with no pending import shows no import notice`` () : unit =
        withLauncher (fun launcher ->
            Assert.True(tryFindLauncherControl launcher ConstructorUiId.launcherPendingImportsText |> Option.isNone)
            Assert.True(tryFindLauncherControl launcher ConstructorUiId.launcherReviewImportsButton |> Option.isNone)
            Assert.Equal<string list>(
                [ ConstructorUiId.openMainButton.value
                  ConstructorUiId.openInverseButton.value
                  ConstructorUiId.openMaterialsButton.value
                  ConstructorUiId.openLibraryButton.value ],
                buttons launcher |> List.map _.Name))

    [<Fact>]
    let ``each launcher button carries its stable AutomationId and label`` () =
        withLauncher (fun launcher ->
            let expected =
                [ ConstructorUiId.openMainButton.value, "Main"
                  ConstructorUiId.openInverseButton.value, "Inverse"
                  ConstructorUiId.openMaterialsButton.value, "Materials"
                  ConstructorUiId.openLibraryButton.value, "Library" ]
            for id, label in expected do
                match tryFindButton id launcher with
                | Some button ->
                    Assert.Equal(id, AutomationProperties.GetAutomationId button)
                    Assert.Equal(label, string button.Content)
                | None -> Assert.Fail($"launcher button '%s{id}' was not found"))

    [<Fact>]
    let ``a library that fails to open is shown on the launcher and the four buttons are disabled`` () : unit =
        HeadlessSession.run (fun () ->
            let mismatch : LibrarySchemaMismatchInfo =
                {
                    foundOpt = Some (LibrarySchemaVersion 2)
                    expected = LibrarySchemaVersion.current
                }
            let error = LibrarySchemaMismatchErr mismatch
            let launcher = LauncherWindow(TestAppScope.testLocations owner, WorkbenchSettings.defaults, Error error,
                fun _ -> TestAppScope.kitPickerOf (TestAppScope.freshKitFolder owner))
            launcher.Show()
            Dispatcher.UIThread.RunJobs()
            try
                match tryFindLibraryError launcher with
                | Some text ->
                    Assert.Equal($"The library could not be opened: {error.describe}", text.Text)
                    Assert.Contains("schema version 2", text.Text)
                    Assert.Contains($"expected schema version {LibrarySchemaVersion.current.value}", text.Text)
                    Assert.Equal(ConstructorUiId.launcherLibraryErrorText.value, AutomationProperties.GetAutomationId text)
                    Assert.Equal(text.Text, AutomationProperties.GetName text)
                    Assert.Equal(Avalonia.Media.TextWrapping.Wrap, text.TextWrapping)
                    Assert.True(text.IsEffectivelyVisible)
                | None -> Assert.Fail("the library open error must be shown on the launcher")

                for id in
                    [ ConstructorUiId.openMainButton; ConstructorUiId.openInverseButton
                      ConstructorUiId.openMaterialsButton; ConstructorUiId.openLibraryButton ] do
                    match tryFindButton id.value launcher with
                    | Some button -> Assert.False(button.IsEnabled)
                    | None -> Assert.Fail($"launcher button '{id.value}' was not found")
                    let opened = clickAndObserve launcher id.value
                    try Assert.Empty opened
                    finally
                        for window in opened do window.Close()
            finally launcher.Close())

    [<Fact>]
    let ``the launcher offers Recreate library from catalogues for a schema mismatch only`` () : unit =
        HeadlessSession.run (fun () ->
            let mismatch : LibrarySchemaMismatchInfo =
                {
                    foundOpt = Some (LibrarySchemaVersion 2)
                    expected = LibrarySchemaVersion.current
                }
            for error in [ LibrarySchemaMismatchErr mismatch; LibraryDatabaseExn (System.Exception "disk full") ] do
                let launcher = LauncherWindow(TestAppScope.testLocations owner, WorkbenchSettings.defaults, Error error,
                    fun _ -> TestAppScope.kitPickerOf (TestAppScope.freshKitFolder owner))
                launcher.Show()
                Dispatcher.UIThread.RunJobs()
                try
                    match tryFindLibraryError launcher with
                    | Some text -> Assert.Equal($"The library could not be opened: {error.describe}", text.Text)
                    | None -> Assert.Fail("the library open error must be shown on the launcher")

                    let recreate = tryFindButton ConstructorUiId.launcherRecreateLibraryButton.value launcher
                    match error, recreate with
                    | LibrarySchemaMismatchErr _, Some button ->
                        Assert.Equal("Recreate library from catalogues", string button.Content)
                        Assert.Equal(ConstructorUiId.launcherRecreateLibraryButton.value, AutomationProperties.GetAutomationId button)
                        Assert.Equal("Recreate library from catalogues", AutomationProperties.GetName button)
                        Assert.True(button.IsEnabled)
                    | LibraryDatabaseExn _, None -> ()
                    | _ -> Assert.Fail("only a schema mismatch must offer Recreate library from catalogues")
                finally launcher.Close())

    [<Fact>]
    let ``the launcher footer shows the app version from BuildInfo, below the buttons`` () =
        withLauncher (fun launcher ->
            let footer =
                launcher.GetVisualDescendants()
                |> Seq.tryPick (function :? TextBlock as t when matchesId ConstructorUiId.launcherVersionText.value t -> Some t | _ -> None)
            match footer with
            | Some text ->
                Assert.Equal($"Version {Berreman.BuildInfo.version.value}", text.Text)
                Assert.Equal(ConstructorUiId.launcherVersionText.value, AutomationProperties.GetAutomationId text)
                Assert.True(text.IsEffectivelyVisible, "the version footer must be visible")
                // It sits at the bottom of the form: below every surface button.
                let footerTop = text.TranslatePoint(Point(0.0, 0.0), launcher) |> Option.ofNullable
                for button in buttons launcher do
                    let buttonBottom = button.TranslatePoint(Point(0.0, button.Bounds.Height), launcher) |> Option.ofNullable
                    match footerTop, buttonBottom with
                    | Some (f : Point), Some (b : Point) -> Assert.True(f.Y >= b.Y, $"the footer (y = {f.Y}) must sit below '{button.Name}' (bottom y = {b.Y})")
                    | _ -> Assert.Fail("the footer and the buttons must have on-screen positions")
            | None -> Assert.Fail($"the launcher footer '%s{ConstructorUiId.launcherVersionText.value}' was not found"))

    [<Fact>]
    let ``the app version is the FSharp.Core prefix plus the build number`` () =
        Assert.Equal($"{Berreman.BuildInfo.VersionPrefix}.{Berreman.BuildInfo.BuildNumber}", Berreman.BuildInfo.version.value)
        Assert.Matches(@"^\d+\.\d+\.\d+\.\d+$", Berreman.BuildInfo.version.value)

    // ---------------------- behaviour (gate `ui-smoke`) ----------------------
    // Click each launcher button headlessly and observe the surface it opens via
    // `Window.WindowOpenedEvent` (the WireUiCompositionTests seam).

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: a start-up catalogue whose rows await a choice opens the import dialog from the launcher; Cancel keeps it waiting and Apply settles it`` () : unit =
        HeadlessSession.run (fun () ->
            let fileLocations = TestAppScope.fileLocations owner
            let shipped = FolderName (Path.Combine(Path.GetDirectoryName fileLocations.backupsFolder.value, "ShippedCatalogue"))
            let locations = { fileLocations with shippedCatalogueFolder = shipped }
            let catalogue = BuiltinCatalogue.catalogueData ()
            let writeCatalogue (data : CatalogueData) : unit =
                let folder = CatalogueProxy.builtInFolderUnder locations.shippedCatalogueFolder
                (CatalogueProxy.create folder).writeCatalogue data
                |> Result.defaultWith (fun error -> failwith error.describe)
            let openContext () : AppContext =
                AppContext.create locations WorkbenchSettings.defaults
                |> Result.defaultWith (fun error -> failwith error.describe)
            let heldEntries (context : AppContext) : HeldEntry list =
                context.catalogueImports.proxy.heldEntriesOf CatalogueIds.builtInCatalogue
                |> Result.defaultWith (fun error -> failwith error.describe)
            let loggedImports (context : AppContext) : CatalogueImportLogEntry list =
                context.catalogueImports.proxy.loggedImports ()
                |> Result.defaultWith (fun error -> failwith error.describe)
            let materials (context : AppContext) : MaterialEntry list =
                context.materials.listMaterials IncludeInactive
                |> Result.defaultWith (fun error -> failwith error.caption.value)
                |> List.sortBy _.id
            let exercisePendingLauncher (context : AppContext) : unit =
                let pending = Assert.Single context.pendingImports
                let heldBefore = heldEntries context
                let importsBefore = loggedImports context
                let materialsBefore = materials context
                let expectedCount = context.pendingImports |> List.sumBy (fun waiting -> waiting.decisions.Length)
                let launcher = LauncherWindow(locations, WorkbenchSettings.defaults, Ok context,
                    fun _ -> TestAppScope.kitPickerOf (TestAppScope.freshKitFolder owner))
                let opened = ResizeArray<Window>()
                use _subscription =
                    Window.WindowOpenedEvent.Raised
                    |> Observable.subscribe (fun (struct (sender, _)) ->
                        match sender with
                        | :? Window as window when not (obj.ReferenceEquals(window, launcher)) -> opened.Add window
                        | _ -> ())
                let assertUnchanged () : unit =
                    Assert.Equal<HeldEntry list>(heldBefore, heldEntries context)
                    Assert.Equal<CatalogueImportLogEntry list>(importsBefore, loggedImports context)
                    Assert.Equal<MaterialEntry list>(materialsBefore, materials context)
                let assertNotice () : unit =
                    let notice = Assert.IsType<TextBlock>(launcherControl launcher ConstructorUiId.launcherPendingImportsText)
                    Assert.Equal($"Library entries waiting for an import decision: {expectedCount}.", notice.Text)
                    Assert.Equal(ConstructorUiId.launcherPendingImportsText.value, notice.Name)
                    Assert.Equal(notice.Text, AutomationProperties.GetName notice)
                    Assert.Equal(Avalonia.Media.TextWrapping.Wrap, notice.TextWrapping)
                    let review = Assert.IsType<Button>(launcherControl launcher ConstructorUiId.launcherReviewImportsButton)
                    Assert.Equal(ConstructorUiId.launcherReviewImportsButton.value, review.Name)
                    Assert.Equal("Review imports…", string review.Content)
                    Assert.Equal("Review imports…", AutomationProperties.GetName review)
                let assertRows (dialog : ImportDialogWindow) : unit =
                    let rows = Assert.IsType<StackPanel>(launcherControl dialog ConstructorUiId.importDecisionList)
                    Assert.Equal(pending.decisions.Length, rows.Children.Count)
                    for decision in pending.decisions do
                        let row = launcherControl dialog (ConstructorUiId.importDecisionRow decision.entryId.uiKey)
                        let texts =
                            row.GetVisualDescendants()
                            |> Seq.choose (function :? TextBlock as text -> Some text.Text | _ -> None)
                            |> Seq.toList
                        Assert.Contains($"Default: {(decision.actionCaption decision.defaultAction).value}", texts)
                        launcherControl dialog (ConstructorUiId.importActionOption decision.entryId.uiKey decision.defaultAction.uiKey)
                        |> ignore
                try
                    launcher.Show()
                    Dispatcher.UIThread.RunJobs()
                    let initialDialog = Assert.IsType<ImportDialogWindow>(Assert.Single opened)
                    Assert.True(initialDialog.IsVisible)
                    assertNotice ()
                    assertRows initialDialog
                    assertUnchanged ()
                    clickBorder initialDialog ConstructorUiId.importCancelButton.value
                    Assert.False(initialDialog.IsVisible)
                    Assert.Single opened |> ignore
                    assertNotice ()
                    assertUnchanged ()

                    let reviewed = clickAndObserve launcher ConstructorUiId.launcherReviewImportsButton.value
                    let reviewDialog = Assert.IsType<ImportDialogWindow>(Assert.Single reviewed)
                    Assert.Equal(2, opened.Count)
                    Assert.True(reviewDialog.IsVisible)
                    assertRows reviewDialog
                    assertUnchanged ()
                    clickBorder reviewDialog ConstructorUiId.importApplyButton.value
                    Assert.False(reviewDialog.IsVisible)
                    Assert.True(tryFindLauncherControl launcher ConstructorUiId.launcherPendingImportsText |> Option.isNone)
                    Assert.True(tryFindLauncherControl launcher ConstructorUiId.launcherReviewImportsButton |> Option.isNone)
                    for decision in pending.decisions do
                        let held =
                            context.catalogueImports.proxy.tryHeldEntry decision.entryId
                            |> Result.defaultWith (fun error -> failwith error.describe)
                        Assert.True(held |> Option.isSome, "Apply must settle every decision row.")
                    let logged = Assert.Single(loggedImports context)
                    Assert.Equal(pending.entry.catalogueId, logged.catalogueId)
                    Assert.Equal(pending.entry.manifestHash, logged.manifestHash)
                    Assert.Equal(pending.entry.entryCount, logged.entryCount)
                    let reopened = openContext ()
                    Assert.Empty reopened.pendingImports
                    Assert.Equal<HeldEntry list>(heldEntries context, heldEntries reopened)
                    Assert.Equal<CatalogueImportLogEntry list>(loggedImports context, loggedImports reopened)
                finally
                    for window in opened do window.Close()
                    launcher.Close()
                    Dispatcher.UIThread.RunJobs()

            writeCatalogue catalogue
            let fresh = openContext ()
            Assert.Empty(heldEntries fresh)
            Assert.Empty(loggedImports fresh)
            Assert.Empty(materials fresh)
            let freshPending = Assert.Single fresh.pendingImports
            Assert.Equal(catalogue.envelopes.Length, freshPending.decisions.Length)
            Assert.All(freshPending.decisions, fun decision -> Assert.Equal(AddIncomingEntry, decision.defaultAction))
            exercisePendingLauncher fresh

            // Spec 0050 S32-LAUNCHER-PENDING: a changed manifest still asks when every row reuses.
            let reuseCatalogue =
                CatalogueData.tryCreate catalogue.manifest.id catalogue.manifest.name catalogue.manifest.version.next catalogue.envelopes
                |> Result.defaultWith (fun error -> failwith error.describe)
            writeCatalogue reuseCatalogue
            let reusing = openContext ()
            let reusePending = Assert.Single reusing.pendingImports
            Assert.Equal(catalogue.envelopes.Length, reusePending.decisions.Length)
            Assert.All(reusePending.decisions, fun decision -> Assert.Equal(ReuseHeldEntry, decision.defaultAction))
            exercisePendingLauncher reusing

            let userMaterial =
                {
                    BuiltinCatalogue.material MaterialIds.glass152 with
                        id = newMaterialId ()
                        name = EntryName "Lab glass"
                }
            Assert.Equal<Result<unit, MaterialError>>(Ok (), reusing.materials.saveMaterial userMaterial)
            let incomingMaterialId = newMaterialId ()
            let glass = catalogue.envelopes |> List.find (fun envelope -> envelope.content.id = MaterialIds.glass152.toLibraryEntryId)
            let incomingContent =
                {
                    glass.content with
                        id = incomingMaterialId.toLibraryEntryId
                        name = userMaterial.name
                        provenance = BuiltInProvenance CatalogueIds.builtInCatalogue
                }
            let incoming = incomingContent.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
            let source = catalogue.envelopes |> List.find (fun envelope -> envelope.content.schema.kind = SourceEntryKind)
            let sourceContent =
                {
                    source.content with
                        id = LibraryEntryId (System.Guid.NewGuid())
                        name = EntryName "New startup source"
                        provenance = BuiltInProvenance CatalogueIds.builtInCatalogue
                }
            let incomingSource = sourceContent.``sealed`` |> Result.defaultWith (fun error -> failwith error.describe)
            let changed =
                CatalogueData.tryCreate catalogue.manifest.id catalogue.manifest.name reuseCatalogue.manifest.version.next
                    (catalogue.envelopes @ [ incoming; incomingSource ])
                |> Result.defaultWith (fun error -> failwith error.describe)
            writeCatalogue changed
            let mixed = openContext ()
            let mixedPending = Assert.Single mixed.pendingImports
            Assert.Equal(changed.envelopes.Length, mixedPending.decisions.Length)
            Assert.Equal(catalogue.envelopes.Length, mixedPending.decisions |> List.filter (fun decision -> decision.defaultAction = ReuseHeldEntry) |> List.length)
            let collision = mixedPending.decisions |> List.find (fun decision -> decision.entryId = incoming.content.id)
            match collision.status with
            | NameCollision (_, sameName) -> Assert.Equal(userMaterial.id.toLibraryEntryId, (Assert.Single sameName).id)
            | _ -> Assert.Fail("Lab glass must await a same-name import decision.")
            let added = mixedPending.decisions |> List.find (fun decision -> decision.entryId = incomingSource.content.id)
            Assert.Equal(AddIncomingEntry, added.defaultAction)
            exercisePendingLauncher mixed
            Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok (Some userMaterial), mixed.materials.tryGetMaterial userMaterial.id)
            let glasses = materials mixed |> List.filter (fun material -> material.name = userMaterial.name)
            Assert.Equal<MaterialId list>([ userMaterial.id; incomingMaterialId ] |> List.sort, glasses |> List.map _.id |> List.sort))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Recreate library from catalogues keeps the old file in Backups, opens a new library and enables the buttons`` () : unit =
        HeadlessSession.run (fun () ->
            let locations = TestAppScope.fileLocations owner
            do
                use database =
                    LibraryDatabase.openOrCreate locations.libraryConnection
                    |> Result.defaultWith (fun error -> failwith error.describe)
                Assert.Equal(CreatedLibraryDatabase, database.origin)

            let incompatible = LibrarySchemaVersion 2
            let builder = SqliteConnectionStringBuilder(locations.libraryConnection.value)
            builder.Pooling <- false
            do
                use connection = new SqliteConnection(builder.ConnectionString)
                connection.Open()
                use command = connection.CreateCommand()
                command.CommandText <- "UPDATE SchemaStamp SET SchemaVersion = $version WHERE Id = 1;"
                command.Parameters.AddWithValue("$version", incompatible.value) |> ignore
                Assert.Equal(1, command.ExecuteNonQuery())

            let before = File.ReadAllBytes builder.DataSource
            let opened = AppContext.create locations WorkbenchSettings.defaults
            match opened with
            | Error (LibrarySchemaMismatchErr mismatch) -> Assert.Equal(Some incompatible, mismatch.foundOpt)
            | Error error -> Assert.Fail(error.describe)
            | Ok _ -> Assert.Fail("the incompatible library must be refused before recreation")

            let launcher = LauncherWindow(locations, WorkbenchSettings.defaults, opened,
                fun _ -> TestAppScope.kitPickerOf (TestAppScope.freshKitFolder owner))
            launcher.Show()
            Dispatcher.UIThread.RunJobs()
            try
                // Spec 0050 S24-LAUNCHER-RECREATE: a failed backup must leave the remedy available for retry.
                File.WriteAllText(locations.backupsFolder.value, "blocked backup folder")
                Assert.Empty(clickAndObserve launcher ConstructorUiId.launcherRecreateLibraryButton.value)
                match tryFindLibraryError launcher with
                | Some text -> Assert.Contains(locations.backupsFolder.value, text.Text)
                | None -> Assert.Fail("a failed recreation must show its diagnostic")
                match tryFindButton ConstructorUiId.launcherRecreateLibraryButton.value launcher with
                | Some button -> Assert.True(button.IsEnabled)
                | None -> Assert.Fail("a failed recreation must keep its retry button")
                for id in
                    [ ConstructorUiId.openMainButton; ConstructorUiId.openInverseButton
                      ConstructorUiId.openMaterialsButton; ConstructorUiId.openLibraryButton ] do
                    match tryFindButton id.value launcher with
                    | Some button -> Assert.False(button.IsEnabled)
                    | None -> Assert.Fail($"launcher button '{id.value}' was not found")
                Assert.Equal<byte>(before, File.ReadAllBytes builder.DataSource)
                File.Delete locations.backupsFolder.value

                Assert.Empty(clickAndObserve launcher ConstructorUiId.launcherRecreateLibraryButton.value)
                match tryFindLibraryError launcher with
                | None -> ()
                | Some text -> Assert.Fail($"the recreation error must be removed: {text.Text}")
                match tryFindButton ConstructorUiId.launcherRecreateLibraryButton.value launcher with
                | None -> ()
                | Some _ -> Assert.Fail("the recreate button must be removed after success")
                for id in
                    [ ConstructorUiId.openMainButton; ConstructorUiId.openInverseButton
                      ConstructorUiId.openMaterialsButton; ConstructorUiId.openLibraryButton ] do
                    match tryFindButton id.value launcher with
                    | Some button -> Assert.True(button.IsEnabled)
                    | None -> Assert.Fail($"launcher button '{id.value}' was not found")

                let backup = FileName (Assert.Single(Directory.GetFiles(locations.backupsFolder.value, "library-*.db")))
                Assert.Equal<byte>(before, File.ReadAllBytes backup.value)
                let backupBuilder = SqliteConnectionStringBuilder()
                backupBuilder.DataSource <- backup.value
                backupBuilder.Mode <- SqliteOpenMode.ReadOnly
                backupBuilder.Pooling <- false
                let backupConnection = ConnectionString backupBuilder.ConnectionString
                builder.Mode <- SqliteOpenMode.ReadOnly
                let liveConnection = ConnectionString builder.ConnectionString
                for connectionString, expected in [ backupConnection, incompatible; liveConnection, LibrarySchemaVersion.current ] do
                    use connection = new SqliteConnection(connectionString.value)
                    connection.Open()
                    use command = connection.CreateCommand()
                    command.CommandText <- "SELECT SchemaVersion FROM SchemaStamp WHERE Id = 1;"
                    Assert.Equal(int64 expected.value, unbox<int64> (command.ExecuteScalar()))

                use live = new SqliteConnection(liveConnection.value)
                live.Open()
                use entries = live.CreateCommand()
                entries.CommandText <- "SELECT COUNT(*) FROM EntryVersion;"
                Assert.Equal(0L, unbox<int64> (entries.ExecuteScalar()))
            finally launcher.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main button opens the forward Main constructor scene`` () =
        let folder = TestAppScope.freshKitFolder owner
        withLauncherPicker (TestAppScope.kitPickerOf folder) (fun launcher ->
            match clickAndObserve launcher ConstructorUiId.openMainButton.value with
            | [ opened ] ->
                try
                    Assert.True(opened :? MainConstructorWindow, $"'OpenMainButton' opened a {opened.GetType().Name}")
                    Assert.True(opened.IsVisible, "the opened scene window must be shown")
                    Assert.True(File.Exists(Path.Combine(folder.value.value, ".kit-json")))
                    Assert.True(File.Exists(Path.Combine(folder.value.value, ".oc", "lock")))
                finally opened.Close()
                Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock")))
            | others -> Assert.Fail($"expected exactly one window, found {others.Length}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Inverse button opens the inverse constructor scene`` () =
        let folder = TestAppScope.freshKitFolder owner
        withLauncherPicker (TestAppScope.kitPickerOf folder) (fun launcher ->
            match clickAndObserve launcher ConstructorUiId.openInverseButton.value with
            | [ opened ] ->
                try
                    Assert.True(opened :? InverseConstructorWindow, $"'OpenInverseButton' opened a {opened.GetType().Name}")
                    Assert.True(opened.IsVisible, "the opened inverse scene window must be shown")
                    Assert.True(File.Exists(Path.Combine(folder.value.value, ".kit-json")))
                    Assert.True(File.Exists(Path.Combine(folder.value.value, ".oc", "lock")))
                finally opened.Close()
                Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock")))
            | others -> Assert.Fail($"expected exactly one window, found {others.Length}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main button opens the kit the picker answers, with its experiments`` () : unit =
        let folder = TestAppScope.freshKitFolder owner
        let context = freshContext ()
        let author = KitAuthor "test"
        let kit =
            openKitAt folder ReadWriteKitAccess ClockProvider.system author context.catalogueImports
            |> Result.defaultWith (fun error -> failwith error.describe)
        let experiment : Experiment =
            {
                id = ExperimentId 1
                setup = []
                varied = None
                measurement = CaptureTransmitted
                range = VariableRange.forVariable VaryR1
                dataFileOpt = None
                sourceWaveLengthOpt = None
            }
        try kit.library.kit.proxy.writeExperiment experiment |> SceneTestFixture.kitSuccess
        finally kit.library.kit.close () |> SceneTestFixture.kitSuccess

        withLauncherPicker (TestAppScope.kitPickerOf folder) (fun launcher ->
            let window = Assert.IsType<MainConstructorWindow>(Assert.Single(clickAndObserve launcher ConstructorUiId.openMainButton.value))
            try
                Assert.True(window.IsVisible)
                Assert.True(File.Exists(Path.Combine(folder.value.value, ".oc", "lock")))
                let experimentsTab = ConstructorUiId.ribbonTab (ConstructorUiKey TableAndElementRotationView.BayNames.experiments.value)
                clickBorder window experimentsTab.value
                let row = launcherControl window (ConstructorUiId.experimentEditButton experiment.id.uiKey)
                Assert.True(row.IsEffectivelyVisible, "the picked kit's saved experiment must appear in the real constructor")
                launcherControl window (ConstructorUiId.experimentViewButton experiment.id.uiKey) |> ignore
            finally window.Close()
            Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock"))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a cancelled pick opens nothing`` () : unit =
        let kitsFolder = (TestAppScope.testLocations owner).kitsFolder
        let kitFolders () : string[] =
            if Directory.Exists kitsFolder.value then Directory.GetDirectories(kitsFolder.value) |> Array.sort
            else [||]
        let before = kitFolders ()
        let calls = ResizeArray<unit>()
        let picker : KitFolderPicker =
            {
                pickKitFolder = fun () ->
                    async {
                        calls.Add ()
                        return None
                    }
            }
        withLauncherPicker picker (fun launcher ->
            for button in [ ConstructorUiId.openMainButton; ConstructorUiId.openInverseButton ] do
                Assert.Empty(clickAndObserve launcher button.value)
                Assert.Equal<string>(before, kitFolders ())
                Assert.True(tryFindLauncherControl launcher ConstructorUiId.launcherKitErrorText |> Option.isNone)
            Assert.Equal(2, calls.Count))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: a kit open elsewhere offers Open read-only and Cancel in one row, and Open read-only opens it without writing`` () : unit =
        let folder = TestAppScope.freshKitFolder owner
        let context = freshContext ()
        let author = KitAuthor "test"
        let held =
            openKitAt folder ReadWriteKitAccess ClockProvider.system author context.catalogueImports
            |> Result.defaultWith (fun error -> failwith error.describe)
        try
            let holder = held.library.kit.proxy.acquireLock held.library.kit.kitFile.created |> SceneTestFixture.kitSuccess
            let lockFile = FileName (Path.Combine(folder.value.value, ".oc", "lock"))
            let filesBefore =
                Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
                |> Array.sort
                |> Array.map (fun path ->
                    let file = FileName path
                    let content = FileByteContent (File.ReadAllBytes file.value)
                    file, content)
            let assertFilesUnchanged () : unit =
                let filesNow =
                    Directory.GetFiles(folder.value.value, "*", SearchOption.AllDirectories)
                    |> Array.sort
                    |> Array.map FileName
                Assert.Equal<FileName>(filesBefore |> Array.map fst, filesNow)
                for file, content in filesBefore do
                    Assert.Equal<byte>(content.value, File.ReadAllBytes file.value)
                Assert.True(File.Exists lockFile.value)
            let picks = ResizeArray<unit>()
            let picker : KitFolderPicker =
                {
                    pickKitFolder = fun () ->
                        async {
                            picks.Add ()
                            return Some folder
                        }
                }
            withLauncherPicker picker (fun launcher ->
                Assert.Empty(clickAndObserve launcher ConstructorUiId.openMainButton.value)
                let errorText = Assert.IsType<TextBlock>(launcherControl launcher ConstructorUiId.launcherKitErrorText)
                Assert.Equal($"The kit is open elsewhere: {holder.describe}.", errorText.Text)
                Assert.Contains($"process {System.Environment.ProcessId}", errorText.Text)
                Assert.Equal(ConstructorUiId.launcherKitErrorText.value, errorText.Name)
                Assert.Equal(errorText.Text, AutomationProperties.GetName errorText)
                Assert.Equal(TextWrapping.Wrap, errorText.TextWrapping)
                Assert.True(errorText.IsEffectivelyVisible)
                let readOnly = Assert.IsType<Button>(launcherControl launcher ConstructorUiId.launcherOpenKitReadOnlyButton)
                let cancel = Assert.IsType<Button>(launcherControl launcher ConstructorUiId.launcherCancelKitOpenButton)
                for button, id, label in
                    [ readOnly, ConstructorUiId.launcherOpenKitReadOnlyButton, "Open read-only"
                      cancel, ConstructorUiId.launcherCancelKitOpenButton, "Cancel" ] do
                    Assert.Equal(id.value, button.Name)
                    Assert.Equal(label, Assert.IsType<string>(button.Content))
                    Assert.Equal(label, AutomationProperties.GetName button)
                    Assert.True(button.IsEffectivelyVisible)
                    Assert.True(button.IsEnabled)
                let row = Assert.IsType<StackPanel>(readOnly.Parent)
                Assert.Same(row, cancel.Parent)
                Assert.Same(errorText.Parent, row.Parent)
                Assert.True(row.Bounds.Top >= errorText.Bounds.Bottom)
                Assert.Equal(2, row.Children.Count)
                Assert.Equal(Avalonia.Layout.Orientation.Horizontal, row.Orientation)
                Assert.Equal(12.0, row.Spacing)
                Assert.Equal(readOnly.Bounds.Top, cancel.Bounds.Top)
                Assert.Equal(readOnly.Bounds.Right + 12.0, cancel.Bounds.Left)
                Assert.Equal(MaterialsWindowView.positiveBackground, Assert.IsType<SolidColorBrush>(readOnly.Background).Color)
                Assert.Equal(MaterialsWindowView.negativeBackground, Assert.IsType<SolidColorBrush>(cancel.Background).Color)
                assertFilesUnchanged ()

                let window = Assert.IsType<MainConstructorWindow>(Assert.Single(clickAndObserve launcher readOnly.Name))
                try
                    Assert.True(window.IsVisible)
                    Assert.Single(picks) |> ignore
                    for id in
                        [ ConstructorUiId.launcherKitErrorText; ConstructorUiId.launcherOpenKitReadOnlyButton
                          ConstructorUiId.launcherCancelKitOpenButton ] do
                        Assert.True(tryFindLauncherControl launcher id |> Option.isNone)
                    Assert.DoesNotContain<Visual>(row, launcher.GetVisualDescendants())
                    assertFilesUnchanged ()

                    let experimentsTab = ConstructorUiId.ribbonTab (ConstructorUiKey TableAndElementRotationView.BayNames.experiments.value)
                    clickBorder window experimentsTab.value
                    let sceneName = Assert.IsType<TextBox>(launcherControl window ConstructorUiId.sceneNameField)
                    sceneName.Text <- "Read-only scene"
                    let save = launcherControl window ConstructorUiId.saveSceneButton
                    save.BringIntoView() |> ignore
                    Dispatcher.UIThread.RunJobs()
                    clickBorder window ConstructorUiId.saveSceneButton.value
                    let status = Assert.IsType<TextBlock>(launcherControl window ConstructorUiId.kitStatus)
                    Assert.StartsWith("Not saved to the kit — ", status.Text)
                    Assert.Contains("the kit is open read-only:", status.Text)
                    assertFilesUnchanged ()
                finally window.Close()
                assertFilesUnchanged ()

                Assert.Empty(clickAndObserve launcher ConstructorUiId.openInverseButton.value)
                let inverse = Assert.IsType<InverseConstructorWindow>(Assert.Single(clickAndObserve launcher ConstructorUiId.launcherOpenKitReadOnlyButton.value))
                try
                    Assert.True(inverse.IsVisible)
                    Assert.Equal(2, picks.Count)
                    for id in
                        [ ConstructorUiId.launcherKitErrorText; ConstructorUiId.launcherOpenKitReadOnlyButton
                          ConstructorUiId.launcherCancelKitOpenButton ] do
                        Assert.True(tryFindLauncherControl launcher id |> Option.isNone)
                    assertFilesUnchanged ()
                finally inverse.Close()
                assertFilesUnchanged ())
        finally held.library.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Cancel on a kit open elsewhere opens nothing`` () : unit =
        let folder = TestAppScope.freshKitFolder owner
        let context = freshContext ()
        let author = KitAuthor "test"
        let held =
            openKitAt folder ReadWriteKitAccess ClockProvider.system author context.catalogueImports
            |> Result.defaultWith (fun error -> failwith error.describe)
        try
            let lockFile = FileName (Path.Combine(folder.value.value, ".oc", "lock"))
            let lockBytes = FileByteContent (File.ReadAllBytes lockFile.value)
            withLauncherPicker (TestAppScope.kitPickerOf folder) (fun launcher ->
                for button in [ ConstructorUiId.openMainButton; ConstructorUiId.openInverseButton ] do
                    Assert.Empty(clickAndObserve launcher button.value)
                    launcherControl launcher ConstructorUiId.launcherKitErrorText |> ignore
                    let readOnly = launcherControl launcher ConstructorUiId.launcherOpenKitReadOnlyButton
                    let row = Assert.IsType<StackPanel>(readOnly.Parent)
                    Assert.Empty(clickAndObserve launcher ConstructorUiId.launcherCancelKitOpenButton.value)
                    for id in
                        [ ConstructorUiId.launcherKitErrorText; ConstructorUiId.launcherOpenKitReadOnlyButton
                          ConstructorUiId.launcherCancelKitOpenButton ] do
                        Assert.True(tryFindLauncherControl launcher id |> Option.isNone)
                    Assert.DoesNotContain<Visual>(row, launcher.GetVisualDescendants())
                    Assert.Equal<byte>(lockBytes.value, File.ReadAllBytes lockFile.value))
        finally held.library.kit.close () |> SceneTestFixture.kitSuccess

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a kit that cannot be opened is shown on the launcher and opens no window`` () : unit =
        let folder = TestAppScope.freshKitFolder owner
        let context = freshContext ()
        let author = KitAuthor "test"
        let kit =
            openKitAt folder ReadWriteKitAccess ClockProvider.system author context.catalogueImports
            |> Result.defaultWith (fun error -> failwith error.describe)
        kit.library.kit.close () |> SceneTestFixture.kitSuccess
        let experimentFile = FileName (Path.Combine(folder.value.value, "E1.experiment-json"))
        File.WriteAllText(experimentFile.value, "{")
        withLauncherPicker (TestAppScope.kitPickerOf folder) (fun launcher ->
            for button in [ ConstructorUiId.openMainButton; ConstructorUiId.openInverseButton ] do
                Assert.Empty(clickAndObserve launcher button.value)
                let errorText = Assert.IsType<TextBlock>(launcherControl launcher ConstructorUiId.launcherKitErrorText)
                Assert.Equal(ConstructorUiId.launcherKitErrorText.value, errorText.Name)
                Assert.True(errorText.IsEffectivelyVisible)
                Assert.Equal(Avalonia.Media.TextWrapping.Wrap, errorText.TextWrapping)
                Assert.StartsWith("The kit could not be opened: ", errorText.Text)
                Assert.Contains("E1.experiment-json", errorText.Text)
                Assert.Equal(errorText.Text, AutomationProperties.GetName errorText)
                for id in [ ConstructorUiId.launcherOpenKitReadOnlyButton; ConstructorUiId.launcherCancelKitOpenButton ] do
                    Assert.True(tryFindLauncherControl launcher id |> Option.isNone)
                Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock")))
            File.Delete experimentFile.value
            let window = Assert.IsType<MainConstructorWindow>(Assert.Single(clickAndObserve launcher ConstructorUiId.openMainButton.value))
            try
                Assert.True(window.IsVisible)
                Assert.True(tryFindLauncherControl launcher ConstructorUiId.launcherKitErrorText |> Option.isNone)
            finally window.Close()
            Assert.False(File.Exists(Path.Combine(folder.value.value, ".oc", "lock"))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Materials button opens the single-instance Materials window`` () =
        withLauncher (fun launcher ->
            match clickAndObserve launcher ConstructorUiId.openMaterialsButton.value with
            | [ opened ] ->
                Assert.True(matchesId ConstructorUiId.materialsWindow.value opened, "'OpenMaterialsButton' must open the Materials window")
                Assert.True(opened.IsVisible, "the Materials window must be shown")
                // Close it so the shared WindowRegistry does not leak the MaterialsWindowKey registration.
                opened.Close()
                Dispatcher.UIThread.RunJobs()
            | others -> Assert.Fail($"expected exactly one window, found {others.Length}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Library button opens the single-instance Library window`` () =
        withLauncher (fun launcher ->
            match clickAndObserve launcher ConstructorUiId.openLibraryButton.value with
            | [ opened ] ->
                Assert.True(matchesId ConstructorUiId.libraryWindow.value opened, "'OpenLibraryButton' must open the Library window")
                Assert.True(opened.IsVisible, "the Library window must be shown")
                // Close it so the shared WindowRegistry does not leak the LibraryWindowKey registration.
                opened.Close()
                Dispatcher.UIThread.RunJobs()
            | others -> Assert.Fail($"expected exactly one window, found {others.Length}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Materials from the launcher then the constructor strip activates ONE single instance`` () =
        // The launcher and the constructor share the step-006 app scope AND the host-layer
        // WindowRegistry: opening Materials from the launcher then from the constructor strip must
        // meet in ONE window (the second open ACTIVATES, it does not stack a copy).
        HeadlessSession.run (fun () ->
            let context = freshContext ()
            let launcher = LauncherWindow(TestAppScope.testLocations owner, WorkbenchSettings.defaults, Ok context,
                fun _ -> TestAppScope.kitPickerOf (TestAppScope.freshKitFolder owner))
            launcher.Show()
            Dispatcher.UIThread.RunJobs()
            let constructorWindow = MainConstructorWindow(context, TestAppScope.openedKitIn owner context.catalogueImports)
            constructorWindow.Show()
            Dispatcher.UIThread.RunJobs()
            try
                // Count ONLY Materials-window opens across both actions.
                let materialsOpens = ResizeArray<Window>()
                use _sub =
                    Window.WindowOpenedEvent.Raised
                    |> Observable.subscribe (fun (struct (sender, _args)) ->
                        match sender with
                        | :? Window as w when matchesId ConstructorUiId.materialsWindow.value w -> materialsOpens.Add w
                        | _ -> ())
                // 1) launcher Materials button → CREATES the single instance.
                (match tryFindButton ConstructorUiId.openMaterialsButton.value launcher with
                 | Some button -> button.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
                 | None -> Assert.Fail "launcher button 'OpenMaterialsButton' was not found")
                Dispatcher.UIThread.RunJobs()
                Assert.Equal(1, materialsOpens.Count)
                // 2) constructor strip Materials button → ACTIVATES the SAME instance (no new window).
                clickBorder constructorWindow ConstructorUiId.openMaterialsWindowButton.value
                Dispatcher.UIThread.RunJobs()
                Assert.Equal(1, materialsOpens.Count)
                materialsOpens.[0].Close()
                Dispatcher.UIThread.RunJobs()
            finally
                constructorWindow.Close()
                launcher.Close())
