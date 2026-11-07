namespace OpticalConstructor.Ui.Tests

open System.IO
open Microsoft.Data.Sqlite
open Softellect.Sys.Primitives
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.Domain.ExperimentExport
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.LibraryDatabaseStore
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

/// Pins shared database stores, startup errors and catalogue import across app scopes
/// and real windows (spec 0038 Part C; spec 0050 S2.4).
module AppContextTests =

    let private owner = FolderName "AppContextTests"

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the
    /// workbench rows / verb buttons live in variable-membership lists, so they carry an
    /// AutomationId — the MaterialsControls precedent).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

    /// Present anywhere in the visual tree (visible or not) — the "is it listed" probe.
    let private isPresent (window : Window) (id : string) : bool =
        match tryFindControl window id with
        | Some _ -> true
        | None -> false

    /// Click the centre of the clickable Border carrying `id` (by Name or AutomationId).
    let private clickOn (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Border as b when matchesId id b && b.IsEffectivelyVisible -> Some b | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some b ->
            // Spec 0046: scroll the box into view first (the facet rows now WRAP, so the tree can sit below
            // the visible area of its scroller, where a click at the box's centre lands nowhere).
            b.BringIntoView() |> ignore
            Dispatcher.UIThread.RunJobs()
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                // A Save click closes the editor during the press — skip the release then.
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// Set the text of the TextBox carrying `id` (fires the property-change subscription the
    /// control's `onTextChanged` binds).
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    /// One fresh app scope — exactly what `Startup.context` is in the product app.
    let private freshContext () : AppContext = TestAppScope.freshAppContext owner

    /// A workbench surface composed FROM the app scope — what `MainConstructorWindow` runs (the two
    /// experiment proxies thread by record update, spec 0038 Part L step 037).
    let private surfaceOf (context : AppContext) : TableAndElementRotationModel =
        { initMainWith context.library context.experiments context.materials context.samples context.categories with
            experimentData = context.experimentData
            experimentCollections = context.experimentCollections }

    // ============================ pure contract (gate `ui-tests`) ============================

    [<Fact>]
    let ``create carries the settings record it was given, verbatim`` () =
        let context =
            AppContext.create (TestAppScope.testLocations owner) WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Equal(WorkbenchSettings.defaults, context.settings)

    [<Fact>]
    let ``create imports the Built-in catalogue into a new library database file and a reopen does not import again`` () : unit =
        let locations = TestAppScope.fileLocations owner
        let first =
            AppContext.create locations WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        let expected = BuiltinCatalogue.samples () |> List.sortBy _.id
        Assert.Equal<Result<Sample list, SampleError>>(Ok [], first.samples.listSamples IncludeInactive)
        Assert.Equal((BuiltinCatalogue.catalogueData ()).envelopes.Length, first.catalogueDecisions.Length)
        first.catalogueDecisions |> List.map _.byDefault |> first.catalogueImport.apply
        |> Result.defaultWith (fun error -> failwith error.describe) |> ignore
        Assert.Equal<Result<Sample list, SampleError>>(
            Ok expected,
            first.samples.listSamples IncludeInactive |> Result.map (List.sortBy _.id))
        Assert.Equal<Result<unit, SampleError>>(Ok (), first.samples.removeSample SampleIds.glassFilm600Id)

        let reopened =
            AppContext.create locations WorkbenchSettings.defaults
            |> Result.defaultWith (fun error -> failwith error.describe)
        Assert.Empty reopened.catalogueDecisions
        Assert.Equal<Result<Sample list, SampleError>>(
            Ok (expected |> List.filter (fun sample -> sample.id <> SampleIds.glassFilm600Id)),
            reopened.samples.listSamples IncludeInactive |> Result.map (List.sortBy _.id))

    [<Fact>]
    let ``create answers LibrarySchemaMismatchErr for a library of another schema and leaves the file as it was`` () : unit =
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
        match AppContext.create locations WorkbenchSettings.defaults with
        | Error (LibrarySchemaMismatchErr mismatch) ->
            Assert.Equal(Some incompatible, mismatch.foundOpt)
            Assert.Equal(LibrarySchemaVersion.current, mismatch.expected)
        | Error error -> Assert.Fail(error.describe)
        | Ok _ -> Assert.Fail("an incompatible library must not open")
        Assert.Equal<byte>(before, File.ReadAllBytes builder.DataSource)

        use inspected = new SqliteConnection(builder.ConnectionString)
        inspected.Open()
        use stamp = inspected.CreateCommand()
        stamp.CommandText <- "SELECT SchemaVersion FROM SchemaStamp WHERE Id = 1;"
        Assert.Equal(int64 incompatible.value, unbox<int64> (stamp.ExecuteScalar()))

    [<Fact>]
    let ``create writes experiment exports into the configured exports folder`` () : unit =
        let locations = TestAppScope.testLocations owner
        Directory.CreateDirectory locations.exportsFolder.value |> ignore
        Directory.GetFiles locations.exportsFolder.value |> Array.iter File.Delete
        let run : ExportedRun =
            {
                title = ExportText "Configured export"
                xLabel = ExportText "Wavelength"
                yLabel = ExportText "Intensity"
                description = ExportText "App-scope export destination"
                series =
                    [
                        {
                            name = ExportText "Transmission"
                            points =
                                [
                                    {
                                        x = 500.0
                                        y = 0.5
                                    }
                                    {
                                        x = 600.0
                                        y = 0.75
                                    }
                                ]
                        }
                    ]
            }
        let context = freshContext ()
        Assert.Equal<Result<unit, ExportError>>(Ok (), context.experimentExport.exportRun run)
        Assert.Single(Directory.GetFiles(locations.exportsFolder.value, "experiment-*.csv")) |> ignore

    [<Fact>]
    let ``create imports the Built-in catalogue into empty stores`` () : unit =
        let context = freshContext ()
        Assert.Equal<Result<Sample list, SampleError>>(
            Ok (BuiltinCatalogue.samples () |> List.sortBy _.id),
            context.samples.listSamples IncludeInactive |> Result.map (List.sortBy _.id))
        Assert.Equal<Result<MaterialEntry list, MaterialError>>(
            Ok (BuiltinCatalogue.materials () |> List.sortBy _.id),
            context.materials.listMaterials IncludeInactive |> Result.map (List.sortBy _.id))
        Assert.Equal<Result<MaterialCategory list, CategoryError>>(
            Ok (BuiltinCatalogue.categories () |> List.sortBy _.id),
            context.categories.listCategories () |> Result.map (List.sortBy _.id))
        for preset in BuiltinCatalogue.presets () do
            Assert.Equal<Result<LibraryEntry option, LibraryError>>(
                Ok (Some preset), context.library.tryGetEntry preset.entryId)

    [<Fact>]
    let ``create couples the materials store to the LIVE samples store: a referenced material is not removable`` () =
        let context = freshContext ()
        // BuiltinCatalogue.samples () reference glass152; removal must consult the SAME
        // samples store `create` built (the composition order matters).
        match context.materials.removeMaterial MaterialIds.glass152 with
        | Error (MaterialStillReferencedErr reason) -> Assert.Contains("is still referenced by", reason)
        | other -> Assert.Fail($"expected MaterialStillReferencedErr from the coupled store, got %A{other}")

    [<Fact>]
    let ``two workbench surfaces composed from ONE app scope observe the same store`` () =
        let context = freshContext ()
        let first = surfaceOf context
        let second = surfaceOf context
        // Mutate through the FIRST surface's injected seam…
        match first.samples.removeSample SampleIds.glassFilm600Id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"removing the seeded sample must succeed, got %A{e}")
        // …and the SECOND surface no longer lists it — one store, not a copy per window.
        match second.samples.listSamples ActiveOnly with
        | Ok all -> Assert.DoesNotContain(SampleIds.glassFilm600Id, all |> List.map (fun s -> s.id))
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")

    [<Fact>]
    let ``two separate app scopes do NOT share stores — each create imports its own`` () =
        let scopeA = freshContext ()
        let scopeB = freshContext ()
        match scopeA.samples.removeSample SampleIds.glassFilm600Id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"removing the seeded sample must succeed, got %A{e}")
        match scopeB.samples.listSamples ActiveOnly with
        | Ok all -> Assert.Contains(SampleIds.glassFilm600Id, all |> List.map (fun s -> s.id))
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")

    // ======================== headless acceptance (gate `ui-smoke`) ========================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: two Main windows over ONE app scope share the stores — a sample added through one is listed by the other`` () =
        HeadlessSession.run (fun () ->
            let context = freshContext ()
            // Two REAL Main windows over the SAME injected scope — the launcher's Main button
            // clicked twice (spec 0038 step 006: launcher-opened windows share the stores).
            let first = OpticalConstructor.App.MainConstructorWindow(context)
            first.Show()
            Dispatcher.UIThread.RunJobs()
            let second = OpticalConstructor.App.MainConstructorWindow(context)
            second.Show()
            Dispatcher.UIThread.RunJobs()
            // Observe the windows the REAL launcher seam opens (subscribed only after both Main
            // windows are shown — the WireUiCompositionTests seam).
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // Surface 1: the Library… strip button (spec 0038 step 015) → the REAL Library
            // window → Make-multilayer → the REAL Sample editor over the SHARED store (the
            // seeded-period creation path, so a plain name + Save persists).
            clickOn first ConstructorUiId.openLibraryWindowButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let libraryOfFirst = opened.[0]
            Assert.True(matchesId ConstructorUiId.libraryWindow.value libraryOfFirst, "the strip button must open the Library window")
            clickOn libraryOfFirst ConstructorUiId.libraryMakeMultilayerButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            let editor = opened.[1]
            Assert.True(matchesId ConstructorUiId.sampleEditorWindow.value editor, "the opened window must be the Sample editor")
            setText editor ConstructorUiId.sampleNameBox.value "Shared-scope sample"
            clickOn editor ConstructorUiId.sampleEditorSaveButton.value
            Assert.False(editor.IsVisible, "Save must persist the entry and close the editor")
            // The id was minted at the verb dispatch — recover it through the SHARED app-scope store.
            let savedId =
                match context.samples.listSamples ActiveOnly with
                | Ok all ->
                    match all |> List.tryFind (fun s -> s.name.value = "Shared-scope sample") with
                    | Some sample -> sample.id
                    | None -> failwith "the saved sample must be in the app-scope store"
                | Error e -> failwith $"listSamples failed: %A{e}"
            // Close the first surface's Library window — the registry is app-global, so the
            // second surface's strip click must CREATE afresh over the same scope, not activate
            // a stale window.
            libraryOfFirst.Close()
            Dispatcher.UIThread.RunJobs()
            // Surface 2: ITS strip button opens a fresh Library window that re-queries the SAME
            // store — the added sample is listed by the other window.
            clickOn second ConstructorUiId.openLibraryWindowButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(3, opened.Count)
            let libraryOfSecond = opened.[2]
            Assert.True(matchesId ConstructorUiId.libraryWindow.value libraryOfSecond, "the strip button must open the Library window")
            // The tree opens collapsed (spec 0040 step 002) — expand entries to reach the leaf.
            clickOn libraryOfSecond (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value
            Assert.True(isPresent libraryOfSecond ((ConstructorUiId.libraryEntryNode savedId.toElementValueId).value),
                        "the second Main window's Library window must list the sample added through the first")
            libraryOfSecond.Close()
            Dispatcher.UIThread.RunJobs()
            second.Close()
            first.Close())
