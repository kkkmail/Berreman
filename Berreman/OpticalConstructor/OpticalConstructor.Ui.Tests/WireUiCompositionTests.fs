namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0033 (026) / 0035 (019) — the WIRE_UI composition acceptance. Every proof here drives the
/// REAL `OpticalConstructor.App.MainConstructorWindow` over the app-scope `AppContext` (spec 0038
/// step 006 hoisted the five-proxy composition out of the window's constructor: `AppContext.create`
/// builds the samples store first, then the materials store whose remove-block consults the LIVE
/// samples through `samplesReferencing`, then the category store whose remove-block consults the
/// LIVE materials through `materialsReferencingCategory`, beside the library / experiments proxies)
/// — all five injected through `initMainWith`, NOT a test-side model with
/// injected stores (that layer is `MainWorkbenchTests`). The editor windows are opened by the REAL
/// `EditorLaunchers.defaults` and observed through the public global `Window.WindowOpenedEvent` (the
/// seam the desktop lifetime itself uses for window tracking), so the proof is end-to-end:
/// launcher-composed window → ribbon click by UiIds → reordered full-surface bay over the root-wired
/// store → verb click → the real editor window mounts headless. Spec 0035 (019) adds the third editor:
/// the Materials bay's "Categories…" verb opens the REAL Category editor over the root-wired
/// `CategoryProxy`, so all three editor windows (Material / Sample / Category) are proven from the root.
module WireUiCompositionTests =

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
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
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

    /// The text of the TextBlock carrying `id`.
    let private textOf (window : Window) (id : string) : string =
        match tryFindControl window id with
        | Some (:? TextBlock as tb) -> tb.Text
        | Some c -> failwith $"%s{id} is a %s{c.GetType().Name}, not a TextBlock"
        | None -> failwith $"%s{id} was not found in the visual tree"

    /// Mount the REAL composition root headless: a FRESH app-scope context (test isolation —
    /// the stores are mutable) injected into the real window, whose constructor runs the
    /// Elmish loop over `initMainWith` — exactly what the launcher's Main button does with
    /// `Startup.context` (spec 0038 step 006).
    let private mountRoot () : Window =
        let window = OpticalConstructor.App.MainConstructorWindow(AppContext.create WorkbenchSettings.defaults)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window :> Window

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance: the REAL Main window renders every ribbon bay, with the Selector / Materials / Library bays over the root-wired stores`` () =
        HeadlessSession.run (fun () ->
            let window = mountRoot ()
            Assert.True(window.IsVisible, "the Main window must open headless")
            // One frame per bay, on the real window (the whole ribbon surface, not just the
            // three slice-named bays — a bay whose content throws fails the sweep here).
            for bay in BayNames.all do
                clickOn window (Ribbon.UiIds.tab bay)
                Assert.True(window.IsVisible, $"the %s{bay} bay must render one frame without throwing")
            // Selector: the kind-constrained binding surface mounts (its ids are unconditional).
            clickOn window (Ribbon.UiIds.tab BayNames.selector)
            for id in [ LibraryControls.UiIds.kindLabel; LibraryControls.UiIds.readout; LibraryControls.UiIds.tree ] do
                Assert.True(isPresent window id, $"the Selector bay must mount %s{id}")
            // Materials: the workbench lists the SEEDED store — the root wired a live
            // MaterialProxy, not an empty stand-in.
            clickOn window (Ribbon.UiIds.tab BayNames.materials)
            Assert.True(isPresent window MaterialsControls.UiIds.searchBox, "the Materials bay must mount its search box")
            Assert.True(isPresent window (MaterialsControls.UiIds.row (string MaterialIds.glass152.value)),
                        "the Materials bay must list the seeded glass152 row from the root-wired store")
            Assert.True(isPresent window (MaterialsControls.UiIds.row (string MaterialIds.silicon.value)),
                        "the Materials bay must list the seeded silicon row from the root-wired store")
            // Library: the samples workbench lists the SEEDED store — the root wired a live
            // SampleProxy.
            clickOn window (Ribbon.UiIds.tab BayNames.library)
            Assert.True(isPresent window SampleLibraryControls.UiIds.searchBox, "the Library bay must mount its search box")
            Assert.True(isPresent window (SampleLibraryControls.UiIds.row (string SeedSamples.glassFilm600.id.value)),
                        "the Library bay must list the seeded glassFilm600 row from the root-wired store")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance: Add and Edit on the REAL Main window open both real editor windows through the default launchers, and the root couples materials to the live samples`` () =
        HeadlessSession.run (fun () ->
            let window = mountRoot ()
            // Observe the windows the REAL `EditorLaunchers.defaults` open (unowned `.Show()`):
            // subscribed only after the Main window is shown, so exactly the editors arrive.
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // Materials → Add: the REAL step-023 Material editor mounts headless.
            clickOn window (Ribbon.UiIds.tab BayNames.materials)
            clickOn window MaterialsControls.UiIds.addButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(opened.[0].IsVisible, "the Material editor window must be shown")
            Assert.True(matchesId MaterialEditorView.UiIds.window opened.[0], "the opened window must be the Material editor")
            opened.[0].Close()
            // Library → narrow, select, Edit: the REAL step-022 Sample editor mounts, seeded
            // with the picked sample.
            clickOn window (Ribbon.UiIds.tab BayNames.library)
            setText window SampleLibraryControls.UiIds.searchBox "n=1.75"
            clickOn window (SampleLibraryControls.UiIds.row (string SeedSamples.glassFilm600.id.value))
            clickOn window SampleLibraryControls.UiIds.editButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            Assert.True(opened.[1].IsVisible, "the Sample editor window must be shown")
            Assert.True(matchesId SampleEditorView.UiIds.window opened.[1], "the opened window must be the Sample editor")
            Assert.Contains("Glass thin film", opened.[1].Title)
            opened.[1].Close()
            // The root coupling itself: removing a REFERENCED material through the real window
            // surfaces the typed refusal NAMING the referencing sample — the materials store's
            // remove-block consults the LIVE samples store (`samplesReferencing`) composed at
            // the root, not a detached lookup.
            clickOn window (Ribbon.UiIds.tab BayNames.materials)
            setText window MaterialsControls.UiIds.searchBox "1.52"
            clickOn window (MaterialsControls.UiIds.row (string MaterialIds.glass152.value))
            clickOn window MaterialsControls.UiIds.removeButton
            clickOn window WorkbenchIds.removeMaterialConfirm
            let message = textOf window WorkbenchIds.materialsMessage
            Assert.Contains("still referenced", message)
            Assert.Contains("Glass plate", message)
            Assert.True(isPresent window (MaterialsControls.UiIds.row (string MaterialIds.glass152.value)),
                        "the refused remove must leave the row listed")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance: Categories… on the REAL Main window opens the real Category editor over the root-wired CategoryProxy`` () =
        HeadlessSession.run (fun () ->
            let window = mountRoot ()
            // Same window-tracking seam as the Material/Sample proof: subscribe only after the Main
            // window is shown, so exactly the editor the verb requests arrives.
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // Materials → Categories…: the REAL step-006 Category editor mounts headless over the
            // root-composed CategoryProxy. The launcher passes `model.categories` — the proxy the
            // composition root wired through `initMainWith` (over `materialsReferencingCategory`)
            // beside the material / sample proxies — so this proves the CategoryProxy is wired at
            // the root, and that the third editor window opens without throwing.
            clickOn window (Ribbon.UiIds.tab BayNames.materials)
            clickOn window WorkbenchIds.categoriesButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(opened.[0].IsVisible, "the Category editor window must be shown")
            Assert.True(matchesId CategoryEditorView.UiIds.window opened.[0], "the opened window must be the Category editor")
            opened.[0].Close()
            window.Close())
