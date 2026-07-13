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

/// Spec 0033 (026) / 0035 (019) / 0038 (013) — the WIRE_UI composition acceptance. Every proof
/// here drives the REAL `OpticalConstructor.App.MainConstructorWindow` over the app-scope
/// `AppContext` (spec 0038 step 006 hoisted the five-proxy composition out of the window's
/// constructor: `AppContext.create` builds the samples store first, then the materials store
/// whose remove-block consults the LIVE samples through `samplesReferencing`, then the category
/// store whose remove-block consults the LIVE materials through `materialsReferencingCategory`,
/// beside the library / experiments proxies) — all five injected through `initMainWith`, NOT a
/// test-side model with injected stores (that layer is `MainWorkbenchTests`). Windows are opened
/// by the REAL `EditorLaunchers.defaults` and observed through the public global
/// `Window.WindowOpenedEvent` (the seam the desktop lifetime itself uses for window tracking),
/// so the proof is end-to-end. Spec 0038 (013): the Materials bay left the ribbon — the
/// materials surface is now the single-instance Materials WINDOW behind the ribbon strip's
/// right-aligned `Materials…` button, so the material-side proofs go strip button → real
/// Materials window over the root-wired stores → verb click → the real Material / Category
/// editors mount headless. Spec 0038 (015): the Library (samples) workbench bay left the same
/// way — the sample-side proofs go `Library…` strip button → real Library window → verb click →
/// the real Sample editor.
module WireUiCompositionTests =

    module MW = OpticalConstructor.Ui.MaterialsWindowView
    module LW = OpticalConstructor.Ui.LibraryWindowView

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

    /// Commit `text` through the REAL faceted filter box (the Materials window's box commits on
    /// Enter/LostFocus only — never per keystroke — so `setText` alone dispatches nothing).
    let private commitFilter (window : Window) (text : string) : unit =
        match tryFindControl window UiIds.FacetedTree.filterBox with
        | Some (:? TextBox as tb) ->
            tb.Focus() |> ignore
            Dispatcher.UIThread.RunJobs()
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
            window.KeyPressQwerty(Avalonia.Input.PhysicalKey.Enter, Avalonia.Input.RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            window.KeyReleaseQwerty(Avalonia.Input.PhysicalKey.Enter, Avalonia.Input.RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"the filter box is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail("the filter box was not found")

    /// Mount the REAL composition root headless: a FRESH app-scope context (test isolation —
    /// the stores are mutable) injected into the real window, whose constructor runs the
    /// Elmish loop over `initMainWith` — exactly what the launcher's Main button does with
    /// `Startup.context` (spec 0038 step 006).
    let private mountRoot () : Window =
        let window = OpticalConstructor.App.MainConstructorWindow(AppContext.create WorkbenchSettings.defaults)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window :> Window

    /// Observe the windows the REAL `EditorLaunchers.defaults` open (unowned `.Show()`) through
    /// the public global `Window.WindowOpenedEvent` — the seam the desktop lifetime itself uses.
    let private trackOpened () : ResizeArray<Window> * System.IDisposable =
        let opened = ResizeArray<Window>()
        let sub =
            Window.WindowOpenedEvent.Raised
            |> Observable.subscribe (fun (struct (sender, _args)) ->
                match sender with
                | :? Window as w -> opened.Add w
                | _ -> ())
        opened, sub

    /// Open the REAL single-instance Materials window through the ribbon strip's right-aligned
    /// `Materials…` button (spec 0038 step 013) and hand it back for driving. The caller MUST
    /// close it — the shared `WindowRegistry` keys it under `MaterialsWindowKey`, and a leaked
    /// registration would make a LATER test's strip click activate a stale window over the
    /// wrong stores.
    let private openMaterialsWindow (root : Window) : Window =
        let opened, sub = trackOpened ()
        use _sub = sub
        clickOn root WorkbenchIds.openMaterialsButton
        Dispatcher.UIThread.RunJobs()
        Assert.Equal(1, opened.Count)
        Assert.True(matchesId UiIds.MaterialsWindow.window opened.[0], "the strip button must open the Materials window")
        Assert.True(opened.[0].IsVisible, "the Materials window must be shown")
        opened.[0]

    /// Open the REAL single-instance Library window through the ribbon strip's right-aligned
    /// `Library…` button (spec 0038 step 015) and hand it back for driving. The caller MUST
    /// close it — the shared `WindowRegistry` keys it under `LibraryWindowKey` (the
    /// `openMaterialsWindow` discipline above).
    let private openLibraryWindow (root : Window) : Window =
        let opened, sub = trackOpened ()
        use _sub = sub
        clickOn root WorkbenchIds.openLibraryButton
        Dispatcher.UIThread.RunJobs()
        Assert.Equal(1, opened.Count)
        Assert.True(matchesId UiIds.LibraryWindow.window opened.[0], "the strip button must open the Library window")
        Assert.True(opened.[0].IsVisible, "the Library window must be shown")
        opened.[0]

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance: the REAL Main window renders every ribbon bay, with the Selector bay and the Materials / Library windows over the root-wired stores`` () =
        HeadlessSession.run (fun () ->
            let window = mountRoot ()
            Assert.True(window.IsVisible, "the Main window must open headless")
            // One frame per bay, on the real window (the whole ribbon surface — a bay whose
            // content throws fails the sweep here). Spec 0038 steps 013/015: neither workbench
            // tab is in the roster any more.
            Assert.DoesNotContain("Materials", BayNames.all)
            Assert.DoesNotContain("Library", BayNames.all)
            for bay in BayNames.all do
                clickOn window (UiIds.Ribbon.tab bay)
                Assert.True(window.IsVisible, $"the %s{bay} bay must render one frame without throwing")
            // Selector: the kind-constrained binding surface mounts (its ids are unconditional).
            clickOn window (UiIds.Ribbon.tab BayNames.selector)
            for id in [ UiIds.Library.kindLabel; UiIds.Library.readout; UiIds.Library.tree ] do
                Assert.True(isPresent window id, $"the Selector bay must mount %s{id}")
            // Materials: the strip button opens the REAL single-instance Materials window,
            // whose faceted tree lists the SEEDED store — the root wired a live MaterialProxy,
            // not an empty stand-in.
            let materialsWindow = openMaterialsWindow window
            // The tree opens collapsed (spec 0040 step 002) — expand entries to reach the leaves.
            clickOn materialsWindow (UiIds.FacetedTree.treeNodeChevron "entries")
            Assert.True(isPresent materialsWindow (MW.entryNode MaterialIds.glass152),
                        "the Materials window must list the seeded glass152 entry from the root-wired store")
            Assert.True(isPresent materialsWindow (MW.entryNode MaterialIds.silicon),
                        "the Materials window must list the seeded silicon entry from the root-wired store")
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs()
            // Library: the strip button opens the REAL single-instance Library window, whose
            // by-kind faceted tree lists BOTH the seeded live samples store and the read-only
            // preset entries — the root wired live proxies, not empty stand-ins.
            let libraryWindow = openLibraryWindow window
            // The tree opens collapsed (spec 0040 step 002) — expand entries to reach the leaves.
            clickOn libraryWindow (UiIds.FacetedTree.treeNodeChevron "entries")
            Assert.True(isPresent libraryWindow (LW.entryNode (string SeedSamples.glassFilm600.id.value)),
                        "the Library window must list the seeded glassFilm600 sample from the root-wired store")
            Assert.True(isPresent libraryWindow (LW.entryNode "src-600"),
                        "the Library window must list the seeded source preset from the root-wired proxy")
            libraryWindow.Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance: Add and Edit on the REAL Main window open both real editor windows through the default launchers, and the root couples materials to the live samples`` () =
        HeadlessSession.run (fun () ->
            let window = mountRoot ()
            // Materials (spec 0038 step 013): strip button → the REAL Materials window over the
            // root-wired stores.
            let materialsWindow = openMaterialsWindow window
            // Observe the editors the REAL launchers open: subscribed only after both host
            // windows are shown, so exactly the editors arrive.
            let opened, sub = trackOpened ()
            use _sub = sub
            // Materials window → Add: the REAL step-023 Material editor mounts headless.
            clickOn materialsWindow UiIds.MaterialsWindow.addButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(opened.[0].IsVisible, "the Material editor window must be shown")
            Assert.True(matchesId UiIds.MaterialEditor.window opened.[0], "the opened window must be the Material editor")
            opened.[0].Close()
            // Library window (spec 0038 step 015) → narrow, select the leaf, Edit: the REAL
            // step-022 Sample editor mounts, seeded with the picked sample.
            clickOn window WorkbenchIds.openLibraryButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            let libraryWindow = opened.[1]
            Assert.True(matchesId UiIds.LibraryWindow.window libraryWindow, "the strip button must open the Library window")
            commitFilter libraryWindow "n=1.75"
            clickOn libraryWindow (UiIds.FacetedTree.treeNodeChevron "entries")
            clickOn libraryWindow (LW.entryNode (string SeedSamples.glassFilm600.id.value))
            clickOn libraryWindow UiIds.LibraryWindow.editButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(3, opened.Count)
            Assert.True(opened.[2].IsVisible, "the Sample editor window must be shown")
            Assert.True(matchesId UiIds.SampleEditor.window opened.[2], "the opened window must be the Sample editor")
            Assert.Contains("Glass thin film", opened.[2].Title)
            opened.[2].Close()
            libraryWindow.Close()
            Dispatcher.UIThread.RunJobs()
            // The root coupling itself: removing a REFERENCED material through the real
            // Materials window surfaces the typed refusal NAMING the referencing sample — the
            // materials store's remove-block consults the LIVE samples store
            // (`samplesReferencing`) composed at the root, not a detached lookup.
            commitFilter materialsWindow "1.52"
            clickOn materialsWindow (UiIds.FacetedTree.treeNodeChevron "entries")
            clickOn materialsWindow (MW.entryNode MaterialIds.glass152)
            clickOn materialsWindow UiIds.MaterialsWindow.removeButton
            clickOn materialsWindow UiIds.MaterialsWindow.removeConfirmButton
            let message = textOf materialsWindow UiIds.MaterialsWindow.message
            Assert.Contains("still referenced", message)
            Assert.Contains("Glass plate", message)
            Assert.True(isPresent materialsWindow (MW.entryNode MaterialIds.glass152),
                        "the refused remove must leave the entry listed")
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance: Categories… on the REAL Materials window opens the real Category editor over the root-wired CategoryProxy`` () =
        HeadlessSession.run (fun () ->
            let window = mountRoot ()
            // Strip button → the REAL Materials window (spec 0038 step 013), then its
            // Categories… verb: the REAL step-006 Category editor mounts headless over the
            // root-composed CategoryProxy — the SAME proxy the composition root wired through
            // `initMainWith` (over `materialsReferencingCategory`) and handed to the window, so
            // this proves the CategoryProxy is wired at the root and that the third editor
            // window opens without throwing.
            let materialsWindow = openMaterialsWindow window
            let opened, sub = trackOpened ()
            use _sub = sub
            clickOn materialsWindow UiIds.MaterialsWindow.categoriesButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(opened.[0].IsVisible, "the Category editor window must be shown")
            Assert.True(matchesId UiIds.CategoryEditor.window opened.[0], "the opened window must be the Category editor")
            opened.[0].Close()
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs()
            window.Close())
