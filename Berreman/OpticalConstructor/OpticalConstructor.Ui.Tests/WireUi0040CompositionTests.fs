/// Spec 0040 Part E (step 013, WIRE_UI) — the CLOSING composition acceptance for arc 0040. Prior
/// steps built every changed surface (the collapsed sorted faceted tree + visible selection of
/// steps 002/003; the restricted gyration/μ preview tabs + wrapping description of steps 006/007;
/// the sample R/T emission checkboxes + film-less-Plate substrate-via-Materials-Select of steps
/// 010/011). This step is STRUCTURAL COMPOSITION — no new behaviour, no new wiring — it finalises
/// the composition and drives the WHOLE changed surface headless over ONE app scope composed
/// through `TestAppScope` with test-owned locations (spec 0050 .spec-md §3.9).
///
/// The WIRE_UI family mandate is a headless wiring assertion that drives REAL input on the REAL
/// wired views and asserts the resulting semantic-tree projection: render one frame each — without
/// throwing — for the Materials window (collapsed tree, visible selection), the Library window
/// (same), the Material editor (restricted gyration/μ tabs, wrapping description), and the Sample
/// editor (R/T checkboxes for a Plate, R fixed for a ThinFilm, substrate Set… via the Materials
/// Select window). The Materials / Library windows open through the REAL App-root ribbon strip on
/// `OpticalConstructor.App.MainConstructorWindow` over the app scope (the `WireUiCompositionTests`
/// precedent); the two editors compose over the SAME app-scope store proxies (the
/// `WireUiFinalCompositionTests` precedent). Every window opened through the app-global
/// `WindowRegistry` is CLOSED before the next, so no key leaks (the `WindowLauncherTests`
/// discipline). The step-006 g₁₁/g₃₃ restriction, the step-010 pin invariants, and the alphabetical
/// sort are proven by the per-window suites; this file proves they COMPOSE at the App root.
namespace OpticalConstructor.Ui.Tests

open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Storage.CatalogueImportStore
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Controls
open OpticalConstructor.Tests
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

module WireUi0040CompositionTests =

    let private owner = FolderName "WireUi0040CompositionTests"

    module MW = OpticalConstructor.Ui.MaterialsWindowView
    module LW = OpticalConstructor.Ui.LibraryWindowView

    // ============================ shared driving helpers ============================

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the tree
    /// rows / verbs live in variable-membership lists, so they carry an AutomationId — the
    /// FacetedTreeControls / WireUiCompositionTests discipline).
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

    /// Click the centre of the clickable Border carrying `id` (by Name or AutomationId). A Save /
    /// Select click can close the window during the press, so the release is skipped when the
    /// window is already gone.
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
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// Click the centre of ANY control carrying `id` (not only a Border) — a CheckBox toggles and
    /// raises its Click on this pointer press/release exactly as a user interaction does (the
    /// emission R/T boxes, spec 0040 step 010).
    let private clickControl (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Control as c when matchesId id c && c.IsEffectivelyVisible -> Some c | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some c ->
            let p = c.TranslatePoint(Point(c.Bounds.Width / 2.0, c.Bounds.Height / 2.0), window)
            if p.HasValue then
                window.MouseDown(p.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                window.MouseUp(p.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// The CheckBox carrying `id` (fails loudly when absent or a different control) — the proof
    /// reads its `IsChecked` / `IsEnabled` directly.
    let private checkBox (window : Window) (id : string) : CheckBox =
        match tryFindControl window id with
        | Some (:? CheckBox as cb) -> cb
        | Some c -> failwith $"%s{id} is a %s{c.GetType().Name}, not a CheckBox"
        | None -> failwith $"%s{id} was not found"

    /// The display text under the control carrying `id` (the control itself when it is a
    /// TextBlock, its first TextBlock descendant otherwise).
    let private textOf (window : Window) (id : string) : string =
        match tryFindControl window id with
        | None -> ""
        | Some (:? TextBlock as t) -> (if isNull t.Text then "" else t.Text)
        | Some c ->
            c.GetVisualDescendants()
            |> Seq.tryPick (function :? TextBlock as t when not (isNull t.Text) -> Some t.Text | _ -> None)
            |> Option.defaultValue ""

    /// Commit `text` through the REAL faceted filter box (the Materials / Library windows commit on
    /// Enter/LostFocus only — never per keystroke — so a bare text set dispatches nothing).
    let private commitFilter (window : Window) (text : string) : unit =
        match tryFindControl window ConstructorUiId.facetFilterBox.value with
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

    /// Expand the tree node with this code by clicking its disclosure chevron (spec 0040 step 002).
    /// The tree opens collapsed, so a node's leaves render only after this — and the expansion
    /// PERSISTS across the window's later re-renders (a filter commit does not re-collapse it).
    let private expandNode (window : Window) (code : string) : unit =
        clickOn window (ConstructorUiId.facetTreeChevron (ConstructorUiKey code)).value

    /// The clickable label Border of the tree row carrying `id` (the chevron carries a DISTINCT id,
    /// so this is unambiguously the label box).
    let private labelBorderOf (window : Window) (id : string) : Border =
        match window.GetVisualDescendants() |> Seq.tryPick (function :? Border as b when matchesId id b -> Some b | _ -> None) with
        | Some b -> b
        | None -> failwith $"%s{id} label border was not found"

    /// A Border's solid fill colour (None when it carries a non-solid brush).
    let private backgroundColorOf (b : Border) : Color option =
        match b.Background with
        | :? SolidColorBrush as s -> Some s.Color
        | _ -> None

    // The FacetedTreeControls idle / chosen row fills (kept private there; mirrored for the proof).
    let private idleFill = Color.FromRgb(232uy, 232uy, 232uy)
    let private chosenFill = Color.FromRgb(150uy, 185uy, 235uy)

    /// Observe the windows the REAL launchers open (unowned `.Show()`) through the public global
    /// `Window.WindowOpenedEvent` — the seam the desktop lifetime itself uses for window tracking.
    let private trackOpened () : ResizeArray<Window> * System.IDisposable =
        let opened = ResizeArray<Window>()
        let sub =
            Window.WindowOpenedEvent.Raised
            |> Observable.subscribe (fun (struct (sender, _args)) ->
                match sender with
                | :? Window as w -> opened.Add w
                | _ -> ())
        opened, sub

    /// Mount the REAL composition root headless over a FRESH app scope (test isolation — the stores
    /// are mutable): the SAME `MainConstructorWindow(AppContext.create …)` the launcher's Main
    /// button builds with `Startup.context` (spec 0038 step 006 / 045).
    let private mountRoot () : Window =
        let window = OpticalConstructor.App.MainConstructorWindow(TestAppScope.freshAppContext owner)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window :> Window

    /// Open the REAL single-instance Materials window through the ribbon strip's right-aligned
    /// `Materials…` button (spec 0038 step 013). The caller MUST close it — a leaked
    /// `MaterialsWindowKey` registration would make a later test's strip click activate a stale
    /// window over the wrong stores.
    let private openMaterialsWindow (root : Window) : Window =
        let opened, sub = trackOpened ()
        use _sub = sub
        clickOn root ConstructorUiId.openMaterialsWindowButton.value
        Dispatcher.UIThread.RunJobs()
        Assert.Equal(1, opened.Count)
        Assert.True(matchesId ConstructorUiId.materialsWindow.value opened.[0], "the strip button must open the Materials window")
        Assert.True(opened.[0].IsVisible, "the Materials window must be shown")
        opened.[0]

    /// Open the REAL single-instance Library window through the ribbon strip's right-aligned
    /// `Library…` button (spec 0038 step 015). The caller MUST close it (the `openMaterialsWindow`
    /// discipline, keyed under `LibraryWindowKey`).
    let private openLibraryWindow (root : Window) : Window =
        let opened, sub = trackOpened ()
        use _sub = sub
        clickOn root ConstructorUiId.openLibraryWindowButton.value
        Dispatcher.UIThread.RunJobs()
        Assert.Equal(1, opened.Count)
        Assert.True(matchesId ConstructorUiId.libraryWindow.value opened.[0], "the strip button must open the Library window")
        Assert.True(opened.[0].IsVisible, "the Library window must be shown")
        opened.[0]

    /// A `Plate` with NO films and no substrate plate — the step-011 D.4 fixture. A Plate exposes
    /// the R and T checkboxes, and its ONLY route to a substrate material is the substrate Set…
    /// verb (no film rows means no per-layer Choose material…).
    let private filmlessPlate () : Sample =
        {
            id = newSampleId ()
            name = EntryName "Bare plate"
            structure = { films = []; substrate = None; lower = None }
            substrate = ThickPlate
            description = EntryDescription "film-less Plate — substrate set via the Materials Select window (spec 0040 step 011)"
            supportedEmission = ThickPlate.defaultSupportedEmission
        }

    // ============================ the headless wiring assertion (gate `ui-smoke`) ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance (013): the Materials and Library windows open from the App root and render the collapsed tree with a visible selection`` () =
        HeadlessSession.run (fun () ->
            let root = mountRoot ()
            // ---- Materials window (steps 002/003) over the app scope ----
            let materialsWindow = openMaterialsWindow root
            // Collapsed by default (step 002): the entries group renders a chevron, but no leaf.
            Assert.True(isPresent materialsWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value,
                        "the Materials tree's entries group must carry a disclosure chevron")
            Assert.False(isPresent materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value),
                         "a collapsed Materials tree must hide its entry leaves on first open")
            // Expanding reveals the leaves in the same render pass (expansion persists across the filter).
            expandNode materialsWindow "entries"
            commitFilter materialsWindow "glass"
            Assert.True(isPresent materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value),
                        "the expanded Materials tree lists the seeded glass152 leaf from the root-wired store")
            // Visible selection (step 003): exactly the clicked row reads the chosen fill; a sibling stays idle.
            clickOn materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
            let mSelected = labelBorderOf materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass152).value)
            let mSibling = labelBorderOf materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass200).value)
            Assert.Equal(Some chosenFill, backgroundColorOf mSelected)
            Assert.Equal(Some idleFill, backgroundColorOf mSibling)
            Assert.True(mSelected.BorderThickness.Top > mSibling.BorderThickness.Top,
                        "the selected Materials row's border must be thicker than an idle row's (the colourblind-safe cue)")
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs()
            // ---- Library window (steps 002/003) over the same app scope ----
            let libraryWindow = openLibraryWindow root
            Assert.True(isPresent libraryWindow (ConstructorUiId.facetTreeChevron (ConstructorUiKey "entries")).value,
                        "the Library tree's entries group must carry a disclosure chevron")
            Assert.False(isPresent libraryWindow ((ConstructorUiId.libraryEntryNode PresetIds.source600).value),
                         "a collapsed Library tree must hide its entry leaves on first open")
            expandNode libraryWindow "entries"
            commitFilter libraryWindow "polarizer"
            Assert.True(isPresent libraryWindow ((ConstructorUiId.libraryEntryNode PresetIds.polarizerLp).value),
                        "the expanded Library tree lists the seeded polarizer preset from the root-wired proxy")
            clickOn libraryWindow ((ConstructorUiId.libraryEntryNode PresetIds.polarizerLp).value)
            let lSelected = labelBorderOf libraryWindow ((ConstructorUiId.libraryEntryNode PresetIds.polarizerLp).value)
            let lSibling = labelBorderOf libraryWindow ((ConstructorUiId.libraryEntryNode PresetIds.polarizerCpLeft).value)
            Assert.Equal(Some chosenFill, backgroundColorOf lSelected)
            Assert.Equal(Some idleFill, backgroundColorOf lSibling)
            Assert.True(lSelected.BorderThickness.Top > lSibling.BorderThickness.Top,
                        "the selected Library row's border must be thicker than an idle row's")
            libraryWindow.Close()
            Dispatcher.UIThread.RunJobs()
            root.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance (013): the Material editor composes over the app scope with a wrapping description and restricted gyration/mu tabs`` () =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            // The Add-open shape the Materials window's launcher builds: NewMaterial over the
            // app-scope materials + category proxies (spec 0038 step 013 / 023).
            let window =
                OpticalConstructor.Ui.MaterialEditorWindow(
                    ctx.materials, MaterialEditorView.NewMaterial (newMaterialId ()), categories = ctx.categories)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible, "the Material editor must render one frame headless over the app scope")
            // Wrapping description (step 007): the box wraps and accepts returns so it grows multiline.
            match tryFindControl window ConstructorUiId.materialDescriptionBox.value with
            | Some (:? TextBox as tb) ->
                Assert.Equal(TextWrapping.Wrap, tb.TextWrapping)
                Assert.True(tb.AcceptsReturn, "the description box must accept returns so it grows multiline")
            | Some c -> Assert.Fail($"the description box is a %s{c.GetType().Name}, not a TextBox")
            | None -> Assert.Fail("the description box was not found in the mounted editor")
            // Restricted gyration/μ tabs (step 006): the Gyration tab appears exactly when active,
            // the μ tab exactly when magnetic — the restricted preview surface composes and renders.
            Assert.False(isPresent window ConstructorUiId.materialPreviewGyrationTab.value, "no Gyration tab before activity is enabled")
            clickOn window ConstructorUiId.activeToggle.value
            Assert.True(isPresent window ConstructorUiId.materialPreviewGyrationTab.value, "the Gyration tab appears when optically active")
            Assert.False(isPresent window ConstructorUiId.materialPreviewMuTab.value, "no μ tab before magnetic is enabled")
            clickOn window ConstructorUiId.magneticToggle.value
            Assert.True(isPresent window ConstructorUiId.materialPreviewMuTab.value, "the μ tab appears when magnetic")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance (013): a Plate Sample editor over the app scope renders R/T checkboxes and sets its substrate through the Materials Select window`` () =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            let window =
                OpticalConstructor.Ui.SampleEditorWindow(
                    ctx.materials, ctx.samples, ctx.categories, CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy, SampleEditorView.EditSample (filmlessPlate ()))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // R/T checkboxes for a Plate (step 010): both render, both checked (EmitBoth), T enabled.
            Assert.True(isPresent window ConstructorUiId.sampleEmitReflectedCheck.value, "the R checkbox must render for a Plate")
            Assert.True(isPresent window ConstructorUiId.sampleEmitTransmittedCheck.value, "the T checkbox must render for a Plate")
            Assert.True((checkBox window ConstructorUiId.sampleEmitReflectedCheck.value).IsChecked.GetValueOrDefault false, "R starts checked")
            Assert.True((checkBox window ConstructorUiId.sampleEmitTransmittedCheck.value).IsChecked.GetValueOrDefault false, "T starts checked")
            Assert.True((checkBox window ConstructorUiId.sampleEmitTransmittedCheck.value).IsEnabled, "the Plate T checkbox is enabled")
            clickControl window ConstructorUiId.sampleEmitTransmittedCheck.value
            Assert.False((checkBox window ConstructorUiId.sampleEmitTransmittedCheck.value).IsChecked.GetValueOrDefault true, "the T click cleared T")
            Assert.True((checkBox window ConstructorUiId.sampleEmitReflectedCheck.value).IsChecked.GetValueOrDefault false, "R remains on after clearing T")
            // Substrate Set… via the Materials Select window (step 011): the film-less Plate has no
            // films, so the substrate Set… verb is its only route to the Materials window.
            Assert.Equal("none", textOf window ConstructorUiId.sampleSubstrateSummary.value)
            let opened, sub = trackOpened ()
            use _sub = sub
            clickOn window ConstructorUiId.setSubstrateButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let materialsWindow = opened.[0]
            Assert.True(matchesId ConstructorUiId.materialsWindow.value materialsWindow, "the substrate Set… verb must open the Materials window")
            Assert.True(isPresent materialsWindow ConstructorUiId.materialsSelectButton.value, "the Materials window must be in Select state")
            Assert.Contains("substrate", textOf materialsWindow ConstructorUiId.materialsSelectConstraint.value)
            // Pick BuiltinCatalogue.material's 1.75 glass: filter → expand → select.
            // The targeted return sets the substrate plate and closes the window.
            commitFilter materialsWindow "1.75"
            expandNode materialsWindow "entries"
            clickOn materialsWindow ((ConstructorUiId.materialsEntryNode MaterialIds.glass175).value)
            clickOn materialsWindow ConstructorUiId.materialsSelectButton.value
            Dispatcher.UIThread.RunJobs()
            Assert.False(materialsWindow.IsVisible, "Select must close the Materials window")
            Assert.Contains("Transparent glass (n = 1.75)", textOf window ConstructorUiId.sampleSubstrateSummary.value)
            Assert.Equal("", textOf window ConstructorUiId.sampleEditorStatus.value)
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``composition acceptance (013): a Mirror Sample editor over the app scope pins R on and shows no T control`` () =
        HeadlessSession.run (fun () ->
            let ctx = TestAppScope.freshAppContext owner
            // BuiltinCatalogue.sample's EUV Mo/Si MIRROR opens over the app scope — R fixed on, no T
            // (spec 0046: only mirrors are observed in reflection alone).
            let window =
                OpticalConstructor.Ui.SampleEditorWindow(
                    ctx.materials, ctx.samples, ctx.categories, CatalogueImportContext.create (CatalogueImportProxy.createInMemory ()).proxy, SampleEditorView.EditSample (BuiltinCatalogue.sample SampleIds.euvMoSiId))
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(isPresent window ConstructorUiId.sampleEmitReflectedCheck.value, "the R checkbox must render for a Mirror")
            let rBox = checkBox window ConstructorUiId.sampleEmitReflectedCheck.value
            Assert.True(rBox.IsChecked.GetValueOrDefault false, "the Mirror R checkbox is fixed on")
            Assert.False(rBox.IsEnabled, "the Mirror R checkbox is disabled (fixed)")
            Assert.False(isPresent window ConstructorUiId.sampleEmitTransmittedCheck.value, "a Mirror shows no T control")
            window.Close())
