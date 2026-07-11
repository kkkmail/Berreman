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

/// Spec 0038 Part C (step 006) — the app-scope composition context (`AppContext`). The five
/// domain proxies hoisted out of `MainConstructorWindow`'s constructor into ONE record built at
/// startup and injected into every window, so launcher-opened and constructor-opened windows
/// share the same in-memory stores. Two layers: pure contract pins on `AppContext.create`
/// (settings carried verbatim, the materials→samples remove-block coupling, one scope = one
/// store / two scopes = two stores — gate `ui-tests`), and the slice's headless acceptance
/// (gate `ui-smoke`): TWO real Main windows composed from ONE app scope observe the same store
/// — a sample added through one window's real editor is listed by the other window's bay.
/// (The proof rode the Materials bay until spec 0038 step 013 moved that bay into the
/// single-instance Materials window; the Library bay carries the same two-surface argument.)
module AppContextTests =

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
    let private freshContext () : AppContext = AppContext.create WorkbenchSettings.defaults

    /// A workbench surface composed FROM the app scope — what `MainConstructorWindow` runs.
    let private surfaceOf (context : AppContext) : Model =
        initMainWith context.library context.experiments context.materials context.samples context.categories

    // ============================ pure contract (gate `ui-tests`) ============================

    [<Fact>]
    let ``create carries the settings record it was given, verbatim`` () =
        let context = AppContext.create WorkbenchSettings.defaults
        Assert.Equal(WorkbenchSettings.defaults, context.settings)

    [<Fact>]
    let ``create couples the materials store to the LIVE samples store: a referenced material is not removable`` () =
        let context = freshContext ()
        // glass152 is referenced by seeded samples — the remove-block must consult the SAME
        // samples store `create` built (the composition order is the load-bearing part).
        match context.materials.removeMaterial MaterialIds.glass152 with
        | Error (MaterialStillReferenced reason) -> Assert.Contains("is still referenced by", reason)
        | other -> Assert.Fail($"expected MaterialStillReferenced from the coupled store, got %A{other}")

    [<Fact>]
    let ``two workbench surfaces composed from ONE app scope observe the same store`` () =
        let context = freshContext ()
        let first = surfaceOf context
        let second = surfaceOf context
        // Mutate through the FIRST surface's injected seam…
        match first.samples.removeSample SeedSamples.glassFilm600.id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"removing the seeded sample must succeed, got %A{e}")
        // …and the SECOND surface no longer lists it — one store, not a copy per window.
        match second.samples.listSamples () with
        | Ok all -> Assert.DoesNotContain(SeedSamples.glassFilm600.id, all |> List.map (fun s -> s.id))
        | Error e -> Assert.Fail($"listSamples failed: %A{e}")

    [<Fact>]
    let ``two separate app scopes do NOT share stores — each create re-seeds its own`` () =
        let scopeA = freshContext ()
        let scopeB = freshContext ()
        match scopeA.samples.removeSample SeedSamples.glassFilm600.id with
        | Ok () -> ()
        | Error e -> Assert.Fail($"removing the seeded sample must succeed, got %A{e}")
        match scopeB.samples.listSamples () with
        | Ok all -> Assert.Contains(SeedSamples.glassFilm600.id, all |> List.map (fun s -> s.id))
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
            // Observe the editor the REAL launcher seam opens (subscribed only after both Main
            // windows are shown, so exactly the editor arrives — the WireUiCompositionTests seam).
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // Surface 1: Library bay → Make-multilayer → the REAL Sample editor over the SHARED
            // store (the seeded-period creation path, so a plain name + Save persists).
            clickOn first (Ribbon.UiIds.tab BayNames.library)
            clickOn first SampleLibraryControls.UiIds.makeMultilayerButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let editor = opened.[0]
            Assert.True(matchesId SampleEditorView.UiIds.window editor, "the opened window must be the Sample editor")
            setText editor SampleEditorView.UiIds.nameBox "Shared-scope sample"
            clickOn editor SampleEditorView.UiIds.saveButton
            Assert.False(editor.IsVisible, "Save must persist the entry and close the editor")
            // The id was minted at the verb dispatch — recover it through the SHARED app-scope store.
            let savedId =
                match context.samples.listSamples () with
                | Ok all ->
                    match all |> List.tryFind (fun s -> s.name = "Shared-scope sample") with
                    | Some sample -> sample.id
                    | None -> failwith "the saved sample must be in the app-scope store"
                | Error e -> failwith $"listSamples failed: %A{e}"
            // Surface 2: opening ITS Library bay re-queries the SAME store in that render pass —
            // the added sample is listed by the other window.
            clickOn second (Ribbon.UiIds.tab BayNames.library)
            Assert.True(isPresent second (SampleLibraryControls.UiIds.row (string savedId.value)),
                        "the second Main window must list the sample added through the first")
            // And surface 1 lists it on its own next render (leave the bay and come back).
            clickOn first (Ribbon.UiIds.tab BayNames.rotation)
            clickOn first (Ribbon.UiIds.tab BayNames.library)
            Assert.True(isPresent first (SampleLibraryControls.UiIds.row (string savedId.value)),
                        "the first Main window must list the sample it added")
            second.Close()
            first.Close())
