/// Spec 0038 Part C (steps 007/008, SVC_XDUO_0001 — implemented): the
/// WindowLauncher contract, pinned mock-driven (step 007) AND real (step 008).
/// The mock section exercises every record field through its exact signature
/// against `WindowLauncher.createMock` over recording stub factories. The real
/// section drives `WindowLauncher.create` over the ONE host-layer
/// `WindowRegistry`: a second open ACTIVATES the live window, `Closed`
/// unregisters (a stale close never drops a successor), a forgotten key
/// re-creates, and a Select-state open follows the step-005 modality switch —
/// `ShowDialog` owned by the requesting window under `ModalSelectWindows`,
/// unowned `Show` under `ModelessSelectWindows`. The slice acceptance runs
/// end-to-end through the rewired launcher paths: Edit of the same material
/// twice meets ONE window and two Adds create two `NewUnsaved` editors whose
/// distinct upfront-minted Guids persist through `addMaterial` — since spec
/// 0038 step 013 those verbs live on the REAL Materials window (its context
/// launchers bake the editor keys), so the proofs drive that window; a sample
/// Add composes the SAME Browse launcher the step-015 Library window bakes
/// (`EditorLaunchers.defaults` lost `openSampleEditor` when the samples
/// workbench moved into that window). Every real-registry test CLOSES the
/// windows it opens — the registry is app-global, so a leaked key would
/// couple tests.
namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Ui
open OpticalConstructor.Ui.WindowLauncher

module WindowLauncherTests =

    /// A recording stub factory: appends every key it is asked to build to
    /// `created` and returns a fresh bare `Window` (never shown — the callers
    /// run on the shared headless session, which the construction needs).
    let private recordingFactory (created : ResizeArray<WindowKey>) : WindowKey -> Result<Window, WindowLauncherError> =
        fun (key : WindowKey) ->
            created.Add key
            Window() |> Ok

    /// A stub factory that types-out every request as a construction failure.
    let private failingFactory : WindowKey -> Result<Window, WindowLauncherError> =
        fun (key : WindowKey) -> WindowFactoryFailed(key, "stub factory refuses") |> Error

    /// A stub factory for tests that must never reach construction.
    let private unreachableFactory : WindowKey -> Result<Window, WindowLauncherError> =
        fun (key : WindowKey) -> failwith $"the factory must not be consulted, got %A{key}"

    /// The mock over a recording factory and the given step-005 switch.
    let private mockWith (created : ResizeArray<WindowKey>) (modality : SelectWindowModality) : WindowLauncher =
        WindowLauncher.createMock (recordingFactory created) modality

    // ======================= pure signature pins (gate `ui-tests`) =======================

    [<Fact>]
    let ``decideSelectModality returns the modal switch baked in at construction`` () =
        let launcher = WindowLauncher.createMock unreachableFactory ModalSelectWindows
        match launcher.decideSelectModality () with
        | Ok ModalSelectWindows -> ()
        | other -> Assert.Fail($"expected Ok ModalSelectWindows, got %A{other}")

    [<Fact>]
    let ``decideSelectModality follows the step-005 default — modeless`` () =
        // The defaults record is the single source of the built-in switch value
        // (WorkbenchSettings.fs); the launcher must hand it back untranslated.
        let launcher = WindowLauncher.createMock unreachableFactory WorkbenchSettings.defaults.selectWindowModality
        match launcher.decideSelectModality () with
        | Ok ModelessSelectWindows -> ()
        | other -> Assert.Fail($"expected Ok ModelessSelectWindows, got %A{other}")

    [<Fact>]
    let ``forgetting a key with no live window is the typed WindowNotRegistered error — never a throw`` () =
        let launcher = WindowLauncher.createMock unreachableFactory ModelessSelectWindows
        match launcher.forgetWindow MaterialsWindowKey with
        | Error (WindowNotRegistered MaterialsWindowKey) -> ()
        | other -> Assert.Fail($"expected Error (WindowNotRegistered MaterialsWindowKey), got %A{other}")

    [<Fact>]
    let ``a factory failure propagates as the typed error and the key is NOT registered — the next open retries`` () =
        let launcher = WindowLauncher.createMock failingFactory ModelessSelectWindows
        match launcher.openOrActivate LibraryWindowKey with
        | Error (WindowFactoryFailed (LibraryWindowKey, reason)) -> Assert.Contains("refuses", reason)
        | other -> Assert.Fail($"expected Error (WindowFactoryFailed (LibraryWindowKey, …)), got %A{other}")
        // A failed create must not poison the registry: the key is still absent,
        // so the retry consults the factory again (and fails the same typed way)
        // rather than "activating" a window that was never built.
        match launcher.openOrActivate LibraryWindowKey with
        | Error (WindowFactoryFailed (LibraryWindowKey, _)) -> ()
        | other -> Assert.Fail($"the retry must reach the factory again, got %A{other}")

    [<Fact>]
    let ``editor keys are keyed by the edited entity's id — same id same key, Add-minted ids distinct keys`` () =
        // The Add-open pattern: the Guid is minted AT WINDOW OPEN (off the save
        // path), so the registry keys by id from the first moment.
        let mintedA = MaterialLibrary.newMaterialId ()
        let mintedB = MaterialLibrary.newMaterialId ()
        Assert.Equal(MaterialEditorKey mintedA, MaterialEditorKey mintedA)
        Assert.NotEqual(MaterialEditorKey mintedA, MaterialEditorKey mintedB)
        let sampleId = Library.newSampleId ()
        Assert.Equal(SampleEditorKey sampleId, SampleEditorKey sampleId)
        // An Add-opened editor carries NewUnsaved, an Edit-opened one Persisted —
        // the two-case DU (never a bool) step 008 routes Save on.
        Assert.NotEqual(NewUnsaved, Persisted)

    // ================ registry behaviour over stub windows (gate `ui-smoke`) ================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: a second open of the same WindowKey activates the live window instead of creating`` () =
        HeadlessSession.run (fun () ->
            // Every single-instance key, plus an id-keyed editor key opened twice
            // with the SAME minted id (step 008's Edit-twice shape) — one registry,
            // uniform semantics.
            let sameKeyTwiceActivates (key : WindowKey) : unit =
                let created = ResizeArray<WindowKey>()
                let launcher = mockWith created ModelessSelectWindows
                let firstWindow =
                    match launcher.openOrActivate key with
                    | Ok (CreatedWindow w) -> w
                    | other -> failwith $"the first open of %A{key} must create, got %A{other}"
                match launcher.openOrActivate key with
                | Ok (ActivatedWindow w) ->
                    Assert.True(obj.ReferenceEquals(w, firstWindow), $"%A{key} must activate the SAME live window")
                | other -> Assert.Fail($"the second open of %A{key} must activate, got %A{other}")
                Assert.Equal<WindowKey list>([ key ], List.ofSeq created)
            [
                MaterialsWindowKey
                LibraryWindowKey
                SolverHandoffWindowKey
                CategoryEditorKey
                MaterialEditorKey (MaterialLibrary.newMaterialId ())
                SampleEditorKey (Library.newSampleId ())
            ]
            |> List.iter sameKeyTwiceActivates)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: two different editor keys create two windows — while the same minted id activates`` () =
        HeadlessSession.run (fun () ->
            let created = ResizeArray<WindowKey>()
            let launcher = mockWith created ModelessSelectWindows
            // Two Add opens mint two ids upfront — two distinct multi-instance editors.
            let mintedA = MaterialLibrary.newMaterialId ()
            let mintedB = MaterialLibrary.newMaterialId ()
            let windowOf (key : WindowKey) : Window =
                match launcher.openOrActivate key with
                | Ok (CreatedWindow w) -> w
                | other -> failwith $"opening %A{key} must create a window, got %A{other}"
            let windowA = windowOf (MaterialEditorKey mintedA)
            let windowB = windowOf (MaterialEditorKey mintedB)
            Assert.False(obj.ReferenceEquals(windowA, windowB), "two different editor keys must create two windows")
            // A DIFFERENT editor family keyed by a sample id is a third window again.
            let sampleWindow = windowOf (SampleEditorKey (Library.newSampleId ()))
            Assert.False(obj.ReferenceEquals(windowA, sampleWindow), "editor families never share a window")
            // …while re-opening an id already live meets its existing window.
            match launcher.openOrActivate (MaterialEditorKey mintedA) with
            | Ok (ActivatedWindow w) -> Assert.True(obj.ReferenceEquals(w, windowA), "the same minted id must activate its live editor")
            | other -> Assert.Fail($"re-opening the live editor key must activate, got %A{other}")
            Assert.Equal(3, created.Count))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``forgetting a live key on close makes the next open create afresh`` () =
        HeadlessSession.run (fun () ->
            let created = ResizeArray<WindowKey>()
            let launcher = mockWith created ModelessSelectWindows
            let firstWindow =
                match launcher.openOrActivate MaterialsWindowKey with
                | Ok (CreatedWindow w) -> w
                | other -> failwith $"the first open must create, got %A{other}"
            // The close path: the registry forgets the key (step 008 wires this
            // from the window's Closed hook)…
            match launcher.forgetWindow MaterialsWindowKey with
            | Ok () -> ()
            | Error e -> Assert.Fail($"forgetting a live key must succeed, got %A{e}")
            // …so the next open builds a FRESH window through the factory.
            match launcher.openOrActivate MaterialsWindowKey with
            | Ok (CreatedWindow w) ->
                Assert.False(obj.ReferenceEquals(w, firstWindow), "a forgotten key must not resurrect the old window")
            | other -> Assert.Fail($"the open after a forget must create, got %A{other}")
            Assert.Equal<WindowKey list>([ MaterialsWindowKey; MaterialsWindowKey ], List.ofSeq created))

    // ==================== the REAL launcher over the host-layer registry (step 008) ====================

    /// The step-008 real launcher, Browse-mode, over a recording stub factory. Windows it
    /// creates ARE shown (that is the real behaviour) — every test closes what it opens,
    /// because the registry is the ONE app-global host map.
    let private realWith (created : ResizeArray<WindowKey>) (modality : SelectWindowModality) : WindowLauncher =
        WindowLauncher.create (recordingFactory created) modality BrowseOpen

    /// In-memory app-shaped stores for the end-to-end defaults proofs (the canonical coupling
    /// order; the `Library` open is scoped here for the `createInMemory` type extensions).
    module private Stores =
        open OpticalConstructor.Domain.Library
        open OpticalConstructor.Domain.Lifecycle
        open OpticalConstructor.Domain.MaterialStore
        open OpticalConstructor.Domain.SampleStore

        let create () : MaterialLibrary.MaterialProxy * SampleProxy * MaterialLibrary.CategoryProxy =
            let samples = SampleProxy.createInMemory VersionsInUse.empty
            let materials = MaterialLibrary.MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty
            let categories = MaterialLibrary.CategoryProxy.createInMemory (MaterialLibrary.materialsReferencingCategory materials)
            materials, samples, categories

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId`
    /// (the sibling suites' probe — the editors' controls carry AutomationIds).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

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
                // A Save click closes the window during the press — skip the release then.
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// Set the text of the TextBox carrying `id`.
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    // ---------------- pure pins on the real launcher (gate `ui-tests`) ----------------

    [<Fact>]
    let ``the real launcher hands back the modality switch baked in at construction`` () =
        match (WindowLauncher.create unreachableFactory ModalSelectWindows BrowseOpen).decideSelectModality () with
        | Ok ModalSelectWindows -> ()
        | other -> Assert.Fail($"expected Ok ModalSelectWindows, got %A{other}")
        match (WindowLauncher.create unreachableFactory ModelessSelectWindows BrowseOpen).decideSelectModality () with
        | Ok ModelessSelectWindows -> ()
        | other -> Assert.Fail($"expected Ok ModelessSelectWindows, got %A{other}")

    [<Fact>]
    let ``the real launcher's forget of a never-registered key is the typed WindowNotRegistered error`` () =
        // A freshly minted id cannot be in the app-global registry.
        let key = MaterialEditorKey (MaterialLibrary.newMaterialId ())
        match (WindowLauncher.create unreachableFactory ModelessSelectWindows BrowseOpen).forgetWindow key with
        | Error (WindowNotRegistered k) -> Assert.Equal(key, k)
        | other -> Assert.Fail($"expected Error (WindowNotRegistered …), got %A{other}")

    [<Fact>]
    let ``a real-launcher factory failure (or throw) is the typed error and the key stays unregistered`` () =
        let key = SampleEditorKey (Library.newSampleId ())
        let launcher = WindowLauncher.create failingFactory ModelessSelectWindows BrowseOpen
        match launcher.openOrActivate key with
        | Error (WindowFactoryFailed (k, reason)) ->
            Assert.Equal(key, k)
            Assert.Contains("refuses", reason)
        | other -> Assert.Fail($"expected Error (WindowFactoryFailed …), got %A{other}")
        // The retry consults the factory again — nothing was registered for the key.
        match launcher.openOrActivate key with
        | Error (WindowFactoryFailed _) -> ()
        | other -> Assert.Fail($"the retry must reach the factory again, got %A{other}")
        // A THROWING factory is caught at the launcher boundary and mapped, never rethrown.
        let throwing : WindowKey -> Result<Window, WindowLauncherError> = fun _ -> failwith "boom"
        match (WindowLauncher.create throwing ModelessSelectWindows BrowseOpen).openOrActivate key with
        | Error (WindowFactoryFailed (_, reason)) -> Assert.Contains("boom", reason)
        | other -> Assert.Fail($"a factory throw must map to the typed error, got %A{other}")

    // ------------- real registry behaviour over shown windows (gate `ui-smoke`) -------------

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``real launcher: a second open of the same key ACTIVATES the live shown window — one factory call`` () =
        HeadlessSession.run (fun () ->
            let created = ResizeArray<WindowKey>()
            let launcher = realWith created ModelessSelectWindows
            let key = MaterialEditorKey (MaterialLibrary.newMaterialId ())
            let firstWindow =
                match launcher.openOrActivate key with
                | Ok (CreatedWindow w) -> w
                | other -> failwith $"the first open must create, got %A{other}"
            Assert.True(firstWindow.IsVisible, "a Browse-mode create must SHOW the window")
            match launcher.openOrActivate key with
            | Ok (ActivatedWindow w) ->
                Assert.True(obj.ReferenceEquals(w, firstWindow), "the second open must activate the SAME live window")
            | other -> Assert.Fail($"the second open must activate, got %A{other}")
            Assert.Equal(1, created.Count)
            firstWindow.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``real launcher: closing the window unregisters it — the next open creates afresh`` () =
        HeadlessSession.run (fun () ->
            let created = ResizeArray<WindowKey>()
            let launcher = realWith created ModelessSelectWindows
            let key = SampleEditorKey (Library.newSampleId ())
            let firstWindow =
                match launcher.openOrActivate key with
                | Ok (CreatedWindow w) -> w
                | other -> failwith $"the first open must create, got %A{other}"
            // The REAL unregister path: the window's own Closed event, not an explicit forget.
            firstWindow.Close()
            Dispatcher.UIThread.RunJobs()
            match launcher.openOrActivate key with
            | Ok (CreatedWindow w) ->
                Assert.False(obj.ReferenceEquals(w, firstWindow), "a closed key must not resurrect the old window")
                w.Close()
                Dispatcher.UIThread.RunJobs()
            | other -> Assert.Fail($"the open after a close must create, got %A{other}")
            Assert.Equal(2, created.Count))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``real launcher: a stale window's close never drops its successor's registration`` () =
        HeadlessSession.run (fun () ->
            let created = ResizeArray<WindowKey>()
            let launcher = realWith created ModelessSelectWindows
            let key = MaterialEditorKey (MaterialLibrary.newMaterialId ())
            let stale =
                match launcher.openOrActivate key with
                | Ok (CreatedWindow w) -> w
                | other -> failwith $"the first open must create, got %A{other}"
            // Forget-then-recreate: the key now holds a SUCCESSOR window…
            match launcher.forgetWindow key with
            | Ok () -> ()
            | Error e -> Assert.Fail($"forgetting a live key must succeed, got %A{e}")
            let successor =
                match launcher.openOrActivate key with
                | Ok (CreatedWindow w) -> w
                | other -> failwith $"the open after a forget must create, got %A{other}"
            // …so the STALE window's late close must not unhook it (the Closed hook removes
            // the registration only while it still points at the closing window).
            stale.Close()
            Dispatcher.UIThread.RunJobs()
            match launcher.openOrActivate key with
            | Ok (ActivatedWindow w) ->
                Assert.True(obj.ReferenceEquals(w, successor), "the successor must still be registered after the stale close")
            | other -> Assert.Fail($"the successor must activate, got %A{other}")
            successor.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: a Select-state open follows the step-005 switch — ShowDialog owned by the requester under modal, unowned Show under modeless`` () =
        HeadlessSession.run (fun () ->
            let requester = Window()
            requester.Show()
            // Modal: the created window is shown as a DIALOG owned by the requesting window.
            let modalKey = SampleEditorKey (Library.newSampleId ())
            let modalLauncher =
                WindowLauncher.create (recordingFactory (ResizeArray())) ModalSelectWindows (SelectOpen (requester, ignore))
            (match modalLauncher.openOrActivate modalKey with
             | Ok (CreatedWindow w) ->
                 Assert.True(w.IsVisible, "the modal Select open must show the dialog")
                 Assert.True(obj.ReferenceEquals(w.Owner, requester), "ShowDialog must own the dialog to the requesting window")
                 w.Close()
                 Dispatcher.UIThread.RunJobs()
             | other -> Assert.Fail($"the modal Select open must create, got %A{other}"))
            // Modeless: the created window is an ordinary unowned Show.
            let modelessKey = SampleEditorKey (Library.newSampleId ())
            let modelessLauncher =
                WindowLauncher.create (recordingFactory (ResizeArray())) ModelessSelectWindows (SelectOpen (requester, ignore))
            (match modelessLauncher.openOrActivate modelessKey with
             | Ok (CreatedWindow w) ->
                 Assert.True(w.IsVisible, "the modeless Select open must show the window")
                 Assert.Null(w.Owner)
                 w.Close()
                 Dispatcher.UIThread.RunJobs()
             | other -> Assert.Fail($"the modeless Select open must create, got %A{other}"))
            requester.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance (016): a second Select-state open RE-TARGETS the existing single instance — no second factory call`` () =
        HeadlessSession.run (fun () ->
            let requester = Window()
            requester.Show()
            let key = LibraryWindowKey
            let created = ResizeArray<WindowKey>()
            // The first Select open CREATES (and shows) the single instance…
            let retargeted1 = ResizeArray<Window>()
            let launcher1 =
                WindowLauncher.create (recordingFactory created) ModelessSelectWindows (SelectOpen (requester, retargeted1.Add))
            let firstWindow =
                match launcher1.openOrActivate key with
                | Ok (CreatedWindow w) -> w
                | other -> failwith $"the first Select open must create, got %A{other}"
            Assert.Empty(retargeted1)
            // …and a SECOND Select open (a fresh launcher view baking its own re-target
            // closure — the per-open composition) RE-TARGETS the LIVE window instead of
            // creating or merely activating: the baked closure receives exactly the live
            // instance, once, and the factory is never consulted again.
            let retargeted2 = ResizeArray<Window>()
            let launcher2 =
                WindowLauncher.create (recordingFactory created) ModelessSelectWindows (SelectOpen (requester, retargeted2.Add))
            (match launcher2.openOrActivate key with
             | Ok (RetargetedWindow w) ->
                 Assert.True(obj.ReferenceEquals(w, firstWindow), "the re-target must meet the SAME live window")
             | other -> Assert.Fail($"the second Select open must re-target, got %A{other}"))
            Assert.Equal(1, retargeted2.Count)
            Assert.True(obj.ReferenceEquals(retargeted2.[0], firstWindow), "the baked closure must receive the live window")
            Assert.Equal<WindowKey list>([ key ], List.ofSeq created)
            firstWindow.Close()
            Dispatcher.UIThread.RunJobs()
            requester.Close()
            Dispatcher.UIThread.RunJobs())

    // ------ the slice acceptance, end-to-end through the step-013 Materials window ------

    /// Commit `text` through the REAL faceted filter box (Enter is the commit gesture — the box
    /// has no text-change subscription, spec 0038 §0.4).
    let private commitFilter (window : Window) (text : string) : unit =
        match tryFindControl window OpticalConstructor.Controls.FacetedTreeControls.UiIds.filterBox with
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

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Edit of the same material twice ACTIVATES one editor window — never a second copy`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = Stores.create ()
            // The REAL step-013 Materials window: its context launchers bake the step-008
            // editor keys over the shared registry — the rewired Edit path under test.
            let materialsWindow = MaterialsWindow(materials, categories)
            materialsWindow.Show()
            Dispatcher.UIThread.RunJobs()
            commitFilter materialsWindow "1.52"
            clickOn materialsWindow (MaterialsWindowView.UiIds.entryNode MaterialLibrary.MaterialIds.glass152)
            // Observe the editors the real launcher opens (the WireUiComposition seam).
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            clickOn materialsWindow MaterialsWindowView.UiIds.editButton
            Dispatcher.UIThread.RunJobs()
            clickOn materialsWindow MaterialsWindowView.UiIds.editButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            Assert.True(opened.[0].IsVisible, "the one editor window must be live")
            opened.[0].Close()
            Dispatcher.UIThread.RunJobs()
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Add twice creates two NewUnsaved editors whose distinct upfront Guids persist through addMaterial`` () =
        HeadlessSession.run (fun () ->
            let materials, _, categories = Stores.create ()
            let seededIds =
                match materials.listMaterials MaterialLibrary.ActiveOnly with
                | Ok entries -> entries |> List.map (fun e -> e.id) |> Set.ofList
                | Error e -> failwith $"listMaterials failed: %A{e}"
            let materialsWindow = MaterialsWindow(materials, categories)
            materialsWindow.Show()
            Dispatcher.UIThread.RunJobs()
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // Each Add mints its own upfront id AT the verb dispatch (spec 0038 step 008), so
            // two Adds meet two DISTINCT registry keys: two NewUnsaved editor windows.
            clickOn materialsWindow MaterialsWindowView.UiIds.addButton
            Dispatcher.UIThread.RunJobs()
            clickOn materialsWindow MaterialsWindowView.UiIds.addButton
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(2, opened.Count)
            Assert.False(obj.ReferenceEquals(opened.[0], opened.[1]), "two Adds must open two windows")
            // Save each: the NewUnsaved freshness routes addMaterial under the id minted AT OPEN.
            setText opened.[0] MaterialEditorView.UiIds.nameBox "Launcher add A"
            clickOn opened.[0] MaterialEditorView.UiIds.saveButton
            setText opened.[1] MaterialEditorView.UiIds.nameBox "Launcher add B"
            clickOn opened.[1] MaterialEditorView.UiIds.saveButton
            Assert.False(opened.[0].IsVisible)
            Assert.False(opened.[1].IsVisible)
            // Both persisted, under two DISTINCT non-seeded ids.
            match materials.listMaterials MaterialLibrary.ActiveOnly with
            | Ok entries ->
                let added = entries |> List.filter (fun e -> not (Set.contains e.id seededIds))
                Assert.Equal(2, List.length added)
                Assert.Contains(added, fun (e : MaterialLibrary.MaterialEntry) -> e.name = "Launcher add A")
                Assert.Contains(added, fun (e : MaterialLibrary.MaterialEntry) -> e.name = "Launcher add B")
                match added |> List.map (fun e -> e.id) with
                | [ a; b ] -> Assert.NotEqual(a, b)
                | other -> Assert.Fail($"expected two added ids, got %A{other}")
            | Error e -> Assert.Fail($"listMaterials failed: %A{e}")
            materialsWindow.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: a sample Add opened through the launcher persists through addSample under its minted id`` () =
        HeadlessSession.run (fun () ->
            let materials, samples, categories = Stores.create ()
            let minted = Library.newSampleId ()
            let opened = ResizeArray<Window>()
            use _sub =
                Window.WindowOpenedEvent.Raised
                |> Observable.subscribe (fun (struct (sender, _args)) ->
                    match sender with
                    | :? Window as w -> opened.Add w
                    | _ -> ())
            // The SAME Browse-mode composition the step-015 Library window bakes for its
            // Add-sample verb: the real launcher over the shared registry, keyed by the
            // upfront-minted id (spec 0038 step 008).
            let launcher =
                WindowLauncher.create
                    (fun (_ : WindowKey) -> SampleEditorWindow(materials, samples, categories, SampleEditorView.NewBlankSample minted) :> Window |> Ok)
                    SelectWindowModality.defaultValue
                    BrowseOpen
            launcher.openOrActivate (SampleEditorKey minted) |> ignore
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(1, opened.Count)
            let editor = opened.[0]
            // Name it and give it one layer (a valid stack — Add layer takes the first listed
            // material now that the inline picker is gone, step 019), then Save — NewUnsaved
            // → addSample.
            setText editor SampleEditorView.UiIds.nameBox "Launcher sample"
            clickOn editor SampleEditorView.UiIds.addLayerButton
            clickOn editor SampleEditorView.UiIds.saveButton
            Assert.False(editor.IsVisible)
            match samples.tryGetSample minted with
            | Ok (Some s) -> Assert.Equal("Launcher sample", s.name)
            | other -> Assert.Fail($"the sample Add must persist under its upfront-minted id, got %A{other}"))
