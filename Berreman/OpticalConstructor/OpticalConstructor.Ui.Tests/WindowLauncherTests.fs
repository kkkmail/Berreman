/// Spec 0038 Part C (step 007, SVC_XDUO_0001 — declared): the WindowLauncher
/// contract, pinned mock-driven. Every record field is exercised through its
/// exact signature against `WindowLauncher.createMock` over recording stub
/// factories — no real windowing behaviour is under test (that is step 008).
/// Pure signature pins (gate `ui-tests`): the modality decision for both
/// step-005 switch values, the typed forget-of-unknown-key error, factory
/// failure propagating without registering, and the id-keyed editor-key /
/// `EntryFreshness` shape. Registry behaviour over stub-built windows (gate
/// `ui-smoke`, windows constructed but never shown, on the shared headless
/// session): a second open of the same `WindowKey` activates instead of
/// creating (every single-instance key and a same-id editor key), two
/// different editor keys create two windows, and a forgotten key re-creates
/// on its next open.
namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.WorkbenchSettings
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
