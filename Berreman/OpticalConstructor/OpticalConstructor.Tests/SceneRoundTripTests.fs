namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Scene
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0038 step 027 (IMPLEMENT_CONTRACT STORE_XDUO_0004): the scene persistence seam is now
/// implemented — a real in-memory `SceneProxy.createInMemory` (a `ref`-map store) plus the pure
/// `captureScene` / `restoreScene` pair in the workbench host. This drives the full acceptance
/// round-trip: capture the live model into a `SceneSnapshot`, `saveScene` it, `tryLoadScene` it
/// back, and `restoreScene` it onto a DIFFERENT base model — reproducing element ids, kinds,
/// placements, bindings, and view state over deterministic data, with no window present.
module SceneRoundTripTests =

    /// Build a `SceneName` through the validated `tryCreate` path (the test names are non-blank).
    let private sceneName (s : string) : SceneName =
        match SceneName.tryCreate s with
        | Ok n -> n
        | Error e -> failwith $"test scene name unexpectedly rejected: %A{e}"

    // Deterministic scene elements: a source and a linear polarizer each bound to a Library entry,
    // and an unbound detector — distinct kinds, placements, bindings, and draw zooms, so the
    // round-trip proves each captured facet rather than coinciding on a shared default. The leading
    // field label is qualified because `TestElement` and `Scene.SceneElement` share `{ id; placement;
    // zoom }`.

    let private sourceEl : TestElement =
        { TestElement.id = Library.elementId "src"
          placement = { ElementPlacement.create LightSource { x = -0.75<meter>; y = 0.10<meter> } with valueId = Some "src-600" }
          zoom = 1.5 }

    let private polarizerEl : TestElement =
        { TestElement.id = Library.elementId "pol"
          placement = { ElementPlacement.create LinearPolarizer { x = 0.0<meter>; y = 0.0<meter> } with valueId = Some "pol-lp" }
          zoom = 3.25 }

    let private detectorEl : TestElement =
        { TestElement.id = Library.elementId "det"
          placement = ElementPlacement.create Detector { x = 0.80<meter>; y = -0.20<meter> }
          zoom = 2.0 }

    /// A NON-default table view (pan + zoom dialled off the straight top-down default), so the
    /// captured / restored view state is distinguishable from a fresh model's.
    let private dialledView : Table.TableViewState =
        { Table.defaultView with panX = 12.5; panY = -7.25; zoom = 1.875 }

    /// The live workbench model under capture: deterministic elements, the dialled view, snap-to-beam
    /// on. Built off `initMain ()` (windowless — the Main scene's pure model constructor) so every
    /// injected proxy / palette field is present; only the scene-owned fields are overridden.
    let private sourceModel () : Model =
        { initMain () with
            elements = [ sourceEl; polarizerEl; detectorEl ]
            view = dialledView
            snapChain = true }

    [<Fact>]
    let ``createInMemory round-trips capture-save-load-restore reproducing ids, kinds, placements, bindings, and view state`` () =
        let model = sourceModel ()
        let proxy = SceneProxy.createInMemory ()
        let name = sceneName "Experiment A"

        // capture -> save.
        let captured = captureScene model
        match proxy.saveScene name captured with
        | Ok () -> ()
        | Error e -> Assert.Fail($"saveScene failed: %A{e}")

        // listScenes lists exactly the saved name.
        match proxy.listScenes () with
        | Ok names -> Assert.Equal<string list>([ "Experiment A" ], names |> List.map (fun n -> n.value))
        | Error e -> Assert.Fail($"%A{e}")

        // load -> the persisted snapshot equals the captured one (pure data survived the store).
        let loaded =
            match proxy.tryLoadScene name with
            | Ok (Some s) -> s
            | other -> failwith $"expected Ok (Some _) from tryLoadScene, got %A{other}"
        Assert.Equal<SceneSnapshot>(captured, loaded)

        // restore onto a DIFFERENT base model (the static test scene: other elements, free
        // placement, empty palette) — proving the round-trip reproduces the source scene rather
        // than leaving a no-op.
        let restored = restoreScene loaded (init ())

        // element ids ...
        Assert.Equal<string list>(
            [ "src"; "pol"; "det" ],
            restored.elements |> List.map (fun e -> e.id.value))
        // ... kinds ...
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; Detector ],
            restored.elements |> List.map (fun e -> e.placement.catalogueKind))
        // ... bindings (the Library `valueId`) ...
        Assert.Equal<string option list>(
            [ Some "src-600"; Some "pol-lp"; None ],
            restored.elements |> List.map (fun e -> e.placement.valueId))
        // ... full placements + draw zoom (the whole `TestElement`, in order) ...
        Assert.Equal<TestElement list>([ sourceEl; polarizerEl; detectorEl ], restored.elements)
        // ... and the table view state + snap flag.
        Assert.Equal<Table.TableViewState>(dialledView, restored.view)
        Assert.True(restored.snapChain)

    [<Fact>]
    let ``saveScene overwrites a scene stored under the same name (upsert)`` () =
        let proxy = SceneProxy.createInMemory ()
        let name = sceneName "Bench"
        let first = captureScene (sourceModel ())
        let second = captureScene { sourceModel () with snapChain = false; view = Table.defaultView }

        match proxy.saveScene name first with
        | Ok () -> ()
        | Error e -> Assert.Fail($"first saveScene failed: %A{e}")
        match proxy.saveScene name second with
        | Ok () -> ()
        | Error e -> Assert.Fail($"second saveScene failed: %A{e}")

        // The name is stored once (overwrite, not a duplicate key) ...
        match proxy.listScenes () with
        | Ok names -> Assert.Equal<string list>([ "Bench" ], names |> List.map (fun n -> n.value))
        | Error e -> Assert.Fail($"%A{e}")
        // ... and the latest write wins.
        match proxy.tryLoadScene name with
        | Ok (Some loaded) -> Assert.Equal<SceneSnapshot>(second, loaded)
        | other -> Assert.Fail($"expected Ok (Some _), got %A{other}")

    [<Fact>]
    let ``tryLoadScene returns None for a name that was never saved`` () =
        let proxy = SceneProxy.createInMemory ()
        match proxy.tryLoadScene (sceneName "missing") with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for an unsaved name, got %A{other}")

    [<Fact>]
    let ``saveScene rejects a directly-constructed blank name as InvalidScene`` () =
        // A `SceneName` constructed OUTSIDE `tryCreate` can carry a blank; the store re-validates
        // and rejects it rather than storing an unaddressable key.
        let proxy = SceneProxy.createInMemory ()
        match proxy.saveScene (SceneName "   ") (captureScene (sourceModel ())) with
        | Error (InvalidScene reason) -> Assert.False(System.String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidScene _), got %A{other}")
        // ... and nothing was stored.
        match proxy.listScenes () with
        | Ok names -> Assert.Equal<string list>([], names |> List.map (fun n -> n.value))
        | Error e -> Assert.Fail($"%A{e}")
