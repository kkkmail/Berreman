namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Scene

/// Spec 0038 step 026 (ADD_CONTRACT STORE_XDUO_0004): the scene persistence seam — the elevated
/// `SceneName` key (blank-rejecting `tryCreate`), the `SnapMode` two-case DU (never a naked bool),
/// and the `[<ReferenceEquality>] SceneProxy`. A DECLARED contract: this file supplies the MOCK (an
/// inline stub over a fixed map) and a mock-driven test exercising `saveScene`, `tryLoadScene`, and
/// `listScenes` through their exact signatures — the real store lands in a later IMPLEMENT_CONTRACT.
module SceneProxyTests =

    /// Build a `SceneName` through the validated `tryCreate` path (the test names are all
    /// non-blank, so this never fails); a `failwith` on the impossible `Error` keeps the happy
    /// path total without an option dance at each call site.
    let private sceneName (s : string) : SceneName =
        match SceneName.tryCreate s with
        | Ok n -> n
        | Error e -> failwith $"test scene name unexpectedly rejected: %A{e}"

    /// A pure `SceneSnapshot` fixture: a light-source element pre-bound to a Library entry and an
    /// unbound detector (so the captured `catalogueKind` / `valueId` carried by `placement` differ
    /// across elements), the default top-down table view, and the given snap mode. No IO, no live
    /// element reference — deterministic across runs.
    let private sampleSnapshot (snap : SnapMode) : SceneSnapshot =
        let source : SceneElement =
            {
                id = Library.elementId "src"
                placement = { ElementPlacement.create LightSource TablePoint.origin with valueId = Some "src-600" }
                zoom = 1.0
            }
        let detector : SceneElement =
            {
                id = Library.elementId "det"
                placement = ElementPlacement.create Detector TablePoint.origin
                zoom = 2.0
            }
        {
            elements = [ source; detector ]
            view = Table.defaultView
            snap = snap
        }

    /// The MOCK: an inline stub `SceneProxy` over a fixed map (a `ref` so `saveScene` genuinely
    /// inserts, proving the seam round-trips rather than exercising an inert stub). A later slice
    /// substitutes the real disk-backed store for this and exercises the exact same logic.
    let private makeStub (initial : Map<string, SceneSnapshot>) : SceneProxy =
        let store = ref initial
        {
            saveScene =
                fun (name : SceneName) (snapshot : SceneSnapshot) ->
                    store.Value <- Map.add name.value snapshot store.Value
                    Ok ()
            tryLoadScene =
                fun (name : SceneName) -> Ok (Map.tryFind name.value store.Value)
            listScenes =
                fun () -> store.Value |> Map.toList |> List.map (fun (k, _) -> SceneName k) |> Ok
        }

    // ===================== SceneName / SnapMode — the elevated primitives =====================

    [<Theory>]
    [<InlineData("")>]
    [<InlineData("   ")>]
    let ``SceneName.tryCreate rejects a blank name as InvalidScene carrying a reason`` (raw : string) =
        match SceneName.tryCreate raw with
        | Error (InvalidScene reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidScene _), got %A{other}")

    [<Fact>]
    let ``SceneName.tryCreate rejects a null name as InvalidScene`` () =
        match SceneName.tryCreate (null : string) with
        | Error (InvalidScene reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidScene _), got %A{other}")

    [<Fact>]
    let ``SceneName.tryCreate accepts a non-blank name and round-trips its value`` () =
        match SceneName.tryCreate "Experiment A" with
        | Ok n -> Assert.Equal("Experiment A", n.value)
        | Error e -> Assert.Fail($"expected Ok, got %A{e}")

    [<Fact>]
    let ``SnapMode round-trips its wire form: true snaps to the beam, false is free placement`` () =
        Assert.Equal(SnapToBeam, SnapMode.create true)
        Assert.Equal(FreePlacement, SnapMode.create false)
        Assert.True(SnapToBeam.value)
        Assert.False(FreePlacement.value)

    // =============================== the SceneProxy seam ===============================

    [<Fact>]
    let ``a SceneProxy compares by reference (the Elmish-required equality)`` () =
        // Function-valued fields have no structural equality; the [<ReferenceEquality>] proxy
        // compares by identity so a host model holding one stays comparable.
        let make () : SceneProxy = makeStub Map.empty
        let p = make ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = make ()))

    [<Fact>]
    let ``a stub SceneProxy over a fixed map exercises saveScene, tryLoadScene, and listScenes through their exact signatures`` () =
        // The stub proves the seam: the SAME record shape, in-test functions over a fixed map — a
        // later slice substitutes the real store for these and exercises the exact same logic.
        let seeded = sceneName "Seeded scene"
        let seededSnapshot = sampleSnapshot SnapToBeam
        let proxy = makeStub (Map.ofList [ seeded.value, seededSnapshot ])

        // listScenes : unit -> Result<SceneName list, SceneStoreError> — over the pre-seeded map.
        match proxy.listScenes () with
        | Ok names -> Assert.Equal<string list>([ "Seeded scene" ], names |> List.map (fun n -> n.value))
        | Error e -> Assert.Fail($"%A{e}")

        // tryLoadScene : SceneName -> Result<SceneSnapshot option, SceneStoreError> — hit and miss.
        match proxy.tryLoadScene seeded with
        | Ok (Some loaded) -> Assert.Equal<SceneSnapshot>(seededSnapshot, loaded)
        | other -> Assert.Fail($"expected Ok (Some _), got %A{other}")
        match proxy.tryLoadScene (sceneName "nowhere") with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for an unknown name, got %A{other}")

        // saveScene : SceneName -> SceneSnapshot -> Result<unit, SceneStoreError> — insert + round-trip.
        let added = sceneName "Added scene"
        let addedSnapshot = sampleSnapshot FreePlacement
        match proxy.saveScene added addedSnapshot with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () from saveScene, got %A{other}")
        match proxy.tryLoadScene added with
        | Ok (Some loaded) -> Assert.Equal<SceneSnapshot>(addedSnapshot, loaded)
        | other -> Assert.Fail($"expected Ok (Some _) after save, got %A{other}")
        match proxy.listScenes () with
        | Ok names ->
            Assert.Equal<string list>(
                [ "Added scene"; "Seeded scene" ],
                names |> List.map (fun n -> n.value) |> List.sort)
        | Error e -> Assert.Fail($"%A{e}")
