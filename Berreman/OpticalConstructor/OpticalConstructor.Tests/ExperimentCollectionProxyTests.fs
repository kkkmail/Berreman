namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ExperimentCollectionStore

/// Spec 0038 step 028 (ADD_CONTRACT STORE_XDUO_0005): the experiment-collection persistence seam —
/// the elevated `CollectionName` key (blank-rejecting `tryCreate`), the pure
/// `ExperimentCollectionSnapshot` over a NAMED list of the step-25 full-setup `Experiment` values
/// (each carrying its optional `DataFilePath` attachment), and the `[<ReferenceEquality>]`
/// `ExperimentCollectionProxy`. A DECLARED contract: this file supplies the MOCK (an inline stub over
/// a fixed map) and a mock-driven test exercising `saveCollection`, `tryLoadCollection`, and
/// `listCollections` through their exact signatures — the real store lands in a later
/// IMPLEMENT_CONTRACT. Mirrors `SceneProxyTests.fs`.
module ExperimentCollectionProxyTests =

    /// Build a `CollectionName` through the validated `tryCreate` path (the test names are all
    /// non-blank, so this never fails); a `failwith` on the impossible `Error` keeps the happy path
    /// total without an option dance at each call site.
    let private collectionName (s : string) : CollectionName =
        match CollectionName.tryCreate s with
        | Ok n -> n
        | Error e -> failwith $"test collection name unexpectedly rejected: %A{e}"

    let private el (s : string) : ElementId = Library.elementId s

    /// A placement/orientation summary for a rest-pose element of the given kind.
    let private placementOf (kind : CatalogueKind) : PlacementSummary =
        PlacementSummary.ofPlacement (ElementPlacement.create kind TablePoint.origin)

    /// A captured setup descriptor (id / label / kind / rest placement / binding).
    let private desc (idStr : string) (label : string) (kind : CatalogueKind) (binding : ElementBinding) : ElementDescriptor =
        { ElementDescriptor.elementId = el idStr; label = label; kind = kind; placement = placementOf kind; binding = binding }

    /// A step-25 full-setup experiment (source + polarizer + detector, the polarizer varied) carrying
    /// its optional `DataFilePath` attachment — the field that must ride along through the snapshot.
    let private experimentWith (n : int) (dataFileOpt : DataFilePath option) : Experiment =
        {
            id = ExperimentId n
            setup =
                [
                    desc "src" "Light source #1" LightSource (BoundByEntryId "src-600")
                    desc "lp" "Linear polarizer #2" LinearPolarizer (BoundByEntryId "pol-lp")
                    desc "det" "Detector #3" Detector (BoundByEntryId "det-intensity")
                ]
            varied = Some { elementId = el "lp"; variable = VaryR1 }
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = dataFileOpt
        }

    /// A pure `ExperimentCollectionSnapshot` fixture under the given name: two full-setup experiments,
    /// the first WITH an attached measured-data file and the second WITHOUT — so the round-trip proves
    /// the optional `DataFilePath` attachment survives per experiment. No IO, deterministic across runs.
    let private sampleSnapshot (name : CollectionName) : ExperimentCollectionSnapshot =
        {
            name = name
            experiments =
                [
                    experimentWith 1 (Some (DataFilePath.create "C:/data/run1.csv"))
                    experimentWith 2 None
                ]
        }

    /// The MOCK: an inline stub `ExperimentCollectionProxy` over a fixed map (a `ref` so
    /// `saveCollection` genuinely inserts, proving the seam round-trips rather than exercising an inert
    /// stub). A later slice substitutes the real disk-backed store for this and exercises the exact
    /// same logic.
    let private makeStub (initial : Map<string, ExperimentCollectionSnapshot>) : ExperimentCollectionProxy =
        let store = ref initial
        {
            saveCollection =
                fun (snapshot : ExperimentCollectionSnapshot) ->
                    store.Value <- Map.add snapshot.name.value snapshot store.Value
                    Ok ()
            tryLoadCollection =
                fun (name : CollectionName) -> Ok (Map.tryFind name.value store.Value)
            listCollections =
                fun () -> store.Value |> Map.toList |> List.map (fun (k, _) -> CollectionName k) |> Ok
        }

    // ===================== CollectionName — the elevated key =====================

    [<Theory>]
    [<InlineData("")>]
    [<InlineData("   ")>]
    let ``CollectionName.tryCreate rejects a blank name as InvalidCollection carrying a reason`` (raw : string) =
        match CollectionName.tryCreate raw with
        | Error (InvalidCollection reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidCollection _), got %A{other}")

    [<Fact>]
    let ``CollectionName.tryCreate rejects a null name as InvalidCollection`` () =
        match CollectionName.tryCreate (null : string) with
        | Error (InvalidCollection reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidCollection _), got %A{other}")

    [<Fact>]
    let ``CollectionName.tryCreate accepts a non-blank name and round-trips its value`` () =
        match CollectionName.tryCreate "Ellipsometry sweeps" with
        | Ok n -> Assert.Equal("Ellipsometry sweeps", n.value)
        | Error e -> Assert.Fail($"expected Ok, got %A{e}")

    // =============================== the ExperimentCollectionProxy seam ===============================

    [<Fact>]
    let ``an ExperimentCollectionProxy compares by reference (the Elmish-required equality)`` () =
        // Function-valued fields have no structural equality; the [<ReferenceEquality>] proxy compares
        // by identity so a host model holding one stays comparable.
        let make () : ExperimentCollectionProxy = makeStub Map.empty
        let p = make ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = make ()))

    [<Fact>]
    let ``a stub ExperimentCollectionProxy over a fixed map exercises saveCollection, tryLoadCollection, and listCollections through their exact signatures`` () =
        // The stub proves the seam: the SAME record shape, in-test functions over a fixed map — a later
        // slice substitutes the real store for these and exercises the exact same logic.
        let seeded = collectionName "Seeded collection"
        let seededSnapshot = sampleSnapshot seeded
        let proxy = makeStub (Map.ofList [ seeded.value, seededSnapshot ])

        // listCollections : unit -> Result<CollectionName list, ExperimentCollectionError> — pre-seeded.
        match proxy.listCollections () with
        | Ok names -> Assert.Equal<string list>([ "Seeded collection" ], names |> List.map (fun n -> n.value))
        | Error e -> Assert.Fail($"%A{e}")

        // tryLoadCollection : CollectionName -> Result<ExperimentCollectionSnapshot option, _> — hit + miss.
        match proxy.tryLoadCollection seeded with
        | Ok (Some loaded) ->
            Assert.Equal<ExperimentCollectionSnapshot>(seededSnapshot, loaded)
            // the optional DataFilePath attachment rides along per experiment (step-25 shape)
            Assert.Equal(Some (DataFilePath.create "C:/data/run1.csv"), (List.head loaded.experiments).dataFileOpt)
            Assert.Equal(None, (List.item 1 loaded.experiments).dataFileOpt)
        | other -> Assert.Fail($"expected Ok (Some _), got %A{other}")
        match proxy.tryLoadCollection (collectionName "nowhere") with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for an unknown name, got %A{other}")

        // saveCollection : ExperimentCollectionSnapshot -> Result<unit, _> — insert + round-trip.
        let added = collectionName "Added collection"
        let addedSnapshot = sampleSnapshot added
        match proxy.saveCollection addedSnapshot with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () from saveCollection, got %A{other}")
        match proxy.tryLoadCollection added with
        | Ok (Some loaded) -> Assert.Equal<ExperimentCollectionSnapshot>(addedSnapshot, loaded)
        | other -> Assert.Fail($"expected Ok (Some _) after save, got %A{other}")
        match proxy.listCollections () with
        | Ok names ->
            Assert.Equal<string list>(
                [ "Added collection"; "Seeded collection" ],
                names |> List.map (fun n -> n.value) |> List.sort)
        | Error e -> Assert.Fail($"%A{e}")
