namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle             // VersionRef / SampleVersionRef
open OpticalConstructor.Domain.Experiments
open OpticalConstructor.Domain.ExperimentCollectionStore

/// Spec 0038 step 029 (IMPLEMENT_CONTRACT STORE_XDUO_0005): the experiment-collection persistence
/// seam is now implemented — a real in-memory `ExperimentCollectionProxy.createInMemory` (a `ref`-map
/// store) behind the seam declared at step 028. This drives the acceptance round-trip over
/// deterministic data: a collection holding a full E1 setup, a sample-less E2, and a dark E3 — with
/// and without `DataFilePath` attachments — `saveCollection` → `listCollections` → `tryLoadCollection`
/// reproduces the whole snapshot value-identically, preserving ordered setups, the optional
/// sample/source shapes (incl. the dark experiment), and the per-experiment attachments. Plus the
/// upsert, load-miss, blank-name rejection, and multi-collection listing. Mirrors
/// `SceneRoundTripTests.fs`.
module ExperimentCollectionRoundTripTests =

    /// Build a `CollectionName` through the validated `tryCreate` path (the test names are non-blank).
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

    /// The version-one binding of the seeded 1 mm glass plate — a sample bound BY VERSION (the only
    /// setup facet that pins a versioned reference; the round-trip proves it rides along unchanged).
    let private glassPlateBinding : ElementBinding =
        BoundByVersion (SampleVersionRef (SampleVersionId.firstOf SeedSamples.glassPlate1mm.id))

    // Deterministic experiments spanning the three shapes the acceptance enumerates (E1/E2/E3), each
    // fixing its optional `DataFilePath` so the round-trip proves the attachment survives with AND
    // without a file.

    /// E1 — a full setup: source + polarizer + SAMPLE (bound BY VERSION) + rotating polarizer +
    /// detector, the rotating polarizer varied, WITH an attached measured-data file. The ordered
    /// 5-element chain and the sample's versioned binding must ride through the snapshot unchanged.
    let private e1 : Experiment =
        {
            id = ExperimentId 1
            setup =
                [
                    desc "src" "Light source #1" LightSource (BoundByEntryId "src-600")
                    desc "lp1" "Linear polarizer #2" LinearPolarizer (BoundByEntryId "pol-lp")
                    desc "s" "Sample #3" Sample glassPlateBinding
                    desc "lp2" "Linear polarizer #4" LinearPolarizer (BoundByEntryId "pol-lp")
                    desc "det" "Detector #5" Detector (BoundByEntryId "det-intensity")
                ]
            varied = Some { elementId = el "lp2"; variable = VaryR1 }
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = Some (DataFilePath.create "C:/data/e1-transmitted.csv")
        }

    /// E2 — the sample-LESS variant (source + two polarizers + detector), WITHOUT a data file: the
    /// optional sample shape absent, and the optional attachment absent.
    let private e2 : Experiment =
        { e1 with
            id = ExperimentId 2
            setup =
                [
                    desc "src" "Light source #1" LightSource (BoundByEntryId "src-600")
                    desc "lp1" "Linear polarizer #2" LinearPolarizer (BoundByEntryId "pol-lp")
                    desc "lp2" "Linear polarizer #3" LinearPolarizer (BoundByEntryId "pol-lp")
                    desc "det" "Detector #4" Detector (BoundByEntryId "det-intensity")
                ]
            dataFileOpt = None }

    /// E3 — an empty DARK line (nothing varied), WITH an attached calibration file: proves the
    /// optional attachment rides along even on a setup-less experiment and that the dark
    /// (`varied = None`) shape survives the round-trip.
    let private e3 : Experiment =
        {
            id = ExperimentId 3
            setup = []
            varied = None
            measurement = CaptureTransmitted
            range = VariableRange.forVariable VaryR1
            dataFileOpt = Some (DataFilePath.create "C:/data/e3-dark.csv")
        }

    /// The deterministic fixture snapshot under the given name: the ordered E1/E2/E3.
    let private snapshotOf (name : CollectionName) : ExperimentCollectionSnapshot =
        { name = name; experiments = [ e1; e2; e3 ] }

    [<Fact>]
    let ``createInMemory round-trips a collection of E1 (full) / E2 (sample-less) / E3 (dark), preserving ordered setups, optional shapes, and DataFilePath attachments`` () =
        let proxy = ExperimentCollectionProxy.createInMemory ()
        let name = collectionName "Ellipsometry sweeps"
        let snapshot = snapshotOf name

        // save.
        match proxy.saveCollection snapshot with
        | Ok () -> ()
        | Error e -> Assert.Fail($"saveCollection failed: %A{e}")

        // listCollections lists exactly the saved name.
        match proxy.listCollections () with
        | Ok names -> Assert.Equal<string list>([ "Ellipsometry sweeps" ], names |> List.map (fun n -> n.value))
        | Error e -> Assert.Fail($"%A{e}")

        // load -> the persisted snapshot equals the saved one (the whole pure DATA survived the store).
        let loaded =
            match proxy.tryLoadCollection name with
            | Ok (Some s) -> s
            | other -> failwith $"expected Ok (Some _) from tryLoadCollection, got %A{other}"
        Assert.Equal<ExperimentCollectionSnapshot>(snapshot, loaded)

        // ordered setups preserved: E1's 5-element chain (detector included) ...
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; Sample; LinearPolarizer; Detector ],
            (List.item 0 loaded.experiments).setup |> List.map (fun d -> d.kind))
        // ... E2's sample-less 4-element chain ...
        Assert.Equal<CatalogueKind list>(
            [ LightSource; LinearPolarizer; LinearPolarizer; Detector ],
            (List.item 1 loaded.experiments).setup |> List.map (fun d -> d.kind))
        // ... and E3's empty dark line.
        Assert.Empty((List.item 2 loaded.experiments).setup)

        // optional sample/source shapes incl. the dark experiment: E1 binds a sample BY VERSION,
        // E2 binds none (sample-less), E3 varies nothing (dark).
        Assert.Contains(
            SampleVersionRef (SampleVersionId.firstOf SeedSamples.glassPlate1mm.id),
            boundVersions [ List.item 0 loaded.experiments ])
        Assert.Empty(boundVersions [ List.item 1 loaded.experiments ])
        Assert.Equal(None, (List.item 2 loaded.experiments).varied)

        // DataFilePath attachments (with AND without) ride along per experiment.
        Assert.Equal<DataFilePath option list>(
            [ Some (DataFilePath.create "C:/data/e1-transmitted.csv")
              None
              Some (DataFilePath.create "C:/data/e3-dark.csv") ],
            loaded.experiments |> List.map (fun e -> e.dataFileOpt))

    [<Fact>]
    let ``saveCollection overwrites a collection stored under the same name (upsert)`` () =
        let proxy = ExperimentCollectionProxy.createInMemory ()
        let name = collectionName "Bench"
        let first = snapshotOf name
        // A distinct second value under the SAME name (only the dark E3 kept, no attachments).
        let second = { name = name; experiments = [ { e3 with dataFileOpt = None } ] }

        match proxy.saveCollection first with
        | Ok () -> ()
        | Error e -> Assert.Fail($"first saveCollection failed: %A{e}")
        match proxy.saveCollection second with
        | Ok () -> ()
        | Error e -> Assert.Fail($"second saveCollection failed: %A{e}")

        // The name is stored once (overwrite, not a duplicate key) ...
        match proxy.listCollections () with
        | Ok names -> Assert.Equal<string list>([ "Bench" ], names |> List.map (fun n -> n.value))
        | Error e -> Assert.Fail($"%A{e}")
        // ... and the latest write wins.
        match proxy.tryLoadCollection name with
        | Ok (Some loaded) -> Assert.Equal<ExperimentCollectionSnapshot>(second, loaded)
        | other -> Assert.Fail($"expected Ok (Some _), got %A{other}")

    [<Fact>]
    let ``tryLoadCollection returns None for a name that was never saved`` () =
        let proxy = ExperimentCollectionProxy.createInMemory ()
        match proxy.tryLoadCollection (collectionName "missing") with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for an unsaved name, got %A{other}")

    [<Fact>]
    let ``saveCollection rejects a directly-constructed blank name as InvalidCollection and stores nothing`` () =
        // A `CollectionName` constructed OUTSIDE `tryCreate` can carry a blank; the store re-validates
        // through `CollectionName.tryCreate` and rejects it rather than storing an unaddressable key.
        let proxy = ExperimentCollectionProxy.createInMemory ()
        let blank = { name = CollectionName "   "; experiments = [ e1 ] }
        match proxy.saveCollection blank with
        | Error (InvalidCollection reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidCollection _), got %A{other}")
        // ... and nothing was stored.
        match proxy.listCollections () with
        | Ok names -> Assert.Equal<string list>([], names |> List.map (fun n -> n.value))
        | Error e -> Assert.Fail($"%A{e}")

    [<Fact>]
    let ``listCollections enumerates every saved collection and each loads back value-identically`` () =
        let proxy = ExperimentCollectionProxy.createInMemory ()
        let sweeps = collectionName "Ellipsometry sweeps"
        let calib = collectionName "Calibration set"
        let sweepsSnapshot = snapshotOf sweeps
        let calibSnapshot = { name = calib; experiments = [ e2; e3 ] }

        for s in [ sweepsSnapshot; calibSnapshot ] do
            match proxy.saveCollection s with
            | Ok () -> ()
            | Error e -> Assert.Fail($"saveCollection failed: %A{e}")

        // both names are listed (sorted for a stable comparison — the map has no insertion order) ...
        match proxy.listCollections () with
        | Ok names ->
            Assert.Equal<string list>(
                [ "Calibration set"; "Ellipsometry sweeps" ],
                names |> List.map (fun n -> n.value) |> List.sort)
        | Error e -> Assert.Fail($"%A{e}")
        // ... and each loads back to its own value, independently.
        match proxy.tryLoadCollection sweeps with
        | Ok (Some loaded) -> Assert.Equal<ExperimentCollectionSnapshot>(sweepsSnapshot, loaded)
        | other -> Assert.Fail($"expected Ok (Some _) for the sweeps collection, got %A{other}")
        match proxy.tryLoadCollection calib with
        | Ok (Some loaded) -> Assert.Equal<ExperimentCollectionSnapshot>(calibSnapshot, loaded)
        | other -> Assert.Fail($"expected Ok (Some _) for the calibration collection, got %A{other}")
