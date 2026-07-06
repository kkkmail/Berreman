namespace OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Placement

/// Spec 0027 (024) Phase 1 — pure domain tests for the Library: the elevated `ElementId`, the
/// kind-constrained `entriesForKind`, `tryGetEntry`, the grouping trees, and `LibraryEntry.forKinds`.
/// The proxy is the in-memory mock (`createInMemory`); a stub of the SAME record shape proves the seam.
module LibraryProxyTests =

    let private proxy = Library.createInMemory ()

    [<Fact>]
    let ``ElementId round-trips through its value accessor`` () =
        let id = Library.elementId "abc-123"
        Assert.Equal("abc-123", id.value)
        Assert.Equal<ElementId>(ElementId "abc-123", id)

    [<Fact>]
    let ``entriesForKind Sample returns only SampleItems`` () =
        match proxy.entriesForKind CatalogueKind.Sample with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.All(entries, fun e -> Assert.True((match e with SampleItem _ -> true | _ -> false), e.displayName))
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``entriesForKind Detector returns only DetectorItems`` () =
        match proxy.entriesForKind Detector with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.All(entries, fun e -> Assert.True((match e with DetectorItem _ -> true | _ -> false), e.displayName))
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``entriesForKind LinearPolarizer returns ONLY the ideal LP (not the CPs)`` () =
        match proxy.entriesForKind LinearPolarizer with
        | Ok entries ->
            Assert.Equal(1, List.length entries)
            match entries with
            | [ PolarizerItem p ] -> Assert.Equal(IdealLinear, p.kind)
            | _ -> Assert.Fail("expected exactly one ideal linear polarizer")
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``entriesForKind CircularPolarizer returns EXACTLY the two CP presets (not the LP)`` () =
        match proxy.entriesForKind CircularPolarizer with
        | Ok entries ->
            Assert.Equal(2, List.length entries)
            let kinds =
                entries
                |> List.choose (function PolarizerItem p -> Some p.kind | _ -> None)
                |> Set.ofList
            Assert.Equal<Set<PolarizerKind>>(Set.ofList [ IdealCircularLeft; IdealCircularRight ], kinds)
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``tryGetEntry hits a known id and misses an unknown id`` () =
        match proxy.tryGetEntry "src-600" with
        | Ok (Some (SourceItem s)) -> Assert.Equal("src-600", s.id)
        | other -> Assert.Fail(sprintf "expected the 600 nm source, got %A" other)
        match proxy.tryGetEntry "no-such-id" with
        | Ok None -> ()
        | other -> Assert.Fail(sprintf "expected Ok None, got %A" other)

    [<Fact>]
    let ``libraryTrees returns at least one tree and every leaf entry id resolves`` () =
        match proxy.libraryTrees () with
        | Ok trees ->
            Assert.NotEmpty trees
            let rec leafIds (node : LibraryTreeNode) : string list =
                match node with
                | Leaf (_, id) -> [ id ]
                | Group (_, children) -> children |> List.collect leafIds
            let ids = trees |> List.collect (fun t -> leafIds t.root)
            Assert.NotEmpty ids
            for id in ids do
                match proxy.tryGetEntry id with
                | Ok (Some _) -> ()
                | other -> Assert.Fail(sprintf "tree leaf %s did not resolve: %A" id other)
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``forKinds maps each entry case to its valid catalogue kinds`` () =
        let lp = PolarizerItem { id = "x"; name = "lp"; kind = IdealLinear }
        let cpL = PolarizerItem { id = "y"; name = "cpL"; kind = IdealCircularLeft }
        let cpR = PolarizerItem { id = "z"; name = "cpR"; kind = IdealCircularRight }
        let src = SourceItem { id = "s"; name = "s"; waveLength = WaveLength.nm 500.0<nm> }
        let det = DetectorItem { id = "d"; name = "d"; kind = Intensity }
        let smp =
            SampleItem
                {
                    id = SampleId.create ()
                    name = "p"
                    structure =
                        {
                            films = []
                            substrate = Some { materialId = MaterialLibrary.MaterialIds.glass152; thickness = Thickness.mm 1.0<mm> }
                            lower = None
                        }
                    substrate = Plate
                    description = "a glass plate"
                }
        Assert.Equal<CatalogueKind list>([ LinearPolarizer ], lp.forKinds)
        Assert.Equal<CatalogueKind list>([ CircularPolarizer ], cpL.forKinds)
        Assert.Equal<CatalogueKind list>([ CircularPolarizer ], cpR.forKinds)
        Assert.Equal<CatalogueKind list>([ LightSource ], src.forKinds)
        Assert.Equal<CatalogueKind list>([ Detector ], det.forKinds)
        Assert.Equal<CatalogueKind list>([ CatalogueKind.Sample ], smp.forKinds)

    [<Fact>]
    let ``every seeded entry has a non-empty fullDescription`` () =
        Assert.NotEmpty seedEntries
        Assert.All(seedEntries, fun e ->
            Assert.False(System.String.IsNullOrWhiteSpace e.fullDescription, e.displayName))

    [<Fact>]
    let ``a Sample entry's fullDescription is exactly its curated description`` () =
        let sampleDescriptions =
            seedEntries
            |> List.choose (function SampleItem s -> Some (s, (SampleItem s).fullDescription) | _ -> None)
        Assert.NotEmpty sampleDescriptions
        Assert.All(sampleDescriptions, fun (s, full) -> Assert.Equal(s.description, full))

    [<Fact>]
    let ``a non-Sample entry's fullDescription is prose built from its kind`` () =
        // The detectors / polarizers / source describe what they DO (so a bare name is never the whole story).
        let det = DetectorItem { id = "d"; name = "d"; kind = Ellipsometer }
        Assert.Contains("Ψ", det.fullDescription)
        let lp = PolarizerItem { id = "x"; name = "lp"; kind = IdealLinear }
        Assert.Contains("linear", lp.fullDescription)
        let src = SourceItem { id = "s"; name = "s"; waveLength = WaveLength.nm 500.0<nm> }
        Assert.Contains("500", src.fullDescription)

    [<Fact>]
    let ``expandedFilms mirrors RepeatBuilder.expand (count copies of the cell, order preserved)`` () =
        // Spec 0033 step 001: a Repeated period group flattens to `count` copies of its cell, in cell
        // order, before/after any single layers — the same `List.replicate count cell |> List.concat`
        // shape as `RepeatBuilder.expand`.
        let a = { materialId = MaterialLibrary.MaterialIds.glass152; thickness = Thickness.nm 100.0<nm> }
        let b = { materialId = MaterialLibrary.MaterialIds.vacuum; thickness = Thickness.nm 150.0<nm> }
        let c = { materialId = MaterialLibrary.MaterialIds.silicon; thickness = Thickness.nm 25.0<nm> }
        let structure =
            {
                films = [ Repeated { cell = [ a; b ]; count = 3 }; SingleLayer c ]
                substrate = None
                lower = None
            }
        Assert.Equal<SampleLayer list>([ a; b; a; b; a; b; c ], structure.expandedFilms)

    // ============================ Spec 0033 (002) — elevated ids, round-trips ============================

    [<Fact>]
    let ``every seeded sample round-trips store-lookup by its Guid entry id`` () =
        // Spec 0033 step 002 acceptance: `Sample.id` is a Guid-backed `SampleId`; it crosses the
        // Selector valueId seam as its Guid STRING form (`entryId`) and `tryGetEntry` resolves that
        // string back to the SAME sample.
        Assert.NotEmpty SeedSamples.all
        for s in SeedSamples.all do
            let entryId = (SampleItem s).entryId
            Assert.Equal(string s.id.value, entryId)
            match proxy.tryGetEntry entryId with
            | Ok (Some (SampleItem found)) -> Assert.Equal<SampleId>(s.id, found.id)
            | other -> Assert.Fail(sprintf "sample %s did not round-trip by entry id %s: %A" s.name entryId other)

    [<Fact>]
    let ``a MINTED SampleId round-trips create-store-lookup through a proxy of the same shape`` () =
        // Create (mint) → store (a stub proxy over the entry list) → lookup (by the entry id string).
        let sample =
            {
                id = SampleId.create ()
                name = "Round-trip sample"
                structure =
                    {
                        films = []
                        substrate = Some { materialId = MaterialLibrary.MaterialIds.glass152; thickness = Thickness.mm 1.0<mm> }
                        lower = None
                    }
                substrate = Plate
                description = "A minted-id glass plate."
            }
        let entries = [ SampleItem sample ]
        let stub : LibraryProxy =
            {
                entriesForKind = fun kind -> Ok (entries |> List.filter (fun e -> e.forKinds |> List.contains kind))
                libraryTrees = fun () -> Ok []
                tryGetEntry = fun id -> Ok (entries |> List.tryFind (fun e -> e.entryId = id))
            }
        match stub.tryGetEntry (SampleItem sample).entryId with
        | Ok (Some (SampleItem found)) -> Assert.Equal<SampleId>(sample.id, found.id)
        | other -> Assert.Fail(sprintf "expected the minted sample back, got %A" other)

    [<Fact>]
    let ``the seeded sample ids are distinct, non-empty Guids`` () =
        // The seeds parse fixed literal Guids (never Guid.NewGuid), so every id is distinct and no
        // seed carries the empty Guid (a Guid.Parse typo would surface here).
        let ids = SeedSamples.all |> List.map (fun s -> s.id)
        Assert.Equal(List.length ids, ids |> List.distinct |> List.length)
        Assert.All(SeedSamples.all, fun s -> Assert.NotEqual(System.Guid.Empty, s.id.value))

    [<Fact>]
    let ``a STUB proxy of the same shape drives the same kind-constraint logic`` () =
        // The functional-proxy seam: a test substitutes in-memory stub functions for the proxy fields.
        let onlyDetector = DetectorItem { id = "stub-det"; name = "Stub detector"; kind = Intensity }
        let stub : LibraryProxy =
            {
                entriesForKind = fun kind -> if kind = Detector then Ok [ onlyDetector ] else Ok []
                libraryTrees = fun () -> Ok []
                tryGetEntry = fun id -> Ok (if id = "stub-det" then Some onlyDetector else None)
            }
        Assert.Equal<LibraryEntry list>([ onlyDetector ], (match stub.entriesForKind Detector with Ok e -> e | Error _ -> []))
        Assert.Equal<LibraryEntry list>([], (match stub.entriesForKind LinearPolarizer with Ok e -> e | Error _ -> [ onlyDetector ]))
        match stub.tryGetEntry "stub-det" with
        | Ok (Some (DetectorItem d)) -> Assert.Equal("stub-det", d.id)
        | other -> Assert.Fail(sprintf "%A" other)
