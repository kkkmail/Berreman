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
/// The proxy is `BuiltinCatalogue.library`; a stub of the SAME record shape proves the seam.
module LibraryProxyTests =

    let private proxy = BuiltinCatalogue.library ()

    [<Fact>]
    let ``ElementId round-trips through its value accessor`` () =
        let id = Library.ElementId "abc-123"
        Assert.Equal("abc-123", id.value)
        Assert.Equal<ElementId>(ElementId "abc-123", id)

    [<Fact>]
    let ``entriesForKind Sample returns only SampleItems`` () =
        match proxy.entriesForKind CatalogueKind.Sample with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.All(entries, fun e -> Assert.True((match e with SampleItem _ -> true | _ -> false), e.displayName))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``entriesForKind Detector returns only DetectorItems`` () =
        match proxy.entriesForKind Detector with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.All(entries, fun e -> Assert.True((match e with DetectorItem _ -> true | _ -> false), e.displayName))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``entriesForKind LinearPolarizer returns ONLY the ideal LP (not the CPs)`` () =
        match proxy.entriesForKind LinearPolarizer with
        | Ok entries ->
            Assert.Equal(1, List.length entries)
            match entries with
            | [ PolarizerItem p ] -> Assert.Equal<PolarizerBehavior>(ComputedIdeal IdealLinear, p.behavior)
            | _ -> Assert.Fail("expected exactly one ideal linear polarizer")
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``entriesForKind CircularPolarizer returns EXACTLY the two CP presets (not the LP)`` () =
        match proxy.entriesForKind CircularPolarizer with
        | Ok entries ->
            Assert.Equal(2, List.length entries)
            let kinds =
                entries
                |> List.choose (function PolarizerItem { behavior = ComputedIdeal k } -> Some k | _ -> None)
                |> Set.ofList
            Assert.Equal<Set<PolarizerKind>>(Set.ofList [ IdealCircularLeft; IdealCircularRight ], kinds)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``tryGetEntry hits a known id and misses an unknown id`` () =
        match proxy.tryGetEntry PresetIds.source600 with
        | Ok (Some (SourceItem s)) -> Assert.Equal(PresetIds.source600, s.id)
        | other -> Assert.Fail($"expected the 600 nm source, got %A{other}")
        match proxy.tryGetEntry (ElementValueId "no-such-id") with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None, got %A{other}")

    [<Fact>]
    let ``libraryTrees returns at least one tree and every leaf entry id resolves`` () =
        match proxy.libraryTrees () with
        | Ok trees ->
            Assert.NotEmpty trees
            let rec leafIds (node : LibraryTreeNode) : ElementValueId list =
                match node with
                | Leaf (_, id) -> [ id ]
                | Group (_, children) -> children |> List.collect leafIds
            let ids = trees |> List.collect (fun t -> leafIds t.root)
            Assert.NotEmpty ids
            for id in ids do
                match proxy.tryGetEntry id with
                | Ok (Some _) -> ()
                | other -> Assert.Fail($"tree leaf %s{id.value} did not resolve: %A{other}")
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``the By kind tree groups entries by kind, then facet, then leaf`` () : unit =
        let film = BuiltinCatalogue.sample SampleIds.glassFilm200Id |> SampleItem
        let mirror = BuiltinCatalogue.sample SampleIds.euvMoSiId |> SampleItem
        let fixedSource = BuiltinCatalogue.preset PresetIds.heNe633
        let tunableSource = BuiltinCatalogue.preset PresetIds.xenonMonochromator
        let detector = BuiltinCatalogue.preset PresetIds.detectorEllipsometer
        let linear = BuiltinCatalogue.preset PresetIds.polarizerLp
        let circular = BuiltinCatalogue.preset PresetIds.polarizerCpLeft
        let entries = [ circular; mirror; tunableSource; detector; film; linear; fixedSource ]
        let expected : LibraryTree =
            {
                name = TreeName "By kind"
                root =
                    Group (TreeLabel "Library",
                        [
                            Group (TreeLabel "Samples",
                                [
                                    Group (TreeLabel "Thin films", [ Leaf (film.entryName.toTreeLabel, film.entryId) ])
                                    Group (TreeLabel "Mirrors", [ Leaf (mirror.entryName.toTreeLabel, mirror.entryId) ])
                                ])
                            Group (TreeLabel "Sources",
                                [
                                    Group (TreeLabel "Fixed lines", [ Leaf (fixedSource.entryName.toTreeLabel, fixedSource.entryId) ])
                                    Group (TreeLabel "Tunable", [ Leaf (tunableSource.entryName.toTreeLabel, tunableSource.entryId) ])
                                ])
                            Group (TreeLabel "Detectors", [ Leaf (detector.entryName.toTreeLabel, detector.entryId) ])
                            Group (TreeLabel "Polarizers",
                                [
                                    Group (TreeLabel "Linear", [ Leaf (linear.entryName.toTreeLabel, linear.entryId) ])
                                    Group (TreeLabel "Circular", [ Leaf (circular.entryName.toTreeLabel, circular.entryId) ])
                                ])
                        ])
            }
        Assert.Equal<LibraryTree>(expected, LibraryTree.byKind entries)
        let empty : LibraryTree =
            {
                name = TreeName "By kind"
                root = Group (TreeLabel "Library", [])
            }
        Assert.Equal<LibraryTree>(empty, LibraryTree.byKind [])

        let secondFixedSource = BuiltinCatalogue.preset PresetIds.heNe543
        let sourceOrder : LibraryTree =
            {
                name = TreeName "By kind"
                root =
                    Group (TreeLabel "Library",
                        [
                            Group (TreeLabel "Sources",
                                [
                                    Group (TreeLabel "Fixed lines",
                                        [
                                            Leaf (secondFixedSource.entryName.toTreeLabel, secondFixedSource.entryId)
                                            Leaf (fixedSource.entryName.toTreeLabel, fixedSource.entryId)
                                        ])
                                ])
                        ])
            }
        Assert.Equal<LibraryTree>(sourceOrder, LibraryTree.byKind [ secondFixedSource; fixedSource ])

    [<Fact>]
    let ``the library-entry store derives its By kind tree from what it holds`` () : unit =
        let store = Library.createInMemory ()
        let empty : LibraryTree =
            {
                name = TreeName "By kind"
                root = Group (TreeLabel "Library", [])
            }
        Assert.Equal<Result<LibraryTree list, LibraryError>>(Ok [ empty ], store.proxy.libraryTrees ())
        let detector = BuiltinCatalogue.preset PresetIds.detectorIntensity
        Assert.Equal<Result<unit, LibraryError>>(Ok (), store.addEntry detector)
        let populated : LibraryTree =
            {
                name = TreeName "By kind"
                root =
                    Group (TreeLabel "Library",
                        [ Group (TreeLabel "Detectors", [ Leaf (detector.entryName.toTreeLabel, detector.entryId) ]) ])
            }
        Assert.Equal<Result<LibraryTree list, LibraryError>>(Ok [ populated ], store.proxy.libraryTrees ())

    [<Fact>]
    let ``the live Library's By kind tree holds every live entry exactly once`` () : unit =
        let stores = BuiltinCatalogue.freshStores ()
        let sources = SourceProxy.createInMemory ()
        let library = liveLibrary stores.libraryEntries stores.samples sources
        let source : SourcePreset =
            {
                BuiltinCatalogue.source PresetIds.heNe633 with
                    id = newSourceId ()
                    name = MaterialLibrary.EntryName "By kind user source"
                    protection = UserManaged
            }
        let sample : Sample =
            {
                BuiltinCatalogue.sample SampleIds.euvMoSiId with
                    id = newSampleId ()
                    name = MaterialLibrary.EntryName "By kind user mirror"
            }
        Assert.Equal<Result<unit, SourceError>>(Ok (), sources.saveSource source)
        Assert.Equal<Result<unit, SampleError>>(Ok (), stores.samples.saveSample sample)
        let kinds = [ CatalogueKind.Sample; LightSource; Detector; LinearPolarizer; CircularPolarizer ]
        let offeredIds =
            kinds
            |> List.collect (fun kind ->
                match library.entriesForKind kind with
                | Ok entries -> entries |> List.map (fun entry -> entry.entryId)
                | Error _ ->
                    Assert.Fail("the live library could not list its entries")
                    [])
            |> List.distinct
        let rec leafIds (node : LibraryTreeNode) : ElementValueId list =
            match node with
            | Leaf (_, id) -> [ id ]
            | Group (_, children) -> children |> List.collect leafIds
        match library.libraryTrees () with
        | Ok [ tree ] ->
            Assert.Equal<TreeName>(TreeName "By kind", tree.name)
            let ids = leafIds tree.root
            Assert.Equal<ElementValueId list>(List.sort offeredIds, List.sort ids)
            Assert.Equal(ids.Length, ids |> List.distinct |> List.length)
            Assert.Contains(source.id, ids)
            Assert.Contains(sample.id.toElementValueId, ids)
        | Ok _ | Error _ -> Assert.Fail("expected one derived By kind tree from the live library")

        let firstError = LibraryUnavailableErr "sample listing unavailable"
        let laterError = LibraryUnavailableErr "source listing unavailable"
        let unavailablePresets : LibraryProxy =
            {
                stores.libraryEntries with
                    entriesForKind = fun kind -> Error (if kind = CatalogueKind.Sample then firstError else laterError)
            }
        let unavailableLibrary = liveLibrary unavailablePresets stores.samples sources
        Assert.Equal<Result<LibraryTree list, LibraryError>>(Error firstError, unavailableLibrary.libraryTrees ())

    [<Fact>]
    let ``forKinds maps each entry case to its valid catalogue kinds`` () =
        let lp = PolarizerItem { id = ElementValueId "x"; name = MaterialLibrary.EntryName "lp"; behavior = ComputedIdeal IdealLinear; category = LpCategory; protection = UserManaged }
        let cpL = PolarizerItem { id = ElementValueId "y"; name = MaterialLibrary.EntryName "cpL"; behavior = ComputedIdeal IdealCircularLeft; category = CpCategory; protection = UserManaged }
        let cpR = PolarizerItem { id = ElementValueId "z"; name = MaterialLibrary.EntryName "cpR"; behavior = ComputedIdeal IdealCircularRight; category = CpCategory; protection = UserManaged }
        let src = SourceItem { id = ElementValueId "s"; name = MaterialLibrary.EntryName "s"; waveLength = WaveLength.nm 500.0<nm>; tuning = FixedLine; protection = UserManaged }
        let det = DetectorItem { id = ElementValueId "d"; name = MaterialLibrary.EntryName "d"; kind = Intensity; protection = UserManaged }
        let smp =
            SampleItem
                {
                    id = SampleId.create ()
                    name = MaterialLibrary.EntryName "p"
                    structure =
                        {
                            films = []
                            substrate = Some { materialId = MaterialLibrary.MaterialVersionId.firstOf MaterialLibrary.MaterialIds.glass152; thickness = Thickness.mm 1.0<mm>; orientation = PrimaryAxes }
                            lower = None
                        }
                    substrate = ThickPlate
                    description = MaterialLibrary.EntryDescription "a glass plate"
                    supportedEmission = ThickPlate.defaultSupportedEmission
                }
        Assert.Equal<CatalogueKind list>([ LinearPolarizer ], lp.forKinds)
        Assert.Equal<CatalogueKind list>([ CircularPolarizer ], cpL.forKinds)
        Assert.Equal<CatalogueKind list>([ CircularPolarizer ], cpR.forKinds)
        Assert.Equal<CatalogueKind list>([ LightSource ], src.forKinds)
        Assert.Equal<CatalogueKind list>([ Detector ], det.forKinds)
        Assert.Equal<CatalogueKind list>([ CatalogueKind.Sample ], smp.forKinds)

    [<Fact>]
    let ``every seeded entry has a non-empty fullDescription`` () =
        Assert.NotEmpty ((BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ())
        Assert.All(((BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ()), fun e ->
            Assert.False(System.String.IsNullOrWhiteSpace e.fullDescription, e.displayName))

    [<Fact>]
    let ``a Sample entry's fullDescription is exactly its curated description`` () =
        let sampleDescriptions =
            ((BuiltinCatalogue.samples () |> List.map SampleItem) @ BuiltinCatalogue.presets ())
            |> List.choose (function SampleItem s -> Some (s, (SampleItem s).fullDescription) | _ -> None)
        Assert.NotEmpty sampleDescriptions
        Assert.All(sampleDescriptions, fun (s, full) -> Assert.Equal(s.description.value, full))

    [<Fact>]
    let ``a non-Sample entry's fullDescription is prose built from its kind`` () =
        // The detectors / polarizers / source describe what they DO (so a bare name is never the whole story).
        let det = DetectorItem { id = ElementValueId "d"; name = MaterialLibrary.EntryName "d"; kind = Ellipsometer; protection = UserManaged }
        Assert.Contains("Ψ", det.fullDescription)
        let lp = PolarizerItem { id = ElementValueId "x"; name = MaterialLibrary.EntryName "lp"; behavior = ComputedIdeal IdealLinear; category = LpCategory; protection = UserManaged }
        Assert.Contains("linear", lp.fullDescription)
        let src = SourceItem { id = ElementValueId "s"; name = MaterialLibrary.EntryName "s"; waveLength = WaveLength.nm 500.0<nm>; tuning = FixedLine; protection = UserManaged }
        Assert.Contains("500", src.fullDescription)

    [<Fact>]
    let ``expandedFilms mirrors RepeatBuilder.expand (count copies of the cell, order preserved)`` () =
        // Spec 0033 step 001: a Repeated period group flattens to `count` copies of its cell, in cell
        // order, before/after any single layers — the same `List.replicate count cell |> List.concat`
        // shape as `RepeatBuilder.expand`.
        let a = { materialId = MaterialLibrary.MaterialVersionId.firstOf MaterialLibrary.MaterialIds.glass152; thickness = Thickness.nm 100.0<nm>; orientation = PrimaryAxes }
        let b = { materialId = MaterialLibrary.MaterialVersionId.firstOf MaterialLibrary.MaterialIds.vacuumMaterial; thickness = Thickness.nm 150.0<nm>; orientation = PrimaryAxes }
        let c = { materialId = MaterialLibrary.MaterialVersionId.firstOf MaterialLibrary.MaterialIds.silicon; thickness = Thickness.nm 25.0<nm>; orientation = PrimaryAxes }
        let structure =
            {
                films = [ Repeated { cell = [ a; b ]; count = PeriodCount 3 }; SingleLayer c ]
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
        Assert.NotEmpty (BuiltinCatalogue.samples ())
        for s in BuiltinCatalogue.samples () do
            let entryId = (SampleItem s).entryId
            Assert.Equal(string s.id.value, entryId.value)
            match proxy.tryGetEntry entryId with
            | Ok (Some (SampleItem found)) -> Assert.Equal<SampleId>(s.id, found.id)
            | other -> Assert.Fail($"sample %s{s.name.value} did not round-trip by entry id %s{entryId.value}: %A{other}")

    [<Fact>]
    let ``a MINTED SampleId round-trips create-store-lookup through a proxy of the same shape`` () =
        // Create (mint) → store (a stub proxy over the entry list) → lookup (by the entry id string).
        let sample =
            {
                id = SampleId.create ()
                name = MaterialLibrary.EntryName "Round-trip sample"
                structure =
                    {
                        films = []
                        substrate = Some { materialId = MaterialLibrary.MaterialVersionId.firstOf MaterialLibrary.MaterialIds.glass152; thickness = Thickness.mm 1.0<mm>; orientation = PrimaryAxes }
                        lower = None
                    }
                substrate = ThickPlate
                description = MaterialLibrary.EntryDescription "A minted-id glass plate."
                supportedEmission = ThickPlate.defaultSupportedEmission
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
        | other -> Assert.Fail($"expected the minted sample back, got %A{other}")

    [<Fact>]
    let ``the seeded sample ids are distinct, non-empty Guids`` () =
        // BuiltinCatalogue.samples must retain its frozen, non-empty Guids (spec 0050 §3.3).
        let ids = BuiltinCatalogue.samples () |> List.map (fun s -> s.id)
        Assert.Equal(List.length ids, ids |> List.distinct |> List.length)
        Assert.All((BuiltinCatalogue.samples ()), fun s -> Assert.NotEqual(System.Guid.Empty, s.id.value))

    [<Fact>]
    let ``a STUB proxy of the same shape drives the same kind-constraint logic`` () =
        // The functional-proxy seam: a test substitutes in-memory stub functions for the proxy fields.
        let onlyDetector = DetectorItem { id = ElementValueId "stub-det"; name = MaterialLibrary.EntryName "Stub detector"; kind = Intensity; protection = UserManaged }
        let stub : LibraryProxy =
            {
                entriesForKind = fun kind -> if kind = Detector then Ok [ onlyDetector ] else Ok []
                libraryTrees = fun () -> Ok []
                tryGetEntry = fun id -> Ok (if id = ElementValueId "stub-det" then Some onlyDetector else None)
            }
        Assert.Equal<LibraryEntry list>([ onlyDetector ], (match stub.entriesForKind Detector with Ok e -> e | Error _ -> []))
        Assert.Equal<LibraryEntry list>([], (match stub.entriesForKind LinearPolarizer with Ok e -> e | Error _ -> [ onlyDetector ]))
        match stub.tryGetEntry (ElementValueId "stub-det") with
        | Ok (Some (DetectorItem d)) -> Assert.Equal("stub-det", d.id.value)
        | other -> Assert.Fail($"%A{other}")
