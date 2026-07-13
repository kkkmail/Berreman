namespace OpticalConstructor.Tests

open System
open Xunit
open Berreman.Constants
open Berreman.Media
open OpticalConstructor.Domain              // the Propagation module (the version-pin round-trip)
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialStore
open OpticalConstructor.Domain.SampleStore

/// Spec 0038 Part H step 022 (contract STORE_XDUO_0002) — the mutating samples write-seam re-typed
/// IN PLACE into the VERSIONED store, the exact mirror of step 021's `MaterialProxy`.
/// `SampleProxy.createInMemory versionsInUse` keeps a per-`SampleId` history (`ref Map<SampleId,
/// version list>`, seeded from `seedEntries`' samples as version 1 active); the LATEST version is
/// what the offers surface and the library edits, older versions are view-only resolution targets.
/// These facts enumerate the version-growth decision table over a STUBBED `VersionsInUse`,
/// latest-active default listing, inactive hidden from offers yet resolvable by version, supersede
/// behaving as inactive, and the used-version removal hard-block — PLUS the sample-layer version-pin
/// round-trip (a layer keeps resolving its pinned material version after the material mints a newer
/// one). Every test builds a FRESH proxy (a shared stateful proxy would be order-dependent).
module SampleProxyTests =

    /// A fresh store with NO versions in use — the default over which the unused-side of the decision
    /// table and the read surface are exercised.
    let private freshProxy () : SampleProxy =
        SampleProxy.createInMemory VersionsInUse.empty

    /// A fresh store over which the given versioned references are reported as in-use by a live
    /// experiment (the step-20 seam, stubbed) — the used-side of the decision table and the
    /// used-version removal block.
    let private proxyWithUsed (refs : VersionRef list) : SampleProxy =
        SampleProxy.createInMemory { versionsInUse = fun () -> Set.ofList refs }

    /// A seeded template sample (fixed id, thick glass plate) the write tests reuse.
    let private plate : Sample = SeedSamples.glassPlate1mm

    /// A distinct seeded sample whose STRUCTURE differs from `plate` (a 2 mm plate vs a 1 mm plate,
    /// same material) — the physics-change source.
    let private otherStructure : Sample = SeedSamples.glassPlate2mm

    /// A physics change of `plate`: SAME id / name / description / substrate kind, a DIFFERENT
    /// `SampleStructure`. `decideVersioning` sees the physics (the structure) as changed, the
    /// metadata as unchanged.
    let private platePhysicsChanged : Sample =
        { plate with structure = otherStructure.structure }

    /// A metadata-only change of `plate`: SAME structure, a different display name. Physics unchanged,
    /// metadata changed.
    let private plateRenamed : Sample =
        { plate with name = "Renamed plate (metadata only)" }

    /// The version-1 reference of a sample — the key a live experiment binds.
    let private v1Ref (id : SampleId) : VersionRef =
        SampleVersionRef { sampleId = id; version = VersionNumber.first }

    let private v1Id (id : SampleId) : SampleVersionId = { sampleId = id; version = VersionNumber.first }
    let private v2Id (id : SampleId) : SampleVersionId = { sampleId = id; version = VersionNumber.first.next }

    /// A fresh sample under a MINTED id (not in the seeded map) over the plate's structure.
    let private minted () : Sample =
        { plate with id = newSampleId (); name = "Test plate (minted)" }

    /// A structurally-empty structure — no films AND no substrate (spec 0035 step 012): the engine
    /// mapping has nothing to expand, so the write-seam must reject a sample carrying it.
    let private emptyStructure : SampleStructure =
        { films = []; substrate = None; lower = None }

    // ============================ the read surface over the seeded store ============================

    [<Fact>]
    let ``listSamples ActiveOnly returns the seeded samples with distinct ids`` () =
        match (freshProxy ()).listSamples ActiveOnly with
        | Ok samples ->
            Assert.NotEmpty samples
            Assert.Equal(List.length SeedSamples.all, List.length samples)
            let ids = samples |> List.map (fun s -> s.id)
            Assert.Equal(List.length ids, ids |> List.distinct |> List.length)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchSamples with the empty query matches every seeded sample`` () =
        match (freshProxy ()).searchSamples SampleQuery.empty with
        | Ok samples -> Assert.Equal(List.length SeedSamples.all, List.length samples)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchSamples filters by name fragment case-insensitively`` () =
        match (freshProxy ()).searchSamples { SampleQuery.empty with text = "GLASS" } with
        | Ok samples ->
            Assert.NotEmpty samples
            Assert.All(samples, fun s ->
                Assert.True(s.name.IndexOf("glass", StringComparison.OrdinalIgnoreCase) >= 0, s.name))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchSamples filters by substrate kind, composing with the text facet`` () =
        let proxy = freshProxy ()
        match proxy.searchSamples { SampleQuery.empty with substrate = Some Plate } with
        | Ok plates ->
            Assert.NotEmpty plates
            Assert.All(plates, fun s -> Assert.Equal(Plate, s.substrate))
            Assert.Equal(
                SeedSamples.all |> List.filter (fun s -> s.substrate = Plate) |> List.length,
                List.length plates)
        | Error err -> Assert.Fail($"%A{err}")
        // "langasite" matches only the dispersive langasite-on-silicon seed, a thin film.
        match proxy.searchSamples { text = "langasite"; substrate = Some ThinFilm } with
        | Ok [ only ] -> Assert.Equal<SampleId>(SeedSamples.langasiteSilicon.id, only.id)
        | other -> Assert.Fail($"expected exactly the langasite sample, got %A{other}")

    [<Fact>]
    let ``tryGetSample hits a known id and misses a minted one`` () =
        let proxy = freshProxy ()
        match proxy.tryGetSample SeedSamples.euvMoSi.id with
        | Ok (Some s) -> Assert.Equal<SampleId>(SeedSamples.euvMoSi.id, s.id)
        | other -> Assert.Fail($"expected the EUV Mo/Si sample, got %A{other}")
        match proxy.tryGetSample (newSampleId ()) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None, got %A{other}")

    [<Fact>]
    let ``resolveVersion hits version 1 of a seed, misses a non-existent version, and misses an unknown id`` () =
        let proxy = freshProxy ()
        match proxy.resolveVersion (v1Id SeedSamples.glassPlate1mm.id) with
        | Ok (Some s) -> Assert.Equal<SampleId>(SeedSamples.glassPlate1mm.id, s.id)
        | other -> Assert.Fail($"expected version 1 of the glass plate, got %A{other}")
        match proxy.resolveVersion (v2Id SeedSamples.glassPlate1mm.id) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for a non-existent version 2, got %A{other}")
        match proxy.resolveVersion (v1Id (newSampleId ())) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for an unknown id, got %A{other}")

    // ============================ saveSample: new sample ============================

    [<Fact>]
    let ``saveSample on a new id inserts version 1 active — appears in listing and resolves at v1`` () =
        let proxy = freshProxy ()
        let sample = minted ()
        match proxy.saveSample sample with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples ActiveOnly with
        | Ok samples ->
            Assert.Equal(List.length SeedSamples.all + 1, List.length samples)
            Assert.Contains(sample.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id sample.id) with
        | Ok (Some s) -> Assert.Equal<SampleId>(sample.id, s.id)
        | other -> Assert.Fail($"expected version 1 of the minted sample, got %A{other}")

    [<Fact>]
    let ``saveSample rejects a blank name as InvalidSample and persists nothing`` () =
        let proxy = freshProxy ()
        let blank = { minted () with name = "   " }
        match proxy.saveSample blank with
        | Error (InvalidSample reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidSample _), got %A{other}")
        match proxy.tryGetSample blank.id with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None (nothing persisted), got %A{other}")

    [<Fact>]
    let ``saveSample rejects a structurally-empty sample (no films, no substrate) as InvalidSample even when named`` () =
        let proxy = freshProxy ()
        match proxy.saveSample { minted () with structure = emptyStructure } with
        | Error (InvalidSample reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidSample _), got %A{other}")

    // ============================ the version-creation decision table ============================

    [<Fact>]
    let ``KeepCurrent — an identical save on a used version leaves exactly version 1 unchanged`` () =
        let proxy = proxyWithUsed [ v1Ref plate.id ]
        match proxy.saveSample plate with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v2Id plate.id) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected no version 2 after a no-op save, got %A{other}")
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some s) -> Assert.Equal(plate.structure, s.structure)
        | other -> Assert.Fail($"expected version 1 intact, got %A{other}")

    [<Fact>]
    let ``MutateInPlace — a metadata-only edit of a USED version updates version 1 and does NOT mint`` () =
        // The load-bearing rule: metadata (name / description / substrate kind) NEVER versions, even used.
        let proxy = proxyWithUsed [ v1Ref plate.id ]
        match proxy.saveSample plateRenamed with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v2Id plate.id) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected NO version 2 from a metadata-only edit, got %A{other}")
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some s) -> Assert.Equal(plateRenamed.name, s.name)
        | other -> Assert.Fail($"expected version 1 renamed in place, got %A{other}")

    [<Fact>]
    let ``MutateInPlace — a structure change of an UNUSED version updates version 1 in place`` () =
        let proxy = freshProxy ()
        match proxy.saveSample platePhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        Assert.Equal<Result<Sample option, SampleError>>(Ok None, proxy.resolveVersion (v2Id plate.id))
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some s) -> Assert.Equal(otherStructure.structure, s.structure)
        | other -> Assert.Fail($"expected version 1 structure mutated in place, got %A{other}")

    [<Fact>]
    let ``MintNextVersion — a structure change of a USED version freezes version 1 and mints version 2`` () =
        let proxy = proxyWithUsed [ v1Ref plate.id ]
        match proxy.saveSample platePhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        // version 1 preserved with the ORIGINAL structure (view-only history)…
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some s) -> Assert.Equal(plate.structure, s.structure)
        | other -> Assert.Fail($"expected version 1 frozen with original structure, got %A{other}")
        // …version 2 carries the new structure…
        match proxy.resolveVersion (v2Id plate.id) with
        | Ok (Some s) -> Assert.Equal(otherStructure.structure, s.structure)
        | other -> Assert.Fail($"expected version 2 with the new structure, got %A{other}")
        // …and the latest (what the library edits and the offers surface) is version 2.
        match proxy.tryGetSample plate.id with
        | Ok (Some s) -> Assert.Equal(otherStructure.structure, s.structure)
        | other -> Assert.Fail($"expected the latest sample to be version 2, got %A{other}")

    [<Fact>]
    let ``latest-active default listing shows one entry per sample after a mint, not the frozen history`` () =
        let proxy = proxyWithUsed [ v1Ref plate.id ]
        match proxy.saveSample platePhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples ActiveOnly with
        | Ok samples ->
            Assert.Equal(List.length SeedSamples.all, List.length samples)
            let plateEntries = samples |> List.filter (fun s -> s.id = plate.id)
            Assert.Equal(1, List.length plateEntries)
            Assert.Equal(otherStructure.structure, plateEntries.Head.structure)
        | Error err -> Assert.Fail($"%A{err}")

    // ============================ active / inactive / supersede ============================

    [<Fact>]
    let ``markSampleInactive hides a sample from the offers yet keeps it resolvable, and markSampleActive revives it`` () =
        let proxy = freshProxy ()
        match proxy.markSampleInactive plate.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        // hidden from the default (offers) listing and from search…
        match proxy.listSamples ActiveOnly with
        | Ok samples -> Assert.DoesNotContain(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.searchSamples { SampleQuery.empty with text = plate.name } with
        | Ok samples -> Assert.DoesNotContain(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        // …shown with the include-inactive switch…
        match proxy.listSamples IncludeInactive with
        | Ok samples -> Assert.Contains(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        // …still resolvable by version (existing references never break)…
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the inactive version to resolve, got %A{other}")
        // …and revivable.
        match proxy.markSampleActive plate.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples ActiveOnly with
        | Ok samples -> Assert.Contains(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``supersedeSample behaves as inactive — hidden from offers, present under include-inactive, resolvable by version`` () =
        let proxy = freshProxy ()
        match proxy.supersedeSample plate.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples ActiveOnly with
        | Ok samples -> Assert.DoesNotContain(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listSamples IncludeInactive with
        | Ok samples -> Assert.Contains(plate.id, samples |> List.map (fun s -> s.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id plate.id) with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the superseded version to resolve, got %A{other}")

    [<Fact>]
    let ``the lifecycle verbs reject an unknown id with a diagnostic reason`` () =
        let proxy = freshProxy ()
        let missing = (minted ()).id
        for verb in [ proxy.markSampleInactive; proxy.markSampleActive; proxy.supersedeSample ] do
            match verb missing with
            | Error (UnknownSampleId reason) -> Assert.Contains(string missing.value, reason)
            | other -> Assert.Fail($"expected Error (UnknownSampleId _), got %A{other}")

    // ============================ removal: the used-version hard-block ============================

    [<Fact>]
    let ``removeSample on a sample with a used version is blocked with a typed refusal naming the version`` () =
        let proxy = proxyWithUsed [ v1Ref plate.id ]
        match proxy.removeSample plate.id with
        | Error (SampleVersionInUse reason) ->
            Assert.Contains(string plate.id.value, reason)
            Assert.Contains("v1", reason)
        | other -> Assert.Fail($"expected Error (SampleVersionInUse _), got %A{other}")
        // never silently deletes: the sample survives.
        match proxy.tryGetSample plate.id with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the sample to survive the used-version block, got %A{other}")

    [<Fact>]
    let ``removeSample succeeds when no version is in use, then the id is unknown`` () =
        let proxy = freshProxy ()
        match proxy.removeSample SeedSamples.langasiteSilicon.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.tryGetSample SeedSamples.langasiteSilicon.id with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None after removal, got %A{other}")
        match proxy.removeSample SeedSamples.langasiteSilicon.id with
        | Error (UnknownSampleId reason) -> Assert.Contains(string SeedSamples.langasiteSilicon.id.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownSampleId _), got %A{other}")

    // ============================ the seam (reference equality + the stub) ============================

    [<Fact>]
    let ``a SampleProxy compares by reference (the Elmish-required equality)`` () =
        let p = freshProxy ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = freshProxy ()))

    [<Fact>]
    let ``a STUB SampleProxy over the versioned surface exercises every function through its exact signature`` () =
        // The stub proves the seam: the SAME record shape, in-test functions over a fixed
        // two-sample list — a test substitutes these for the real store and exercises the exact same
        // logic.
        let kept = plate
        let free = { plate with id = newSampleId (); name = "Unreferenced test plate" }
        let fixedSamples = [ kept; free ]
        let stub : SampleProxy =
            {
                listSamples = fun _ -> Ok fixedSamples
                searchSamples =
                    fun (q : SampleQuery) ->
                        Ok (fixedSamples |> List.filter (fun s -> s.name.IndexOf(q.text, StringComparison.OrdinalIgnoreCase) >= 0))
                tryGetSample = fun (id : SampleId) -> Ok (fixedSamples |> List.tryFind (fun s -> s.id = id))
                resolveVersion =
                    fun (svid : SampleVersionId) ->
                        Ok (fixedSamples |> List.tryFind (fun s -> s.id = svid.sampleId))
                saveSample =
                    fun (s : Sample) ->
                        if String.IsNullOrWhiteSpace s.name then Error (InvalidSample "blank sample name") else Ok ()
                markSampleInactive = fun (_ : SampleId) -> Ok ()
                markSampleActive = fun (_ : SampleId) -> Ok ()
                supersedeSample = fun (_ : SampleId) -> Ok ()
                removeSample =
                    fun (id : SampleId) ->
                        if id = kept.id
                        then Error (SampleVersionInUse ($"sample '%s{(string id.value)}' has a version bound by a live experiment"))
                        else Ok ()
            }
        match stub.listSamples ActiveOnly with
        | Ok l -> Assert.Equal(2, List.length l)
        | Error err -> Assert.Fail($"%A{err}")
        match stub.searchSamples { SampleQuery.empty with text = "unreferenced" } with
        | Ok [ hit ] -> Assert.Equal<SampleId>(free.id, hit.id)
        | other -> Assert.Fail($"expected exactly the unreferenced sample, got %A{other}")
        match stub.tryGetSample kept.id with
        | Ok (Some s) -> Assert.Equal<SampleId>(kept.id, s.id)
        | other -> Assert.Fail($"expected the kept sample, got %A{other}")
        match stub.resolveVersion (v1Id free.id) with
        | Ok (Some s) -> Assert.Equal<SampleId>(free.id, s.id)
        | other -> Assert.Fail($"expected the free sample by version, got %A{other}")
        match stub.saveSample { free with name = " " } with
        | Error (InvalidSample _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidSample _), got %A{other}")
        match stub.saveSample free with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        for verb in [ stub.markSampleInactive; stub.markSampleActive; stub.supersedeSample ] do
            match verb free.id with
            | Ok () -> ()
            | other -> Assert.Fail($"expected Ok (), got %A{other}")
        match stub.removeSample kept.id with
        | Error (SampleVersionInUse reason) -> Assert.Contains(string kept.id.value, reason)
        | other -> Assert.Fail($"expected Error (SampleVersionInUse _), got %A{other}")
        match stub.removeSample free.id with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")

    // ============================ the sample-layer version-pin round-trip (the acceptance) ============================

    [<Fact>]
    let ``a sample layer keeps resolving its pinned material version after the material mints a newer one`` () =
        // The MATERIAL store: glass152 version 1 is USED by a live experiment, so a physics change
        // mints version 2 rather than mutating the bound version.
        let usedRef = MaterialVersionRef { materialId = MaterialIds.glass152; version = VersionNumber.first }
        let materials = MaterialProxy.createInMemory (fun _ -> []) { versionsInUse = fun () -> Set.ofList [ usedRef ] }
        let glass = builtInEntries |> List.find (fun e -> e.id = MaterialIds.glass152)
        let other = builtInEntries |> List.find (fun e -> e.id = MaterialIds.glass200)
        let glassPhysicsChanged = { glass with complexity = other.complexity; properties = other.properties }
        match materials.saveMaterial glassPhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"the mint failed: %A{err}")
        // A sample whose single film pins glass152 VERSION ONE.
        let pinned = MaterialVersionId.firstOf MaterialIds.glass152
        let sample : Sample =
            { SeedSamples.glassFilm200 with
                id = newSampleId ()
                structure =
                    {
                        films = [ SingleLayer { materialId = pinned; thickness = Thickness.nm 100.0<nm>; orientation = PrimaryAxes } ]
                        substrate = None
                        lower = None
                    } }
        // The pinned layer STILL resolves — through the by-version resolve — after the mint.
        match Propagation.resolveSampleMaterials materials sample with
        | Ok _ -> ()
        | Error err -> Assert.Fail($"the pinned layer failed to resolve after the mint: %A{err}")
        // …and it resolves the ORIGINAL version-1 physics, not the newer version 2 the material now
        // offers as its latest — the whole point of pinning a version.
        match materials.resolveVersion pinned with
        | Ok (Some e) -> Assert.Equal(glass.complexity, e.complexity)
        | other -> Assert.Fail($"expected the pinned v1 to keep its original physics, got %A{other}")
        match materials.tryGetMaterial MaterialIds.glass152 with
        | Ok (Some e) -> Assert.Equal(other.complexity, e.complexity)
        | other -> Assert.Fail($"expected the latest (v2) to carry the new physics, got %A{other}")
