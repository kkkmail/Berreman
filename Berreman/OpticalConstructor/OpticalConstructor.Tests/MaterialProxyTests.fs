namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Lifecycle
open OpticalConstructor.Domain.MaterialStore

/// Spec 0038 Part H step 021 (contract STORE_XDUO_0001) — the mutating materials write-seam
/// re-typed IN PLACE into the VERSIONED store. `MaterialProxy.createInMemory samplesReferencing
/// versionsInUse` keeps a per-`MaterialId` history (`ref Map<MaterialId, version list>`, seeded
/// from `builtInEntries` as version 1 active); the LATEST version is what the offers surface and
/// the library edits, older versions are view-only resolution targets. These facts enumerate the
/// version-growth decision table over a STUBBED `VersionsInUse`, latest-active default listing,
/// inactive hidden from offers yet resolvable by version, supersede behaving as inactive, and BOTH
/// removal hard-blocks (a sample reference and a used version). Every test builds a FRESH proxy (a
/// shared stateful proxy would be order-dependent under xUnit).
module MaterialProxyTests =

    /// A fresh store with NO referencing samples and NO versions in use — the default over which the
    /// unused-side of the decision table and the read surface are exercised.
    let private freshProxy () : MaterialProxy =
        MaterialProxy.createInMemory (fun _ -> []) VersionsInUse.empty

    /// A fresh store over which the given versioned references are reported as in-use by a live
    /// experiment (the step-20 seam, stubbed) — the used-side of the decision table and the
    /// used-version removal block.
    let private proxyWithUsed (refs : VersionRef list) : MaterialProxy =
        MaterialProxy.createInMemory (fun _ -> []) { versionsInUse = fun () -> Set.ofList refs }

    /// The composed pair the slice pins: a fresh step-005 samples store and a materials store whose
    /// referencing lookup is backed by it (`samplesReferencing` — the composition-root wiring; the
    /// `MaterialStillReferenced` block).
    let private composedProxies () : SampleProxy * MaterialProxy =
        let samples = SampleProxy.createInMemory ()
        samples, MaterialProxy.createInMemory (samplesReferencing samples) VersionsInUse.empty

    /// A built-in EDITABLE template entry (fixed id, non-dispersive glass, `complexity = Some`) the
    /// write tests reuse, and a distinct built-in whose physics (complexity) differs.
    let private glass : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = MaterialIds.glass152)

    let private otherPhysics : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = MaterialIds.glass200)

    /// A physics change of `glass`: SAME id / name / category / description, a DIFFERENT complexity
    /// (and its derived properties). `decideVersioning` sees the physics as changed, the metadata as
    /// unchanged.
    let private glassPhysicsChanged : MaterialEntry =
        { glass with complexity = otherPhysics.complexity; properties = otherPhysics.properties }

    /// A metadata-only change of `glass`: SAME complexity, a different display name. Physics
    /// unchanged, metadata changed.
    let private glassRenamed : MaterialEntry =
        { glass with name = "Renamed glass (metadata only)" }

    /// The version-1 reference of a material — the key a live experiment binds.
    let private v1Ref (id : MaterialId) : VersionRef =
        MaterialVersionRef { materialId = id; version = VersionNumber.first }

    let private v1Id (id : MaterialId) : MaterialVersionId = { materialId = id; version = VersionNumber.first }
    let private v2Id (id : MaterialId) : MaterialVersionId = { materialId = id; version = VersionNumber.first.next }

    /// A fresh entry under a MINTED id (not in the seeded map) over the glass properties.
    let private minted () : MaterialEntry =
        { glass with id = newMaterialId (); name = "Test glass (minted)" }

    // ============================ the read surface over the seeded store ============================

    [<Fact>]
    let ``listMaterials ActiveOnly returns the seeded built-in entries with distinct ids`` () =
        match (freshProxy ()).listMaterials ActiveOnly with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.Equal(List.length builtInEntries, List.length entries)
            let ids = entries |> List.map (fun e -> e.id)
            Assert.Equal(List.length ids, ids |> List.distinct |> List.length)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchMaterials with the empty query matches every seeded entry`` () =
        match (freshProxy ()).searchMaterials MaterialQuery.empty with
        | Ok entries -> Assert.Equal(List.length builtInEntries, List.length entries)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchMaterials filters by name fragment case-insensitively`` () =
        match (freshProxy ()).searchMaterials { MaterialQuery.empty with text = "GLASS" } with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.All(entries, fun e ->
                Assert.True(e.name.IndexOf("glass", StringComparison.OrdinalIgnoreCase) >= 0, e.name))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchMaterials dispersion facets partition the library`` () =
        let proxy = freshProxy ()
        match proxy.searchMaterials { MaterialQuery.empty with dispersion = OnlyDispersive } with
        | Ok dispersive ->
            let ids = dispersive |> List.map (fun e -> e.id) |> Set.ofList
            Assert.Equal<Set<MaterialId>>(Set.ofList [ MaterialIds.silicon; MaterialIds.langasite ], ids)
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.searchMaterials { MaterialQuery.empty with dispersion = OnlyNonDispersive } with
        | Ok nonDispersive -> Assert.Equal(List.length builtInEntries - 2, List.length nonDispersive)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``tryGetMaterial hits a known id and misses a minted one`` () =
        let proxy = freshProxy ()
        match proxy.tryGetMaterial MaterialIds.silicon with
        | Ok (Some e) -> Assert.Equal<MaterialId>(MaterialIds.silicon, e.id)
        | other -> Assert.Fail($"expected the silicon entry, got %A{other}")
        match proxy.tryGetMaterial (newMaterialId ()) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None, got %A{other}")

    [<Fact>]
    let ``resolveVersion hits version 1 of a seed, misses a non-existent version, and misses an unknown id`` () =
        let proxy = freshProxy ()
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal<MaterialId>(MaterialIds.glass152, e.id)
        | other -> Assert.Fail($"expected version 1 of glass152, got %A{other}")
        match proxy.resolveVersion (v2Id MaterialIds.glass152) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for a non-existent version 2, got %A{other}")
        match proxy.resolveVersion (v1Id (newMaterialId ())) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None for an unknown id, got %A{other}")

    // ============================ saveMaterial: new material ============================

    [<Fact>]
    let ``saveMaterial on a new id inserts version 1 active — appears in listing and resolves at v1`` () =
        let proxy = freshProxy ()
        let entry = minted ()
        match proxy.saveMaterial entry with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials ActiveOnly with
        | Ok entries ->
            Assert.Equal(List.length builtInEntries + 1, List.length entries)
            Assert.Contains(entry.id, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id entry.id) with
        | Ok (Some e) -> Assert.Equal<MaterialId>(entry.id, e.id)
        | other -> Assert.Fail($"expected version 1 of the minted entry, got %A{other}")

    [<Fact>]
    let ``saveMaterial rejects a blank name as InvalidMaterial and persists nothing`` () =
        let proxy = freshProxy ()
        let blank = { minted () with name = "   " }
        match proxy.saveMaterial blank with
        | Error (InvalidMaterial reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidMaterial _), got %A{other}")
        match proxy.tryGetMaterial blank.id with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None (nothing persisted), got %A{other}")

    // ============================ the version-creation decision table ============================

    [<Fact>]
    let ``KeepCurrent — an identical save on a used version leaves exactly version 1 unchanged`` () =
        let proxy = proxyWithUsed [ v1Ref MaterialIds.glass152 ]
        match proxy.saveMaterial glass with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        // no version 2 minted, version 1 unchanged
        match proxy.resolveVersion (v2Id MaterialIds.glass152) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected no version 2 after a no-op save, got %A{other}")
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal(glass.complexity, e.complexity)
        | other -> Assert.Fail($"expected version 1 intact, got %A{other}")

    [<Fact>]
    let ``MutateInPlace — a metadata-only edit of a USED version updates version 1 and does NOT mint`` () =
        // The load-bearing rule: metadata (name / description / category) NEVER versions, even used.
        let proxy = proxyWithUsed [ v1Ref MaterialIds.glass152 ]
        match proxy.saveMaterial glassRenamed with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v2Id MaterialIds.glass152) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected NO version 2 from a metadata-only edit, got %A{other}")
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal(glassRenamed.name, e.name)
        | other -> Assert.Fail($"expected version 1 renamed in place, got %A{other}")

    [<Fact>]
    let ``MutateInPlace — a physics change of an UNUSED version updates version 1 in place`` () =
        let proxy = freshProxy ()
        match proxy.saveMaterial glassPhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        Assert.Equal<Result<MaterialEntry option, MaterialError>>(Ok None, proxy.resolveVersion (v2Id MaterialIds.glass152))
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal(otherPhysics.complexity, e.complexity)
        | other -> Assert.Fail($"expected version 1 physics mutated in place, got %A{other}")

    [<Fact>]
    let ``MintNextVersion — a physics change of a USED version freezes version 1 and mints version 2`` () =
        let proxy = proxyWithUsed [ v1Ref MaterialIds.glass152 ]
        match proxy.saveMaterial glassPhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        // version 1 preserved with the ORIGINAL physics (view-only history)…
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal(glass.complexity, e.complexity)
        | other -> Assert.Fail($"expected version 1 frozen with original physics, got %A{other}")
        // …version 2 carries the new physics…
        match proxy.resolveVersion (v2Id MaterialIds.glass152) with
        | Ok (Some e) -> Assert.Equal(otherPhysics.complexity, e.complexity)
        | other -> Assert.Fail($"expected version 2 with the new physics, got %A{other}")
        // …and the latest (what the library edits and the offers surface) is version 2.
        match proxy.tryGetMaterial MaterialIds.glass152 with
        | Ok (Some e) -> Assert.Equal(otherPhysics.complexity, e.complexity)
        | other -> Assert.Fail($"expected the latest entry to be version 2, got %A{other}")

    [<Fact>]
    let ``latest-active default listing shows one entry per material after a mint, not the frozen history`` () =
        let proxy = proxyWithUsed [ v1Ref MaterialIds.glass152 ]
        match proxy.saveMaterial glassPhysicsChanged with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials ActiveOnly with
        | Ok entries ->
            Assert.Equal(List.length builtInEntries, List.length entries)
            let glassEntries = entries |> List.filter (fun e -> e.id = MaterialIds.glass152)
            Assert.Equal(1, List.length glassEntries)
            Assert.Equal(otherPhysics.complexity, glassEntries.Head.complexity)
        | Error err -> Assert.Fail($"%A{err}")

    // ============================ active / inactive / supersede ============================

    [<Fact>]
    let ``markMaterialInactive hides an entry from the offers yet keeps it resolvable, and markMaterialActive revives it`` () =
        let proxy = freshProxy ()
        match proxy.markMaterialInactive MaterialIds.glass152 with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        // hidden from the default (offers) listing and from search…
        match proxy.listMaterials ActiveOnly with
        | Ok entries -> Assert.DoesNotContain(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.searchMaterials { MaterialQuery.empty with text = glass.name } with
        | Ok entries -> Assert.DoesNotContain(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        // …shown with the include-inactive switch…
        match proxy.listMaterials IncludeInactive with
        | Ok entries -> Assert.Contains(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        // …still resolvable by version (existing references never break)…
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the inactive version to resolve, got %A{other}")
        // …and revivable.
        match proxy.markMaterialActive MaterialIds.glass152 with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials ActiveOnly with
        | Ok entries -> Assert.Contains(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``supersedeMaterial behaves as inactive — hidden from offers, present under include-inactive, resolvable by version`` () =
        let proxy = freshProxy ()
        match proxy.supersedeMaterial MaterialIds.glass152 with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials ActiveOnly with
        | Ok entries -> Assert.DoesNotContain(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials IncludeInactive with
        | Ok entries -> Assert.Contains(MaterialIds.glass152, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.resolveVersion (v1Id MaterialIds.glass152) with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the superseded version to resolve, got %A{other}")

    [<Fact>]
    let ``the lifecycle verbs reject an unknown id with a diagnostic reason`` () =
        let proxy = freshProxy ()
        let missing = (minted ()).id
        for verb in [ proxy.markMaterialInactive; proxy.markMaterialActive; proxy.supersedeMaterial ] do
            match verb missing with
            | Error (UnknownMaterialId reason) -> Assert.Contains(string missing.value, reason)
            | other -> Assert.Fail($"expected Error (UnknownMaterialId _), got %A{other}")

    // ============================ removal: the two hard-blocks ============================

    [<Fact>]
    let ``removeMaterial on a material referenced by seeded samples is blocked and leaves the store unchanged`` () =
        let samples, proxy = composedProxies ()
        match proxy.removeMaterial MaterialIds.glass152 with
        | Error (MaterialStillReferenced reason) ->
            Assert.Contains(SeedSamples.glassPlate1mm.name, reason)
            Assert.Contains(SeedSamples.multilayerQw.name, reason)
        | other -> Assert.Fail($"expected Error (MaterialStillReferenced _), got %A{other}")
        match proxy.tryGetMaterial MaterialIds.glass152 with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the glass entry to survive, got %A{other}")
        match samples.listSamples () with
        | Ok _ -> ()
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``removeMaterial on a material with a used version is blocked with a typed refusal naming the version`` () =
        // No referencing samples on this store — the block is purely the used-version rule.
        let proxy = proxyWithUsed [ v1Ref MaterialIds.glass200 ]
        match proxy.removeMaterial MaterialIds.glass200 with
        | Error (MaterialVersionInUse reason) ->
            Assert.Contains(string MaterialIds.glass200.value, reason)
            Assert.Contains("v1", reason)
        | other -> Assert.Fail($"expected Error (MaterialVersionInUse _), got %A{other}")
        // never silently deletes: the entry survives.
        match proxy.tryGetMaterial MaterialIds.glass200 with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the entry to survive the used-version block, got %A{other}")

    [<Fact>]
    let ``removeMaterial succeeds when the material is neither sample-referenced nor version-used`` () =
        // No seeded sample references the n = 2.00 glass, and no version is in use.
        let _, proxy = composedProxies ()
        match proxy.removeMaterial MaterialIds.glass200 with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.tryGetMaterial MaterialIds.glass200 with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None after removal, got %A{other}")
        match proxy.removeMaterial MaterialIds.glass200 with
        | Error (UnknownMaterialId reason) -> Assert.Contains(string MaterialIds.glass200.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownMaterialId _), got %A{other}")

    [<Fact>]
    let ``the referencing lookup is live — removing the referencing sample unblocks the material`` () =
        // glass175 is referenced only by the 600 nm film seed; the lookup consults the CURRENT
        // samples store, not a snapshot taken at construction.
        let samples, proxy = composedProxies ()
        match proxy.removeMaterial MaterialIds.glass175 with
        | Error (MaterialStillReferenced reason) -> Assert.Contains(SeedSamples.glassFilm600.name, reason)
        | other -> Assert.Fail($"expected Error (MaterialStillReferenced _), got %A{other}")
        match samples.removeSample SeedSamples.glassFilm600.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.removeMaterial MaterialIds.glass175 with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () once unreferenced, got %A{other}")

    // ============================ the seam (reference equality + the stub) ============================

    [<Fact>]
    let ``a MaterialProxy compares by reference (the Elmish-required equality)`` () =
        let p = freshProxy ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = freshProxy ()))

    [<Fact>]
    let ``a STUB MaterialProxy over the versioned surface exercises every function through its exact signature`` () =
        // The stub proves the seam: the SAME record shape, in-test functions over a fixed
        // two-entry list — a test substitutes these for the real store and exercises the exact same
        // logic.
        let referenced = glass
        let free = { glass with id = newMaterialId (); name = "Unreferenced test glass" }
        let fixedEntries = [ referenced; free ]
        let stub : MaterialProxy =
            {
                listMaterials = fun _ -> Ok fixedEntries
                searchMaterials =
                    fun (q : MaterialQuery) ->
                        Ok (fixedEntries |> List.filter (fun e -> e.name.IndexOf(q.text, StringComparison.OrdinalIgnoreCase) >= 0))
                tryGetMaterial = fun (id : MaterialId) -> Ok (fixedEntries |> List.tryFind (fun e -> e.id = id))
                resolveVersion =
                    fun (mvid : MaterialVersionId) ->
                        Ok (fixedEntries |> List.tryFind (fun e -> e.id = mvid.materialId))
                saveMaterial =
                    fun (e : MaterialEntry) ->
                        if String.IsNullOrWhiteSpace e.name then Error (InvalidMaterial "blank material name") else Ok ()
                markMaterialInactive = fun (_ : MaterialId) -> Ok ()
                markMaterialActive = fun (_ : MaterialId) -> Ok ()
                supersedeMaterial = fun (_ : MaterialId) -> Ok ()
                removeMaterial =
                    fun (id : MaterialId) ->
                        if id = referenced.id
                        then Error (MaterialStillReferenced ($"material '%s{(string id.value)}' is still referenced by a sample structure"))
                        else Ok ()
            }
        match stub.listMaterials ActiveOnly with
        | Ok l -> Assert.Equal(2, List.length l)
        | Error err -> Assert.Fail($"%A{err}")
        match stub.searchMaterials { MaterialQuery.empty with text = "unreferenced" } with
        | Ok [ hit ] -> Assert.Equal<MaterialId>(free.id, hit.id)
        | other -> Assert.Fail($"expected exactly the unreferenced entry, got %A{other}")
        match stub.tryGetMaterial referenced.id with
        | Ok (Some e) -> Assert.Equal<MaterialId>(referenced.id, e.id)
        | other -> Assert.Fail($"expected the referenced entry, got %A{other}")
        match stub.resolveVersion (v1Id free.id) with
        | Ok (Some e) -> Assert.Equal<MaterialId>(free.id, e.id)
        | other -> Assert.Fail($"expected the free entry by version, got %A{other}")
        match stub.saveMaterial { free with name = " " } with
        | Error (InvalidMaterial _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidMaterial _), got %A{other}")
        match stub.saveMaterial free with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        for verb in [ stub.markMaterialInactive; stub.markMaterialActive; stub.supersedeMaterial ] do
            match verb free.id with
            | Ok () -> ()
            | other -> Assert.Fail($"expected Ok (), got %A{other}")
        match stub.removeMaterial referenced.id with
        | Error (MaterialStillReferenced reason) -> Assert.Contains(string referenced.id.value, reason)
        | other -> Assert.Fail($"expected Error (MaterialStillReferenced _), got %A{other}")
        match stub.removeMaterial free.id with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")

    // ============================ referencedMaterials (structure coverage, unchanged) ============================

    [<Fact>]
    let ``referencedMaterials covers films, repeated cells, the substrate plate, and the lower half-space`` () =
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.glass152 ],
            SeedSamples.glassFilm200.structure.referencedMaterials)
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.euvMolybdenum; MaterialIds.euvSilicon ],
            SeedSamples.euvMoSi.structure.referencedMaterials)
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.glass152 ],
            SeedSamples.glassPlate1mm.structure.referencedMaterials)
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.langasite; MaterialIds.silicon ],
            SeedSamples.langasiteSilicon.structure.referencedMaterials)
