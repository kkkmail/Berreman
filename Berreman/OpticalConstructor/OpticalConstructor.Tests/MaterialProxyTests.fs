namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library

/// Spec 0033 steps 003/006 (contract STORE_XDUO_0001) — the mutating materials write-seam,
/// now at IMPLEMENTED lifecycle: `MaterialProxy.createInMemory (samplesReferencing …)` is the
/// real, stateful in-memory store (a `ref Map<MaterialId, MaterialEntry>` seeded from
/// `builtInEntries`; mutation confined to the closure). Reads/searches answer from the current
/// map through the pure `byQuery` seam; writes persist; `removeMaterial` consults the
/// referencing lookup and hard-blocks with `MaterialStillReferenced` naming the referencing
/// samples — at composition the lookup is `samplesReferencing`, backed by the step-005
/// `SampleProxy` store. Every test builds a FRESH proxy (a shared stateful proxy would be
/// order-dependent under xUnit); the step-003 stub acceptance test is kept verbatim.
module MaterialProxyTests =

    /// A fresh store with NO referencing samples (the referencing facet is exercised
    /// separately through the composed pair below).
    let private freshProxy () : MaterialProxy = MaterialProxy.createInMemory (fun _ -> [])

    /// The composed pair the slice pins: a fresh step-005 samples store and a materials store
    /// whose referencing lookup is backed by it (`samplesReferencing` — the composition-root
    /// wiring).
    let private composedProxies () : SampleProxy * MaterialProxy =
        let samples = SampleProxy.createInMemory ()
        samples, MaterialProxy.createInMemory (samplesReferencing samples)

    /// A built-in template entry (fixed id, non-dispersive glass) the write tests reuse.
    let private glass : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = MaterialIds.glass152)

    /// A fresh entry under a MINTED id (not in the seeded map) over the glass properties.
    let private minted () : MaterialEntry =
        { glass with id = newMaterialId (); name = "Test glass (minted)" }

    // ============================ the read surface over the seeded store ============================

    [<Fact>]
    let ``listMaterials returns the seeded built-in entries with distinct ids`` () =
        match (freshProxy ()).listMaterials () with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.Equal(List.length builtInEntries, List.length entries)
            let ids = entries |> List.map (fun e -> e.id)
            Assert.Equal(List.length ids, ids |> List.distinct |> List.length)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchMaterials with the empty query matches everything`` () =
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
    let ``searchMaterials filters by category`` () =
        match (freshProxy ()).searchMaterials { MaterialQuery.empty with category = Some Crystal } with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.All(entries, fun e -> Assert.Equal(Crystal, e.category))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchMaterials dispersion facets partition the library`` () =
        // Silicon and langasite are the only function-backed (wavelength-dependent) built-ins;
        // every other entry is a `.dispersive` lift of a constant tensor.
        let proxy = freshProxy ()
        match proxy.searchMaterials { MaterialQuery.empty with dispersion = OnlyDispersive } with
        | Ok dispersive ->
            let ids = dispersive |> List.map (fun e -> e.id) |> Set.ofList
            Assert.Equal<Set<MaterialId>>(Set.ofList [ MaterialIds.silicon; MaterialIds.langasite ], ids)
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.searchMaterials { MaterialQuery.empty with dispersion = OnlyNonDispersive } with
        | Ok nonDispersive ->
            Assert.Equal(List.length builtInEntries - 2, List.length nonDispersive)
            Assert.DoesNotContain(MaterialIds.silicon, nonDispersive |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``searchMaterials composes the text, category, and dispersion facets`` () =
        // "si" matches Silicon / Silicon (Si, EUV) / Langasite... — the dispersion facet narrows to
        // the two function-backed entries and the category facet then isolates the semiconductor.
        let q = { text = "si"; category = Some Semiconductor; dispersion = OnlyDispersive }
        match (freshProxy ()).searchMaterials q with
        | Ok [ only ] -> Assert.Equal<MaterialId>(MaterialIds.silicon, only.id)
        | other -> Assert.Fail($"expected exactly the silicon entry, got %A{other}")

    [<Fact>]
    let ``tryGetMaterial hits a known id and misses a minted one`` () =
        let proxy = freshProxy ()
        match proxy.tryGetMaterial MaterialIds.silicon with
        | Ok (Some e) -> Assert.Equal<MaterialId>(MaterialIds.silicon, e.id)
        | other -> Assert.Fail($"expected the silicon entry, got %A{other}")
        match proxy.tryGetMaterial (newMaterialId ()) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None, got %A{other}")

    // ============================ the write surface (the stateful round-trips) ============================

    [<Fact>]
    let ``addMaterial persists — add-then-list and search round-trip`` () =
        let proxy = freshProxy ()
        let entry = minted ()
        match proxy.addMaterial entry with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.listMaterials () with
        | Ok entries ->
            Assert.Equal(List.length builtInEntries + 1, List.length entries)
            Assert.Contains(entry.id, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.searchMaterials { MaterialQuery.empty with text = "minted" } with
        | Ok [ hit ] -> Assert.Equal<MaterialId>(entry.id, hit.id)
        | other -> Assert.Fail($"expected exactly the minted entry, got %A{other}")

    [<Fact>]
    let ``addMaterial rejects an id the store already holds with a diagnostic reason`` () =
        match (freshProxy ()).addMaterial { minted () with id = glass.id } with
        | Error (DuplicateMaterialId reason) -> Assert.Contains(string glass.id.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateMaterialId _), got %A{other}")

    [<Fact>]
    let ``addMaterial rejects a re-added minted id — the stateful duplicate`` () =
        let proxy = freshProxy ()
        let entry = minted ()
        match proxy.addMaterial entry with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.addMaterial { entry with name = "Test glass (re-added)" } with
        | Error (DuplicateMaterialId reason) ->
            Assert.Contains(string entry.id.value, reason)
            Assert.Contains(entry.name, reason)
        | other -> Assert.Fail($"expected Error (DuplicateMaterialId _), got %A{other}")

    [<Fact>]
    let ``addMaterial rejects a blank name as InvalidMaterial and persists nothing`` () =
        let proxy = freshProxy ()
        let blank = { minted () with name = "   " }
        match proxy.addMaterial blank with
        | Error (InvalidMaterial reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidMaterial _), got %A{other}")
        match proxy.tryGetMaterial blank.id with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None (nothing persisted), got %A{other}")

    [<Fact>]
    let ``updateMaterial persists — update-then-get round-trip — and rejects an unknown id`` () =
        let proxy = freshProxy ()
        match proxy.updateMaterial { glass with description = Some "updated" } with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.tryGetMaterial glass.id with
        | Ok (Some e) -> Assert.Equal(Some "updated", e.description)
        | other -> Assert.Fail($"expected the updated glass entry, got %A{other}")
        let missing = minted ()
        match proxy.updateMaterial missing with
        | Error (UnknownMaterialId reason) -> Assert.Contains(string missing.id.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownMaterialId _), got %A{other}")

    [<Fact>]
    let ``updateMaterial rejects a blank name as InvalidMaterial and keeps the stored entry`` () =
        let proxy = freshProxy ()
        match proxy.updateMaterial { glass with name = "" } with
        | Error (InvalidMaterial reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidMaterial _), got %A{other}")
        match proxy.tryGetMaterial glass.id with
        | Ok (Some e) -> Assert.Equal(glass.name, e.name)
        | other -> Assert.Fail($"expected the unchanged glass entry, got %A{other}")

    [<Fact>]
    let ``removeMaterial persists — remove-then-search round-trip — and rejects an unknown id`` () =
        // No referencing samples on this store — removal of a seeded entry succeeds.
        let proxy = freshProxy ()
        match proxy.removeMaterial glass.id with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.tryGetMaterial glass.id with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None after removal, got %A{other}")
        match proxy.searchMaterials { MaterialQuery.empty with text = glass.name } with
        | Ok entries -> Assert.DoesNotContain(glass.id, entries |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.removeMaterial glass.id with
        | Error (UnknownMaterialId reason) -> Assert.Contains(string glass.id.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownMaterialId _), got %A{other}")

    // ============================ the referenced-material hard block (the acceptance) ============================

    [<Fact>]
    let ``removeMaterial on a material referenced by seeded samples is blocked and leaves both stores unchanged`` () =
        let samples, proxy = composedProxies ()
        let samplesBefore =
            match samples.listSamples () with
            | Ok list -> List.length list
            | Error err -> failwith ($"%A{err}")
        // glass152 is referenced by the seeded plates/films and the quarter-wave multilayer.
        match proxy.removeMaterial MaterialIds.glass152 with
        | Error (MaterialStillReferenced reason) ->
            Assert.Contains(SeedSamples.glassPlate1mm.name, reason)
            Assert.Contains(SeedSamples.multilayerQw.name, reason)
        | other -> Assert.Fail($"expected Error (MaterialStillReferenced _), got %A{other}")
        // Never cascades, never silently deletes: the materials store still holds the entry…
        match proxy.tryGetMaterial MaterialIds.glass152 with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the glass entry to survive, got %A{other}")
        match proxy.listMaterials () with
        | Ok entries -> Assert.Equal(List.length builtInEntries, List.length entries)
        | Error err -> Assert.Fail($"%A{err}")
        // …and the samples store is untouched.
        match samples.listSamples () with
        | Ok list -> Assert.Equal(samplesBefore, List.length list)
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``removeMaterial on an unreferenced material succeeds at composition`` () =
        // No seeded sample references the n = 2.00 glass.
        let _, proxy = composedProxies ()
        match proxy.removeMaterial MaterialIds.glass200 with
        | Ok () -> ()
        | Error err -> Assert.Fail($"%A{err}")
        match proxy.tryGetMaterial MaterialIds.glass200 with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None after removal, got %A{other}")

    [<Fact>]
    let ``a lower-half-space reference blocks removal too`` () =
        // Silicon is referenced ONLY as the lower half-space of the langasite-on-silicon seed.
        let _, proxy = composedProxies ()
        match proxy.removeMaterial MaterialIds.silicon with
        | Error (MaterialStillReferenced reason) -> Assert.Contains(SeedSamples.langasiteSilicon.name, reason)
        | other -> Assert.Fail($"expected Error (MaterialStillReferenced _), got %A{other}")

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

    [<Fact>]
    let ``referencedMaterials covers films, repeated cells, the substrate plate, and the lower half-space`` () =
        // Single film layer between vacuum.
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.glass152 ],
            SeedSamples.glassFilm200.structure.referencedMaterials)
        // A `Repeated` period group's cell (each material counted once).
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.euvMolybdenum; MaterialIds.euvSilicon ],
            SeedSamples.euvMoSi.structure.referencedMaterials)
        // A thick substrate plate.
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.glass152 ],
            SeedSamples.glassPlate1mm.structure.referencedMaterials)
        // A film plus the lower half-space material.
        Assert.Equal<Set<MaterialId>>(
            Set.ofList [ MaterialIds.langasite; MaterialIds.silicon ],
            SeedSamples.langasiteSilicon.structure.referencedMaterials)

    // ============================ the stub seam (the step-003 acceptance, kept) ============================

    [<Fact>]
    let ``a MaterialProxy compares by reference (the Elmish-required equality)`` () =
        let p = freshProxy ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = freshProxy ()))

    [<Fact>]
    let ``a STUB MaterialProxy over a fixed entry list exercises all six functions through their exact signatures`` () =
        // The stub proves the seam: the SAME record shape, in-test functions over a fixed
        // two-entry list — a test substitutes these for the real store and exercises the
        // exact same logic.
        let referenced = glass
        let free = { glass with id = newMaterialId (); name = "Unreferenced test glass" }
        let fixedEntries = [ referenced; free ]
        let stub : MaterialProxy =
            {
                listMaterials = fun () -> Ok fixedEntries
                searchMaterials =
                    fun (q : MaterialQuery) ->
                        Ok (fixedEntries |> List.filter (fun e -> e.name.IndexOf(q.text, StringComparison.OrdinalIgnoreCase) >= 0))
                tryGetMaterial =
                    fun (id : MaterialId) -> Ok (fixedEntries |> List.tryFind (fun e -> e.id = id))
                addMaterial =
                    fun (e : MaterialEntry) ->
                        if fixedEntries |> List.exists (fun x -> x.id = e.id)
                        then Error (DuplicateMaterialId ($"material id '%s{(string e.id.value)}' is already in the library"))
                        else Ok ()
                updateMaterial =
                    fun (e : MaterialEntry) ->
                        if String.IsNullOrWhiteSpace e.name
                        then Error (InvalidMaterial "blank material name")
                        else Ok ()
                removeMaterial =
                    fun (id : MaterialId) ->
                        if id = referenced.id
                        then Error (MaterialStillReferenced ($"material '%s{(string id.value)}' is still referenced by a sample structure"))
                        else Ok ()
            }
        // listMaterials : unit -> Result<MaterialEntry list, MaterialError>
        match stub.listMaterials () with
        | Ok l -> Assert.Equal(2, List.length l)
        | Error err -> Assert.Fail($"%A{err}")
        // searchMaterials : MaterialQuery -> Result<MaterialEntry list, MaterialError>
        match stub.searchMaterials { MaterialQuery.empty with text = "unreferenced" } with
        | Ok [ hit ] -> Assert.Equal<MaterialId>(free.id, hit.id)
        | other -> Assert.Fail($"expected exactly the unreferenced entry, got %A{other}")
        // tryGetMaterial : MaterialId -> Result<MaterialEntry option, MaterialError>
        match stub.tryGetMaterial referenced.id with
        | Ok (Some e) -> Assert.Equal<MaterialId>(referenced.id, e.id)
        | other -> Assert.Fail($"expected the referenced entry, got %A{other}")
        match stub.tryGetMaterial (newMaterialId ()) with
        | Ok None -> ()
        | other -> Assert.Fail($"expected Ok None, got %A{other}")
        // addMaterial : MaterialEntry -> Result<unit, MaterialError>
        match stub.addMaterial free with
        | Error (DuplicateMaterialId reason) -> Assert.Contains(string free.id.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateMaterialId _), got %A{other}")
        match stub.addMaterial (minted ()) with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        // updateMaterial : MaterialEntry -> Result<unit, MaterialError>
        match stub.updateMaterial { free with name = " " } with
        | Error (InvalidMaterial _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidMaterial _), got %A{other}")
        match stub.updateMaterial free with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        // removeMaterial : MaterialId -> Result<unit, MaterialError>
        match stub.removeMaterial referenced.id with
        | Error (MaterialStillReferenced reason) -> Assert.Contains(string referenced.id.value, reason)
        | other -> Assert.Fail($"expected Error (MaterialStillReferenced _), got %A{other}")
        match stub.removeMaterial free.id with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
