namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.MaterialLibrary

/// Spec 0033 step 003 (ADD_CONTRACT STORE_XDUO_0001) — pins the mutating materials write-seam:
/// `DispersionFilter` / `MaterialQuery`, the extended `MaterialError` (DuplicateMaterialId /
/// MaterialStillReferenced / InvalidMaterial, each carrying `reason`), and the `MaterialProxy`
/// functional-proxy record. The proxy is the in-memory mock (`createInMemory`, an inline stub
/// record over the FIXED built-in entry list — validates, never persists); a stub of the SAME
/// record shape exercises all six functions through their exact signatures (the acceptance).
module MaterialProxyTests =

    let private proxy : MaterialProxy = createInMemory ()

    /// A built-in template entry (fixed id, non-dispersive glass) the write tests reuse.
    let private glass : MaterialEntry =
        builtInEntries |> List.find (fun e -> e.id = MaterialIds.glass152)

    /// A fresh entry under a MINTED id (not in the fixed list) over the glass properties.
    let private minted () : MaterialEntry =
        { glass with id = newMaterialId (); name = "Test glass (minted)" }

    // ============================ the read surface over the fixed list ============================

    [<Fact>]
    let ``listMaterials returns the fixed built-in entries with distinct ids`` () =
        match proxy.listMaterials () with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.Equal(List.length builtInEntries, List.length entries)
            let ids = entries |> List.map (fun e -> e.id)
            Assert.Equal(List.length ids, ids |> List.distinct |> List.length)
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``searchMaterials with the empty query matches everything`` () =
        match proxy.searchMaterials MaterialQuery.empty with
        | Ok entries -> Assert.Equal(List.length builtInEntries, List.length entries)
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``searchMaterials filters by name fragment case-insensitively`` () =
        match proxy.searchMaterials { MaterialQuery.empty with text = "GLASS" } with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.All(entries, fun e ->
                Assert.True(e.name.IndexOf("glass", StringComparison.OrdinalIgnoreCase) >= 0, e.name))
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``searchMaterials filters by category`` () =
        match proxy.searchMaterials { MaterialQuery.empty with category = Some Crystal } with
        | Ok entries ->
            Assert.NotEmpty entries
            Assert.All(entries, fun e -> Assert.Equal(Crystal, e.category))
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``searchMaterials dispersion facets partition the library`` () =
        // Silicon and langasite are the only function-backed (wavelength-dependent) built-ins;
        // every other entry is a `.dispersive` lift of a constant tensor.
        match proxy.searchMaterials { MaterialQuery.empty with dispersion = OnlyDispersive } with
        | Ok dispersive ->
            let ids = dispersive |> List.map (fun e -> e.id) |> Set.ofList
            Assert.Equal<Set<MaterialId>>(Set.ofList [ MaterialIds.silicon; MaterialIds.langasite ], ids)
        | Error err -> Assert.Fail(sprintf "%A" err)
        match proxy.searchMaterials { MaterialQuery.empty with dispersion = OnlyNonDispersive } with
        | Ok nonDispersive ->
            Assert.Equal(List.length builtInEntries - 2, List.length nonDispersive)
            Assert.DoesNotContain(MaterialIds.silicon, nonDispersive |> List.map (fun e -> e.id))
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``searchMaterials composes the text, category, and dispersion facets`` () =
        // "si" matches Silicon / Silicon (Si, EUV) / Langasite... — the dispersion facet narrows to
        // the two function-backed entries and the category facet then isolates the semiconductor.
        let q = { text = "si"; category = Some Semiconductor; dispersion = OnlyDispersive }
        match proxy.searchMaterials q with
        | Ok [ only ] -> Assert.Equal<MaterialId>(MaterialIds.silicon, only.id)
        | other -> Assert.Fail(sprintf "expected exactly the silicon entry, got %A" other)

    [<Fact>]
    let ``tryGetMaterial hits a known id and misses a minted one`` () =
        match proxy.tryGetMaterial MaterialIds.silicon with
        | Ok (Some e) -> Assert.Equal<MaterialId>(MaterialIds.silicon, e.id)
        | other -> Assert.Fail(sprintf "expected the silicon entry, got %A" other)
        match proxy.tryGetMaterial (newMaterialId ()) with
        | Ok None -> ()
        | other -> Assert.Fail(sprintf "expected Ok None, got %A" other)

    // ============================ the write surface (validates, never persists) ============================

    [<Fact>]
    let ``addMaterial accepts a fresh minted entry`` () =
        match proxy.addMaterial (minted ()) with
        | Ok () -> ()
        | Error err -> Assert.Fail(sprintf "%A" err)

    [<Fact>]
    let ``addMaterial rejects an id the library already holds with a diagnostic reason`` () =
        match proxy.addMaterial { minted () with id = glass.id } with
        | Error (DuplicateMaterialId reason) -> Assert.Contains(string glass.id.value, reason)
        | other -> Assert.Fail(sprintf "expected Error (DuplicateMaterialId _), got %A" other)

    [<Fact>]
    let ``addMaterial rejects a blank name as InvalidMaterial`` () =
        match proxy.addMaterial { minted () with name = "   " } with
        | Error (InvalidMaterial reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail(sprintf "expected Error (InvalidMaterial _), got %A" other)

    [<Fact>]
    let ``updateMaterial accepts a known id and rejects an unknown one`` () =
        match proxy.updateMaterial { glass with description = Some "updated" } with
        | Ok () -> ()
        | Error err -> Assert.Fail(sprintf "%A" err)
        let missing = minted ()
        match proxy.updateMaterial missing with
        | Error (UnknownMaterialId reason) -> Assert.Contains(string missing.id.value, reason)
        | other -> Assert.Fail(sprintf "expected Error (UnknownMaterialId _), got %A" other)

    [<Fact>]
    let ``updateMaterial rejects a blank name as InvalidMaterial`` () =
        match proxy.updateMaterial { glass with name = "" } with
        | Error (InvalidMaterial reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail(sprintf "expected Error (InvalidMaterial _), got %A" other)

    [<Fact>]
    let ``removeMaterial accepts a known id and rejects an unknown one`` () =
        match proxy.removeMaterial glass.id with
        | Ok () -> ()
        | Error err -> Assert.Fail(sprintf "%A" err)
        let missing = newMaterialId ()
        match proxy.removeMaterial missing with
        | Error (UnknownMaterialId reason) -> Assert.Contains(string missing.value, reason)
        | other -> Assert.Fail(sprintf "expected Error (UnknownMaterialId _), got %A" other)

    // ============================ the stub seam (the acceptance) ============================

    [<Fact>]
    let ``a MaterialProxy compares by reference (the Elmish-required equality)`` () =
        let p = createInMemory ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = createInMemory ()))

    [<Fact>]
    let ``a STUB MaterialProxy over a fixed entry list exercises all six functions through their exact signatures`` () =
        // The stub proves the seam: the SAME record shape, in-test functions over a fixed
        // two-entry list, including the one outcome the domain mock cannot see —
        // `MaterialStillReferenced` (reference tracking belongs to the real store).
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
                        then Error (DuplicateMaterialId (sprintf "material id '%s' is already in the library" (string e.id.value)))
                        else Ok ()
                updateMaterial =
                    fun (e : MaterialEntry) ->
                        if String.IsNullOrWhiteSpace e.name
                        then Error (InvalidMaterial "blank material name")
                        else Ok ()
                removeMaterial =
                    fun (id : MaterialId) ->
                        if id = referenced.id
                        then Error (MaterialStillReferenced (sprintf "material '%s' is still referenced by a sample structure" (string id.value)))
                        else Ok ()
            }
        // listMaterials : unit -> Result<MaterialEntry list, MaterialError>
        match stub.listMaterials () with
        | Ok l -> Assert.Equal(2, List.length l)
        | Error err -> Assert.Fail(sprintf "%A" err)
        // searchMaterials : MaterialQuery -> Result<MaterialEntry list, MaterialError>
        match stub.searchMaterials { MaterialQuery.empty with text = "unreferenced" } with
        | Ok [ hit ] -> Assert.Equal<MaterialId>(free.id, hit.id)
        | other -> Assert.Fail(sprintf "expected exactly the unreferenced entry, got %A" other)
        // tryGetMaterial : MaterialId -> Result<MaterialEntry option, MaterialError>
        match stub.tryGetMaterial referenced.id with
        | Ok (Some e) -> Assert.Equal<MaterialId>(referenced.id, e.id)
        | other -> Assert.Fail(sprintf "expected the referenced entry, got %A" other)
        match stub.tryGetMaterial (newMaterialId ()) with
        | Ok None -> ()
        | other -> Assert.Fail(sprintf "expected Ok None, got %A" other)
        // addMaterial : MaterialEntry -> Result<unit, MaterialError>
        match stub.addMaterial free with
        | Error (DuplicateMaterialId reason) -> Assert.Contains(string free.id.value, reason)
        | other -> Assert.Fail(sprintf "expected Error (DuplicateMaterialId _), got %A" other)
        match stub.addMaterial (minted ()) with
        | Ok () -> ()
        | other -> Assert.Fail(sprintf "expected Ok (), got %A" other)
        // updateMaterial : MaterialEntry -> Result<unit, MaterialError>
        match stub.updateMaterial { free with name = " " } with
        | Error (InvalidMaterial _) -> ()
        | other -> Assert.Fail(sprintf "expected Error (InvalidMaterial _), got %A" other)
        match stub.updateMaterial free with
        | Ok () -> ()
        | other -> Assert.Fail(sprintf "expected Ok (), got %A" other)
        // removeMaterial : MaterialId -> Result<unit, MaterialError>
        match stub.removeMaterial referenced.id with
        | Error (MaterialStillReferenced reason) -> Assert.Contains(string referenced.id.value, reason)
        | other -> Assert.Fail(sprintf "expected Error (MaterialStillReferenced _), got %A" other)
        match stub.removeMaterial free.id with
        | Ok () -> ()
        | other -> Assert.Fail(sprintf "expected Ok (), got %A" other)
