namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library   // samplesReferencing (the composed referencing store)
open OpticalConstructor.Domain.Lifecycle // VersionsInUse.empty (the injected in-use seam)
open OpticalConstructor.Domain.MaterialStore   // MaterialProxy.createInMemory (versioned, spec 0038 step 021)

/// Spec 0035 steps 002/003 (contract STORE_XDUO_0003) — the mutating category write-seam. The
/// first two tests keep the step-002 DECLARED-lifecycle stubs verbatim (a STUB `CategoryProxy` of
/// the exact declared shape over a fixed category list, plus the reference equality the Elmish host
/// relies on). The remaining tests exercise the step-003 IMPLEMENTED store —
/// `CategoryProxy.createInMemory (materialsReferencingCategory …)`, the real `ref Map` seeded from
/// `standardCategories` — through the SAME four signatures against live state: add/rename/remove
/// round-trips with fixed Guids, the built-in-not-removable block, the referenced-category hard
/// block via the composed `MaterialProxy`, and the live referencing lookup.
module CategoryProxyTests =

    /// A built-in `SelectableOnCreate` category from the step-1 seeded catalogue (the stub reuses
    /// the real records rather than fabricating a category).
    let private glass : MaterialCategory =
        standardCategories |> List.find (fun c -> c.id = CategoryIds.glass)

    /// A `HiddenOnCreate` built-in — the removal test proves a `BuiltInCategory` is refused.
    let private vacuum : MaterialCategory =
        standardCategories |> List.find (fun c -> c.id = CategoryIds.vacuum)

    /// A fresh USER category under a MINTED id (not in the seeded catalogue): the add/update/remove
    /// happy paths and the referenced-block scenario hang off this.
    let private minted () : MaterialCategory =
        { id = CategoryId.create (); name = "Test category (minted)"; visibility = SelectableOnCreate; origin = UserCategory }

    [<Fact>]
    let ``a CategoryProxy compares by reference (the Elmish-required equality)`` () =
        // Function-valued fields have no structural equality; the [<ReferenceEquality>] proxy
        // compares by identity so a host model holding one stays comparable.
        let make () : CategoryProxy =
            {
                listCategories = fun () -> Ok standardCategories
                addCategory = fun _ -> Ok ()
                updateCategory = fun _ -> Ok ()
                removeCategory = fun _ -> Ok ()
            }
        let p = make ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = make ()))

    [<Fact>]
    let ``a STUB CategoryProxy over a fixed category list exercises all four functions through their exact signatures`` () =
        // The stub proves the seam: the SAME record shape, in-test functions over a fixed
        // category list — a later slice substitutes the real store for these and exercises the
        // exact same logic. Each function goes through the `validateCategory` /
        // `UnknownCategoryId` / `DuplicateCategoryId` / `CategoryStillReferenced` /
        // `BuiltInNotRemovable` outcomes the contract declares.
        let referenced = glass
        let builtIn = vacuum
        let user = minted ()
        let fixedCategories = [ referenced; builtIn; user ]
        let stub : CategoryProxy =
            {
                listCategories = fun () -> Ok fixedCategories
                addCategory =
                    fun (c : MaterialCategory) ->
                        match validateCategory c with
                        | Error err -> Error err
                        | Ok () ->
                            if fixedCategories |> List.exists (fun x -> x.id = c.id)
                            then Error (DuplicateCategoryId $"category id '%s{string c.id.value}' is already in the catalogue")
                            else Ok ()
                updateCategory =
                    fun (c : MaterialCategory) ->
                        match validateCategory c with
                        | Error err -> Error err
                        | Ok () ->
                            if fixedCategories |> List.exists (fun x -> x.id = c.id)
                            then Ok ()
                            else Error (UnknownCategoryId $"unknown category id '%s{string c.id.value}'")
                removeCategory =
                    fun (id : CategoryId) ->
                        match fixedCategories |> List.tryFind (fun x -> x.id = id) with
                        | None -> Error (UnknownCategoryId $"unknown category id '%s{string id.value}'")
                        | Some c ->
                            match c.origin with
                            | BuiltInCategory -> Error (BuiltInNotRemovable $"category '%s{c.name}' is a built-in and cannot be removed")
                            | UserCategory ->
                                if id = referenced.id
                                then Error (CategoryStillReferenced $"category '%s{c.name}' is still referenced by a material entry")
                                else Ok ()
            }

        // listCategories : unit -> Result<MaterialCategory list, CategoryError>
        match stub.listCategories () with
        | Ok l -> Assert.Equal(3, List.length l)
        | Error err -> Assert.Fail($"%A{err}")

        // addCategory : MaterialCategory -> Result<unit, CategoryError>
        match stub.addCategory (minted ()) with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        match stub.addCategory user with
        | Error (DuplicateCategoryId reason) -> Assert.Contains(string user.id.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateCategoryId _), got %A{other}")
        match stub.addCategory { minted () with name = "   " } with
        | Error (InvalidCategory reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidCategory _), got %A{other}")

        // updateCategory : MaterialCategory -> Result<unit, CategoryError>
        match stub.updateCategory { user with name = "Renamed user category" } with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        match stub.updateCategory { user with name = "" } with
        | Error (InvalidCategory _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidCategory _), got %A{other}")
        let missing = minted ()
        match stub.updateCategory missing with
        | Error (UnknownCategoryId reason) -> Assert.Contains(string missing.id.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownCategoryId _), got %A{other}")

        // removeCategory : CategoryId -> Result<unit, CategoryError>
        match stub.removeCategory builtIn.id with
        | Error (BuiltInNotRemovable reason) -> Assert.Contains(builtIn.name, reason)
        | other -> Assert.Fail($"expected Error (BuiltInNotRemovable _), got %A{other}")
        match stub.removeCategory referenced.id with
        | Error (BuiltInNotRemovable _) -> ()  // `glass` is also a built-in — the built-in guard fires first.
        | other -> Assert.Fail($"expected Error (BuiltInNotRemovable _) for the built-in glass, got %A{other}")
        match stub.removeCategory user.id with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () for an unreferenced user category, got %A{other}")
        match stub.removeCategory (CategoryId.create ()) with
        | Error (UnknownCategoryId _) -> ()
        | other -> Assert.Fail($"expected Error (UnknownCategoryId _), got %A{other}")

    [<Fact>]
    let ``a STUB CategoryProxy surfaces CategoryStillReferenced when a user category is in use`` () =
        // A dedicated stub whose fixed user category IS referenced — proves the
        // `CategoryStillReferenced` case is reachable through `removeCategory`'s exact signature
        // (the previous stub's built-in guard shadowed it for the built-in seeds).
        let inUse = minted ()
        let stub : CategoryProxy =
            {
                listCategories = fun () -> Ok [ inUse ]
                addCategory = fun _ -> Ok ()
                updateCategory = fun _ -> Ok ()
                removeCategory =
                    fun (id : CategoryId) ->
                        if id = inUse.id
                        then Error (CategoryStillReferenced $"category '%s{inUse.name}' is still referenced by a material entry")
                        else Error (UnknownCategoryId $"unknown category id '%s{string id.value}'")
            }
        match stub.removeCategory inUse.id with
        | Error (CategoryStillReferenced reason) -> Assert.Contains(inUse.name, reason)
        | other -> Assert.Fail($"expected Error (CategoryStillReferenced _), got %A{other}")

    // ============================ the real store (spec 0035 step 003, IMPLEMENTED) ============================

    /// A FIXED literal id for the user category the round-trip / referenced-block tests use, so
    /// every scenario is deterministic across runs (the how-to's "fixed Guids").
    let private userCategoryId : CategoryId = Guid.Parse "7f1e2d3c-4b5a-6978-8091-a2b3c4d5e6f7" |> CategoryId

    /// A second FIXED literal id — the unknown-id / never-added scenarios use it so they never
    /// collide with `userCategoryId` or a seeded built-in.
    let private otherCategoryId : CategoryId = Guid.Parse "8a2b3c4d-5e6f-7081-9192-b3c4d5e6f708" |> CategoryId

    /// A FIXED literal id for the material entry the referenced-block tests categorise under the
    /// user category (not a seeded `MaterialIds` value).
    let private referencingMaterialId : MaterialId = Guid.Parse "9b3c4d5e-6f70-8192-a2b3-c4d5e6f70819" |> MaterialId

    /// A fresh `UserCategory` under a fixed id (the real store seeds only `BuiltInCategory` records,
    /// so a `UserCategory` is the only origin that can reach the remove / referenced paths).
    let private userCategory (id : CategoryId) (name : string) : MaterialCategory =
        { id = id; name = name; visibility = SelectableOnCreate; origin = UserCategory }

    /// A material entry (a copy of the seeded glass, re-identified and re-categorised) that
    /// references the given category id — the referenced-block scenarios add it to the materials
    /// store the category store's referencing lookup reads.
    let private referencingMaterial (categoryId : CategoryId) : MaterialEntry =
        let glass = builtInEntries |> List.find (fun e -> e.id = MaterialIds.glass152)
        { glass with id = referencingMaterialId; name = "Referencing material"; category = categoryId }

    [<Fact>]
    let ``the real store add/rename/remove of an unreferenced user category round-trips with fixed Guids`` () =
        let categories = CategoryProxy.createInMemory (fun _ -> [])
        let cat = userCategory userCategoryId "My category"
        // add
        match categories.addCategory cat with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () from add, got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            Assert.Equal(List.length standardCategories + 1, List.length l)
            Assert.Contains(userCategoryId, l |> List.map (fun c -> c.id))
        | Error err -> Assert.Fail($"%A{err}")
        // rename
        match categories.updateCategory { cat with name = "Renamed category" } with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () from update, got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            match l |> List.tryFind (fun c -> c.id = userCategoryId) with
            | Some c -> Assert.Equal("Renamed category", c.name)
            | None -> Assert.Fail("the renamed category vanished from the store")
        | Error err -> Assert.Fail($"%A{err}")
        // remove (unreferenced → succeeds)
        match categories.removeCategory userCategoryId with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () from remove, got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            Assert.Equal(List.length standardCategories, List.length l)
            Assert.DoesNotContain(userCategoryId, l |> List.map (fun c -> c.id))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``the real store rejects a duplicate add — a seeded built-in id and a re-added user id`` () =
        let categories = CategoryProxy.createInMemory (fun _ -> [])
        // an id the seeded catalogue already holds
        match categories.addCategory (userCategory CategoryIds.glass "Clashing glass") with
        | Error (DuplicateCategoryId reason) -> Assert.Contains(string CategoryIds.glass.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateCategoryId _), got %A{other}")
        // re-adding a freshly-added user id — the stateful duplicate
        let cat = userCategory userCategoryId "My category"
        match categories.addCategory cat with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () from the first add, got %A{other}")
        match categories.addCategory { cat with name = "My category (re-added)" } with
        | Error (DuplicateCategoryId reason) -> Assert.Contains(string userCategoryId.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateCategoryId _), got %A{other}")

    [<Fact>]
    let ``the real store rejects a blank-name add and update as InvalidCategory and persists nothing`` () =
        let categories = CategoryProxy.createInMemory (fun _ -> [])
        let blank = userCategory userCategoryId "   "
        match categories.addCategory blank with
        | Error (InvalidCategory reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidCategory _), got %A{other}")
        match categories.listCategories () with
        | Ok l -> Assert.DoesNotContain(userCategoryId, l |> List.map (fun c -> c.id))
        | Error err -> Assert.Fail($"%A{err}")
        // a blank rename of a seeded built-in is rejected and keeps the stored name
        let glass = standardCategories |> List.find (fun c -> c.id = CategoryIds.glass)
        match categories.updateCategory { glass with name = "" } with
        | Error (InvalidCategory _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidCategory _), got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            match l |> List.tryFind (fun c -> c.id = CategoryIds.glass) with
            | Some c -> Assert.Equal(glass.name, c.name)
            | None -> Assert.Fail("the glass built-in vanished from the store")
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``the real store renames a built-in category — built-ins ARE renamable`` () =
        let categories = CategoryProxy.createInMemory (fun _ -> [])
        let glass = standardCategories |> List.find (fun c -> c.id = CategoryIds.glass)
        match categories.updateCategory { glass with name = "Glass (renamed)" } with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () renaming a built-in, got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            match l |> List.tryFind (fun c -> c.id = CategoryIds.glass) with
            | Some c -> Assert.Equal("Glass (renamed)", c.name)
            | None -> Assert.Fail("the renamed built-in vanished from the store")
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``the real store rejects updateCategory and removeCategory on an unknown id`` () =
        let categories = CategoryProxy.createInMemory (fun _ -> [])
        match categories.updateCategory (userCategory otherCategoryId "Never added") with
        | Error (UnknownCategoryId reason) -> Assert.Contains(string otherCategoryId.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownCategoryId _), got %A{other}")
        match categories.removeCategory otherCategoryId with
        | Error (UnknownCategoryId reason) -> Assert.Contains(string otherCategoryId.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownCategoryId _), got %A{other}")

    [<Fact>]
    let ``the real store refuses to remove a built-in category and leaves the store unchanged`` () =
        // Every seeded category is a `BuiltInCategory`, so the built-in guard fires before the
        // referenced check could — the acceptance's built-in block.
        let categories = CategoryProxy.createInMemory (fun _ -> [])
        match categories.removeCategory CategoryIds.glass with
        | Error (BuiltInNotRemovable reason) -> Assert.Contains("Glass", reason)
        | other -> Assert.Fail($"expected Error (BuiltInNotRemovable _), got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            Assert.Equal(List.length standardCategories, List.length l)
            Assert.Contains(CategoryIds.glass, l |> List.map (fun c -> c.id))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    let ``the real store blocks removeCategory on a user category a material references and leaves both stores unchanged`` () =
        // The composition-root wiring: a materials store, and a category store whose referencing
        // lookup is backed by it (`materialsReferencingCategory` — the acceptance's referenced block).
        let materials = MaterialProxy.createInMemory (fun _ -> []) VersionsInUse.empty
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        match categories.addCategory (userCategory userCategoryId "Referenced category") with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () adding the user category, got %A{other}")
        let referencing = referencingMaterial userCategoryId
        match materials.saveMaterial referencing with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () adding the referencing material, got %A{other}")
        // blocked, NAMING the referencing material
        match categories.removeCategory userCategoryId with
        | Error (CategoryStillReferenced reason) -> Assert.Contains(referencing.name, reason)
        | other -> Assert.Fail($"expected Error (CategoryStillReferenced _), got %A{other}")
        // never cascades, never silently deletes: the category survives…
        match categories.listCategories () with
        | Ok l -> Assert.Contains(userCategoryId, l |> List.map (fun c -> c.id))
        | Error err -> Assert.Fail($"%A{err}")
        // …and the referencing material is untouched.
        match materials.tryGetMaterial referencingMaterialId with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the referencing material to survive, got %A{other}")

    [<Fact>]
    let ``the referencing lookup is live — removing the referencing material unblocks the category`` () =
        // The lookup consults the CURRENT materials store, not a snapshot taken at construction.
        let materials = MaterialProxy.createInMemory (fun _ -> []) VersionsInUse.empty
        let categories = CategoryProxy.createInMemory (materialsReferencingCategory materials)
        match categories.addCategory (userCategory userCategoryId "Referenced then freed") with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () adding the user category, got %A{other}")
        match materials.saveMaterial (referencingMaterial userCategoryId) with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () adding the referencing material, got %A{other}")
        match categories.removeCategory userCategoryId with
        | Error (CategoryStillReferenced _) -> ()
        | other -> Assert.Fail($"expected Error (CategoryStillReferenced _) while referenced, got %A{other}")
        // free the reference
        match materials.removeMaterial referencingMaterialId with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () removing the referencing material, got %A{other}")
        match categories.removeCategory userCategoryId with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () once unreferenced, got %A{other}")
