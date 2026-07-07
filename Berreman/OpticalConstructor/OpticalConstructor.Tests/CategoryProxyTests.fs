namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.MaterialLibrary

/// Spec 0035 step 002 (ADD_CONTRACT STORE_XDUO_0003) — the mutating category write-seam at
/// DECLARED lifecycle: `CategoryProxy` is declared (interface + mock + test), NOT implemented
/// (no real stateful store yet — that is the later `IMPLEMENT_CONTRACT STORE_XDUO_0003` step).
/// The tests below build a STUB `CategoryProxy` of the exact declared shape over a fixed category
/// list and exercise all four functions through their exact signatures, plus the reference
/// equality the Elmish host relies on. A later slice substitutes the real
/// `CategoryProxy.createInMemory` for this stub and runs the SAME assertions against live state.
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
