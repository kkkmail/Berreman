namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.MaterialLibrary

module private CategoryProxyTestData =

    let glass : MaterialCategory =
        BuiltinCatalogue.category CategoryIds.glass

    let vacuum : MaterialCategory =
        BuiltinCatalogue.category CategoryIds.vacuumCategory

    let minted () : MaterialCategory =
        { id = CategoryId.create (); name = CategoryName "Test category (minted)"; visibility = SelectableOnCreate; origin = UserCategory }

    let userCategoryId : CategoryId = Guid.Parse "7f1e2d3c-4b5a-6978-8091-a2b3c4d5e6f7" |> CategoryId

    let otherCategoryId : CategoryId = Guid.Parse "8a2b3c4d-5e6f-7081-9192-b3c4d5e6f708" |> CategoryId

    let referencingMaterialId : MaterialId = Guid.Parse "9b3c4d5e-6f70-8192-a2b3-c4d5e6f70819" |> MaterialId

    let userCategory (id : CategoryId) (name : string) : MaterialCategory =
        { id = id; name = CategoryName name; visibility = SelectableOnCreate; origin = UserCategory }

    let referencingMaterial (categoryId : CategoryId) : MaterialEntry =
        let glass = BuiltinCatalogue.material MaterialIds.glass152
        { glass with id = referencingMaterialId; name = EntryName "Referencing material"; category = categoryId }

open CategoryProxyTestData

/// Category stores preserve duplicate, built-in and live material-reference guards.
[<AbstractClass>]
type CategoryProxyContract () =

    abstract stores : unit -> CategoryProxy * MaterialProxy

    [<Fact>]
    member this.``the real store add/rename/remove of an unreferenced user category round-trips with fixed Guids`` () : unit =
        let categories, _ = this.stores ()
        let cat = userCategory userCategoryId "My category"
        match categories.addCategory cat with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () from add, got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            Assert.Equal(List.length (BuiltinCatalogue.categories ()) + 1, List.length l)
            Assert.Contains(userCategoryId, l |> List.map (fun c -> c.id))
        | Error err -> Assert.Fail($"%A{err}")
        match categories.updateCategory { cat with name = CategoryName "Renamed category" } with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () from update, got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            match l |> List.tryFind (fun c -> c.id = userCategoryId) with
            | Some c -> Assert.Equal("Renamed category", c.name.value)
            | None -> Assert.Fail("the renamed category vanished from the store")
        | Error err -> Assert.Fail($"%A{err}")
        match categories.removeCategory userCategoryId with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () from remove, got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            Assert.Equal(List.length (BuiltinCatalogue.categories ()), List.length l)
            Assert.DoesNotContain(userCategoryId, l |> List.map (fun c -> c.id))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``the real store rejects a duplicate add — a seeded built-in id and a re-added user id`` () : unit =
        let categories, _ = this.stores ()
        match categories.addCategory (userCategory CategoryIds.glass "Clashing glass") with
        | Error (DuplicateCategoryIdErr reason) -> Assert.Contains(string CategoryIds.glass.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateCategoryIdErr _), got %A{other}")
        let cat = userCategory userCategoryId "My category"
        match categories.addCategory cat with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () from the first add, got %A{other}")
        match categories.addCategory { cat with name = CategoryName "My category (re-added)" } with
        | Error (DuplicateCategoryIdErr reason) -> Assert.Contains(string userCategoryId.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateCategoryIdErr _), got %A{other}")

    [<Fact>]
    member this.``the real store rejects a blank-name add and update as InvalidCategory and persists nothing`` () : unit =
        let categories, _ = this.stores ()
        let blank = userCategory userCategoryId "   "
        match categories.addCategory blank with
        | Error (InvalidCategoryErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidCategoryErr _), got %A{other}")
        match categories.listCategories () with
        | Ok l -> Assert.DoesNotContain(userCategoryId, l |> List.map (fun c -> c.id))
        | Error err -> Assert.Fail($"%A{err}")
        let glass = BuiltinCatalogue.category CategoryIds.glass
        match categories.updateCategory { glass with name = CategoryName "" } with
        | Error (InvalidCategoryErr _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidCategoryErr _), got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            match l |> List.tryFind (fun c -> c.id = CategoryIds.glass) with
            | Some c -> Assert.Equal(glass.name, c.name)
            | None -> Assert.Fail("the glass built-in vanished from the store")
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``the real store renames a built-in category — built-ins ARE renamable`` () : unit =
        let categories, _ = this.stores ()
        let glass = BuiltinCatalogue.category CategoryIds.glass
        match categories.updateCategory { glass with name = CategoryName "Glass (renamed)" } with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () renaming a built-in, got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            match l |> List.tryFind (fun c -> c.id = CategoryIds.glass) with
            | Some c -> Assert.Equal("Glass (renamed)", c.name.value)
            | None -> Assert.Fail("the renamed built-in vanished from the store")
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``the real store rejects updateCategory and removeCategory on an unknown id`` () : unit =
        let categories, _ = this.stores ()
        match categories.updateCategory (userCategory otherCategoryId "Never added") with
        | Error (UnknownCategoryIdErr reason) -> Assert.Contains(string otherCategoryId.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownCategoryIdErr _), got %A{other}")
        match categories.removeCategory otherCategoryId with
        | Error (UnknownCategoryIdErr reason) -> Assert.Contains(string otherCategoryId.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownCategoryIdErr _), got %A{other}")

    [<Fact>]
    member this.``the real store refuses to remove a built-in category and leaves the store unchanged`` () : unit =
        let categories, _ = this.stores ()
        match categories.removeCategory CategoryIds.glass with
        | Error (BuiltInNotRemovableErr reason) -> Assert.Contains("Glass", reason)
        | other -> Assert.Fail($"expected Error (BuiltInNotRemovableErr _), got %A{other}")
        match categories.listCategories () with
        | Ok l ->
            Assert.Equal(List.length (BuiltinCatalogue.categories ()), List.length l)
            Assert.Contains(CategoryIds.glass, l |> List.map (fun c -> c.id))
        | Error err -> Assert.Fail($"%A{err}")

    [<Fact>]
    member this.``the real store blocks removeCategory on a user category a material references and leaves both stores unchanged`` () : unit =
        let categories, materials = this.stores ()
        match categories.addCategory (userCategory userCategoryId "Referenced category") with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () adding the user category, got %A{other}")
        let referencing = referencingMaterial userCategoryId
        match materials.saveMaterial referencing with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () adding the referencing material, got %A{other}")
        match categories.removeCategory userCategoryId with
        | Error (CategoryStillReferencedErr reason) -> Assert.Contains(referencing.name.value, reason)
        | other -> Assert.Fail($"expected Error (CategoryStillReferencedErr _), got %A{other}")
        match categories.listCategories () with
        | Ok l -> Assert.Contains(userCategoryId, l |> List.map (fun c -> c.id))
        | Error err -> Assert.Fail($"%A{err}")
        match materials.tryGetMaterial referencingMaterialId with
        | Ok (Some _) -> ()
        | other -> Assert.Fail($"expected the referencing material to survive, got %A{other}")

    [<Fact>]
    member this.``the referencing lookup is live — removing the referencing material unblocks the category`` () : unit =
        let categories, materials = this.stores ()
        match categories.addCategory (userCategory userCategoryId "Referenced then freed") with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () adding the user category, got %A{other}")
        match materials.saveMaterial (referencingMaterial userCategoryId) with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () adding the referencing material, got %A{other}")
        match categories.removeCategory userCategoryId with
        | Error (CategoryStillReferencedErr _) -> ()
        | other -> Assert.Fail($"expected Error (CategoryStillReferencedErr _) while referenced, got %A{other}")
        match materials.removeMaterial referencingMaterialId with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () removing the referencing material, got %A{other}")
        match categories.removeCategory userCategoryId with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () once unreferenced, got %A{other}")

type CategoryProxyTests () =
    inherit CategoryProxyContract ()

    override _.stores () : CategoryProxy * MaterialProxy =
        let materials = BuiltinCatalogue.materialStore (fun _ -> [])
        BuiltinCatalogue.categoryStore (materialsReferencingCategory materials), materials

    [<Fact>]
    member _.``a category resolves by id and by name through the list it is given`` () : unit =
        let renamedName = CategoryName "Glass from the caller"
        let originalName = CategoryName "Glass"
        let originalGlass = BuiltinCatalogue.categories () |> List.find (fun category -> category.id = CategoryIds.glass)
        let renamedGlass : MaterialCategory =
            {
                originalGlass with name = renamedName
            }
        let categories =
            BuiltinCatalogue.categories ()
            |> List.map (fun category -> if category.id = CategoryIds.glass then renamedGlass else category)

        Assert.Equal<MaterialCategory option>(Some renamedGlass, tryFindCategory categories CategoryIds.glass)
        Assert.Equal<MaterialCategory option>(Some renamedGlass, tryFindCategoryByName categories renamedName)
        Assert.Equal<MaterialCategory option>(None, tryFindCategoryByName categories originalName)
        Assert.Equal(renamedName.value, (CategoryIds.glass.categoryName categories).value)
        Assert.Equal<MaterialCategory option>(None, tryFindCategory [] CategoryIds.glass)
        Assert.Equal(string CategoryIds.glass.value, (CategoryIds.glass.categoryName []).value)

    [<Fact>]
    member _.``a CategoryProxy compares by reference (the Elmish-required equality)`` () : unit =
        let make () : CategoryProxy =
            {
                listCategories = fun () -> Ok (BuiltinCatalogue.categories ())
                addCategory = fun _ -> Ok ()
                updateCategory = fun _ -> Ok ()
                removeCategory = fun _ -> Ok ()
            }
        let p = make ()
        let same = p
        Assert.True((p = same))
        Assert.False((p = make ()))

    [<Fact>]
    member _.``a STUB CategoryProxy over a fixed category list exercises all four functions through their exact signatures`` () : unit =
        let referenced = glass
        let builtIn = vacuum
        let user = minted ()
        let fixedCategories = [ referenced; builtIn; user ]
        let stub : CategoryProxy =
            {
                listCategories = fun () -> Ok fixedCategories
                addCategory =
                    fun (c : MaterialCategory) ->
                        match c.validate with
                        | Error err -> Error err
                        | Ok () ->
                            if fixedCategories |> List.exists (fun x -> x.id = c.id)
                            then Error (DuplicateCategoryIdErr $"category id '%s{string c.id.value}' is already in the catalogue")
                            else Ok ()
                updateCategory =
                    fun (c : MaterialCategory) ->
                        match c.validate with
                        | Error err -> Error err
                        | Ok () ->
                            if fixedCategories |> List.exists (fun x -> x.id = c.id)
                            then Ok ()
                            else Error (UnknownCategoryIdErr $"unknown category id '%s{string c.id.value}'")
                removeCategory =
                    fun (id : CategoryId) ->
                        match fixedCategories |> List.tryFind (fun x -> x.id = id) with
                        | None -> Error (UnknownCategoryIdErr $"unknown category id '%s{string id.value}'")
                        | Some c ->
                            match c.origin with
                            | BuiltInCategory -> Error (BuiltInNotRemovableErr $"category '%s{c.name.value}' is a built-in and cannot be removed")
                            | UserCategory ->
                                if id = referenced.id
                                then Error (CategoryStillReferencedErr $"category '%s{c.name.value}' is still referenced by a material entry")
                                else Ok ()
            }

        match stub.listCategories () with
        | Ok l -> Assert.Equal(3, List.length l)
        | Error err -> Assert.Fail($"%A{err}")

        match stub.addCategory (minted ()) with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        match stub.addCategory user with
        | Error (DuplicateCategoryIdErr reason) -> Assert.Contains(string user.id.value, reason)
        | other -> Assert.Fail($"expected Error (DuplicateCategoryIdErr _), got %A{other}")
        match stub.addCategory { minted () with name = CategoryName "   " } with
        | Error (InvalidCategoryErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidCategoryErr _), got %A{other}")

        match stub.updateCategory { user with name = CategoryName "Renamed user category" } with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok (), got %A{other}")
        match stub.updateCategory { user with name = CategoryName "" } with
        | Error (InvalidCategoryErr _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidCategoryErr _), got %A{other}")
        let missing = minted ()
        match stub.updateCategory missing with
        | Error (UnknownCategoryIdErr reason) -> Assert.Contains(string missing.id.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownCategoryIdErr _), got %A{other}")

        match stub.removeCategory builtIn.id with
        | Error (BuiltInNotRemovableErr reason) -> Assert.Contains(builtIn.name.value, reason)
        | other -> Assert.Fail($"expected Error (BuiltInNotRemovableErr _), got %A{other}")
        match stub.removeCategory referenced.id with
        | Error (BuiltInNotRemovableErr _) -> ()  // `glass` is also a built-in — the built-in guard fires first.
        | other -> Assert.Fail($"expected Error (BuiltInNotRemovableErr _) for the built-in glass, got %A{other}")
        match stub.removeCategory user.id with
        | Ok () -> ()
        | other -> Assert.Fail($"expected Ok () for an unreferenced user category, got %A{other}")
        match stub.removeCategory (CategoryId.create ()) with
        | Error (UnknownCategoryIdErr _) -> ()
        | other -> Assert.Fail($"expected Error (UnknownCategoryIdErr _), got %A{other}")

    [<Fact>]
    member _.``a STUB CategoryProxy surfaces CategoryStillReferenced when a user category is in use`` () : unit =
        let inUse = minted ()
        let stub : CategoryProxy =
            {
                listCategories = fun () -> Ok [ inUse ]
                addCategory = fun _ -> Ok ()
                updateCategory = fun _ -> Ok ()
                removeCategory =
                    fun (id : CategoryId) ->
                        if id = inUse.id
                        then Error (CategoryStillReferencedErr $"category '%s{inUse.name.value}' is still referenced by a material entry")
                        else Error (UnknownCategoryIdErr $"unknown category id '%s{string id.value}'")
            }
        match stub.removeCategory inUse.id with
        | Error (CategoryStillReferencedErr reason) -> Assert.Contains(inUse.name.value, reason)
        | other -> Assert.Fail($"expected Error (CategoryStillReferencedErr _), got %A{other}")
