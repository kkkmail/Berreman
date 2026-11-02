namespace OpticalConstructor.Tests

open System
open Xunit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.CategoryEditor

/// Spec 0035 step 004 (IMPLEMENT) — the pure, Avalonia-free `CategoryEditor` edit model. Every
/// message arm (BeginAddCategory / SetCategoryName / RemoveCategoryRow) is exercised without a
/// window, plus the acceptance (Begin-then-name yields a `UserCategory` / `SelectableOnCreate` row
/// carrying the typed name; a blank name surfaces the typed `InvalidCategoryErr`) and the commit
/// projection into the add / rename / remove intents the host dispatches to `CategoryProxy`.
module CategoryEditorTests =

    /// Unwrap an `Ok` edit state or fail the test naming the unexpected error.
    let private okState (result : Result<CategoryEditState, CategoryError>) : CategoryEditState =
        match result with
        | Ok s -> s
        | Error err -> failwith $"expected Ok CategoryEditState, got %A{err}"

    /// The in-progress row id after a BeginAddCategory (the minted id — never a literal).
    let private inProgressId (state : CategoryEditState) : CategoryId =
        match state.editingRow with
        | Some id -> id
        | None -> failwith "expected an in-progress row (editingRow = Some _)"

    [<Fact>]
    let ``init opens the editor over the snapshot with no inline edit`` () =
        let state = init standardCategories
        Assert.Equal<MaterialCategory list>(standardCategories, state.rows)
        Assert.Equal<MaterialCategory list>(standardCategories, state.original)
        Assert.True(Option.isNone state.editingRow)

    [<Fact>]
    let ``BeginAddCategory appends a blank UserCategory / SelectableOnCreate row and marks it in-progress`` () =
        let state1 = okState (applyCategoryMsg BeginAddCategory (init standardCategories))
        Assert.Equal(List.length standardCategories + 1, List.length state1.rows)
        let id = inProgressId state1
        match state1.rows |> List.tryFind (fun c -> c.id = id) with
        | Some row ->
            Assert.Equal("", row.name.value)
            Assert.Equal(SelectableOnCreate, row.visibility)
            Assert.Equal(UserCategory, row.origin)
        | None -> Assert.Fail("the minted row is missing from the working rows")

    [<Fact>]
    let ``BeginAddCategory then SetCategoryName yields a UserCategory / SelectableOnCreate row carrying the typed name`` () =
        // The acceptance: Begin then name produces the typed row, and commit projects the Add intent.
        let state1 = okState (applyCategoryMsg BeginAddCategory (init standardCategories))
        let id = inProgressId state1
        let state2 = okState (applyCategoryMsg (SetCategoryName (id, CategoryName "Ferroelectrics")) state1)
        match state2.rows |> List.tryFind (fun c -> c.id = id) with
        | Some row ->
            Assert.Equal("Ferroelectrics", row.name.value)
            Assert.Equal(SelectableOnCreate, row.visibility)
            Assert.Equal(UserCategory, row.origin)
        | None -> Assert.Fail("the named row vanished from the working rows")
        match state2.commit with
        | Ok [ AddCategory row ] ->
            Assert.Equal(id, row.id)
            Assert.Equal("Ferroelectrics", row.name.value)
            Assert.Equal(SelectableOnCreate, row.visibility)
            Assert.Equal(UserCategory, row.origin)
        | other -> Assert.Fail($"expected Ok [ AddCategory _ ], got %A{other}")

    [<Fact>]
    let ``a blank name surfaces the typed InvalidCategory at commit`` () =
        // The acceptance's negative half: a Begin-added (blank) row, and a name explicitly cleared
        // to whitespace, both surface the typed InvalidCategoryErr when the round is committed.
        let state1 = okState (applyCategoryMsg BeginAddCategory (init standardCategories))
        match state1.commit with
        | Error (InvalidCategoryErr reason) -> Assert.False(String.IsNullOrWhiteSpace reason)
        | other -> Assert.Fail($"expected Error (InvalidCategoryErr _), got %A{other}")
        let id = inProgressId state1
        let state2 = okState (applyCategoryMsg (SetCategoryName (id, CategoryName "   ")) state1)
        match state2.commit with
        | Error (InvalidCategoryErr _) -> ()
        | other -> Assert.Fail($"expected Error (InvalidCategoryErr _), got %A{other}")

    [<Fact>]
    let ``SetCategoryName renames a seeded built-in row and commit projects a RenameCategory intent`` () =
        // Built-ins ARE renamable (step 3) — the editor projects a RenameCategory the proxy accepts.
        let state1 = okState (applyCategoryMsg (SetCategoryName (CategoryIds.glass, CategoryName "Glass (renamed)")) (init standardCategories))
        match state1.rows |> List.tryFind (fun c -> c.id = CategoryIds.glass) with
        | Some row -> Assert.Equal("Glass (renamed)", row.name.value)
        | None -> Assert.Fail("the glass row vanished from the working rows")
        match state1.commit with
        | Ok [ RenameCategory row ] ->
            Assert.Equal(CategoryIds.glass, row.id)
            Assert.Equal("Glass (renamed)", row.name.value)
            Assert.Equal(BuiltInCategory, row.origin)
        | other -> Assert.Fail($"expected Ok [ RenameCategory _ ], got %A{other}")

    [<Fact>]
    let ``SetCategoryName on an id the working rows do not hold surfaces the typed UnknownCategoryId`` () =
        let ghost = CategoryId.create ()
        match applyCategoryMsg (SetCategoryName (ghost, CategoryName "Nowhere")) (init standardCategories) with
        | Error (UnknownCategoryIdErr reason) -> Assert.Contains(string ghost.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownCategoryIdErr _), got %A{other}")

    [<Fact>]
    let ``RemoveCategoryRow drops a row and commit projects a RemoveCategory intent`` () =
        let state1 = okState (applyCategoryMsg (RemoveCategoryRow CategoryIds.metal) (init standardCategories))
        Assert.Equal(List.length standardCategories - 1, List.length state1.rows)
        Assert.DoesNotContain(CategoryIds.metal, state1.rows |> List.map (fun c -> c.id))
        match state1.commit with
        | Ok [ RemoveCategory id ] -> Assert.Equal(CategoryIds.metal, id)
        | other -> Assert.Fail($"expected Ok [ RemoveCategory _ ], got %A{other}")

    [<Fact>]
    let ``RemoveCategoryRow of the in-progress row clears the inline edit and nets no commit intent`` () =
        // Begin-then-Remove the same minted row is a round-trip to nothing — no intent projected.
        let state1 = okState (applyCategoryMsg BeginAddCategory (init standardCategories))
        let id = inProgressId state1
        let state2 = okState (applyCategoryMsg (RemoveCategoryRow id) state1)
        Assert.True(Option.isNone state2.editingRow)
        Assert.Equal(List.length standardCategories, List.length state2.rows)
        match state2.commit with
        | Ok [] -> ()
        | other -> Assert.Fail($"expected Ok [] (Begin-then-Remove nets nothing), got %A{other}")

    [<Fact>]
    let ``RemoveCategoryRow on an id the working rows do not hold surfaces the typed UnknownCategoryId`` () =
        let ghost = CategoryId.create ()
        match applyCategoryMsg (RemoveCategoryRow ghost) (init standardCategories) with
        | Error (UnknownCategoryIdErr reason) -> Assert.Contains(string ghost.value, reason)
        | other -> Assert.Fail($"expected Error (UnknownCategoryIdErr _), got %A{other}")

    [<Fact>]
    let ``commit over an untouched editor projects no intents`` () =
        match (init standardCategories).commit with
        | Ok [] -> ()
        | other -> Assert.Fail($"expected Ok [] for an untouched editor, got %A{other}")

    [<Fact>]
    let ``commit projects add, rename and remove intents together`` () =
        // One round touching all three verbs — the commit projection separates them by diff.
        let s1 = okState (applyCategoryMsg BeginAddCategory (init standardCategories))
        let newId = inProgressId s1
        let s2 = okState (applyCategoryMsg (SetCategoryName (newId, CategoryName "New user category")) s1)
        let s3 = okState (applyCategoryMsg (SetCategoryName (CategoryIds.glass, CategoryName "Glass (renamed)")) s2)
        let s4 = okState (applyCategoryMsg (RemoveCategoryRow CategoryIds.metal) s3)
        match s4.commit with
        | Ok intents ->
            Assert.Equal(3, List.length intents)
            let added = { id = newId; name = CategoryName "New user category"; visibility = SelectableOnCreate; origin = UserCategory }
            Assert.Contains(AddCategory added, intents)
            Assert.True(intents |> List.exists (fun i -> match i with | RenameCategory c -> c.id = CategoryIds.glass | _ -> false))
            Assert.True(intents |> List.exists (fun i -> match i with | RemoveCategory id -> id = CategoryIds.metal | _ -> false))
        | other -> Assert.Fail($"expected Ok intents, got %A{other}")
