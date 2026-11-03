namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Controls

/// Spec 0035 (005) — the CategoryControls component (UICOMP_XDUO_0005): the Category-manager list
/// surface. Covers the pure control contract (empty state, the stable intent-named UiIds + derived
/// per-row ids) and the headless structure proof: mount `view` over a known `State`, find every
/// UiIds control, and observe that a simulated Add click invokes the ADD handler (the functional-proxy
/// seam — the control never touches a proxy). Wiring into a parent view is a later step; these tests
/// mount the component alone.
module CategoryControlsTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the per-row
    /// name box and verb buttons carry an AutomationId — a mutable attached property — so they survive
    /// the host's row add/remove membership changes; see the control).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    /// A known flattened row set: one user category (editable AND removable) and one built-in category
    /// (editable name, but NO Remove verb — removed, not greyed).
    let private userRowId : ConstructorUiKey = ConstructorUiKey "9a1f0c33-user-category"
    let private builtInRowId : ConstructorUiKey = ConstructorUiKey "2a4b6c8d-glass-builtin"

    let private knownRows : CategoryControls.CategoryControlsRow list =
        [
            { categoryId = userRowId; name = ControlCaption "My Category"; origin = CategoryControls.UserCategoryRow }
            { categoryId = builtInRowId; name = ControlCaption "Glass"; origin = CategoryControls.BuiltInCategoryRow }
        ]

    /// The known State the structure test mounts: both rows plus a surfaced block message (so the
    /// `CategoryBlockMessage` slot renders).
    let private knownState : CategoryControls.CategoryControlsState =
        {
            rows = knownRows
            blockMessage = Some (ControlCaption "This category is still referenced by 2 materials and cannot be removed.")
        }

    /// Recording stub handlers (the test substitutes the functional-proxy seam): every dispatch appends
    /// a tag, so a click's MATCHING handler is observable.
    let private recorder () : ResizeArray<string> * CategoryControls.CategoryControlsHandlers =
        let calls = ResizeArray<string>()
        let handlers : CategoryControls.CategoryControlsHandlers =
            {
                addCategory = fun () -> calls.Add("add")
                setCategoryName = fun id name -> calls.Add($"name:%s{id.value}=%s{name.value}")
                saveCategory = fun id -> calls.Add("save:" + id.value)
                removeCategory = fun id -> calls.Add("remove:" + id.value)
                cancelCategory = fun id -> calls.Add("cancel:" + id.value)
            }
        calls, handlers

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the empty Category state has no rows and no block message`` () =
        let s = CategoryControls.emptyCategoryControls
        Assert.Empty(s.rows)
        Assert.Equal(None, s.blockMessage)

    [<Fact>]
    let ``the Category UiIds are the stable intent-named ids`` () =
        Assert.Equal("CategoriesList", ConstructorUiId.categoriesList.value)
        Assert.Equal("AddCategoryButton", ConstructorUiId.addCategoryButton.value)
        Assert.Equal("CategoryNameBox", ConstructorUiId.categoryNameBox.value)
        Assert.Equal("RemoveCategoryButton", ConstructorUiId.removeCategoryButton.value)
        Assert.Equal("CategorySaveButton", ConstructorUiId.categorySaveButton.value)
        Assert.Equal("CategoryCancelButton", ConstructorUiId.categoryCancelButton.value)
        Assert.Equal("CategoryBlockMessage", ConstructorUiId.categoryBlockMessage.value)
        // The derived per-row ids are the base id prefixed with the row's category id (collision-safe).
        Assert.Equal("CategoryNameBox_abc", (ConstructorUiId.categoryRowNameBox (ConstructorUiKey "abc")).value)
        Assert.Equal("CategorySaveButton_abc", (ConstructorUiId.categoryRowSaveButton (ConstructorUiKey "abc")).value)
        Assert.Equal("RemoveCategoryButton_abc", (ConstructorUiId.categoryRowRemoveButton (ConstructorUiKey "abc")).value)
        Assert.Equal("CategoryCancelButton_abc", (ConstructorUiId.categoryRowCancelButton (ConstructorUiKey "abc")).value)

    // ============================ headless structure proof ============================

    let private isPresent (window : Window) (id : string) : bool =
        window.GetVisualDescendants()
        |> Seq.exists (function :? Control as c -> matchesId id c | _ -> false)

    /// Click the centre of the control carrying `id` (by Name or AutomationId).
    let private clickOn (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Border as b when matchesId id b && b.IsEffectivelyVisible -> Some b | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some b ->
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Category view mounts every UiIds control and an Add click dispatches the add handler`` () =
        HeadlessSession.run (fun () ->
            let calls, handlers = recorder ()
            let window = Window(Width = 640.0, Height = 480.0)
            window.Content <- Component(fun _ -> CategoryControls.categoryControlsView knownState handlers)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The singleton controls exist over the known State.
            Assert.True(isPresent window ConstructorUiId.categoriesList.value, "the categories list is missing")
            Assert.True(isPresent window ConstructorUiId.addCategoryButton.value, "the Add verb is missing")
            Assert.True(isPresent window ConstructorUiId.categoryBlockMessage.value, "the block-message slot is missing")
            // Every row exposes an inline name box + Save + Cancel.
            for r in knownRows do
                Assert.True(isPresent window (ConstructorUiId.categoryRowNameBox r.categoryId).value, $"row %s{r.categoryId.value} has no name box")
                Assert.True(isPresent window (ConstructorUiId.categoryRowSaveButton r.categoryId).value, $"row %s{r.categoryId.value} has no Save verb")
                Assert.True(isPresent window (ConstructorUiId.categoryRowCancelButton r.categoryId).value, $"row %s{r.categoryId.value} has no Cancel verb")
            // The user row exposes Remove; the built-in row OMITS it (removed, not greyed — a greyed
            // button would still be found in the tree).
            Assert.True(
                isPresent window (ConstructorUiId.categoryRowRemoveButton userRowId).value,
                "the user row must expose the Remove verb")
            Assert.False(
                isPresent window (ConstructorUiId.categoryRowRemoveButton builtInRowId).value,
                "the built-in row's Remove verb must be REMOVED (not greyed)")
            // The acceptance click: Add invokes the ADD handler — and no other verb handler.
            let count (tag : string) : int = calls |> Seq.filter ((=) tag) |> Seq.length
            clickOn window ConstructorUiId.addCategoryButton.value
            Assert.Equal(1, count "add")
            // Each per-row verb dispatches ITS handler with the row's id.
            clickOn window (ConstructorUiId.categoryRowSaveButton userRowId).value
            Assert.Contains("save:" + userRowId.value, calls)
            clickOn window (ConstructorUiId.categoryRowRemoveButton userRowId).value
            Assert.Contains("remove:" + userRowId.value, calls)
            clickOn window (ConstructorUiId.categoryRowCancelButton builtInRowId).value
            Assert.Contains("cancel:" + builtInRowId.value, calls)
            window.Close())
