namespace OpticalConstructor.Ui.Tests

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.Ui.CategoryEditorView

/// Spec 0035 (006) — the CategoryEditorWindow component (UICOMP_XDUO_0006): the Category editor
/// window in OpticalConstructor.Ui over the step-4 Domain `CategoryEditor` edit model,
/// rendered through the step-5 `CategoryControls` surface. Pure contract + model tests for the
/// MVU wiring, and headless semantic-tree proofs that DRIVE THE REAL WINDOW BY ITS UiIds over a
/// stub `CategoryProxy` — the slice acceptance: Add grows the list, Rename updates a name, and
/// removing a referenced (or built-in) category surfaces the typed block and leaves the list
/// unchanged. The component is declared, not wired — no parent view opens it here.
module CategoryEditorWindowTests =

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the per-row
    /// name boxes / verb buttons carry an AutomationId — the CategoryControls precedent).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || Avalonia.Automation.AutomationProperties.GetAutomationId(c) = id

    let private tryFindControl (window : Window) (id : string) : Control option =
        window.GetVisualDescendants()
        |> Seq.tryPick (function :? Control as c when matchesId id c -> Some c | _ -> None)

    let private isPresent (window : Window) (id : string) : bool =
        match tryFindControl window id with
        | Some _ -> true
        | None -> false

    /// Click the centre of the clickable Border carrying `id` (by Name or AutomationId).
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
                // A window Save / Cancel click closes the window during the press — skip the release then.
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    /// Set the text of the TextBox carrying `id` (fires the property-change subscription the
    /// control's `onTextChanged` binds — still driving the control found by its UiId).
    let private setText (window : Window) (id : string) (text : string) : unit =
        match tryFindControl window id with
        | Some (:? TextBox as tb) ->
            tb.Text <- text
            Dispatcher.UIThread.RunJobs()
        | Some c -> Assert.Fail($"%s{id} is a %s{c.GetType().Name}, not a TextBox")
        | None -> Assert.Fail($"%s{id} was not found")

    /// The text of the TextBlock carrying `id`.
    let private textOf (window : Window) (id : string) : string =
        match tryFindControl window id with
        | Some (:? TextBlock as tb) -> tb.Text
        | Some c -> failwith $"%s{id} is a %s{c.GetType().Name}, not a TextBlock"
        | None -> failwith $"%s{id} was not found in the visual tree"

    /// The guid suffix of the freshly-added, still-blank category row's name box — `BeginAddCategory`
    /// mints the id, so the headless Add proof locates the new row by its empty name box.
    let private newRowGuid (window : Window) : string =
        let prefix = CategoryControls.UiIds.nameBox + "_"
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function
                | :? TextBox as tb ->
                    let auto = Avalonia.Automation.AutomationProperties.GetAutomationId(tb)
                    if auto <> null && auto.StartsWith(prefix) && String.IsNullOrEmpty(tb.Text)
                    then Some (auto.Substring(prefix.Length))
                    else None
                | _ -> None)
        match found with
        | Some g -> g
        | None -> failwith "no freshly-added blank category row was found"

    /// A hand-rolled in-memory stub `CategoryProxy` (the functional-proxy seam — the how-to's "stub
    /// CategoryProxy"): a mutable ordered list that actually stores adds / renames / removes, records
    /// each write verb it reaches, refuses a `BuiltInCategory` remove with `BuiltInNotRemovable`, and
    /// refuses a `UserCategory` remove whose id is in `referenced` with `CategoryStillReferenced`.
    let private stubProxy (seed : MaterialCategory list) (referenced : Set<Guid>) : ResizeArray<string> * CategoryProxy =
        let store = ref seed
        let calls = ResizeArray<string>()
        let proxy : CategoryProxy =
            {
                listCategories = fun () -> Ok store.Value
                addCategory =
                    fun c ->
                        calls.Add("add:" + c.name)
                        store.Value <- store.Value @ [ c ]
                        Ok ()
                updateCategory =
                    fun c ->
                        calls.Add("update:" + c.name)
                        store.Value <- store.Value |> List.map (fun x -> if x.id = c.id then c else x)
                        Ok ()
                removeCategory =
                    fun id ->
                        calls.Add("remove:" + string id.value)
                        match store.Value |> List.tryFind (fun x -> x.id = id) with
                        | None -> Error (UnknownCategoryId $"unknown category id '%s{string id.value}'")
                        | Some c ->
                            match c.origin with
                            | BuiltInCategory ->
                                Error (BuiltInNotRemovable $"'%s{c.name}' ships with the app and cannot be removed")
                            | UserCategory ->
                                if Set.contains id.value referenced
                                then Error (CategoryStillReferenced $"'%s{c.name}' is still referenced by materials")
                                else
                                    store.Value <- store.Value |> List.filter (fun x -> x.id <> id)
                                    Ok ()
            }
        calls, proxy

    let private contextOver (proxy : CategoryProxy) : CategoryEditorContext =
        { categories = proxy; requestClose = ignore }

    let private userCategory (id : CategoryId) (name : string) : MaterialCategory =
        { id = id; name = name; visibility = SelectableOnCreate; origin = UserCategory }

    // ============================ pure control contract ============================

    [<Fact>]
    let ``the Category editor window id is the stable CategoryEditorWindow id`` () =
        Assert.Equal("CategoryEditorWindow", UiIds.window)

    [<Fact>]
    let ``init seeds the editor over the proxy's listCategories with nothing armed`` () =
        let _, proxy = stubProxy standardCategories Set.empty
        let m = init (contextOver proxy)
        Assert.Equal(List.length standardCategories, List.length m.editor.rows)
        Assert.Equal("", m.blockMessage)
        match m.pendingRemove with
        | None -> ()
        | Some _ -> Assert.Fail("a freshly-opened editor must arm no removal")

    // ============================ pure model / update ============================

    [<Fact>]
    let ``BeginAdd appends a blank UserCategory SelectableOnCreate row and marks it editing`` () =
        let _, proxy = stubProxy standardCategories Set.empty
        let m = init (contextOver proxy) |> update BeginAdd
        Assert.Equal(List.length standardCategories + 1, List.length m.editor.rows)
        let added = List.last m.editor.rows
        Assert.Equal("", added.name)
        Assert.Equal(SelectableOnCreate, added.visibility)
        Assert.Equal(UserCategory, added.origin)
        match m.editor.editingRow with
        | Some id when id = added.id -> ()
        | _ -> Assert.Fail("the freshly-added row must be the editing row")

    [<Fact>]
    let ``Add then set-name then Save persists a new category through addCategory`` () =
        let calls, proxy = stubProxy standardCategories Set.empty
        let staged = init (contextOver proxy) |> update BeginAdd
        let newId = (List.last staged.editor.rows).id
        staged |> update (SetName (newId, "Polymer")) |> update (SaveRow newId) |> ignore
        match proxy.listCategories () with
        | Ok l ->
            Assert.Equal(List.length standardCategories + 1, List.length l)
            Assert.True(l |> List.exists (fun c -> c.name = "Polymer" && c.origin = UserCategory), "the new user category must be persisted")
        | Error e -> Assert.Fail($"listCategories failed: %A{e}")
        Assert.Contains("add:Polymer", calls)

    [<Fact>]
    let ``a blank new name is refused at Save with the typed InvalidCategory and nothing persists`` () =
        let calls, proxy = stubProxy standardCategories Set.empty
        let staged = init (contextOver proxy) |> update BeginAdd
        let newId = (List.last staged.editor.rows).id
        let blocked = staged |> update (SaveRow newId)
        Assert.True(blocked.blockMessage <> "", "a blank-named add must surface a typed block")
        Assert.False(calls |> Seq.exists (fun s -> s.StartsWith "add:"), "a blank-named add must not reach the store")

    [<Fact>]
    let ``Rename an existing user category commits through updateCategory`` () =
        let userId = CategoryId.create ()
        let calls, proxy = stubProxy (standardCategories @ [ userCategory userId "Draft" ]) Set.empty
        init (contextOver proxy) |> update (SetName (userId, "Final")) |> update (SaveRow userId) |> ignore
        match proxy.listCategories () with
        | Ok l ->
            match l |> List.tryFind (fun c -> c.id = userId) with
            | Some c -> Assert.Equal("Final", c.name)
            | None -> Assert.Fail("the renamed category vanished")
        | Error e -> Assert.Fail($"listCategories failed: %A{e}")
        Assert.Contains("update:Final", calls)

    [<Fact>]
    let ``a built-in category is renamable through updateCategory`` () =
        let calls, proxy = stubProxy standardCategories Set.empty
        init (contextOver proxy) |> update (SetName (CategoryIds.glass, "Glazing")) |> update (SaveRow CategoryIds.glass) |> ignore
        match proxy.listCategories () with
        | Ok l ->
            match l |> List.tryFind (fun c -> c.id = CategoryIds.glass) with
            | Some c -> Assert.Equal("Glazing", c.name)
            | None -> Assert.Fail("the renamed built-in vanished")
        | Error e -> Assert.Fail($"listCategories failed: %A{e}")
        Assert.Contains("update:Glazing", calls)

    [<Fact>]
    let ``removing a referenced user category is confirm-gated, surfaces CategoryStillReferenced, and leaves the store unchanged`` () =
        let userId = CategoryId.create ()
        let calls, proxy = stubProxy (standardCategories @ [ userCategory userId "Used" ]) (Set.ofList [ userId.value ])
        // First click ARMS the confirm — the store is not yet touched.
        let armed = init (contextOver proxy) |> update (RemoveRow userId)
        Assert.True(armed.blockMessage <> "", "arming must surface the confirm prompt")
        Assert.False(calls |> Seq.exists (fun s -> s.StartsWith "remove:"), "arming must not reach the store")
        // The confirming second click reaches the store, which refuses.
        let blocked = armed |> update (RemoveRow userId)
        Assert.Contains("referenced", blocked.blockMessage)
        match proxy.listCategories () with
        | Ok l -> Assert.True(l |> List.exists (fun c -> c.id = userId), "the referenced category must survive the refusal")
        | Error e -> Assert.Fail($"listCategories failed: %A{e}")

    [<Fact>]
    let ``removing a built-in surfaces BuiltInNotRemovable and leaves the store unchanged`` () =
        let _, proxy = stubProxy standardCategories Set.empty
        let blocked =
            init (contextOver proxy)
            |> update (RemoveRow CategoryIds.glass)
            |> update (RemoveRow CategoryIds.glass)
        Assert.Contains("cannot be removed", blocked.blockMessage)
        match proxy.listCategories () with
        | Ok l -> Assert.Equal(List.length standardCategories, List.length l)
        | Error e -> Assert.Fail($"listCategories failed: %A{e}")

    [<Fact>]
    let ``Cancel discards a freshly-added, unsaved row`` () =
        let calls, proxy = stubProxy standardCategories Set.empty
        let staged = init (contextOver proxy) |> update BeginAdd
        let newId = (List.last staged.editor.rows).id
        let cancelled = staged |> update (SetName (newId, "Temp")) |> update (CancelRow newId)
        Assert.Equal(List.length standardCategories, List.length cancelled.editor.rows)
        Assert.False(calls |> Seq.exists (fun s -> s.StartsWith "add:"), "a cancelled add must not reach the store")

    [<Fact>]
    let ``the window Save flushes a staged rename through the proxy and requests close`` () =
        let calls, proxy = stubProxy standardCategories Set.empty
        let closed = ref false
        let context : CategoryEditorContext = { categories = proxy; requestClose = fun () -> closed.Value <- true }
        init context |> update (SetName (CategoryIds.glass, "Glazing")) |> update CommitAll |> ignore
        Assert.True(closed.Value, "the window Save must request close")
        Assert.Contains("update:Glazing", calls)

    [<Fact>]
    let ``the window Cancel requests close and writes nothing`` () =
        let calls, proxy = stubProxy standardCategories Set.empty
        let closed = ref false
        let context : CategoryEditorContext = { categories = proxy; requestClose = fun () -> closed.Value <- true }
        init context |> update (SetName (CategoryIds.glass, "X")) |> update CancelWindow |> ignore
        Assert.True(closed.Value, "the window Cancel must request close")
        Assert.Empty(calls)

    // ============================ headless semantic-tree proofs ============================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the window mounts carrying the CategoryEditorWindow id and the CategoryControls surface`` () =
        HeadlessSession.run (fun () ->
            let _, proxy = stubProxy standardCategories Set.empty
            let window = CategoryEditorWindow(proxy)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(matchesId UiIds.window window, "the window itself carries the CategoryEditorWindow id")
            Assert.True(isPresent window CategoryControls.UiIds.list, "the categories list is missing")
            Assert.True(isPresent window CategoryControls.UiIds.addButton, "the Add verb is missing")
            Assert.True(isPresent window CategoryControls.UiIds.saveButton, "the window Save is missing")
            Assert.True(isPresent window CategoryControls.UiIds.cancelButton, "the window Cancel is missing")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Add then Save grows the category list through addCategory`` () =
        HeadlessSession.run (fun () ->
            let _, proxy = stubProxy standardCategories Set.empty
            let before =
                match proxy.listCategories () with
                | Ok l -> List.length l
                | Error e -> failwith $"seed listing failed: %A{e}"
            let window = CategoryEditorWindow(proxy)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            clickOn window CategoryControls.UiIds.addButton
            let guid = newRowGuid window
            setText window (CategoryControls.UiIds.rowNameBox guid) "Polymer"
            clickOn window (CategoryControls.UiIds.rowSaveButton guid)
            match proxy.listCategories () with
            | Ok l ->
                Assert.Equal(before + 1, List.length l)
                Assert.True(l |> List.exists (fun c -> c.name = "Polymer"), "the added category must be listed")
            | Error e -> Assert.Fail($"listCategories failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: Rename edits a category name in place through updateCategory`` () =
        HeadlessSession.run (fun () ->
            let userId = CategoryId.create ()
            let _, proxy = stubProxy (standardCategories @ [ userCategory userId "Draft" ]) Set.empty
            let window = CategoryEditorWindow(proxy)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let guid = string userId.value
            setText window (CategoryControls.UiIds.rowNameBox guid) "Renamed"
            clickOn window (CategoryControls.UiIds.rowSaveButton guid)
            match proxy.listCategories () with
            | Ok l ->
                match l |> List.tryFind (fun c -> c.id = userId) with
                | Some c -> Assert.Equal("Renamed", c.name)
                | None -> Assert.Fail("the renamed category vanished")
            | Error e -> Assert.Fail($"listCategories failed: %A{e}")
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``acceptance: removing a referenced category surfaces the block and leaves the list unchanged`` () =
        HeadlessSession.run (fun () ->
            let userId = CategoryId.create ()
            let _, proxy = stubProxy (standardCategories @ [ userCategory userId "Used" ]) (Set.ofList [ userId.value ])
            let window = CategoryEditorWindow(proxy)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let guid = string userId.value
            // First Remove click ARMS the confirm — the block slot shows the prompt.
            clickOn window (CategoryControls.UiIds.rowRemoveButton guid)
            Assert.True(isPresent window CategoryControls.UiIds.blockMessage, "the confirm prompt must show")
            // The confirming second click surfaces the store's typed refusal.
            clickOn window (CategoryControls.UiIds.rowRemoveButton guid)
            Assert.True(isPresent window CategoryControls.UiIds.blockMessage, "the typed block must show")
            Assert.Contains("referenced", textOf window CategoryControls.UiIds.blockMessage)
            // The category — and its row — survive unchanged.
            Assert.True(isPresent window (CategoryControls.UiIds.rowNameBox guid), "the referenced row must still render")
            match proxy.listCategories () with
            | Ok l -> Assert.True(l |> List.exists (fun c -> c.id = userId), "the referenced category must survive")
            | Error e -> Assert.Fail($"listCategories failed: %A{e}")
            window.Close())
