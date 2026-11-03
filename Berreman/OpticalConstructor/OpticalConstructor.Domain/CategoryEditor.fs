/// Spec 0035 (004) — the pure, Avalonia-free CATEGORY edit model over the seeded
/// `MaterialCategory` catalogue (MaterialLibrary.fs). It mirrors the message-DU
/// discipline of `MaterialComplexityEditor` (state record + one message DU + a
/// `Result`-returning apply), so the Category-manager window is a thin projection
/// and every behaviour is testable without a window.
///
/// The editor holds the WORKING rows plus the catalogue snapshot it opened over;
/// the three message arms mutate the working rows, and `commit` diffs them back
/// against the snapshot to project the add / rename / remove intents the host
/// dispatches to `CategoryProxy` (steps 2/3: `addCategory` / `updateCategory` /
/// `removeCategory`). The only typed rejection the editor itself raises reuses
/// the step-2 `CategoryError`: a blank display name surfaces `InvalidCategoryErr`
/// at `commit` (via the shared `MaterialCategory.validate`), and a name/remove aimed at a
/// row the working set does not hold surfaces `UnknownCategoryIdErr` — the
/// `MaterialComplexityEditor.NoSuchSegmentErr` precedent. The store's own guards
/// (`DuplicateCategoryIdErr`, `BuiltInNotRemovableErr`, `CategoryStillReferencedErr`) stay
/// where they belong, in the proxy the host dispatches the intents to.
module OpticalConstructor.Domain.CategoryEditor

open OpticalConstructor.Domain.MaterialLibrary

/// The editor's message DU: one arm per inline category operation, applied by
/// `applyCategoryMsg`. `BeginAddCategory` carries no payload — the arm MINTS the
/// id (`CategoryId.create ()`), the one non-deterministic transition.
type CategoryMsg =
    /// Begin an inline add: append a fresh, blank-named `UserCategory` /
    /// `SelectableOnCreate` row under a freshly minted id and mark it in-progress.
    | BeginAddCategory
    /// Set the display name of the row with the given id (an inline rename or the
    /// name of a Begin-added row). An id the working rows do not hold is an
    /// `UnknownCategoryIdErr`.
    | SetCategoryName of categoryId : CategoryId * name : CategoryName
    /// Drop the row with the given id from the working set (removal is projected
    /// as a `RemoveCategory` intent — the store enforces the actual block). An id
    /// the working rows do not hold is an `UnknownCategoryIdErr`.
    | RemoveCategoryRow of categoryId : CategoryId

/// One committed intent the host dispatches to `CategoryProxy` — exactly one case
/// per write verb (spec 0035 A.2): `AddCategory` → `addCategory`, `RenameCategory`
/// → `updateCategory`, `RemoveCategory` → `removeCategory`.
type CategoryCommit =
    | AddCategory of MaterialCategory
    | RenameCategory of MaterialCategory
    | RemoveCategory of categoryId : CategoryId

/// The editor state: the current working rows (`rows`), the catalogue snapshot the
/// editor opened over (`original`, the `commit` diff baseline), and the row
/// currently under inline edit (`editingRow` — the freshly Begin-added, not-yet-
/// named row the view focuses; `None` when no inline edit is open).
type CategoryEditState =
    {
        original : MaterialCategory list
        rows : MaterialCategory list
        editingRow : CategoryId option
    }

/// Open the editor over a category catalogue snapshot (the host seeds it with
/// `CategoryProxy.listCategories`). Both the working rows and the diff baseline
/// start at the snapshot; no inline edit is open.
let init (categories : MaterialCategory list) : CategoryEditState =
    {
        original = categories
        rows = categories
        editingRow = None
    }

type CategoryId with

    /// The typed "no such row" rejection, reusing the step-2 `UnknownCategoryIdErr`.
    member private id.unknownEditorRow : CategoryError =
        UnknownCategoryIdErr $"category row '%s{string id.value}' is not in the editor"

/// Apply one message onto one immutable transform (the `applyMaterialComplexityMsg`
/// discipline). The structural transitions succeed; `SetCategoryName` /
/// `RemoveCategoryRow` reject an id the working rows do not hold with the typed
/// `UnknownCategoryIdErr`. Blank-name rejection is deferred to `commit` so a name box
/// may be transiently cleared mid-edit (the store's `MaterialCategory.validate` boundary).
let applyCategoryMsg
    (msg : CategoryMsg)
    (state : CategoryEditState)
    : Result<CategoryEditState, CategoryError> =
    match msg with
    | BeginAddCategory ->
        let id = CategoryId.create ()
        let row =
            {
                id = id
                name = CategoryName ""
                visibility = SelectableOnCreate
                origin = UserCategory
            }
        Ok { state with rows = state.rows @ [ row ]; editingRow = Some id }
    | SetCategoryName (id, name) ->
        if state.rows |> List.exists (fun c -> c.id = id) then
            let rows = state.rows |> List.map (fun c -> if c.id = id then { c with name = name } else c)
            Ok { state with rows = rows }
        else Error id.unknownEditorRow
    | RemoveCategoryRow id ->
        if state.rows |> List.exists (fun c -> c.id = id) then
            let rows = state.rows |> List.filter (fun c -> c.id <> id)
            let editingRow =
                match state.editingRow with
                | Some editing when editing = id -> None
                | other -> other
            Ok { state with rows = rows; editingRow = editingRow }
        else Error id.unknownEditorRow

/// Sequence a `Result`-producing map over a list, keeping the produced values in
/// order and short-circuiting on the first error (the `MaterialComplexityEditor`
/// `traverse` precedent).
let private traverse (f : 'Item -> Result<'Out, CategoryError>) (xs : 'Item list) : Result<'Out list, CategoryError> =
    List.foldBack
        (fun x acc ->
            match acc with
            | Error e -> Error e
            | Ok tail ->
                match f x with
                | Ok y -> Ok (y :: tail)
                | Error e -> Error e)
        xs
        (Ok [])

type CategoryEditState with

    /// The commit projection: diff the working rows against the opened-over snapshot
    /// into the `CategoryProxy` write intents the host dispatches. A row whose id is
    /// new is an `AddCategory`; a row whose record differs from its snapshot (only the
    /// display name is editable) is a `RenameCategory`; a snapshot id no longer among
    /// the rows is a `RemoveCategory`. Every add and rename is validated through the
    /// shared `MaterialCategory.validate`, so a blank display name surfaces the typed
    /// `InvalidCategory` (the first offending row wins); removes need no validation.
    /// Adds and renames appear in working-row order, then removes in snapshot order.
    member state.commit : Result<CategoryCommit list, CategoryError> =
        let originalById = state.original |> List.map (fun c -> c.id, c) |> Map.ofList
        let rowIds = state.rows |> List.map (fun c -> c.id) |> Set.ofList
        let intentOf (c : MaterialCategory) : Result<CategoryCommit option, CategoryError> =
            match Map.tryFind c.id originalById with
            | None -> c.validate |> Result.map (fun () -> Some (AddCategory c))
            | Some existing when existing <> c -> c.validate |> Result.map (fun () -> Some (RenameCategory c))
            | Some _ -> Ok None
        state.rows
        |> traverse intentOf
        |> Result.map (fun intents ->
            let addsAndRenames = intents |> List.choose id
            let removes =
                state.original
                |> List.filter (fun c -> not (Set.contains c.id rowIds))
                |> List.map (fun c -> RemoveCategory c.id)
            addsAndRenames @ removes)
