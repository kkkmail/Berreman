/// Spec 0035 (006) — the Category editor view (UICOMP_XDUO_0006): the pure MVU model and the
/// FuncUI projection behind `CategoryEditorWindow`. It sits over the step-4 Domain
/// `CategoryEditor` edit model (working rows + the opened-over catalogue snapshot) and renders
/// through the step-5 domain-free `CategoryControls` component — the host FLATTENS the working
/// `MaterialCategory` rows into `CategoryControls.Row`s and injects the behaviour as a
/// `CategoryControls.Handlers` record. Add opens an inline `UserCategory` / `SelectableOnCreate`
/// row (`BeginAddCategory`); Rename edits the name inline (built-ins ARE renamable — the store
/// carries no origin guard on update); each row's Save projects that row's add / rename intent
/// and dispatches the matching `CategoryProxy` verb (`addCategory` / `updateCategory`); Remove is
/// confirm-gated inline (arm → confirm on the SAME Remove verb, the block slot doubling as the
/// prompt) and surfaces the store's typed `CategoryStillReferenced` (naming the referencing
/// materials) or `BuiltInNotRemovable` refusal; Cancel discards the row's in-progress edit. The
/// window-level Save/Cancel row (distinct positive/negative styling) flushes any staged edit
/// through the step-4 `commit` diff then closes, or closes discarding. Pure: `update` reaches IO
/// only through the context's `CategoryProxy` fields.
module OpticalConstructor.Ui.CategoryEditorView

open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.CategoryEditor
open OpticalConstructor.Controls

/// The window's IO seam (the functional-proxy Context convention, the `SampleEditorContext`
/// precedent): the category write-seam the inline verbs and the window Save persist through, plus
/// the host's close request (the window passes `this.Close`; tests substitute recording stubs).
/// Function-valued fields have no structural equality, so the context compares by reference — the
/// model holding it keeps its equality.
[<ReferenceEquality>]
type CategoryEditorContext =
    {
        categories : CategoryProxy
        requestClose : unit -> unit
    }

    /// The current catalogue the proxy holds (an empty list if the total listing ever fails).
    member internal this.currentCategories : MaterialCategory list =
        match this.categories.listCategories () with
        | Ok list -> list
        | Error _ -> []

type MaterialCategory with

    /// The working category flattened into the domain-free `CategoryControls` row (a
    /// `BuiltInCategory` row's Remove verb is omitted by the control; the store enforces the actual block).
    member private this.categoryControlsRow : CategoryControls.CategoryControlsRow =
        {
            categoryId = this.id.uiKey
            name = this.name.caption
            origin =
                match this.origin with
                | BuiltInCategory -> CategoryControls.BuiltInCategoryRow
                | UserCategory -> CategoryControls.UserCategoryRow
        }

/// The editor's model: the step-4 `CategoryEditState` (the working rows + the opened-over
/// catalogue snapshot the per-row Save diffs against), the row currently armed for a
/// confirm-gated remove (`pendingRemove`), and the inline block/confirm message. Reference-compared
/// — the context carries function fields (no structural equality) and `update` returns a fresh
/// record anyway, so the Elmish equality gate re-renders on every dispatch.
[<ReferenceEquality>]
type CategoryEditorModel =
    {
        context : CategoryEditorContext
        editor : CategoryEditState
        /// The category id armed for removal (the first Remove click); a second click on the same
        /// row confirms. `None` when no removal is armed.
        pendingRemove : CategoryId option
        /// The inline block/confirm message (the confirm prompt, or the store's typed refusal);
        /// the `CategoryControls` block slot renders only while it is set.
        blockMessage : ControlCaption option
    }

    /// Re-open the editor over the proxy's current catalogue — the diff baseline tracks the store, so
    /// a committed add/rename/remove is reflected and a later Save of the same row is a no-op, not a
    /// duplicate. Clears the confirm arm and the block.
    member internal this.reload () : CategoryEditorModel =
        { this with
            editor = CategoryEditor.init this.context.currentCategories
            pendingRemove = None
            blockMessage = None }

    /// The window-level Save: flush every staged add/rename/remove through the step-4 `commit` diff
    /// (validated there), dispatching each intent to the matching proxy verb and stopping on the first
    /// refusal (surfaced into the block, the window stays open). On a clean flush the window closes.
    member internal this.commitAll () : CategoryEditorModel =
        match this.editor.commit with
        | Error e -> { this with blockMessage = Some e.caption }
        | Ok intents ->
            let applyIntent (acc : Result<unit, CategoryError>) (intent : CategoryCommit) : Result<unit, CategoryError> =
                match acc with
                | Error _ -> acc
                | Ok () ->
                    match intent with
                    | AddCategory c -> this.context.categories.addCategory c
                    | RenameCategory c -> this.context.categories.updateCategory c
                    | RemoveCategory id -> this.context.categories.removeCategory id
            match List.fold applyIntent (Ok ()) intents with
            | Ok () ->
                this.context.requestClose ()
                this.reload ()
            | Error e -> { this with blockMessage = Some e.caption }

    /// The domain-free `CategoryControls` state: the working rows and the block slot.
    member internal this.controlsState : CategoryControls.CategoryControlsState =
        {
            rows = this.editor.rows |> List.map (fun c -> c.categoryControlsRow)
            blockMessage = this.blockMessage
        }

type CategoryEditorContext with

    /// The editor opened over the proxy's current catalogue, nothing armed.
    member this.initCategoryEditor : CategoryEditorModel =
        {
            context = this
            editor = CategoryEditor.init this.currentCategories
            pendingRemove = None
            blockMessage = None
        }

type CategoryEditorMsg =
    /// Begin an inline add — the step-4 `BeginAddCategory` (mints the id, a blank row).
    | BeginAdd
    /// A row's inline name box changed — the step-4 `SetCategoryName (id, newName)`.
    | SetCategoryEditorName of categoryId : CategoryId * name : CategoryName
    /// Commit the row's add (new id) or rename (existing id) through the matching proxy verb.
    | SaveRow of categoryId : CategoryId
    /// The confirm-gated remove (arm, then confirm) — dispatches `removeCategory` on confirm.
    | RemoveRow of categoryId : CategoryId
    /// Discard the row's in-progress edit.
    | CancelRow of categoryId : CategoryId
    /// The window-level Save: flush any staged edit through the step-4 `commit`, then close.
    | CommitAll
    /// The window-level Cancel: close, discarding.
    | CancelWindow

// ---------------------------------------------------------------------------
// Pure helpers.
// ---------------------------------------------------------------------------

let private rowById (editor : CategoryEditState) (id : CategoryId) : MaterialCategory option =
    editor.rows |> List.tryFind (fun c -> c.id = id)

let private snapshotById (editor : CategoryEditState) (id : CategoryId) : MaterialCategory option =
    editor.original |> List.tryFind (fun c -> c.id = id)

/// Commit ONE row: a row whose id is new to the snapshot is an add, a row that differs from its
/// snapshot is a rename, an unchanged row is a no-op. The name is validated through the shared
/// `MaterialCategory.validate`, so a blank name surfaces the typed `InvalidCategoryErr` into the block. On a
/// successful proxy write the editor is reloaded so the snapshot tracks the store.
let private saveRow (m : CategoryEditorModel) (id : CategoryId) : CategoryEditorModel =
    match rowById m.editor id with
    | None -> { m with pendingRemove = None }
    | Some row ->
        let intent =
            match snapshotById m.editor id with
            | None -> row.validate |> Result.map (fun () -> Some (AddCategory row))
            | Some existing when existing <> row -> row.validate |> Result.map (fun () -> Some (RenameCategory row))
            | Some _ -> Ok None
        match intent with
        | Error e -> { m with blockMessage = Some e.caption; pendingRemove = None }
        | Ok None -> { m with blockMessage = None; pendingRemove = None }
        | Ok (Some commit) ->
            let written =
                match commit with
                | AddCategory c -> m.context.categories.addCategory c
                | RenameCategory c -> m.context.categories.updateCategory c
                | RemoveCategory _ -> Ok ()
            match written with
            | Ok () -> m.reload ()
            | Error e -> { m with blockMessage = Some e.caption; pendingRemove = None }

/// The confirm-gated remove: the first click on a row's Remove ARMS it (the block slot shows the
/// confirm prompt); a second click on the SAME row dispatches `removeCategory`. The store's typed
/// refusal (`CategoryStillReferenced` / `BuiltInNotRemovable`) is surfaced into the block and the
/// store is left unchanged; a successful remove reloads the editor.
let private removeRow (m : CategoryEditorModel) (id : CategoryId) : CategoryEditorModel =
    match m.pendingRemove with
    | Some armed when armed = id ->
        match m.context.categories.removeCategory id with
        | Ok () -> m.reload ()
        | Error e -> { m with blockMessage = Some e.caption; pendingRemove = None }
    | _ ->
        let name =
            match rowById m.editor id with
            | Some c -> c.name.value
            | None -> string id.value
        { m with
            pendingRemove = Some id
            blockMessage = Some (ControlCaption $"Remove category '%s{name}'? Click Remove again to confirm.") }

/// Discard a row's in-progress edit: a freshly-added, unsaved row (absent from the snapshot) is
/// dropped from the working set; an existing row's name is reset to its persisted value.
let private cancelRow (m : CategoryEditorModel) (id : CategoryId) : CategoryEditorModel =
    let editorResult =
        match snapshotById m.editor id with
        | None -> CategoryEditor.applyCategoryMsg (RemoveCategoryRow id) m.editor
        | Some original -> CategoryEditor.applyCategoryMsg (SetCategoryName (id, original.name)) m.editor
    match editorResult with
    | Ok editor -> { m with editor = editor; pendingRemove = None; blockMessage = None }
    | Error _ -> { m with pendingRemove = None; blockMessage = None }

// ---------------------------------------------------------------------------
// update (pure — IO only through the context's proxy fields).
// ---------------------------------------------------------------------------

let updateCategoryEditor (msg : CategoryEditorMsg) (m : CategoryEditorModel) : CategoryEditorModel =
    match msg with
    | BeginAdd ->
        match CategoryEditor.applyCategoryMsg BeginAddCategory m.editor with
        | Ok editor -> { m with editor = editor; pendingRemove = None; blockMessage = None }
        | Error e -> { m with blockMessage = Some e.caption }
    | SetCategoryEditorName (id, name) ->
        match CategoryEditor.applyCategoryMsg (SetCategoryName (id, name)) m.editor with
        | Ok editor -> { m with editor = editor; pendingRemove = None; blockMessage = None }
        | Error _ -> m
    | SaveRow id -> saveRow m id
    | RemoveRow id -> removeRow m id
    | CancelRow id -> cancelRow m id
    | CommitAll -> m.commitAll ()
    | CancelWindow ->
        m.context.requestClose ()
        m

// ---------------------------------------------------------------------------
// The FuncUI view: the step-5 CategoryControls surface (host-flattened rows + injected handlers),
// with a window-level Save/Cancel row pinned below it. The window-level action buttons carry a
// mutable AutomationId (never Name) — the SampleEditorWindow precedent.
// ---------------------------------------------------------------------------

let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private saveBackground = Color.FromRgb(186uy, 224uy, 186uy)
let private cancelBackground = Color.FromRgb(236uy, 202uy, 202uy)
let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)

/// A window-level action button (one row, distinct positive/negative styling). `e.Handled <- true`
/// drops FuncUI's duplicate Tunnel|Bubble pass.
let private actionButton (id : ConstructorUiId) (label : ControlCaption) (background : Color) (onClick : unit -> unit) : IView =
    Border.create [
        id.automationAttribute<Border> ()
        Border.background (brush background)
        Border.borderBrush (brush idleBorder)
        Border.borderThickness 1.0
        Border.cornerRadius (CornerRadius 3.0)
        Border.padding (Thickness(22.0, 6.0))
        Border.margin (Thickness(0.0, 0.0, 10.0, 0.0))
        Border.verticalAlignment VerticalAlignment.Center
        Border.child (TextBlock.create [ TextBlock.text label.value ])
        Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box id.value))
    ] :> IView

let private saveCancelRow (dispatch : CategoryEditorMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 0.0
        StackPanel.children [
            actionButton ConstructorUiId.categorySaveButton (ControlCaption "Save") saveBackground (fun () -> dispatch CommitAll)
            actionButton ConstructorUiId.categoryCancelButton (ControlCaption "Cancel") cancelBackground (fun () -> dispatch CancelWindow)
        ]
    ] :> IView

/// The behaviour injected into `CategoryControls` (the functional-proxy seam): each verb reads the
/// row key back as its category (the keys round-trip from real categories, so a parse failure is
/// ignored) and dispatches the matching `Msg`.
let private handlers (dispatch : CategoryEditorMsg -> unit) : CategoryControls.CategoryControlsHandlers =
    let withId (toMsg : CategoryId -> CategoryEditorMsg) (key : ConstructorUiKey) : unit =
        match key.tryCategoryId with
        | Some id -> dispatch (toMsg id)
        | None -> ()
    {
        addCategory = fun () -> dispatch BeginAdd
        setCategoryName = fun key name ->
            match key.tryCategoryId with
            | Some id -> dispatch (SetCategoryEditorName (id, name.toCategoryName))
            | None -> ()
        saveCategory = withId SaveRow
        removeCategory = withId RemoveRow
        cancelCategory = withId CancelRow
    }

/// The whole editor: the step-5 `CategoryControls` surface (Add verb, editable list, inline block
/// slot) filling the centre, with the window-level Save/Cancel row pinned to the bottom.
let categoryEditorView (m : CategoryEditorModel) (dispatch : CategoryEditorMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            Border.create [
                Border.dock Dock.Bottom
                Border.padding (Thickness(8.0, 6.0))
                Border.child (saveCancelRow dispatch)
            ]
            Border.create [
                Border.padding (Thickness 8.0)
                Border.child (CategoryControls.categoryControlsView m.controlsState (handlers dispatch))
            ]
        ]
    ] :> IView
