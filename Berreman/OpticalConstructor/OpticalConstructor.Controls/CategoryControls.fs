namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable CATEGORY-MANAGER control (Spec 0035, step 005 — UICOMP_XDUO_0005): the list surface
/// the Category manager projects the slice-004 `CategoryEditor` onto. A category list with an inline
/// name box per row and Add / Rename(save) / Remove verbs; a BUILT-IN row's name stays editable but
/// its Remove verb is OMITTED (removed, not greyed), matching the `MaterialsControls` view-only-Edit
/// precedent; an inline block-message slot surfaces the store's typed refusals (`CategoryStillReferenced`
/// / `BuiltInNotRemovable`). This control is DOMAIN-FREE (matching `MaterialsControls` / `LibraryControls`):
/// the host flattens the editor's working `MaterialCategory` rows into `Row`s and injects the behaviour
/// as a `Handlers` function record (the functional-proxy seam; a test substitutes stubs). The control
/// renders the rows it is given and never touches a proxy (the host flattens and dispatches).
module CategoryControls =

    /// One category row the host pre-flattens from the `CategoryEditor` working rows (Controls carries
    /// no domain types). `categoryId` is the category's stable id in its string form; `isBuiltIn` marks
    /// a `BuiltInCategory` row — its name box stays editable, but its Remove verb is removed (only a
    /// `UserCategory` may be deleted; the store enforces the actual block).
    type Row =
        {
            categoryId : string
            name : string
            isBuiltIn : bool
        }

    /// The bay's pure, serializable state.
    type State =
        {
            /// The rows the host flattened from the editor's working set — rendered as given.
            rows : Row list
            /// The inline block message (`CategoryStillReferenced` / `BuiltInNotRemovable`), or "" when
            /// no block is surfaced — the slot renders only while a message is present.
            blockMessage : string
        }

    let empty : State =
        {
            rows = []
            blockMessage = ""
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs). The per-row verbs
    /// take the row's category id — the host maps them onto the `CategoryEditor` messages / `commit`.
    type Handlers =
        {
            /// Begin an inline add (a fresh, blank-named row) — the editor's `BeginAddCategory`.
            addCategory : unit -> unit
            /// The row's inline name box changed — the editor's `SetCategoryName (id, newName)`.
            setCategoryName : string -> string -> unit
            /// Save (commit) the row's rename.
            saveCategory : string -> unit
            /// Remove the row (a `UserCategory` only — the store enforces the actual block).
            removeCategory : string -> unit
            /// Cancel the row's in-progress edit.
            cancelCategory : string -> unit
        }

    /// Stable intent-named automation ids (CLAUDE.md UI guidance). The four per-row ids are the base
    /// intent name; the `row*` helpers prefix the row's category id so ids cannot collide and each
    /// row's control is targetable (the `MaterialsControls.row` / `LibraryControls.entry` precedent).
    [<RequireQualifiedAccess>]
    module UiIds =
        [<Literal>]
        let list = "CategoriesList"
        [<Literal>]
        let addButton = "AddCategoryButton"
        [<Literal>]
        let nameBox = "CategoryNameBox"
        [<Literal>]
        let removeButton = "RemoveCategoryButton"
        [<Literal>]
        let saveButton = "CategorySaveButton"
        [<Literal>]
        let cancelButton = "CategoryCancelButton"
        [<Literal>]
        let blockMessage = "CategoryBlockMessage"
        /// A row's inline name box id — the base id prefixed with the row's category id.
        let rowNameBox (categoryId : string) : string = nameBox + "_" + categoryId
        /// A row's Save (rename) button id.
        let rowSaveButton (categoryId : string) : string = saveButton + "_" + categoryId
        /// A row's Remove button id (present only for a non-built-in row).
        let rowRemoveButton (categoryId : string) : string = removeButton + "_" + categoryId
        /// A row's Cancel button id.
        let rowCancelButton (categoryId : string) : string = cancelButton + "_" + categoryId

    // -- The button look, identical to the other bars' idle/chosen boxes (so they MATCH). --
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private chosenBackground = color 150 185 235
    let private blockBackground = color 250 235 235
    let private idleBorder = color 120 120 120

    /// Set `AutomationProperties.AutomationId` (a freely-mutable attached property — unlike
    /// `Control.Name`) on a `Border` through FuncUI's attr builder. The rows and their per-row verb
    /// buttons live in child lists whose MEMBERSHIP changes (the host adds/removes a row; a built-in
    /// row omits Remove), so a sibling can shift onto a reused control's slot — Avalonia forbids
    /// renaming a styled control, and an AutomationId survives that reuse (the `MaterialsControls`
    /// precedent).
    let private borderAutoId (autoId : string) : IAttr<Border> =
        AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

    /// The same reuse-safe AutomationId on a `TextBox` (the per-row inline name box).
    let private textBoxAutoId (autoId : string) : IAttr<TextBox> =
        AttrBuilder<TextBox>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

    /// A clickable verb button (a styled, named Border), accented when it is the primary verb.
    /// `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the id or
    /// the label changes so a reused box can't keep a stale handler.
    let private button (autoId : string) (label : string) (accent : bool) (onClick : unit -> unit) : IView =
        Border.create [
            borderAutoId autoId
            Border.background (brush (if accent then chosenBackground else idleBackground))
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(12.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
            Border.horizontalAlignment HorizontalAlignment.Left
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (autoId, label)))
        ] :> IView

    /// One category row — an inline name box (editable for EVERY row, including built-ins), a Save
    /// (rename) verb and a Cancel verb, and a Remove verb for a `UserCategory` only (a built-in OMITS
    /// Remove — removed from the row, not greyed).
    let private rowView (handlers : Handlers) (r : Row) : IView =
        let nameBox =
            TextBox.create [
                textBoxAutoId (UiIds.rowNameBox r.categoryId)
                TextBox.width 200.0
                TextBox.text r.name
                TextBox.onTextChanged (handlers.setCategoryName r.categoryId)
            ] :> IView
        let removeVerb =
            if r.isBuiltIn then []
            else [ button (UiIds.rowRemoveButton r.categoryId) "Remove" false (fun () -> handlers.removeCategory r.categoryId) ]
        let row =
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 6.0
                StackPanel.margin (Thickness(0.0, 0.0, 0.0, 4.0))
                StackPanel.children (
                    [
                        nameBox
                        button (UiIds.rowSaveButton r.categoryId) "Save" false (fun () -> handlers.saveCategory r.categoryId)
                        button (UiIds.rowCancelButton r.categoryId) "Cancel" false (fun () -> handlers.cancelCategory r.categoryId)
                    ]
                    @ removeVerb)
            ]
            // Keyed by the category id (`View.withKey`, spec 0038): adding / removing a row recreates
            // the rows that shift slots instead of patching one row's controls into another's.
            // Fully qualified: `Avalonia.FuncUI.Types` (opened above for `IView`) also exports a
            // `View<'t>` type, so the bare `View` name would be ambiguous with the DSL `View` module.
            |> Avalonia.FuncUI.DSL.View.withKey r.categoryId
        row :> IView

    /// The category list: a NAMED vertical stack of the host's rows, rendered as given.
    let private listView (state : State) (handlers : Handlers) : IView =
        ScrollViewer.create [
            ScrollViewer.maxHeight 260.0
            ScrollViewer.content (
                StackPanel.create [
                    StackPanel.name UiIds.list
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children (state.rows |> List.map (rowView handlers))
                ])
        ] :> IView

    /// The inline block-message slot — the store's typed refusal (`CategoryStillReferenced` /
    /// `BuiltInNotRemovable`), shown only while a message is present (the `LibraryControls.confirmPanel`
    /// precedent — no message means the control is absent, not an empty box).
    let private blockMessagePanel (state : State) : IView list =
        if state.blockMessage = "" then []
        else
            [
                Border.create [
                    Border.background (brush blockBackground)
                    Border.borderBrush (brush idleBorder)
                    Border.borderThickness 1.0
                    Border.cornerRadius (CornerRadius 3.0)
                    Border.padding (Thickness 8.0)
                    Border.margin (Thickness(0.0, 2.0, 0.0, 2.0))
                    Border.child (
                        TextBlock.create [
                            TextBlock.name UiIds.blockMessage
                            TextBlock.text state.blockMessage
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.maxWidth 360.0
                        ])
                ] :> IView
            ]

    /// The Category-manager surface — an Add verb, the editable category list, and (when the store
    /// refuses an edit) the inline block-message slot, top to bottom.
    let view (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (
                [
                    button UiIds.addButton "Add category" true (fun () -> handlers.addCategory ())
                    listView state handlers
                ]
                @ blockMessagePanel state)
        ] :> IView
