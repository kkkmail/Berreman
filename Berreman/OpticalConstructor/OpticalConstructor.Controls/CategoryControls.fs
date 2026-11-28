namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable CATEGORY-MANAGER control (Spec 0035, step 005 — UICOMP_XDUO_0005): the list surface
/// the Category manager projects the slice-004 `CategoryEditor` onto. A category list with an inline
/// name box per row and Add / Rename(save) / Remove verbs; a BUILT-IN row's name stays editable but
/// its Remove verb is OMITTED (removed, not greyed), matching the `MaterialsControls` view-only-Edit
/// precedent; an inline block-message slot surfaces the store's typed refusals (`CategoryStillReferencedErr`
/// / `BuiltInNotRemovableErr`). This control is DOMAIN-FREE (matching `MaterialsControls`):
/// the host flattens the editor's working `MaterialCategory` rows into `Row`s and injects the behaviour
/// as a `Handlers` function record (the functional-proxy seam; a test substitutes stubs). The control
/// renders the rows it is given and never touches a proxy (the host flattens and dispatches).
module CategoryControls =

    /// Where a listed category comes from. A built-in row's name box stays editable, but its Remove
    /// verb is removed: only a user category may be deleted (the store enforces the actual block).
    type CategoryRowOrigin =
        | BuiltInCategoryRow
        | UserCategoryRow

    /// One category row the host pre-flattens from the `CategoryEditor` working rows (Controls carries
    /// no domain types). `categoryId` is the category's stable id, as the host keys it.
    type CategoryControlsRow =
        {
            categoryId : ConstructorUiKey
            name : ControlCaption
            origin : CategoryRowOrigin
        }

    /// The bay's pure, serializable state.
    type CategoryControlsState =
        {
            /// The rows the host flattened from the editor's working set — rendered as given.
            rows : CategoryControlsRow list
            /// The inline block message (`CategoryStillReferencedErr` / `BuiltInNotRemovableErr`), if one is
            /// surfaced — the slot renders only while a message is present.
            blockMessage : ControlCaption option
        }

    let emptyCategoryControls : CategoryControlsState =
        {
            rows = []
            blockMessage = None
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs). The per-row verbs
    /// take the row's category id — the host maps them onto the `CategoryEditor` messages / `commit`.
    type CategoryControlsHandlers =
        {
            /// Begin an inline add (a fresh, blank-named row) — the editor's `BeginAddCategory`.
            addCategory : unit -> unit
            /// The row's inline name box changed — the editor's `SetCategoryName (id, newName)`.
            setCategoryName : ConstructorUiKey -> ControlCaption -> unit
            /// Save (commit) the row's rename.
            saveCategory : ConstructorUiKey -> unit
            /// Remove the row (a `UserCategory` only — the store enforces the actual block).
            removeCategory : ConstructorUiKey -> unit
            /// Cancel the row's in-progress edit.
            cancelCategory : ConstructorUiKey -> unit
        }

    // -- The button look, identical to the other bars' idle/chosen boxes (so they MATCH). --
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private blockBackground = Color.FromRgb(250uy, 235uy, 235uy)

    /// A verb button, chosen-blue when it is the primary verb. Keyed by its id, since a row's verbs change with
    /// its origin (a built-in row omits Remove).
    let private button (id : ConstructorUiId) (label : ControlCaption) (emphasis : CommandEmphasis) (onClick : unit -> unit) : IView =
        CommandControls.commandButton id label emphasis ControlOffered onClick
            [ Button.margin (Thickness(0.0, 0.0, 8.0, 0.0)); Button.horizontalAlignment HorizontalAlignment.Left ]

    /// One category row — an inline name box (editable for EVERY row, including built-ins), a Save
    /// (rename) verb and a Cancel verb, and a Remove verb for a `UserCategory` only (a built-in OMITS
    /// Remove — removed from the row, not greyed).
    let private rowView (handlers : CategoryControlsHandlers) (r : CategoryControlsRow) : IView =
        let nameBox =
            TextBox.create [
                yield! (ConstructorUiId.categoryRowNameBox r.categoryId).inputAttributes<TextBox> ()
                TextBox.width 200.0
                TextBox.text r.name.value
                TextBox.onTextChanged (ControlCaption >> handlers.setCategoryName r.categoryId)
            ] :> IView
        let removeVerb =
            match r.origin with
            | BuiltInCategoryRow -> []
            | UserCategoryRow -> [ button (ConstructorUiId.categoryRowRemoveButton r.categoryId) (ControlCaption "Remove") NeutralCommand (fun () -> handlers.removeCategory r.categoryId) ]
        let row =
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 6.0
                StackPanel.margin (Thickness(0.0, 0.0, 0.0, 4.0))
                StackPanel.children (
                    [
                        nameBox
                        button (ConstructorUiId.categoryRowSaveButton r.categoryId) (ControlCaption "Save") NeutralCommand (fun () -> handlers.saveCategory r.categoryId)
                        button (ConstructorUiId.categoryRowCancelButton r.categoryId) (ControlCaption "Cancel") NeutralCommand (fun () -> handlers.cancelCategory r.categoryId)
                    ]
                    @ removeVerb)
            ]
            // Keyed by the category id (`View.withKey`, spec 0038): adding / removing a row recreates
            // the rows that shift slots instead of patching one row's controls into another's.
            // Fully qualified: `Avalonia.FuncUI.Types` (opened above for `IView`) also exports a
            // `View<'t>` type, so the bare `View` name would be ambiguous with the DSL `View` module.
            |> Avalonia.FuncUI.DSL.View.withKey r.categoryId.value
        row :> IView

    /// The category list: a NAMED vertical stack of the host's rows, rendered as given.
    let private listView (state : CategoryControlsState) (handlers : CategoryControlsHandlers) : IView =
        ScrollViewer.create [
            ScrollViewer.maxHeight 260.0
            ScrollViewer.content (
                StackPanel.create [
                    yield! ConstructorUiId.categoriesList.groupAttributes<StackPanel> ()
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children (state.rows |> List.map (rowView handlers))
                ])
        ] :> IView

    /// The inline block-message slot — the store's typed refusal (`CategoryStillReferencedErr` /
    /// `BuiltInNotRemovableErr`), shown only while a message is present (the `MaterialsControls` confirm
    /// precedent — no message means the control is absent, not an empty box).
    let private blockMessagePanel (state : CategoryControlsState) : IView list =
        match state.blockMessage with
        | None -> []
        | Some message ->
            [
                Border.create [
                    Border.background (brush blockBackground)
                    Border.borderBrush (brush NeutralCommand.outline)
                    Border.borderThickness 1.0
                    Border.cornerRadius (CornerRadius 3.0)
                    Border.padding (Thickness 8.0)
                    Border.margin (Thickness(0.0, 2.0, 0.0, 2.0))
                    Border.child (
                        SelectableTextBlock.create [
                            ConstructorUiId.categoryBlockMessage.automationAttribute<SelectableTextBlock> ()
                            TextBlock.text message.value
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.maxWidth 360.0
                        ])
                ] :> IView
            ]

    /// The Category-manager surface — an Add verb, the editable category list, and (when the store
    /// refuses an edit) the inline block-message slot, top to bottom.
    let categoryControlsView (state : CategoryControlsState) (handlers : CategoryControlsHandlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (
                [
                    button ConstructorUiId.addCategoryButton (ControlCaption "Add category") ChosenCommand handlers.addCategory
                    listView state handlers
                ]
                @ blockMessagePanel state)
        ] :> IView
