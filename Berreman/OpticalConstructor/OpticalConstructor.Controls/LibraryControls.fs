namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable LIBRARY control (Spec 0027, task 024 — the "Library" bay). The user selects a table
/// element; the Library constrains by the element's kind and shows only the entries valid for it; the
/// user picks one. Selection is CONFIRM-GATED (spec 0027 / 026): clicking an entry makes it PENDING and
/// shows its FULL description (e.g. a multilayer's materials + thicknesses) so the user knows what they
/// are binding; a Confirm action commits the bind (sets the element's `valueId`), a Cancel clears the
/// pending choice. A readout shows the currently-bound entry. This control is DOMAIN-FREE (matching
/// `RendererControls` / `ElementPaletteControls`): the host pre-flattens the chosen grouping tree into
/// `Row`s, supplies the pending entry's name + full description, and injects the behaviour as a `Handlers`
/// function record (the functional-proxy seam; a test substitutes stubs).
module LibraryControls =

    /// How deep a row sits in the tree (0 = the top level); it drives the row's indent.
    type LibraryRowDepth =
        | LibraryRowDepth of int

        member this.value = let (LibraryRowDepth d) = this in d

        /// The row's left indent, in device-independent pixels.
        member this.indent : float = float (12 * this.value)

    /// Whether a leaf row's entry is the selected element's current `valueId`.
    type LibraryEntryBinding =
        | EntryBound
        | EntryUnbound

    /// What a row is: a non-selectable group header, or a selectable entry (its id, as the host keys it).
    type LibraryRowKind =
        | LibraryGroupHeader
        | LibraryEntryLeaf of entry : ConstructorUiKey * binding : LibraryEntryBinding

    /// One row the host pre-flattens from a `LibraryTree` (Controls carries no domain types).
    type Row =
        {
            label : ControlCaption
            depth : LibraryRowDepth
            kind : LibraryRowKind
        }

    /// The bay's pure, serializable state.
    type LibraryControlsState =
        {
            /// The chosen tree representation, flattened to selectable leaf rows + group headers.
            rows : Row list
            /// The constrained kind label ("Sample" / "Detector" / …), or `None` when nothing bindable.
            kindLabel : ControlCaption option
            /// The bound entry's display name for the current selection (the readout), if any.
            boundName : ControlCaption option
            /// `ControlWithheld` when nothing (or a non-bindable thing) is selected.
            availability : ControlAvailability
            /// Spec 0027 (026): the selected-but-not-yet-confirmed entry id (the pending choice), if any.
            pendingEntryId : ConstructorUiKey option
            /// Spec 0027 (026): the pending entry's display name (shown in the confirm panel), if any.
            pendingName : ControlCaption option
            /// Spec 0027 (026): the pending entry's FULL description (shown before the user confirms).
            pendingDescription : ControlCaption
        }

    let empty : LibraryControlsState =
        {
            rows = []
            kindLabel = None
            boundName = None
            availability = ControlWithheld
            pendingEntryId = None
            pendingName = None
            pendingDescription = ControlCaption ""
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs).
    type LibraryControlsHandlers =
        {
            /// Spec 0027 (026): SELECT this entry id as the pending choice (does NOT bind yet — it shows
            /// the entry's full description and offers Confirm / Cancel).
            selectEntry : ConstructorUiKey -> unit
            /// Spec 0027 (026): COMMIT the pending choice — bind it to the selected element's `valueId`.
            confirmEntry : unit -> unit
            /// Spec 0027 (026): CLEAR the pending choice without binding.
            cancelEntry : unit -> unit
        }

    // -- The button look, identical to the other bars' idle button (so they MATCH). --
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
    let private boundBackground = Color.FromRgb(150uy, 185uy, 235uy)
    let private pendingBackground = Color.FromRgb(255uy, 224uy, 160uy)
    let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)

    /// How a leaf row is currently highlighted: the confirmed bound entry, the pending (selected-not-yet-
    /// confirmed) entry, or neither.
    type private RowHighlight =
        | RowBound
        | RowPending
        | RowPlain

    /// A clickable leaf row, highlighted when it is the bound or pending entry. Its id is an AutomationId,
    /// not a `Name`: the kind-constrained list's membership changes with the selection, so FuncUI can shift a
    /// sibling onto a reused Border, and Avalonia forbids renaming a styled control (the `MaterialsControls`
    /// precedent). Keyed by its id, so a membership shift recreates the box instead of patching another's.
    let private leafRow (id : ConstructorUiId) (label : ControlCaption) (depth : LibraryRowDepth) (highlight : RowHighlight) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
        let background =
            match highlight with
            | RowBound -> boundBackground
            | RowPending -> pendingBackground
            | RowPlain -> idleBackground
        let row =
            Border.create [
                id.automationAttribute<Border> ()
                Border.isEnabled availability.isEnabled
                Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
                Border.background (brush background)
                Border.borderBrush (brush idleBorder)
                Border.borderThickness 1.0
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(10.0, 4.0))
                Border.margin (Thickness(depth.indent, 0.0, 6.0, 4.0))
                Border.horizontalAlignment HorizontalAlignment.Left
                Border.child (TextBlock.create [ TextBlock.text label.value ])
                // `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the
                // highlight changes (matching the other bars) so a reused row can't keep a stale handler.
                Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (label.value, highlight)))
            ]
            // Fully qualified: `Avalonia.FuncUI.Types` (opened above for `IView`) also exports a
            // `View<'t>` type, so the bare `View` name would be ambiguous with the DSL `View` module.
            |> Avalonia.FuncUI.DSL.View.withKey id.value
        row :> IView

    /// A non-selectable group-header row (a plain, indented label — no id, no click), keyed by its
    /// label + depth so membership shifts never patch one header's slot into another's.
    let private headerRow (label : ControlCaption) (depth : LibraryRowDepth) : IView =
        let header =
            TextBlock.create [
                TextBlock.text label.value
                TextBlock.fontWeight FontWeight.SemiBold
                TextBlock.margin (Thickness(depth.indent, 4.0, 0.0, 2.0))
            ]
            |> Avalonia.FuncUI.DSL.View.withKey $"LibraryHeader_{depth.value}_{label.value}"
        header :> IView

    let private rowView (state : LibraryControlsState) (handlers : LibraryControlsHandlers) (r : Row) : IView =
        match r.kind with
        | LibraryGroupHeader -> headerRow r.label r.depth
        | LibraryEntryLeaf (entry, binding) ->
            let highlight =
                match binding with
                | _ when state.pendingEntryId = Some entry -> RowPending
                | EntryBound -> RowBound
                | EntryUnbound -> RowPlain
            leafRow (ConstructorUiId.libraryEntry entry) r.label r.depth highlight state.availability (fun () -> handlers.selectEntry entry)

    /// Whether an action button carries the bound-entry accent (Confirm) or the idle look (Cancel).
    type private ActionEmphasis =
        | AccentAction
        | PlainAction

    // -- The Confirm / Cancel action buttons of the pending bind. The panel they live in comes and
    // goes with the pending choice, so they are regenerable: a reuse-safe AutomationId (never a
    // write-once `Name`), keyed by that id.
    let private actionButton (id : ConstructorUiId) (caption : ControlCaption) (emphasis : ActionEmphasis) (onClick : unit -> unit) : IView =
        let background =
            match emphasis with
            | AccentAction -> boundBackground
            | PlainAction -> idleBackground
        let button =
            Border.create [
                id.automationAttribute<Border> ()
                Border.background (brush background)
                Border.borderBrush (brush idleBorder)
                Border.borderThickness 1.0
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(12.0, 5.0))
                Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text caption.value ])
                Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id.value, caption.value)))
            ]
            |> Avalonia.FuncUI.DSL.View.withKey id.value
        button :> IView

    /// The pending-bind confirm panel: the pending entry's name, its FULL description, and Confirm / Cancel
    /// (shown only while an entry is pending — i.e. selected but not yet committed).
    let private confirmPanel (state : LibraryControlsState) (handlers : LibraryControlsHandlers) : IView list =
        match state.pendingEntryId, state.pendingName with
        | Some _, Some name ->
            [
                Border.create [
                    Border.background (brush (Color.FromRgb(250uy, 246uy, 235uy)))
                    Border.borderBrush (brush idleBorder)
                    Border.borderThickness 1.0
                    Border.cornerRadius (CornerRadius 3.0)
                    Border.padding (Thickness 8.0)
                    Border.margin (Thickness(0.0, 2.0, 0.0, 2.0))
                    Border.child (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.spacing 4.0
                            StackPanel.children [
                                TextBlock.create [ TextBlock.text $"Selected: {name.value}"; TextBlock.fontWeight FontWeight.SemiBold ]
                                TextBlock.create [
                                    TextBlock.name ConstructorUiId.libraryEntryDescription.value
                                    TextBlock.text state.pendingDescription.value
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    TextBlock.maxWidth 360.0
                                ]
                                StackPanel.create [
                                    StackPanel.orientation Orientation.Horizontal
                                    StackPanel.spacing 0.0
                                    StackPanel.children [
                                        actionButton ConstructorUiId.libraryConfirmButton (ControlCaption "Confirm") AccentAction handlers.confirmEntry
                                        actionButton ConstructorUiId.libraryCancelButton (ControlCaption "Cancel") PlainAction handlers.cancelEntry
                                    ]
                                ]
                            ]
                        ])
                ] :> IView
            ]
        | _ -> []

    /// The Library bay — the constrained-kind label, the bound-entry readout, the kind-constrained tree of
    /// selectable entries, and (once an entry is picked) the confirm panel showing its full description.
    let libraryControlsView (state : LibraryControlsState) (handlers : LibraryControlsHandlers) : IView =
        let kindText =
            match state.kindLabel with
            | Some kind -> $"Showing: {kind.value}"
            | None -> "Showing: (select an element)"
        let boundText =
            match state.boundName with
            | Some name -> $"Bound: {name.value}"
            | None -> "Bound: (none)"
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (
                [
                    TextBlock.create [ TextBlock.name ConstructorUiId.libraryKindLabel.value; TextBlock.text kindText ] :> IView
                    TextBlock.create [ TextBlock.name ConstructorUiId.libraryBoundReadout.value; TextBlock.text boundText ] :> IView
                    ScrollViewer.create [
                        ScrollViewer.maxHeight 220.0
                        ScrollViewer.content (
                            StackPanel.create [
                                StackPanel.name ConstructorUiId.libraryTree.value
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children (state.rows |> List.map (rowView state handlers))
                            ])
                    ] :> IView
                ]
                @ confirmPanel state handlers)
        ] :> IView
