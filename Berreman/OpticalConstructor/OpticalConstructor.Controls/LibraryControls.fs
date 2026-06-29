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

    /// One row the host pre-flattens from a `LibraryTree` (Controls carries no domain types). `depth`
    /// drives the indent; `entryId` is "" for a non-selectable group header, the entry id for a leaf;
    /// `isBound` marks the row whose entry is the selected element's current `valueId`.
    type Row =
        {
            label : string
            depth : int
            entryId : string
            isBound : bool
        }

    /// The bay's pure, serializable state.
    type State =
        {
            /// The chosen tree representation, flattened to selectable leaf rows + group headers.
            rows : Row list
            /// The constrained kind label ("Sample" / "Detector" / …), or "" when nothing bindable.
            kindLabel : string
            /// The bound entry's display name for the current selection (the readout), if any.
            boundName : string option
            /// `false` when nothing (or a non-bindable thing) is selected.
            enabled : bool
            /// Spec 0027 (026): the selected-but-not-yet-confirmed entry id (the pending choice), if any.
            pendingEntryId : string option
            /// Spec 0027 (026): the pending entry's display name (shown in the confirm panel), if any.
            pendingName : string option
            /// Spec 0027 (026): the pending entry's FULL description (shown before the user confirms).
            pendingDescription : string
        }

    let empty : State =
        {
            rows = []
            kindLabel = ""
            boundName = None
            enabled = false
            pendingEntryId = None
            pendingName = None
            pendingDescription = ""
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs).
    type Handlers =
        {
            /// Spec 0027 (026): SELECT this entry id as the pending choice (does NOT bind yet — it shows
            /// the entry's full description and offers Confirm / Cancel).
            selectEntry : string -> unit
            /// Spec 0027 (026): COMMIT the pending choice — bind it to the selected element's `valueId`.
            confirmEntry : unit -> unit
            /// Spec 0027 (026): CLEAR the pending choice without binding.
            cancelEntry : unit -> unit
        }

    /// Stable automation ids (CLAUDE.md UI guidance).
    [<RequireQualifiedAccess>]
    module UiIds =
        let readout = "LibraryBoundReadout"
        let kindLabel = "LibraryKindLabel"
        let tree = "LibraryTree"
        /// A leaf row's clickable id — the entry id, prefixed so it cannot collide with other ids.
        let entry (entryId : string) : string = "LibraryEntry_" + entryId
        /// The pending-entry full-description text (shown before confirm).
        let description = "LibraryEntryDescription"
        /// The Confirm / Cancel actions of the pending bind.
        let confirm = "LibraryConfirmButton"
        let cancel = "LibraryCancelButton"

    // -- The button look, identical to the other bars' idle button (so they MATCH). --
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private boundBackground = color 150 185 235
    let private pendingBackground = color 255 224 160
    let private idleBorder = color 120 120 120

    /// How a leaf row is currently highlighted: the confirmed bound entry, the pending (selected-not-yet-
    /// confirmed) entry, or neither.
    type private RowHighlight =
        | RowBound
        | RowPending
        | RowPlain

    /// A clickable leaf row (a styled, named Border), highlighted when it is the bound or pending entry.
    let private leafRow (id : string) (label : string) (depth : int) (highlight : RowHighlight) (enabled : bool) (onClick : unit -> unit) : IView =
        let background =
            match highlight with
            | RowBound -> boundBackground
            | RowPending -> pendingBackground
            | RowPlain -> idleBackground
        Border.create [
            Border.name id
            Border.isEnabled enabled
            Border.opacity (if enabled then 1.0 else 0.4)
            Border.background (brush background)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 4.0))
            Border.margin (Thickness(float (12 * depth), 0.0, 6.0, 4.0))
            Border.horizontalAlignment HorizontalAlignment.Left
            Border.child (TextBlock.create [ TextBlock.text label ])
            // `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the
            // highlight changes (matching the other bars) so a reused row can't keep a stale handler.
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (label, highlight)))
        ] :> IView

    /// A non-selectable group-header row (a plain, indented label — no id, no click).
    let private headerRow (label : string) (depth : int) : IView =
        TextBlock.create [
            TextBlock.text label
            TextBlock.fontWeight FontWeight.SemiBold
            TextBlock.margin (Thickness(float (12 * depth), 4.0, 0.0, 2.0))
        ] :> IView

    let private rowView (state : State) (handlers : Handlers) (r : Row) : IView =
        if r.entryId = "" then headerRow r.label r.depth
        else
            let highlight =
                if state.pendingEntryId = Some r.entryId then RowPending
                elif r.isBound then RowBound
                else RowPlain
            leafRow (UiIds.entry r.entryId) r.label r.depth highlight state.enabled (fun () -> handlers.selectEntry r.entryId)

    // -- The Confirm / Cancel action buttons of the pending bind. --
    let private actionButton (id : string) (label : string) (accent : bool) (onClick : unit -> unit) : IView =
        Border.create [
            Border.name id
            Border.background (brush (if accent then boundBackground else idleBackground))
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(12.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 0.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box (id, label)))
        ] :> IView

    /// The pending-bind confirm panel: the pending entry's name, its FULL description, and Confirm / Cancel
    /// (shown only while an entry is pending — i.e. selected but not yet committed).
    let private confirmPanel (state : State) (handlers : Handlers) : IView list =
        match state.pendingEntryId, state.pendingName with
        | Some _, Some name ->
            [
                Border.create [
                    Border.background (brush (color 250 246 235))
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
                                TextBlock.create [ TextBlock.text (sprintf "Selected: %s" name); TextBlock.fontWeight FontWeight.SemiBold ]
                                TextBlock.create [
                                    TextBlock.name UiIds.description
                                    TextBlock.text state.pendingDescription
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    TextBlock.maxWidth 360.0
                                ]
                                StackPanel.create [
                                    StackPanel.orientation Orientation.Horizontal
                                    StackPanel.spacing 0.0
                                    StackPanel.children [
                                        actionButton UiIds.confirm "Confirm" true handlers.confirmEntry
                                        actionButton UiIds.cancel "Cancel" false handlers.cancelEntry
                                    ]
                                ]
                            ]
                        ])
                ] :> IView
            ]
        | _ -> []

    /// The Library bay — the constrained-kind label, the bound-entry readout, the kind-constrained tree of
    /// selectable entries, and (once an entry is picked) the confirm panel showing its full description.
    let view (state : State) (handlers : Handlers) : IView =
        let kindText = if state.kindLabel = "" then "Showing: (select an element)" else sprintf "Showing: %s" state.kindLabel
        let boundText =
            match state.boundName with
            | Some name -> sprintf "Bound: %s" name
            | None -> "Bound: (none)"
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (
                [
                    TextBlock.create [ TextBlock.name UiIds.kindLabel; TextBlock.text kindText ] :> IView
                    TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.text boundText ] :> IView
                    ScrollViewer.create [
                        ScrollViewer.maxHeight 220.0
                        ScrollViewer.content (
                            StackPanel.create [
                                StackPanel.name UiIds.tree
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children (state.rows |> List.map (rowView state handlers))
                            ])
                    ] :> IView
                ]
                @ confirmPanel state handlers)
        ] :> IView
