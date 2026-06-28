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
/// user picks one (the action), which the host binds to the element's `valueId`. A readout shows the
/// currently-bound entry. This control is DOMAIN-FREE (matching `RendererControls` /
/// `ElementPaletteControls`): the host pre-flattens the chosen grouping tree into `Row`s and injects
/// the behaviour as a `Handlers` function record (the functional-proxy seam; a test substitutes stubs).
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
        }

    let empty : State =
        {
            rows = []
            kindLabel = ""
            boundName = None
            enabled = false
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs).
    type Handlers =
        {
            /// Bind this entry id to the selected element's `valueId`.
            chooseEntry : string -> unit
        }

    /// Stable automation ids (CLAUDE.md UI guidance).
    [<RequireQualifiedAccess>]
    module UiIds =
        let readout = "LibraryBoundReadout"
        let kindLabel = "LibraryKindLabel"
        let tree = "LibraryTree"
        /// A leaf row's clickable id — the entry id, prefixed so it cannot collide with other ids.
        let entry (entryId : string) : string = "LibraryEntry_" + entryId

    // -- The button look, identical to the other bars' idle button (so they MATCH). --
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private boundBackground = color 150 185 235
    let private idleBorder = color 120 120 120

    /// A clickable leaf row (a styled, named Border), highlighted when it is the bound entry.
    let private leafRow (id : string) (label : string) (depth : int) (isBound : bool) (enabled : bool) (onClick : unit -> unit) : IView =
        Border.create [
            Border.name id
            Border.isEnabled enabled
            Border.opacity (if enabled then 1.0 else 0.4)
            Border.background (brush (if isBound then boundBackground else idleBackground))
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 4.0))
            Border.margin (Thickness(float (12 * depth), 0.0, 6.0, 4.0))
            Border.horizontalAlignment HorizontalAlignment.Left
            Border.child (TextBlock.create [ TextBlock.text label ])
            // `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the
            // label changes (matching the other bars) so a reused row can't keep a stale handler.
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box label))
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
        else leafRow (UiIds.entry r.entryId) r.label r.depth r.isBound state.enabled (fun () -> handlers.chooseEntry r.entryId)

    /// The Library bay — the constrained-kind label, the bound-entry readout, and the kind-constrained
    /// tree of selectable entries (a compact self-contained unit, a Bay's content).
    let view (state : State) (handlers : Handlers) : IView =
        let kindText = if state.kindLabel = "" then "Showing: (select an element)" else sprintf "Showing: %s" state.kindLabel
        let boundText =
            match state.boundName with
            | Some name -> sprintf "Bound: %s" name
            | None -> "Bound: (none)"
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children [
                TextBlock.create [ TextBlock.name UiIds.kindLabel; TextBlock.text kindText ]
                TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.text boundText ]
                ScrollViewer.create [
                    ScrollViewer.maxHeight 220.0
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.name UiIds.tree
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children (state.rows |> List.map (rowView state handlers))
                        ])
                ]
            ]
        ] :> IView
