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

/// A reusable "element palette" bar (Spec 0027, task 010): one "+ label" button per addable element
/// kind plus a "Remove selected" button, styled to MATCH the rotation-controls bar (same button look)
/// rather than the default Avalonia button. Generic — it knows only the labels/ids to show and calls
/// back; the host owns the catalogue meaning and the selection state.
module ElementPaletteControls =

    /// One addable entry: a stable id (used for the button's automation id and the `add` callback) and
    /// the label shown on the button.
    type AddItem =
        {
            id : string
            label : string
        }

    type State =
        {
            /// The addable kinds, in display order (each becomes a "+ label" button).
            addItems : AddItem list
            /// Whether "Remove selected" is enabled (i.e. an element is currently selected).
            canRemove : bool
        }

    type Handlers =
        {
            /// Add the entry with this id (the host maps the id back to its catalogue kind).
            add : string -> unit
            removeSelected : unit -> unit
        }

    // -- The button look, identical to the rotation-controls bar's idle button (so they MATCH). --
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private idleBorder = color 120 120 120

    /// Set `AutomationProperties.AutomationId` (a freely-mutable attached property — unlike
    /// `Control.Name`) through FuncUI's attr builder. The add buttons are GENERATED from the host's
    /// `addItems` (variable membership), so a sibling can shift onto a reused control's slot — Avalonia
    /// forbids renaming a styled control, and an AutomationId survives that reuse (the
    /// `MaterialsControls` precedent).
    let private automationId (autoId : string) : IAttr<Border> =
        AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

    /// A clickable, button-styled box, KEYED by its id so a membership change recreates the box at a
    /// shifted slot instead of patching another entry's styled control in place.
    let private clickBox (id : string) (label : string) (enabled : bool) (onClick : unit -> unit) : IView =
        let button =
            Border.create [
                automationId id
                Border.isEnabled enabled
                Border.opacity (if enabled then 1.0 else 0.4)
                Border.background (brush idleBackground)
                Border.borderBrush (brush idleBorder)
                Border.borderThickness 1.0
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(10.0, 5.0))
                Border.margin (Thickness(0.0, 0.0, 6.0, 6.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text label ])
                // `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the
                // label changes (matching the rotation bar) so a reused button can't keep a stale handler.
                Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box label))
            ]
            // Fully qualified: `Avalonia.FuncUI.Types` (opened above for `IView`) also exports a
            // `View<'t>` type, so the bare `View` name would be ambiguous with the DSL `View` module.
            |> Avalonia.FuncUI.DSL.View.withKey id
        button :> IView

    /// The palette bar — a wrapping row of "+ label" add buttons followed by "Remove selected".
    let view (state : State) (handlers : Handlers) : IView =
        let addButtons =
            state.addItems
            |> List.map (fun item -> clickBox (UiIds.ElementPalette.addButton item.id) ("+ " + item.label) true (fun () -> handlers.add item.id))
        let removeButton =
            clickBox UiIds.ElementPalette.removeSelected "Remove selected" state.canRemove (fun () -> handlers.removeSelected ())
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (addButtons @ [ removeButton ])
        ] :> IView
