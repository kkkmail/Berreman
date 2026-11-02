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
            id : ConstructorUiKey
            label : ControlCaption
        }

    type State =
        {
            /// The addable kinds, in display order (each becomes a "+ label" button).
            addItems : AddItem list
            /// Whether "Remove selected" is enabled (i.e. an element is currently selected).
            removeAvailability : ControlAvailability
        }

    type Handlers =
        {
            /// Add the entry with this id (the host maps the id back to its catalogue kind).
            add : ConstructorUiKey -> unit
            removeSelected : unit -> unit
        }

    // -- The button look, identical to the rotation-controls bar's idle button (so they MATCH). --
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
    let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)

    /// A clickable, button-styled box, KEYED by its id so a membership change recreates the box at a
    /// shifted slot instead of patching another entry's styled control in place. The id is its
    /// `AutomationProperties.AutomationId` (a freely-mutable attached property — unlike `Control.Name`):
    /// the add buttons are GENERATED from the host's `addItems` (variable membership), so a sibling can
    /// shift onto a reused control's slot, and Avalonia forbids renaming a styled control (the
    /// `MaterialsControls` precedent).
    let private clickBox (id : ConstructorUiId) (caption : ControlCaption) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
        let button =
            Border.create [
                id.automationAttribute<Border> ()
                Border.isEnabled availability.isEnabled
                Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
                Border.background (brush idleBackground)
                Border.borderBrush (brush idleBorder)
                Border.borderThickness 1.0
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(10.0, 5.0))
                Border.margin (Thickness(0.0, 0.0, 6.0, 6.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text caption.value ])
                // `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the
                // label changes (matching the rotation bar) so a reused button can't keep a stale handler.
                Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box caption.value))
            ]
            // Fully qualified: `Avalonia.FuncUI.Types` (opened above for `IView`) also exports a
            // `View<'t>` type, so the bare `View` name would be ambiguous with the DSL `View` module.
            |> Avalonia.FuncUI.DSL.View.withKey id.value
        button :> IView

    /// The palette bar — a wrapping row of "+ label" add buttons followed by "Remove selected".
    let view (state : State) (handlers : Handlers) : IView =
        let addButtons =
            state.addItems
            |> List.map (fun item -> clickBox (ConstructorUiId.paletteAddButton item.id) (ControlCaption $"+ {item.label.value}") ControlOffered (fun () -> handlers.add item.id))
        let removeButton =
            clickBox ConstructorUiId.paletteRemoveSelectedButton (ControlCaption "Remove selected") state.removeAvailability (fun () -> handlers.removeSelected ())
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (addButtons @ [ removeButton ])
        ] :> IView
