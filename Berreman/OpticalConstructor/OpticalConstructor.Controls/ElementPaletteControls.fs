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

    type ElementPaletteControlsState =
        {
            /// The addable kinds, in display order (each becomes a "+ label" button).
            addItems : AddItem list
            /// Whether "Remove selected" is enabled (i.e. an element is currently selected).
            removeAvailability : ControlAvailability
        }

    type ElementPaletteControlsHandlers =
        {
            /// Add the entry with this id (the host maps the id back to its catalogue kind).
            add : ConstructorUiKey -> unit
            removeSelected : unit -> unit
        }

    /// A palette button, keyed by its id so a membership change recreates it at a shifted slot.
    let private clickBox (id : ConstructorUiId) (caption : ControlCaption) (availability : ControlAvailability) (onClick : unit -> unit) : IView =
        CommandControls.commandButton id caption NeutralCommand availability onClick
            [ Button.padding (Thickness(10.0, 5.0)); Button.margin (Thickness(0.0, 0.0, 6.0, 6.0)) ]

    /// The palette bar — a wrapping row of "+ label" add buttons followed by "Remove selected".
    let elementPaletteControlsView (state : ElementPaletteControlsState) (handlers : ElementPaletteControlsHandlers) : IView =
        let addButtons =
            state.addItems
            |> List.map (fun item -> clickBox (ConstructorUiId.paletteAddButton item.id) (ControlCaption $"+ {item.label.value}") ControlOffered (fun () -> handlers.add item.id))
        let removeButton =
            clickBox ConstructorUiId.paletteRemoveSelectedButton (ControlCaption "Remove selected") state.removeAvailability (fun () -> handlers.removeSelected ())
        WrapPanel.create [
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children (addButtons @ [ removeButton ])
        ] :> IView
