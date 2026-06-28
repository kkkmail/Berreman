namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The main-screen ribbon (Spec 0027, task 018). A **Bay** is the project's name for a "large control" —
/// a whole, self-contained, named control unit (the rotation bay, the move bay, the element palette, the
/// renderer bay, …) as opposed to a single button. Bays can be added / removed freely, so the Main screen
/// composes whatever set of large controls it wants and any change to a Bay shows up everywhere it is used.
///
/// The `Ribbon` lays the bays out MS-Word-style: a top strip shows the bay NAMES (the "menu"); selecting a
/// name reveals that bay's content below. The ribbon is fully GENERIC — each Bay carries its `content` as
/// an already-built `IView` (bound by the host to its own model / dispatch), so the ribbon knows nothing of
/// any control's state or message types. Like the other controls here, a Bay's behaviour is injected as a
/// record of functions (the control's own `Handlers` — our functional-proxy seam), not baked in.
module Ribbon =

    /// One large control docked in the ribbon: a display `name` (shown in the top strip) and its already
    /// bound `content` view.
    type Bay =
        {
            name : string
            content : IView
        }

    /// The ribbon's state: the bays to offer and which one is selected (by name).
    type State =
        {
            bays : Bay list
            selected : string
        }

    /// Stable automation ids (CLAUDE.md UI guidance).
    [<RequireQualifiedAccess>]
    module UiIds =
        /// The selectable tab for a bay — its name, prefixed so it cannot collide with other ids.
        let tab (name : string) : string = "RibbonTab_" + name
        /// The content pane for a bay (always present; only the selected one is visible).
        let pane (name : string) : string = "RibbonPane_" + name

    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    // The same "selected / idle" treatment the rotation bar uses, so the ribbon tabs match the controls.
    let private activeBackground = color 204 232 255      // #CCE8FF Windows selected/active light accent
    let private activeBorder = color 0 120 212            // #0078D4 Fluent accent outline
    let private idleBackground = color 232 232 232
    let private idleBorder = color 120 120 120
    let private textForeground = color 0 0 0

    /// One ribbon tab — a clickable, button-styled Border that lights up when it is the selected bay.
    let private tab (name : string) (active : bool) (onSelect : string -> unit) : IView =
        Border.create [
            Border.name (UiIds.tab name)
            Border.background (brush (if active then activeBackground else idleBackground))
            Border.borderBrush (brush (if active then activeBorder else idleBorder))
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(12.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 4.0, 0.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (
                TextBlock.create [
                    TextBlock.text name
                    TextBlock.fontWeight (if active then FontWeight.Bold else FontWeight.Normal)
                    TextBlock.foreground (brush textForeground)
                ])
            // `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the active
            // flag flips so the tab never keeps a stale handler.
            Border.onPointerPressed ((fun e -> e.Handled <- true; onSelect name), SubPatchOptions.OnChangeOf (box active))
        ] :> IView

    /// The bay currently shown — the selected one, falling back to the first bay if the selected name is
    /// not present (so the ribbon always renders something while bays are added / removed).
    let activeBay (state : State) : Bay option =
        match state.bays |> List.tryFind (fun b -> b.name = state.selected) with
        | Some b -> Some b
        | None -> List.tryHead state.bays

    /// The ribbon — a top tab strip of bay names, then EVERY bay's content pane with only the selected one
    /// visible. We deliberately keep every pane present (not swap a single content node) because swapping
    /// one node between two DIFFERENT controls makes FuncUI recycle a styled, named control into a
    /// differently-named one ("Cannot set Name : styled element already styled"). With one stable pane per
    /// bay, FuncUI only ever patches a pane against its own previous self, so no recycling across bays.
    /// `onSelect` is the host's "show this bay" seam.
    let view (state : State) (onSelect : string -> unit) : IView =
        let activeName = activeBay state |> Option.map (fun b -> b.name) |> Option.defaultValue ""
        let tabs =
            state.bays
            |> List.map (fun b -> tab b.name (b.name = activeName) onSelect)
        let panes =
            state.bays
            |> List.map (fun b ->
                Border.create [
                    Border.name (UiIds.pane b.name)
                    Border.isVisible (b.name = activeName)
                    Border.padding (Thickness(0.0, 4.0, 0.0, 0.0))
                    Border.child b.content
                ] :> IView)
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 6.0
            StackPanel.margin (Thickness 8.0)
            StackPanel.children [
                StackPanel.create [
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.children tabs
                ]
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children panes
                ]
            ]
        ] :> IView
