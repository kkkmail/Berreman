namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
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

    /// Where a Bay's content is placed once its tab is selected — a two-case content MODE (never a
    /// bool). `InRibbonPane`: the content docks in the ribbon's own content pane, directly below the tab
    /// strip (the small "control" bays — rotation, move, render, …). `FullSurface`: the ribbon shows ONLY
    /// this bay's tab and hands its content to the HOST, which lays it out across the whole surface below
    /// the strip (a workbench-scale bay that would be cramped inside the pane). A third placement later is
    /// a non-breaking DU case, not a new bool.
    type BayContentMode =
        | InRibbonPane
        | FullSurface

    /// A bay's display name: the text of its tab, and what the ribbon selects it by.
    type BayName =
        | BayName of string

        member this.value = let (BayName n) = this in n

        /// The name as the key of the bay's tab and pane ids.
        member this.uiKey : ConstructorUiKey = ConstructorUiKey this.value

        /// The name as its tab shows it.
        member this.toControlCaption : ControlCaption = ControlCaption this.value

    /// One large control docked in the ribbon: a display `name` (shown in the top strip), its already
    /// bound `content` view, and the `mode` that decides whether the ribbon hosts that content in its own
    /// content pane or hands it to the host to fill the surface below the tab strip.
    type Bay =
        {
            name : BayName
            content : IView
            mode : BayContentMode
        }

    /// The ribbon's state: the bays to offer and which one is selected (by name).
    type RibbonState =
        {
            bays : Bay list
            selected : BayName
        }

        /// The bay currently shown — the selected one, falling back to the first bay if the selected name is
        /// not present (so the ribbon always renders something while bays are added / removed).
        member this.activeBay : Bay option =
            match this.bays |> List.tryFind (fun b -> b.name = this.selected) with
            | Some b -> Some b
            | None -> List.tryHead this.bays

    /// Whether a tab is the selected bay's.
    type private BayTabState =
        | BayTabActive
        | BayTabIdle

    /// One ribbon tab, a choice lit with the accent and bold text when it is the selected bay. The bottom margin keeps a
    /// WRAPPED second row of tabs off the first.
    let private tab (name : BayName) (state : BayTabState) (onSelect : BayName -> unit) : IView =
        let mark, weight =
            match state with
            | BayTabActive -> ChosenChoice, FontWeight.Bold
            | BayTabIdle -> UnchosenChoice, FontWeight.Normal
        CommandControls.choiceToggle (ConstructorUiId.ribbonTab name.uiKey) name.toControlCaption mark ArmedCommand ControlOffered (fun () -> onSelect name)
            [ ToggleButton.margin (Thickness(0.0, 0.0, 4.0, 4.0)); ToggleButton.fontWeight weight ]

    /// The ribbon — a top tab strip of bay names, then (for an IN-PANE bay) the ACTIVE bay's content in ONE
    /// keyed slot. Only the selected bay's content is realized (no hidden panes): the single slot is KEYED by
    /// the active bay name (`View.withKey` → FuncUI's `IView.ViewKey`), so when the active bay changes FuncUI
    /// CREATES a fresh pane rather than patching the previous bay's control in place. That is what stops FuncUI
    /// from recycling a styled, named control from a DIFFERENT bay into this slot ("Cannot set Name : styled
    /// element already styled"): the diff sees the ViewKey change, treats the pane as new, and disposes the old
    /// one — so a pane is only ever patched against its own previous self. A `FullSurface` bay renders NO
    /// in-ribbon pane: the ribbon shows only its tab and the HOST fills the surface below this strip with the
    /// bay's content. `onSelect` is the host's "show this bay" seam.
    /// The ribbon with `trailing` after its tabs on the wrapping tab strip, so they wrap with the tabs and never leave the
    /// window (spec 0051 `.manual/003` 4.3).
    let ribbonViewWith (trailing : IView list) (state : RibbonState) (onSelect : BayName -> unit) : IView =
        let activeName = state.activeBay |> Option.map (fun b -> b.name)
        let tabs =
            state.bays
            |> List.map (fun b -> tab b.name (if activeName = Some b.name then BayTabActive else BayTabIdle) onSelect)
        // The active-bay content region. An IN-PANE bay renders its content in ONE keyed slot, KEYED by the
        // active bay name so a bay change recreates the pane instead of recycling a styled, named control
        // across two DIFFERENT bays (`ConstructorUiId.ribbonPane` names this slot). A FULL-SURFACE bay renders no pane here —
        // the host places its content below the whole strip — so the content region stays empty for it (and
        // for the no-bays case, keeping exactly one realized pane, never two).
        let paneChildren : IView list =
            match state.activeBay with
            | Some b ->
                match b.mode with
                | InRibbonPane ->
                    let pane =
                        Border.create [
                            yield! (ConstructorUiId.ribbonPane b.name.uiKey).groupAttributes<Border> ()
                            Border.padding (Thickness(0.0, 4.0, 0.0, 0.0))
                            Border.child b.content
                        ]
                        // Fully qualified: `Avalonia.FuncUI.Types` (opened above for `IView`) also exports a
                        // `View<'t>` type, so the bare `View` name would be ambiguous with the DSL `View` module.
                        |> Avalonia.FuncUI.DSL.View.withKey b.name.value
                    [ pane :> IView ]
                | FullSurface -> []
            | None -> []
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 6.0
            StackPanel.margin (Thickness 8.0)
            StackPanel.children [
                // WRAP the tab strip: a plain horizontal StackPanel lays all bay tabs on one row and lets
                // them overflow past the window's right edge, so a tab beyond the edge is present but
                // un-clickable (its centre falls outside the window). A WrapPanel flows the tabs onto a
                // second row when they do not fit, keeping every bay tab on-screen and reachable — which
                // the reorder (Materials / Library LAST) needs, since nine bays overflow a narrow window.
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children (tabs @ trailing)
                ]
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.children paneChildren
                ]
            ]
        ] :> IView

    /// The ribbon with nothing after its tabs.
    let ribbonView (state : RibbonState) (onSelect : BayName -> unit) : IView = ribbonViewWith [] state onSelect

