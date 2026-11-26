/// The slot bar above the table: one chip per slot in light order, the open chip's fitting items and Record experiment
/// (spec 0050 S8.4; 001 §J "Quick swap", §O).
module OpticalConstructor.Ui.SlotBarView

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Setups

/// One chip: its slot and role, its caption and the kit's items that fit its slot (spec 0050 S8.4).
type SlotChipState =
    {
        roled : Setups.RoledSlot
        caption : ControlCaption
        fitting : Kit.Item list
    }

/// The bar: its chips in light order, the slot whose list is open and whether Record experiment is offered (spec 0050
/// S8.4).
type SlotBarState =
    {
        chips : SlotChipState list
        openOpt : Library.ElementId option
        recordAvailability : ControlAvailability
    }

/// What the bar sends: a chip's toggle, a listed item's swap, a letter typed on a chip, Record experiment and Setup grid…
/// (spec 0050 S8.4).
type SlotBarHandlers =
    {
        toggleChip : Library.ElementId -> unit
        swapSlot : Library.ElementId -> Kit.ItemId -> unit
        pickByLetter : Library.ElementId -> Setups.PickLetter -> unit
        recordExperiment : unit -> unit
        openSetupGrid : unit -> unit
    }

// `MaterialsWindowView.verbButton`'s box look; that module keeps its idle colours private (spec 0050 S8.4).
let private idleBackground : Color = Color.FromRgb(232uy, 232uy, 232uy)
let private idleBorder : Color = Color.FromRgb(120uy, 120uy, 120uy)

/// The bar over `state`: its chips, then Record experiment while offered and Setup grid…, on a wrapping row, and the open
/// chip's fitting items below. A chip is a tab stop, so Tab walks the chips in light order, and a letter typed on one
/// picks (spec 0050 S8.4).
let slotBarView (state : SlotBarState) (handlers : SlotBarHandlers) : IView =
    let chipBox (chip : SlotChipState) : IView =
        let slotId = chip.roled.slot.id
        let id = ConstructorUiId.slotChip chip.roled.role
        let background =
            match state.openOpt with
            | Some openId when openId = slotId -> MaterialsWindowView.positiveBackground
            | Some _
            | None -> idleBackground
        let chipView =
            Border.create [
                id.automationAttribute<Border> ()
                AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.NameProperty, chip.caption.value, ValueNone)
                Border.focusable true
                Border.isTabStop true
                Border.background (MaterialsWindowView.brush background)
                Border.borderBrush (MaterialsWindowView.brush idleBorder)
                Border.borderThickness 1.0
                Border.cornerRadius (CornerRadius 3.0)
                Border.padding (Thickness(12.0, 5.0))
                Border.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (TextBlock.create [ TextBlock.text $"{chip.caption.value} ▾" ])
                Border.onPointerPressed (
                    (fun e ->
                        e.Handled <- true
                        match e.Source with
                        | :? Visual as pressed ->
                            pressed.FindAncestorOfType<Border>(true) |> Option.ofObj |> Option.iter (fun chipBorder -> chipBorder.Focus() |> ignore)
                        | _ -> ()
                        handlers.toggleChip slotId),
                    SubPatchOptions.Always)
                Border.onTextInput (
                    (fun e ->
                        match Option.ofObj e.Text |> Option.bind PickLetter.tryCreate with
                        | Some letter ->
                            e.Handled <- true
                            handlers.pickByLetter slotId letter
                        | None -> ()),
                    SubPatchOptions.Always)
            ]
            |> View.withKey id.value
        chipView :> IView
    let recordVerb : IView list =
        match state.recordAvailability with
        | ControlOffered ->
            [ MaterialsWindowView.verbButton ConstructorUiId.recordExperimentButton (ControlCaption "Record experiment") handlers.recordExperiment ]
        | ControlWithheld -> []
    // The grid's edits write nothing, so a read-only kit is offered it too (spec 0050 S8.4).
    let gridVerb : IView =
        MaterialsWindowView.verbButton ConstructorUiId.setupGridOpenButton (ControlCaption "Setup grid…") handlers.openSetupGrid
    let choiceList (chip : SlotChipState) : IView =
        let slotId = chip.roled.slot.id
        let choiceBox (item : Kit.Item) : IView =
            let id = ConstructorUiId.slotChipChoice chip.roled.role item.id
            match chip.roled.slot.content.itemOpt with
            | Some held when held = item.id ->
                MaterialsWindowView.actionButton id item.label.caption MaterialsWindowView.positiveBackground (fun () -> handlers.toggleChip slotId)
            | Some _
            | None -> MaterialsWindowView.verbButton id item.label.caption (fun () -> handlers.swapSlot slotId item.id)
        let choices : IView list =
            match chip.fitting with
            | [] -> [ TextBlock.create [ TextBlock.text $"No item of this kit fits {chip.roled.role.text}." ] :> IView ]
            | fitting -> fitting |> List.map choiceBox
        WrapPanel.create [
            ConstructorUiId.slotChipChoiceList.automationAttribute<WrapPanel> ()
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children choices
        ] :> IView
    let openList : IView list =
        match state.openOpt with
        | Some openId -> state.chips |> List.filter (fun chip -> chip.roled.slot.id = openId) |> List.map choiceList
        | None -> []
    StackPanel.create [
        ConstructorUiId.slotBar.automationAttribute<StackPanel> ()
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness(8.0, 4.0))
        StackPanel.children (
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children ((state.chips |> List.map chipBox) @ recordVerb @ [ gridVerb ])
            ]
            :> IView
            :: openList)
    ] :> IView
