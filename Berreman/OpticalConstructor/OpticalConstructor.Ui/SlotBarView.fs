/// The slot bar above an edited table: one chip per slot in light order and the open chip's fitting items, each putting
/// its item into the slot (spec 0050 S8.4; 0051 `.manual/007` 5.3, D9).
module OpticalConstructor.Ui.SlotBarView

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Primitives
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

/// The bar: its chips in light order and the slot whose list is open (spec 0050 S8.4).
type SlotBarState =
    {
        chips : SlotChipState list
        openOpt : Library.ElementId option
    }

/// What the bar sends: a chip's toggle, a listed item put into its slot, a letter typed on a chip (spec 0050 S8.4).
type SlotBarHandlers =
    {
        toggleChip : Library.ElementId -> unit
        assignSlot : Library.ElementId -> Kit.ItemId -> unit
        pickByLetter : Library.ElementId -> Setups.PickLetter -> unit
    }

/// The bar over `state`: its chips on a wrapping row, and the open chip's fitting items below. A chip is a tab stop, so Tab walks the chips in light order, and a letter typed on one
/// picks (spec 0050 S8.4).
let slotBarView (state : SlotBarState) (handlers : SlotBarHandlers) : IView =
    let chipBox (chip : SlotChipState) : IView =
        let slotId = chip.roled.slot.id
        let id = ConstructorUiId.slotChip chip.roled.role
        let mark =
            match state.openOpt with
            | Some openId when openId = slotId -> ChosenChoice
            | Some _
            | None -> UnchosenChoice
        CommandControls.choiceToggle id (ControlCaption $"{chip.caption.value} ▾") mark PositiveCommand ControlOffered (fun () -> handlers.toggleChip slotId)
            [ ToggleButton.margin (Thickness(0.0, 0.0, 8.0, 4.0))
              ToggleButton.onTextInput (
                  (fun e ->
                      match Option.ofObj e.Text |> Option.bind PickLetter.tryCreate with
                      | Some letter ->
                          e.Handled <- true
                          handlers.pickByLetter slotId letter
                      | None -> ()),
                  SubPatchOptions.Always) ]
    let choiceList (chip : SlotChipState) : IView =
        let slotId = chip.roled.slot.id
        let choiceBox (item : Kit.Item) : IView =
            let id = ConstructorUiId.slotChipChoice chip.roled.role item.id
            match chip.roled.slot.content.itemOpt with
            | Some held when held = item.id ->
                CommandControls.choiceCommand id item.label.caption ChosenChoice (fun () -> handlers.toggleChip slotId)
            | Some _
            | None -> CommandControls.choiceCommand id item.label.caption UnchosenChoice (fun () -> handlers.assignSlot slotId item.id)
        let choices : IView list =
            match chip.fitting with
            | [] -> [ TextBlock.create [ TextBlock.text $"No item of this kit fits {chip.roled.role.text}." ] :> IView ]
            | fitting -> fitting |> List.map choiceBox
        WrapPanel.create [
            yield! ConstructorUiId.slotChipChoiceList.groupAttributes<WrapPanel> ()
            WrapPanel.orientation Orientation.Horizontal
            WrapPanel.children choices
        ] :> IView
    let openList : IView list =
        match state.openOpt with
        | Some openId -> state.chips |> List.filter (fun chip -> chip.roled.slot.id = openId) |> List.map choiceList
        | None -> []
    StackPanel.create [
        yield! ConstructorUiId.slotBar.groupAttributes<StackPanel> ()
        StackPanel.orientation Orientation.Vertical
        StackPanel.margin (Thickness(8.0, 4.0))
        StackPanel.children (
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                WrapPanel.children (state.chips |> List.map chipBox)
            ]
            :> IView
            :: openList)
    ] :> IView
