/// The slot bar above the table: one chip per slot in light order, the open chip's fitting items and Record experiment
/// (spec 0050 S8.4; 001 §J "Quick swap", §O).
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

/// The bar: its chips in light order, the slot whose list is open and whether Record experiment is offered (spec 0050
/// S8.4).
type SlotBarState =
    {
        chips : SlotChipState list
        openOpt : Library.ElementId option
        recordAvailability : ControlAvailability
        /// The note Record experiment names its experiment with (spec 0051 `.manual/003` 3.9).
        recordNote : ControlCaption
    }

/// What the bar sends: a chip's toggle, a listed item's swap, a letter typed on a chip, Record experiment and Setup grid…
/// (spec 0050 S8.4).
type SlotBarHandlers =
    {
        toggleChip : Library.ElementId -> unit
        swapSlot : Library.ElementId -> Kit.ItemId -> unit
        pickByLetter : Library.ElementId -> Setups.PickLetter -> unit
        recordExperiment : unit -> unit
        setRecordNote : ControlCaption -> unit
        openSetupGrid : unit -> unit
    }

/// The bar over `state`: its chips, then Record experiment while offered and Setup grid…, on a wrapping row, and the open
/// chip's fitting items below. A chip is a tab stop, so Tab walks the chips in light order, and a letter typed on one
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
    let recordVerb : IView list =
        match state.recordAvailability with
        | ControlOffered ->
            [
                TextBox.create [
                    yield! ConstructorUiId.recordNoteField.inputAttributes<TextBox> ()
                    TextBox.placeHolderText "Note for the name"
                    TextBox.width 160.0
                    TextBox.margin (Thickness(0.0, 0.0, 6.0, 4.0))
                    TextBox.text state.recordNote.value
                    TextBox.onTextChanged (Option.ofObj >> Option.map ControlCaption >> Option.iter handlers.setRecordNote)
                ]
                :> IView
                CommandControls.verbCommand ConstructorUiId.recordExperimentButton (ControlCaption "Record experiment") handlers.recordExperiment
            ]
        | ControlWithheld -> []
    // The grid's edits write nothing, so a read-only kit is offered it too (spec 0050 S8.4).
    let gridVerb : IView =
        CommandControls.verbCommand ConstructorUiId.setupGridOpenButton (ControlCaption "Setup grid…") handlers.openSetupGrid
    let choiceList (chip : SlotChipState) : IView =
        let slotId = chip.roled.slot.id
        let choiceBox (item : Kit.Item) : IView =
            let id = ConstructorUiId.slotChipChoice chip.roled.role item.id
            match chip.roled.slot.content.itemOpt with
            | Some held when held = item.id ->
                CommandControls.choiceCommand id item.label.caption ChosenChoice (fun () -> handlers.toggleChip slotId)
            | Some _
            | None -> CommandControls.choiceCommand id item.label.caption UnchosenChoice (fun () -> handlers.swapSlot slotId item.id)
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
                WrapPanel.children ((state.chips |> List.map chipBox) @ recordVerb @ [ gridVerb ])
            ]
            :> IView
            :: openList)
    ] :> IView
