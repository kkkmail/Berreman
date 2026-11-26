/// The grid generator's panel between the slot bar and the table: each slot's items and R1 angles to try, the
/// combinations they make, and Generate beside Close (spec 0050 S8.4; 001 §J "Grid generator", §O).
module OpticalConstructor.Ui.SetupGridView

open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Geometry
open OpticalConstructor.Controls
open OpticalConstructor.Domain

/// A combination's mark in the panel: ticked, unticked, or impossible and why; never stored, so it has no wire tag
/// (spec 0050 S8.4).
type GridCombinationMark =
    | CombinationTicked
    | CombinationUnticked
    | CombinationImpossible of reason : ControlCaption

/// One line of the panel's combinations: the combination, its caption and its mark (spec 0050 S8.4).
type GridCombinationState =
    {
        combination : SetupGrids.GridCombination
        caption : ControlCaption
        mark : GridCombinationMark
    }

/// One slot's row of the panel: the slot and its role, the kit's items that fit it and its draft (spec 0050 S8.4).
type SetupGridSlotState =
    {
        roled : Setups.RoledSlot
        fitting : Kit.Item list
        draft : SetupGrids.GridSlotDraft
    }

/// The panel: the grid Generate sends, a row per slot in light order, its combinations and whether Generate is offered
/// (spec 0050 S8.4).
type SetupGridState =
    {
        grid : SetupGrids.SetupGrid
        slots : SetupGridSlotState list
        rows : GridCombinationState list
        generateAvailability : ControlAvailability
    }

/// What the panel sends: an item or an R1 tried or not in a slot, a combination's tick, Generate and Close (spec 0050
/// S8.4).
type SetupGridHandlers =
    {
        toggleItem : Library.ElementId -> Kit.ItemId -> unit
        addAngle : Library.ElementId -> BarDegrees -> unit
        removeAngle : Library.ElementId -> Angle -> unit
        toggleCombination : SetupGrids.GridCombination -> unit
        generateGrid : SetupGrids.SetupGrid -> unit
        closeGrid : unit -> unit
    }

/// The panel over `state`: its heading, a row per slot of its fitting items, its R1 field and its tried angles, the
/// combinations, and Generate while offered beside Close (spec 0050 S8.4; .spec-md §10.5).
let setupGridView (state : SetupGridState) (handlers : SetupGridHandlers) : IView =
    let slotRow (slot : SetupGridSlotState) : IView =
        let slotId = slot.roled.slot.id
        let role = slot.roled.role
        let itemBox (item : Kit.Item) : IView =
            let id = ConstructorUiId.setupGridItemChoice role item.id
            let toggle () : unit = handlers.toggleItem slotId item.id
            if List.contains item.id slot.draft.items then
                MaterialsWindowView.actionButton id item.label.caption MaterialsWindowView.positiveBackground toggle
            else
                MaterialsWindowView.verbButton id item.label.caption toggle
        // The rotation bar's field rule (`RotationControls.field`): read on Enter or blur, an unreadable text left as typed.
        let commitAngle (source : obj) : unit =
            match source with
            | :? TextBox as field ->
                match Option.ofObj field.Text |> Option.bind BarDegrees.tryCreate with
                | Some degrees ->
                    handlers.addAngle slotId degrees
                    field.Text <- ""
                | None -> ()
            | _ -> ()
        let angleField : IView =
            TextBox.create [
                (ConstructorUiId.setupGridAngleField role).automationAttribute<TextBox> ()
                TextBox.width 60.0
                TextBox.margin (Thickness(0.0, 0.0, 8.0, 4.0))
                TextBox.verticalAlignment VerticalAlignment.Center
                TextBox.onKeyDown ((fun e -> if e.Key = Key.Enter then commitAngle e.Source), SubPatchOptions.Always)
                TextBox.onLostFocus ((fun e -> commitAngle e.Source), SubPatchOptions.Always)
            ] :> IView
        let angleBox (angle : Angle) : IView =
            MaterialsWindowView.verbButton
                (ConstructorUiId.setupGridAngle role angle)
                (ControlCaption $"{angle.degreeCaption.value} ×")
                (fun () -> handlers.removeAngle slotId angle)
        let angleLabel : IView =
            TextBlock.create [
                TextBlock.margin (Thickness(4.0, 0.0, 6.0, 4.0))
                TextBlock.verticalAlignment VerticalAlignment.Center
                TextBlock.text "R1 (°):"
            ] :> IView
        // A held item's `actionButton` keeps the slot it was drawn for, so a role meeting another slot recreates the row.
        DockPanel.create [
            DockPanel.margin (Thickness(0.0, 2.0, 0.0, 2.0))
            DockPanel.children [
                TextBlock.create [
                    TextBlock.dock Dock.Left
                    TextBlock.minWidth 90.0
                    TextBlock.margin (Thickness(0.0, 5.0, 6.0, 0.0))
                    TextBlock.verticalAlignment VerticalAlignment.Top
                    TextBlock.text $"{role.text}:"
                ]
                WrapPanel.create [
                    WrapPanel.orientation Orientation.Horizontal
                    WrapPanel.children ((slot.fitting |> List.map itemBox) @ [ angleLabel; angleField ] @ (slot.draft.angles |> List.map angleBox))
                ]
            ]
        ]
        |> View.withKey $"SetupGridSlot_{slotId.value}"
        :> IView
    let combinationLine (index : int) (row : GridCombinationState) : IView =
        let id = ConstructorUiId.setupGridCombination (ConstructorUiPosition (index + 1))
        let toggle () : unit = handlers.toggleCombination row.combination
        match row.mark with
        | CombinationTicked -> MaterialsWindowView.verbButton id (ControlCaption $"☑ {row.caption.value}") toggle
        | CombinationUnticked -> MaterialsWindowView.verbButton id (ControlCaption $"☐ {row.caption.value}") toggle
        | CombinationImpossible reason ->
            TextBlock.create [
                id.automationAttribute<TextBlock> ()
                TextBlock.margin (Thickness(0.0, 2.0, 0.0, 6.0))
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.foreground (MaterialsWindowView.brush MaterialsWindowView.messageColor)
                TextBlock.text $"✗ {row.caption.value} — {reason.value}"
            ] :> IView
    let generate : IView list =
        match state.generateAvailability with
        | ControlOffered ->
            [
                MaterialsWindowView.actionButton
                    ConstructorUiId.setupGridGenerateButton
                    (ControlCaption "Generate")
                    MaterialsWindowView.positiveBackground
                    (fun () -> handlers.generateGrid state.grid)
            ]
        | ControlWithheld -> []
    let close =
        MaterialsWindowView.actionButton
            ConstructorUiId.setupGridCloseButton
            (ControlCaption "Close")
            MaterialsWindowView.negativeBackground
            handlers.closeGrid
    // `actionButton` subscribes its press once per id, so the row is keyed by the combinations Generate makes.
    let actionsKey : string =
        let pickKey (pick : SetupGrids.SlotPick) : string =
            let item =
                match pick.choice.itemOpt with
                | Some held -> string held.value
                | None -> "-"
            let degrees = pick.choice.r1.degrees.ToString("R", CultureInfo.InvariantCulture)
            $"{pick.slot.value}={item}@{degrees}"
        let combinations =
            state.grid.ticked |> List.map (fun combination -> combination.picks |> List.map pickKey |> String.concat ",")
        $"""SetupGridActions_{String.concat ";" combinations}"""
    Border.create [
        ConstructorUiId.setupGridPanel.automationAttribute<Border> ()
        Border.margin (Thickness(8.0, 0.0, 8.0, 4.0))
        Border.child (
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.children (
                    [
                        TextBlock.create [ TextBlock.fontWeight FontWeight.SemiBold; TextBlock.text "Setup grid" ] :> IView
                        TextBlock.create [
                            TextBlock.margin (Thickness(0.0, 2.0, 0.0, 4.0))
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.text
                                "Choose the items and the R1 angles to try in each slot. Every ticked combination becomes a setup of the kit, with an experiment of the draft over each."
                        ]
                        :> IView
                    ]
                    @ (state.slots |> List.map slotRow)
                    @ [
                        TextBlock.create [ TextBlock.margin (Thickness(0.0, 4.0, 0.0, 2.0)); TextBlock.text "Combinations:" ] :> IView
                        StackPanel.create [
                            ConstructorUiId.setupGridCombinationList.automationAttribute<StackPanel> ()
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children (state.rows |> List.mapi combinationLine)
                        ]
                        :> IView
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.margin (Thickness(0.0, 4.0, 0.0, 0.0))
                            StackPanel.children (generate @ [ close ])
                        ]
                        |> View.withKey actionsKey
                        :> IView
                    ])
            ])
    ] :> IView
