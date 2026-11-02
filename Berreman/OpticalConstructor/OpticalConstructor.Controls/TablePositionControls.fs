namespace OpticalConstructor.Controls

open System.Globalization
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable FREE table-position bar (spec 0046): one row per table axis — x along the table's length,
/// y across it, z up out of the table plane — each with − / + step buttons, an editable metres field and a
/// Lock / Unlock toggle, plus one Reset. It moves the one element placed freely (the light source; every
/// other element is snapped to its beam and uses `RayPositionControls`). A pure projection of the host
/// `State`; the host owns the selection, the clamps and the locks. Generic — no domain types.
module TablePositionControls =

    /// A table-frame axis the bar moves along.
    type Axis =
        | AxisX
        | AxisY
        | AxisZ

        member this.name : string =
            match this with
            | AxisX -> "X"
            | AxisY -> "Y"
            | AxisZ -> "Z"

        member this.minusButtonId : ConstructorUiId =
            match this with
            | AxisX -> ConstructorUiId.tablePositionXMinusButton
            | AxisY -> ConstructorUiId.tablePositionYMinusButton
            | AxisZ -> ConstructorUiId.tablePositionZMinusButton

        member this.plusButtonId : ConstructorUiId =
            match this with
            | AxisX -> ConstructorUiId.tablePositionXPlusButton
            | AxisY -> ConstructorUiId.tablePositionYPlusButton
            | AxisZ -> ConstructorUiId.tablePositionZPlusButton

        member this.fieldId : ConstructorUiId =
            match this with
            | AxisX -> ConstructorUiId.tablePositionXField
            | AxisY -> ConstructorUiId.tablePositionYField
            | AxisZ -> ConstructorUiId.tablePositionZField

        member this.lockButtonId : ConstructorUiId =
            match this with
            | AxisX -> ConstructorUiId.tablePositionXLockButton
            | AxisY -> ConstructorUiId.tablePositionYLockButton
            | AxisZ -> ConstructorUiId.tablePositionZLockButton

    /// One axis row's value and lock (the rotation bar's `AxisLock`: a locked axis disables its steps and field).
    type AxisState =
        {
            position : BarMeters
            lock : RotationControls.AxisLock
        }

    /// The position the bar displays.
    type State =
        {
            x : AxisState
            y : AxisState
            z : AxisState
            /// `ControlWithheld` when nothing movable is selected → the whole bar is disabled.
            availability : ControlAvailability
        }

        /// The readout line: the three coordinates.
        member this.readoutText : string =
            $"at x = {this.x.position.signedText} m, y = {this.y.position.signedText} m, z = {this.z.position.signedText} m"

    /// What the bar does. The host owns the selection, the clamps and the locks.
    type Handlers =
        {
            moveBy : Axis -> BarMeters -> unit      // nudge along an axis by N metres (the − / + buttons)
            setPosition : Axis -> BarMeters -> unit // set an axis to exactly N metres (the field)
            toggleLock : Axis -> unit               // lock / unlock moving along an axis
            reset : unit -> unit                    // Reset → host returns the element to its home position
        }

    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
    let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)
    let private textForeground = Color.FromRgb(0uy, 0uy, 0uy)

    /// A small clickable, button-styled Border, matching `RayPositionControls`. The subscription re-patches
    /// when the label flips (a lock toggle reads Lock / Unlock), so the action always matches the label.
    let private clickBox (id : ConstructorUiId) (caption : ControlCaption) (availability : ControlAvailability) (onClick : PointerPressedEventArgs -> unit) : IView =
        Border.create [
            id.automationAttribute<Border> ()
            Border.isEnabled availability.isEnabled
            Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
            Border.background (brush idleBackground)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 5.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text caption.value; TextBlock.foreground (brush textForeground) ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick e), SubPatchOptions.OnChangeOf (box caption.value))
        ] :> IView

    let private field (handlers : Handlers) (axis : Axis) (value : BarMeters) (availability : ControlAvailability) : IView =
        let commit (src : obj) =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> BarMeters.tryCreate tb.Text |> Option.iter (handlers.setPosition axis)
            | _ -> ()
        TextBox.create [
            TextBox.name axis.fieldId.value
            TextBox.width 80.0
            TextBox.isEnabled availability.isEnabled
            TextBox.text value.signedText
            TextBox.onKeyDown (fun e -> if e.Key = Key.Enter then commit e.Source)
            TextBox.onLostFocus (fun e -> commit e.Source)
        ] :> IView

    /// One axis row: its label, − / +, the field, "m", and the Lock / Unlock toggle.
    let private axisRow (handlers : Handlers) (availability : ControlAvailability) (axis : Axis) (axisState : AxisState) : IView =
        let free, lockCaption =
            match axisState.lock with
            | RotationControls.AxisFree -> availability, ControlCaption $"Lock {axis.name}"
            | RotationControls.AxisLocked -> ControlWithheld, ControlCaption $"Unlock {axis.name}"
        let step (direction : StepDirection) (e : PointerPressedEventArgs) =
            handlers.moveBy axis ((RayPositionControls.stepMeters e.KeyModifiers).inDirection direction)
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.width 16.0; TextBlock.text $"{axis.name}:" ]
                clickBox axis.minusButtonId (ControlCaption "−") free (step StepDecrease)
                clickBox axis.plusButtonId (ControlCaption "+") free (step StepIncrease)
                field handlers axis axisState.position free
                TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text "m" ]
                clickBox axis.lockButtonId lockCaption availability (fun _ -> handlers.toggleLock axis)
            ]
        ] :> IView

    /// The bar — a PURE projection of `state` / `handlers`, so it always reflects the host's CURRENT
    /// selection and locks.
    /// Spec 0046: a DockPanel (the axis rows docked left, the Reset + readout column filling the rest) so
    /// the readout WRAPS in a narrow window instead of running off its right edge.
    let view (state : State) (handlers : Handlers) : IView =
        DockPanel.create [
            DockPanel.margin (Thickness 8.0)
            DockPanel.isEnabled state.availability.isEnabled
            DockPanel.opacity (match state.availability with ControlOffered -> 1.0 | ControlWithheld -> 0.5)
            DockPanel.children [
                StackPanel.create [
                    StackPanel.dock Dock.Left
                    StackPanel.margin (Thickness(0.0, 0.0, 16.0, 0.0))
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 4.0
                    StackPanel.children [
                        axisRow handlers state.availability AxisX state.x
                        axisRow handlers state.availability AxisY state.y
                        axisRow handlers state.availability AxisZ state.z
                    ]
                ]
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 6.0
                    StackPanel.verticalAlignment VerticalAlignment.Center
                    StackPanel.children [
                        Border.create [
                            Border.horizontalAlignment HorizontalAlignment.Left
                            Border.child (clickBox ConstructorUiId.tablePositionResetButton (ControlCaption "Reset position") state.availability (fun _ -> handlers.reset ()))
                        ]
                        TextBlock.create [ TextBlock.name ConstructorUiId.tablePositionReadout.value; TextBlock.textWrapping TextWrapping.Wrap; TextBlock.text state.readoutText ]
                    ]
                ]
            ]
        ] :> IView
