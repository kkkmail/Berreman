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

    /// Whether moving along an axis is held still. A locked axis disables its steps and field.
    type AxisLock =
        | AxisLocked
        | AxisFree

    /// One axis row's value (metres) and lock.
    type AxisState =
        {
            position : float
            lock : AxisLock
        }

    /// The position the bar displays.
    type State =
        {
            x : AxisState
            y : AxisState
            z : AxisState
            /// `false` when nothing movable is selected → the whole bar is disabled.
            enabled : bool
        }

    /// What the bar does. The host owns the selection, the clamps and the locks.
    type Handlers =
        {
            moveBy : Axis -> float -> unit      // nudge along an axis by N metres (the − / + buttons)
            setPosition : Axis -> float -> unit // set an axis to exactly N metres (the field)
            toggleLock : Axis -> unit           // lock / unlock moving along an axis
            reset : unit -> unit                // Reset → host returns the element to its home position
        }

    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private idleBorder = color 120 120 120
    let private textForeground = color 0 0 0

    let private formatMeters (v : float) : string =
        System.String.Format(CultureInfo.InvariantCulture, "{0:+0.000;-0.000;0.000}", v)

    /// The readout line: the three coordinates.
    let readoutText (state : State) : string =
        $"at x = %s{formatMeters state.x.position} m, y = %s{formatMeters state.y.position} m, z = %s{formatMeters state.z.position} m"

    let private automationId (autoId : string) : IAttr<Border> =
        AttrBuilder<Border>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

    /// A small clickable, button-styled Border, matching `RayPositionControls`. The subscription re-patches
    /// when the label flips (a lock toggle reads Lock / Unlock), so the action always matches the label.
    let private clickBox (id : string) (label : string) (enabled : bool) (onClick : PointerPressedEventArgs -> unit) : IView =
        Border.create [
            automationId id
            Border.isEnabled enabled
            Border.opacity (if enabled then 1.0 else 0.4)
            Border.background (brush idleBackground)
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 5.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label; TextBlock.foreground (brush textForeground) ])
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick e), SubPatchOptions.OnChangeOf (box label))
        ] :> IView

    let private field (handlers : Handlers) (id : string) (axis : Axis) (value : float) (enabled : bool) : IView =
        let commit (src : obj) =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> RayPositionControls.parseMeters tb.Text |> Option.iter (handlers.setPosition axis)
            | _ -> ()
        TextBox.create [
            TextBox.name id
            TextBox.width 80.0
            TextBox.isEnabled enabled
            TextBox.text (formatMeters value)
            TextBox.onKeyDown (fun e -> if e.Key = Key.Enter then commit e.Source)
            TextBox.onLostFocus (fun e -> commit e.Source)
        ] :> IView

    /// One axis row: its label, − / +, the field, "m", and the Lock / Unlock toggle.
    let private axisRow
        (handlers : Handlers) (enabled : bool) (minusId, plusId, fieldId, lockId) (label : string) (axis : Axis) (axisState : AxisState)
        : IView =
        let free =
            match axisState.lock with
            | AxisFree -> enabled
            | AxisLocked -> false
        let lockLabel =
            match axisState.lock with
            | AxisLocked -> $"Unlock %s{label}"
            | AxisFree -> $"Lock %s{label}"
        let step (sign : float) (e : PointerPressedEventArgs) =
            handlers.moveBy axis (sign * RayPositionControls.stepMeters (e.KeyModifiers.HasFlag KeyModifiers.Shift))
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.width 16.0; TextBlock.text $"%s{label}:" ]
                clickBox minusId "−" free (step -1.0)
                clickBox plusId "+" free (step 1.0)
                field handlers fieldId axis axisState.position free
                TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text "m" ]
                clickBox lockId lockLabel enabled (fun _ -> handlers.toggleLock axis)
            ]
        ] :> IView

    /// The bar — a PURE projection of `state` / `handlers`, so it always reflects the host's CURRENT
    /// selection and locks.
    /// Spec 0046: a DockPanel (the axis rows docked left, the Reset + readout column filling the rest) so
    /// the readout WRAPS in a narrow window instead of running off its right edge.
    let view (state : State) (handlers : Handlers) : IView =
        DockPanel.create [
            DockPanel.margin (Thickness 8.0)
            DockPanel.isEnabled state.enabled
            DockPanel.opacity (if state.enabled then 1.0 else 0.5)
            DockPanel.children [
                StackPanel.create [
                    StackPanel.dock Dock.Left
                    StackPanel.margin (Thickness(0.0, 0.0, 16.0, 0.0))
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 4.0
                    StackPanel.children [
                        axisRow handlers state.enabled (UiIds.TablePosition.xMinus, UiIds.TablePosition.xPlus, UiIds.TablePosition.xField, UiIds.TablePosition.xLock) "X" AxisX state.x
                        axisRow handlers state.enabled (UiIds.TablePosition.yMinus, UiIds.TablePosition.yPlus, UiIds.TablePosition.yField, UiIds.TablePosition.yLock) "Y" AxisY state.y
                        axisRow handlers state.enabled (UiIds.TablePosition.zMinus, UiIds.TablePosition.zPlus, UiIds.TablePosition.zField, UiIds.TablePosition.zLock) "Z" AxisZ state.z
                    ]
                ]
                StackPanel.create [
                    StackPanel.orientation Orientation.Vertical
                    StackPanel.spacing 6.0
                    StackPanel.verticalAlignment VerticalAlignment.Center
                    StackPanel.children [
                        Border.create [
                            Border.horizontalAlignment HorizontalAlignment.Left
                            Border.child (clickBox UiIds.TablePosition.reset "Reset position" state.enabled (fun _ -> handlers.reset ()))
                        ]
                        TextBlock.create [ TextBlock.name UiIds.TablePosition.readout; TextBlock.textWrapping TextWrapping.Wrap; TextBlock.text (readoutText state) ]
                    ]
                ]
            ]
        ] :> IView
