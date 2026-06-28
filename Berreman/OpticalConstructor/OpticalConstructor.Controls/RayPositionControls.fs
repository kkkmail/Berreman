namespace OpticalConstructor.Controls

open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable "position-on-the-ray" control bar (Spec 0027, task 014): − / + step buttons, an editable
/// metres field, and a Reset, for sliding an element ALONG THE BEAM (§E.4 — the along-ray position is a
/// distance from the source; the host clamps it to the plate). It is the position analogue of the shared
/// `RotationControls` bar: the visible controls are a pure projection of the host `State`, and the host
/// owns what "the selection" is and how the value is clamped. Generic — no domain types — so any screen
/// that moves something along the ray reuses ONE control.
module RayPositionControls =

    /// The current along-ray position the bar displays. `position` is in metres; `minPosition`/
    /// `maxPosition` are the clamp the host computed (typically ±half the table length).
    type State =
        {
            position : float        // metres along the beam (0 = the host's chosen origin, e.g. table centre)
            minPosition : float
            maxPosition : float
            /// `false` when nothing is selected → the whole bar is disabled (nothing to move).
            enabled : bool
        }

    /// What the bar does. The host owns the selection and the clamping.
    type Handlers =
        {
            moveBy : float -> unit          // nudge the position by N metres (the − / + buttons)
            setPosition : float -> unit     // set the position to exactly N metres (the field)
            reset : unit -> unit            // Reset → host returns the element to its origin
        }

    /// Stable automation ids (CLAUDE.md UI guidance) — addressed by the headless tests.
    [<RequireQualifiedAccess>]
    module UiIds =
        let minus = "RayPositionMinusButton"
        let plus = "RayPositionPlusButton"
        let field = "RayPositionField"
        let reset = "RayPositionResetButton"
        let readout = "RayPositionReadout"

    /// The step a − / + button applies: 0.05 m, or a larger 0.20 m with Shift held (matching the
    /// element-movement screen's arrow-key steps).
    let stepMeters (shiftHeld : bool) : float = if shiftHeld then 0.20 else 0.05

    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private idleBorder = color 120 120 120
    let private textForeground = color 0 0 0

    /// Parse a typed position value (metres, non-integers allowed), invariant culture; `None` if malformed.
    let parseMeters (s : string) : float option =
        match System.Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | _ -> None

    let private formatMeters (v : float) : string =
        System.String.Format(CultureInfo.InvariantCulture, "{0:+0.000;-0.000;0.000}", v)

    /// A small clickable, button-styled Border, matching `RotationControls`. `e.Handled <- true` drops
    /// FuncUI's duplicate Tunnel|Bubble invocation so one click is one action.
    let private clickBox (id : string) (label : string) (enabled : bool) (onClick : PointerPressedEventArgs -> unit) : IView =
        Border.create [
            Border.name id
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

    let private stepButton (handlers : Handlers) (id : string) (label : string) (sign : float) (enabled : bool) : IView =
        clickBox id label enabled (fun e ->
            handlers.moveBy (sign * stepMeters (e.KeyModifiers.HasFlag KeyModifiers.Shift)))

    let private field (handlers : Handlers) (value : float) (enabled : bool) : IView =
        let commit (src : obj) =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> parseMeters tb.Text |> Option.iter handlers.setPosition
            | _ -> ()
        TextBox.create [
            TextBox.name UiIds.field
            TextBox.width 80.0
            TextBox.isEnabled enabled
            TextBox.text (formatMeters value)
            TextBox.onKeyDown (fun e -> if e.Key = Key.Enter then commit e.Source)
            TextBox.onLostFocus (fun e -> commit e.Source)
        ] :> IView

    /// The bar — a PURE projection of `state`/`handlers` (like `RotationControls.view`), so it always
    /// reflects the host's CURRENT selection / position and never freezes stale props.
    let view (state : State) (handlers : Handlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.margin (Thickness 8.0)
            StackPanel.isEnabled state.enabled
            StackPanel.opacity (if state.enabled then 1.0 else 0.5)
            StackPanel.children [
                TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text "Along beam:" ]
                stepButton handlers UiIds.minus "−" -1.0 state.enabled
                stepButton handlers UiIds.plus "+" 1.0 state.enabled
                field handlers state.position state.enabled
                TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text "m" ]
                clickBox UiIds.reset "Reset position" state.enabled (fun _ -> handlers.reset ())
                TextBlock.create [
                    TextBlock.name UiIds.readout
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (Thickness(10.0, 0.0, 0.0, 0.0))
                    TextBlock.text (System.String.Format(CultureInfo.InvariantCulture, "x = {0:+0.000;-0.000;0.000} m along the beam", state.position))
                ]
            ]
        ] :> IView
