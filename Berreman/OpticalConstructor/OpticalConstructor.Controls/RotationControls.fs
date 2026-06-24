namespace OpticalConstructor.Controls

open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable bottom "rotation-controls" bar (Spec 0027, task 008): R1−/R1+/R2−/R2+/R3−/R3+
/// with editable degree fields, a dynamic Lock/Unlock R3 button, and confirmation-gated Reset /
/// Reset All. It is GENERIC and STATELESS — it knows only the three angles (degrees), the R3-lock
/// state, whether it is enabled, and which reset (if any) is awaiting confirmation — and calls back
/// to the host, which owns the meaning of "the selection" and the confirm state. One bar is reused
/// by the three rotation test windows and the main constructor screen.
module RotationControls =

    type Axis =
        | R1
        | R2
        | R3

    /// Which reset (if any) is awaiting confirmation. Held by the host so the bar stays stateless.
    type ResetConfirm =
        | NoConfirm
        | ConfirmReset
        | ConfirmResetAll

    /// The current rotation state the bar displays.
    type State =
        {
            r1 : float          // degrees
            r2 : float
            r3 : float
            /// Drives the Lock/Unlock R3 label and disables the R3 controls. The table is unlocked
            /// by default (button reads "Lock R3"); elements are locked by default ("Unlock R3").
            r3Locked : bool
            /// `false` when nothing is selected → the whole bar is disabled (nothing to rotate).
            enabled : bool
            confirm : ResetConfirm
        }

    /// What the bar does. The host owns what "the selection" is and the confirm state.
    type Handlers =
        {
            rotate : Axis -> float -> unit       // rotate the selection about Axis by N degrees
            setAngle : Axis -> float -> unit     // set the selection's Axis to exactly N degrees
            toggleR3Lock : unit -> unit          // lock (freezing the current R3 value) / unlock R3
            requestReset : unit -> unit          // Reset clicked → host arms ConfirmReset
            requestResetAll : unit -> unit       // Reset All clicked → host arms ConfirmResetAll
            confirm : unit -> unit               // Yes → host performs the armed reset and disarms
            cancel : unit -> unit                // No → host disarms
        }

    /// Stable automation ids (CLAUDE.md UI guidance) — addressed by the headless tests.
    [<RequireQualifiedAccess>]
    module UiIds =
        let r1Minus = "RotationR1MinusButton"
        let r1Plus = "RotationR1PlusButton"
        let r2Minus = "RotationR2MinusButton"
        let r2Plus = "RotationR2PlusButton"
        let r3Minus = "RotationR3MinusButton"
        let r3Plus = "RotationR3PlusButton"
        let r1Field = "RotationR1Field"
        let r2Field = "RotationR2Field"
        let r3Field = "RotationR3Field"
        let lockR3 = "RotationLockR3Button"
        let reset = "RotationResetButton"
        let resetAll = "RotationResetAllButton"
        let resetConfirm = "RotationResetConfirmButton"
        let resetCancel = "RotationResetCancelButton"

    /// The rotation step a +/- button applies: 15°, or 5° with Shift held.
    let buttonStepDegrees (shiftHeld : bool) : float = if shiftHeld then 5.0 else 15.0

    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

    /// Parse a typed degree value (non-integers allowed), invariant culture; `None` if malformed.
    let parseDegrees (s : string) : float option =
        match System.Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | _ -> None

    let private formatDegrees (v : float) : string =
        System.String.Format(CultureInfo.InvariantCulture, "{0:0.##}", v)

    /// A small clickable, button-styled Border. `e.Handled <- true` drops FuncUI's duplicate
    /// Tunnel|Bubble invocation so one click is one action.
    let private clickBox (id : string) (label : string) (enabled : bool) (onClick : PointerPressedEventArgs -> unit) : IView =
        Border.create [
            Border.name id
            Border.isEnabled enabled
            Border.opacity (if enabled then 1.0 else 0.4)
            Border.background (brush (color 232 232 232))
            Border.borderBrush (brush (color 120 120 120))
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 5.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label ])
            Border.onPointerPressed (fun e -> e.Handled <- true; onClick e)
        ] :> IView

    let private stepButton (handlers : Handlers) (id : string) (label : string) (axis : Axis) (sign : float) (enabled : bool) : IView =
        clickBox id label enabled (fun e ->
            handlers.rotate axis (sign * buttonStepDegrees (e.KeyModifiers.HasFlag KeyModifiers.Shift)))

    let private field (handlers : Handlers) (id : string) (axis : Axis) (value : float) (enabled : bool) : IView =
        let commit (src : obj) =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> parseDegrees tb.Text |> Option.iter (handlers.setAngle axis)
            | _ -> ()
        TextBox.create [
            TextBox.name id
            TextBox.width 60.0
            TextBox.isEnabled enabled
            // The text comes from the model; while the user types nothing re-renders it (the model
            // value is unchanged), so the edit is uncontrolled until it is committed on Enter or
            // blur — then it is parsed (non-integers allowed) and pushed back through `setAngle`.
            TextBox.text (formatDegrees value)
            TextBox.onKeyDown (fun e -> if e.Key = Key.Enter then commit e.Source)
            TextBox.onLostFocus (fun e -> commit e.Source)
        ] :> IView

    let private axisGroup (handlers : Handlers) (minusId, plusId, fieldId) (label : string) (axis : Axis) (value : float) (enabled : bool) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 3.0
            StackPanel.verticalAlignment VerticalAlignment.Center
            StackPanel.children [
                stepButton handlers minusId (label + " −") axis -1.0 enabled
                stepButton handlers plusId (label + " +") axis 1.0 enabled
                field handlers fieldId axis value enabled
            ]
        ] :> IView

    /// The bar. A pure function of `state` + `handlers`.
    let view (state : State) (handlers : Handlers) : IView =
        let r3Enabled = state.enabled && not state.r3Locked
        let lockButton =
            clickBox UiIds.lockR3 (if state.r3Locked then "Unlock R3" else "Lock R3") state.enabled (fun _ -> handlers.toggleR3Lock ())
        // Reset / Reset All collapse into a Yes / No gate while a reset is awaiting confirmation.
        let resetArea : IView list =
            match state.confirm with
            | NoConfirm ->
                [ clickBox UiIds.reset "Reset" state.enabled (fun _ -> handlers.requestReset ())
                  clickBox UiIds.resetAll "Reset All" state.enabled (fun _ -> handlers.requestResetAll ()) ]
            | ConfirmReset | ConfirmResetAll ->
                let prompt = if state.confirm = ConfirmReset then "Reset the selection's rotations?" else "Reset ALL rotations?"
                [ TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text prompt ] :> IView
                  clickBox UiIds.resetConfirm "Yes" state.enabled (fun _ -> handlers.confirm ())
                  clickBox UiIds.resetCancel "No" state.enabled (fun _ -> handlers.cancel ()) ]
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 10.0
            StackPanel.margin (Thickness 8.0)
            // The whole bar is disabled (and dimmed) when nothing is selected.
            StackPanel.isEnabled state.enabled
            StackPanel.opacity (if state.enabled then 1.0 else 0.5)
            StackPanel.children
                ([ axisGroup handlers (UiIds.r1Minus, UiIds.r1Plus, UiIds.r1Field) "R1" R1 state.r1 state.enabled
                   axisGroup handlers (UiIds.r2Minus, UiIds.r2Plus, UiIds.r2Field) "R2" R2 state.r2 state.enabled
                   axisGroup handlers (UiIds.r3Minus, UiIds.r3Plus, UiIds.r3Field) "R3" R3 state.r3 r3Enabled
                   lockButton ]
                 @ resetArea)
        ] :> IView
