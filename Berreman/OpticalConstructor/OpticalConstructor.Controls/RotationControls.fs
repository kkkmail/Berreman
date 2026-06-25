namespace OpticalConstructor.Controls

open System
open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable bottom "rotation-controls" bar (Spec 0027, task 008): R1−/R1+/R2−/R2+/R3−/R3+
/// with editable degree fields, a dynamic Lock/Unlock R3 button, and confirmation-gated Reset /
/// Reset All. The visible buttons are a pure projection of the host state; the bar additionally
/// watches the live keyboard so the +/- pair of an axis "lights up" while its rotate modifier is
/// held (task 009) — that key-tracking is self-contained here, so no host screen needs changing.
/// One bar is reused by the three rotation test windows and the main constructor screen.
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

    /// Which axis the currently-held modifiers arm for a wheel rotation — the SAME map the windows
    /// use (Shift → R1, Ctrl+Shift → R2, Alt → R3); anything else arms nothing. Drives the highlight.
    let activeAxisForModifiers (modifiers : KeyModifiers) : Axis option =
        let ctrl = modifiers.HasFlag KeyModifiers.Control
        let shift = modifiers.HasFlag KeyModifiers.Shift
        let alt = modifiers.HasFlag KeyModifiers.Alt
        match ctrl, shift, alt with
        | false, true, false -> Some R1
        | true, true, false -> Some R2
        | false, false, true -> Some R3
        | _ -> None

    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

    // An "active" (armed) button uses the standard Windows "selected/active" treatment — the light
    // accent fill (#CCE8FF) outlined by the Fluent / WinUI system accent (#0078D4). The text only
    // goes bold (it is NOT recoloured), so it stays legible on the light fill. The idle button is the
    // neutral light grey used elsewhere.
    let private activeBackground = color 204 232 255      // #CCE8FF Windows selected/active light accent
    let private activeBorder = color 0 120 212            // #0078D4 Fluent accent outline
    let private idleBackground = color 232 232 232
    let private idleBorder = color 120 120 120
    let private textForeground = color 0 0 0

    /// Parse a typed degree value (non-integers allowed), invariant culture; `None` if malformed.
    let parseDegrees (s : string) : float option =
        match System.Double.TryParse(s, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, v -> Some v
        | _ -> None

    let private formatDegrees (v : float) : string =
        System.String.Format(CultureInfo.InvariantCulture, "{0:0.##}", v)

    /// A small clickable, button-styled Border. `active` lights it up with the accent (task 009).
    /// `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble invocation so one click is one action.
    let private clickBox (id : string) (label : string) (enabled : bool) (active : bool) (onClick : PointerPressedEventArgs -> unit) : IView =
        Border.create [
            Border.name id
            Border.isEnabled enabled
            Border.opacity (if enabled then 1.0 else 0.4)
            Border.background (brush (if active then activeBackground else idleBackground))
            Border.borderBrush (brush (if active then activeBorder else idleBorder))
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 5.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (
                TextBlock.create [
                    TextBlock.text label
                    // "The font can only become bold" — weight is the only text change; colour is fixed.
                    TextBlock.fontWeight (if active then FontWeight.Bold else FontWeight.Normal)
                    TextBlock.foreground (brush textForeground)
                ])
            Border.onPointerPressed (fun e -> e.Handled <- true; onClick e)
        ] :> IView

    let private stepButton (handlers : Handlers) (id : string) (label : string) (axis : Axis) (sign : float) (enabled : bool) (active : bool) : IView =
        clickBox id label enabled active (fun e ->
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

    let private axisGroup (handlers : Handlers) (minusId, plusId, fieldId) (label : string) (axis : Axis) (value : float) (enabled : bool) (active : bool) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 3.0
            StackPanel.verticalAlignment VerticalAlignment.Center
            StackPanel.children [
                stepButton handlers minusId (label + " −") axis -1.0 enabled active
                stepButton handlers plusId (label + " +") axis 1.0 enabled active
                field handlers fieldId axis value enabled
            ]
        ] :> IView

    /// The bar's visible tree: a pure projection of `state` plus which axis (if any) is armed.
    let private renderBar (state : State) (handlers : Handlers) (activeAxis : Axis option) : IView =
        let r3Enabled = state.enabled && not state.r3Locked
        // An axis lights up only when its modifier is armed AND its buttons are actually live.
        let armed (axis : Axis) (groupEnabled : bool) : bool = groupEnabled && activeAxis = Some axis
        let lockButton =
            clickBox UiIds.lockR3 (if state.r3Locked then "Unlock R3" else "Lock R3") state.enabled false (fun _ -> handlers.toggleR3Lock ())
        // Reset / Reset All collapse into a Yes / No gate while a reset is awaiting confirmation.
        let resetArea : IView list =
            match state.confirm with
            | NoConfirm ->
                [ clickBox UiIds.reset "Reset" state.enabled false (fun _ -> handlers.requestReset ())
                  clickBox UiIds.resetAll "Reset All" state.enabled false (fun _ -> handlers.requestResetAll ()) ]
            | ConfirmReset | ConfirmResetAll ->
                let prompt = if state.confirm = ConfirmReset then "Reset the selection's rotations?" else "Reset ALL rotations?"
                [ TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text prompt ] :> IView
                  clickBox UiIds.resetConfirm "Yes" state.enabled false (fun _ -> handlers.confirm ())
                  clickBox UiIds.resetCancel "No" state.enabled false (fun _ -> handlers.cancel ()) ]
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 10.0
            StackPanel.margin (Thickness 8.0)
            // The whole bar is disabled (and dimmed) when nothing is selected.
            StackPanel.isEnabled state.enabled
            StackPanel.opacity (if state.enabled then 1.0 else 0.5)
            StackPanel.children
                ([ axisGroup handlers (UiIds.r1Minus, UiIds.r1Plus, UiIds.r1Field) "R1" R1 state.r1 state.enabled (armed R1 state.enabled)
                   axisGroup handlers (UiIds.r2Minus, UiIds.r2Plus, UiIds.r2Field) "R2" R2 state.r2 state.enabled (armed R2 state.enabled)
                   axisGroup handlers (UiIds.r3Minus, UiIds.r3Plus, UiIds.r3Field) "R3" R3 state.r3 r3Enabled (armed R3 r3Enabled)
                   lockButton ]
                 @ resetArea)
        ] :> IView

    /// A FuncUI store carrying the host's latest DISPLAY state into the (otherwise memoized) bar
    /// component. A FuncUI component re-renders only on its OWN state — a value captured in its render
    /// closure freezes at the first render. The bar must stay a component (for the keyboard highlight's
    /// internal state), so to still reflect new host props — e.g. selecting a different element with
    /// its OWN R3-lock — `view` pushes the state into this store and the component reads it via
    /// `usePassed`, which re-renders on change. (`handlers` need no store: they are stable per screen,
    /// so the per-instance closure capture is correct.) NOTE: one shared store ⇒ the app drives ONE
    /// rotation bar at a time; the armed-axis highlight is global keyboard state anyway.
    let private disabledState : State =
        { r1 = 0.0; r2 = 0.0; r3 = 0.0; r3Locked = false; enabled = false; confirm = NoConfirm }

    let private stateStore : IWritable<State> = Avalonia.FuncUI.State<State>(disabledState) :> IWritable<State>

    /// The bar. A stateful component that mirrors `state`/`handlers` and, on its own, tracks the live
    /// keyboard so an axis lights up while its rotate modifier is held — see `renderBar`. The key
    /// tracking is wired at the window's TopLevel (tunnel phase) so it works no matter which control
    /// holds focus, and it is torn down with the component; the host screens stay untouched.
    let view (state : State) (handlers : Handlers) : IView =
        // Push the latest host state into the store BEFORE building the component, so the first render
        // (and every later host render) reflects the current selection's lock / angles / confirm state.
        stateStore.Set state
        Component.create ("rotation-controls", fun ctx ->
            let state = (ctx.usePassed stateStore).Current
            let activeAxis = ctx.useState (None : Axis option)
            ctx.useEffect (
                handler = (fun () ->
                    let ctrl = ctx.control
                    let onKey = EventHandler<KeyEventArgs>(fun _ e -> activeAxis.Set (activeAxisForModifiers e.KeyModifiers))
                    let mutable top : TopLevel = null
                    let hookTop () =
                        top <- TopLevel.GetTopLevel ctrl
                        if not (isNull (box top)) then
                            top.AddHandler(InputElement.KeyDownEvent, onKey, RoutingStrategies.Tunnel, true)
                            top.AddHandler(InputElement.KeyUpEvent, onKey, RoutingStrategies.Tunnel, true)
                    let unhookTop () =
                        if not (isNull (box top)) then
                            top.RemoveHandler(InputElement.KeyDownEvent, onKey)
                            top.RemoveHandler(InputElement.KeyUpEvent, onKey)
                            top <- null
                    let onAttached = EventHandler<VisualTreeAttachmentEventArgs>(fun _ _ -> hookTop ())
                    let onDetached = EventHandler<VisualTreeAttachmentEventArgs>(fun _ _ -> unhookTop (); activeAxis.Set None)
                    ctrl.AttachedToVisualTree.AddHandler onAttached
                    ctrl.DetachedFromVisualTree.AddHandler onDetached
                    hookTop ()   // already attached on first render → subscribe now
                    { new IDisposable with
                        member _.Dispose() =
                            ctrl.AttachedToVisualTree.RemoveHandler onAttached
                            ctrl.DetachedFromVisualTree.RemoveHandler onDetached
                            unhookTop () }),
                triggers = [ EffectTrigger.AfterInit ])
            renderBar state handlers activeAxis.Current)
