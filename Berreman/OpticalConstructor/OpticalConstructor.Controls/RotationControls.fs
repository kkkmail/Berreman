namespace OpticalConstructor.Controls

open System
open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree
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
        // The two reset buttons keep these stable ids in BOTH modes: "Reset"/"Reset All" when idle,
        // and the confirmation "Yes"/"No" when a reset is armed (the label/action changes, not the id).
        let reset = "RotationResetButton"
        let resetAll = "RotationResetAllButton"

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
            // FuncUI keeps the FIRST subscription forever by default (SubPatchOptions.Never). The two
            // reset buttons change their ACTION in place (Reset→confirm, Reset All→cancel) when the
            // confirmation arms, so re-subscribe whenever the label flips — otherwise "Yes"/"No" would
            // still run the original Reset/Reset All (re-arming, and crossing over). Stable-label buttons
            // (+/- , the angle steps) are unaffected, so they keep their cheap one-time subscription.
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick e), SubPatchOptions.OnChangeOf (box label))
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

    // -----------------------------------------------------------------------------------------------
    // Keyboard highlight (task 009), layered on the PURE bar imperatively so it needs no component
    // state (which would otherwise freeze the host's props — the bar must re-render per host update to
    // stay per-window correct). `armedAxis` is the globally-held modifier's axis; the bar reads it at
    // BUILD time (so a host re-render while a key is held keeps the highlight), and a per-window key
    // hook restyles the live buttons between renders (so pressing/releasing a key with nothing else
    // happening still lights up). Key events only reach the FOCUSED window, so the live restyle is
    // naturally per-window.
    // -----------------------------------------------------------------------------------------------

    let mutable private armedAxis : Axis option = None

    let private axisOfButtonName (name : string) : Axis option =
        if name = UiIds.r1Minus || name = UiIds.r1Plus then Some R1
        elif name = UiIds.r2Minus || name = UiIds.r2Plus then Some R2
        elif name = UiIds.r3Minus || name = UiIds.r3Plus then Some R3
        else None

    /// Paint one axis +/- button (a `clickBox` Border with a `TextBlock` child) armed or idle.
    let private styleButton (b : Border) (isArmed : bool) : unit =
        b.Background <- brush (if isArmed then activeBackground else idleBackground)
        b.BorderBrush <- brush (if isArmed then activeBorder else idleBorder)
        match b.Child with
        | :? TextBlock as t -> t.FontWeight <- (if isArmed then FontWeight.Bold else FontWeight.Normal)
        | _ -> ()

    /// Restyle every axis button currently in a window for the current `armedAxis`.
    let private restyleBar (top : TopLevel) : unit =
        for v in top.GetVisualDescendants() do
            match v with
            | :? Border as b when not (isNull b.Name) ->
                match axisOfButtonName b.Name with
                | Some axis -> styleButton b (b.IsEnabled && armedAxis = Some axis)
                | None -> ()
            | _ -> ()

    // One keyboard subscription per window (never two, even though the pure bar is rebuilt often).
    let private hooked = System.Runtime.CompilerServices.ConditionalWeakTable<TopLevel, obj>()

    /// Hook a window's keyboard ONCE: tunnel-phase Key handlers (so focus does not matter) recompute
    /// `armedAxis` and restyle that window's bar live.
    let private ensureKeyHook (top : TopLevel) : unit =
        match hooked.TryGetValue top with
        | true, _ -> ()
        | _ ->
            hooked.Add(top, obj ())
            let onKey = EventHandler<KeyEventArgs>(fun _ e ->
                armedAxis <- activeAxisForModifiers e.KeyModifiers
                restyleBar top)
            top.AddHandler(InputElement.KeyDownEvent, onKey, RoutingStrategies.Tunnel, true)
            top.AddHandler(InputElement.KeyUpEvent, onKey, RoutingStrategies.Tunnel, true)

    /// The bar's visible tree: a pure projection of `state` plus which axis (if any) is armed.
    let private renderBar (state : State) (handlers : Handlers) (activeAxis : Axis option) : IView =
        let r3Enabled = state.enabled && not state.r3Locked
        // An axis lights up only when its modifier is armed AND its buttons are actually live.
        let armed (axis : Axis) (groupEnabled : bool) : bool = groupEnabled && activeAxis = Some axis
        let lockButton =
            clickBox UiIds.lockR3 (if state.r3Locked then "Unlock R3" else "Lock R3") state.enabled false (fun _ -> handlers.toggleR3Lock ())
        // Reset / Reset All become a Yes / No confirmation IN PLACE: the same two named buttons (plus a
        // prompt) keep their `Name` across the swap — only the label / action / prompt change. A
        // structural swap to differently-named buttons made FuncUI reuse a Border and re-set its Name,
        // which Avalonia forbids once it is styled ("Cannot set Name : styled element already styled").
        let armedConfirm = state.confirm <> NoConfirm
        let prompt =
            match state.confirm with
            | ConfirmReset -> "Reset the selection's rotations?"
            | ConfirmResetAll -> "Reset ALL rotations?"
            | NoConfirm -> ""
        let resetLabel, resetAllLabel = if armedConfirm then "Yes", "No" else "Reset", "Reset All"
        let onReset () = match state.confirm with NoConfirm -> handlers.requestReset () | _ -> handlers.confirm ()
        let onResetAll () = match state.confirm with NoConfirm -> handlers.requestResetAll () | _ -> handlers.cancel ()
        let resetArea : IView list =
            [ TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text prompt; TextBlock.isVisible armedConfirm ] :> IView
              clickBox UiIds.reset resetLabel state.enabled false (fun _ -> onReset ())
              clickBox UiIds.resetAll resetAllLabel state.enabled false (fun _ -> onResetAll ()) ]
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 10.0
            StackPanel.margin (Thickness 8.0)
            // Hook this window's keyboard once (guarded), so holding a rotate modifier lights up its axis.
            StackPanel.onLoaded (fun e ->
                match e.Source with
                | :? Visual as v ->
                    let top = TopLevel.GetTopLevel v
                    if not (isNull (box top)) then ensureKeyHook top
                | _ -> ())
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

    /// The bar — a PURE projection of `state`/`handlers`, so it always reflects the host's CURRENT
    /// selection. Being pure, it re-renders on every host update (per-window: a different window's bar
    /// is a different render of a different model) and never freezes stale props — which is why the
    /// lock/angles track the selected element and why the Reset confirm-swap no longer re-enters the
    /// renderer. The keyboard highlight is layered on imperatively (`ensureKeyHook` / `renderBar`'s
    /// build-time read of `armedAxis`), so no component state is needed and the host screens stay
    /// untouched.
    let view (state : State) (handlers : Handlers) : IView =
        renderBar state handlers armedAxis
