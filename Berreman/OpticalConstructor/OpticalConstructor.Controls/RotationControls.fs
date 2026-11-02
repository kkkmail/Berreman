namespace OpticalConstructor.Controls

open System
open System.Globalization
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// Whether a bar, or one control of it, is offered to the user (spec 0048, the shape of STL's
/// `VncCommandAvailability`). Shared by the rotation, palette and position bars.
type ControlAvailability =
    | ControlOffered
    | ControlWithheld

    /// Whether the control is enabled: the one place this pair becomes Avalonia's own `bool`.
    member this.isEnabled : bool =
        match this with
        | ControlOffered -> true
        | ControlWithheld -> false

/// The text a bar's button shows.
type ControlCaption =
    | ControlCaption of string

    member this.value = let (ControlCaption c) = this in c

/// Which way a − / + step button moves its value.
type StepDirection =
    | StepDecrease
    | StepIncrease

/// An angle in degrees, as the rotation bar shows and takes it.
type BarDegrees =
    | BarDegrees of float

    member this.value = let (BarDegrees d) = this in d

    /// At most two decimals, invariant culture: the text of the bar's degree field.
    member this.text : string = String.Format(CultureInfo.InvariantCulture, "{0:0.##}", this.value)

    /// This step, signed for `direction`.
    member this.inDirection (direction : StepDirection) : BarDegrees =
        match direction with
        | StepDecrease -> BarDegrees -this.value
        | StepIncrease -> this

    /// A typed degree value (non-integers allowed), invariant culture; `None` if malformed.
    static member tryCreate (text : string) : BarDegrees option =
        match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, v -> Some (BarDegrees v)
        | _ -> None

/// The reusable bottom "rotation-controls" bar (Spec 0027, task 008): R1−/R1+/R2−/R2+/R3−/R3+
/// with editable degree fields, a dynamic Lock/Unlock R3 button (spec 0046: and R1 / R2 ones where the
/// host offers them), and confirmation-gated Reset /
/// Reset All. The visible buttons are a pure projection of the host state; the bar additionally
/// watches the live keyboard so the +/- pair of an axis "lights up" while its rotate modifier is
/// held (task 009) — that key-tracking is self-contained here, so no host screen needs changing.
/// One bar is reused by the three rotation test windows and the main constructor screen.
module RotationControls =

    type RotationAxis =
        | R1
        | R2
        | R3

        member this.name : string =
            match this with
            | R1 -> "R1"
            | R2 -> "R2"
            | R3 -> "R3"

        member this.minusButtonId : ConstructorUiId =
            match this with
            | R1 -> ConstructorUiId.rotationR1MinusButton
            | R2 -> ConstructorUiId.rotationR2MinusButton
            | R3 -> ConstructorUiId.rotationR3MinusButton

        member this.plusButtonId : ConstructorUiId =
            match this with
            | R1 -> ConstructorUiId.rotationR1PlusButton
            | R2 -> ConstructorUiId.rotationR2PlusButton
            | R3 -> ConstructorUiId.rotationR3PlusButton

        member this.fieldId : ConstructorUiId =
            match this with
            | R1 -> ConstructorUiId.rotationR1Field
            | R2 -> ConstructorUiId.rotationR2Field
            | R3 -> ConstructorUiId.rotationR3Field

        member this.lockButtonId : ConstructorUiId =
            match this with
            | R1 -> ConstructorUiId.rotationLockR1Button
            | R2 -> ConstructorUiId.rotationLockR2Button
            | R3 -> ConstructorUiId.rotationLockR3Button

        /// The axis whose − or + button carries `id`, if any.
        static member tryOfStepButton (id : ConstructorUiId) : RotationAxis option =
            [ R1; R2; R3 ] |> List.tryFind (fun axis -> axis.minusButtonId = id || axis.plusButtonId = id)

    /// Whether an axis is held still (spec 0046 — R1 / R2 join the task-008 R3 lock).
    type AxisLock =
        | AxisLocked
        | AxisFree

    /// Whether the bar offers a Lock / Unlock toggle for R1 or R2, and the axis' lock when it does. R3's
    /// toggle is always offered (task 008); an element offers all three, the table R3 alone.
    type AxisLockOffer =
        | NotLockable
        | Lockable of AxisLock

    /// Which reset (if any) is awaiting confirmation. Held by the host so the bar stays stateless.
    type ResetConfirm =
        | NoConfirm
        | ConfirmReset
        | ConfirmResetAll

    /// The current rotation state the bar displays.
    type State =
        {
            r1 : BarDegrees
            r2 : BarDegrees
            r3 : BarDegrees
            /// Spec 0046: R1's / R2's Lock/Unlock toggle (absent for `NotLockable`); a locked axis disables
            /// its controls. Elements offer them (the light source starts with both locked).
            r1Lock : AxisLockOffer
            r2Lock : AxisLockOffer
            /// Drives the Lock/Unlock R3 label and disables the R3 controls. The table is unlocked
            /// by default (button reads "Lock R3"); elements are locked by default ("Unlock R3").
            r3Lock : AxisLock
            /// `ControlWithheld` when nothing is selected → the whole bar is disabled (nothing to rotate).
            availability : ControlAvailability
            confirm : ResetConfirm
        }

    /// What the bar does. The host owns what "the selection" is and the confirm state.
    type Handlers =
        {
            rotate : RotationAxis -> BarDegrees -> unit     // rotate the selection about the axis by N degrees
            setAngle : RotationAxis -> BarDegrees -> unit   // set the selection's axis to exactly N degrees
            toggleLock : RotationAxis -> unit               // lock (freezing the current value) / unlock an axis
            requestReset : unit -> unit          // Reset clicked → host arms ConfirmReset
            requestResetAll : unit -> unit       // Reset All clicked → host arms ConfirmResetAll
            confirm : unit -> unit               // Yes → host performs the armed reset and disarms
            cancel : unit -> unit                // No → host disarms
        }

    /// The rotation step a +/- button applies: 15°, or 5° with Shift held.
    let buttonStepDegrees (modifiers : KeyModifiers) : BarDegrees =
        if modifiers.HasFlag KeyModifiers.Shift then BarDegrees 5.0 else BarDegrees 15.0

    /// Which axis the currently-held modifiers arm for a wheel rotation — the SAME map the windows
    /// use (Shift → R1, Ctrl+Shift → R2, Alt → R3); anything else arms nothing. Drives the highlight.
    let activeAxisForModifiers (modifiers : KeyModifiers) : RotationAxis option =
        let ctrl = modifiers.HasFlag KeyModifiers.Control
        let shift = modifiers.HasFlag KeyModifiers.Shift
        let alt = modifiers.HasFlag KeyModifiers.Alt
        match ctrl, shift, alt with
        | false, true, false -> Some R1
        | true, true, false -> Some R2
        | false, false, true -> Some R3
        | _ -> None

    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

    // An "active" (armed) button uses the standard Windows "selected/active" treatment — the light
    // accent fill (#CCE8FF) outlined by the Fluent / WinUI system accent (#0078D4). The text only
    // goes bold (it is NOT recoloured), so it stays legible on the light fill. The idle button is the
    // neutral light grey used elsewhere.
    let private activeBackground = Color.FromRgb(204uy, 232uy, 255uy)     // #CCE8FF Windows selected/active light accent
    let private activeBorder = Color.FromRgb(0uy, 120uy, 212uy)           // #0078D4 Fluent accent outline
    let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
    let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)
    let private textForeground = Color.FromRgb(0uy, 0uy, 0uy)

    /// Whether an axis' +/- pair is lit up because its rotate modifier is held (task 009).
    type private AxisHighlight =
        | AxisArmed
        | AxisIdle

    /// A small clickable, button-styled Border, its id set as the `AutomationProperties.AutomationId` (a
    /// freely-mutable attached property — unlike `Control.Name`): the reset pair swaps its label / action in
    /// place while the bar re-renders per host update, and Avalonia forbids renaming a styled control (the
    /// `MaterialsControls` precedent). `AxisArmed` lights it up with the accent (task 009).
    /// `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble invocation so one click is one action.
    let private clickBox (id : ConstructorUiId) (caption : ControlCaption) (availability : ControlAvailability) (highlight : AxisHighlight) (onClick : PointerPressedEventArgs -> unit) : IView =
        let background, border =
            match highlight with
            | AxisArmed -> activeBackground, activeBorder
            | AxisIdle -> idleBackground, idleBorder
        Border.create [
            id.automationAttribute<Border> ()
            Border.isEnabled availability.isEnabled
            Border.opacity (match availability with ControlOffered -> 1.0 | ControlWithheld -> 0.4)
            Border.background (brush background)
            Border.borderBrush (brush border)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 5.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (
                TextBlock.create [
                    TextBlock.text caption.value
                    // "The font can only become bold" — weight is the only text change; colour is fixed.
                    TextBlock.fontWeight (match highlight with AxisArmed -> FontWeight.Bold | AxisIdle -> FontWeight.Normal)
                    TextBlock.foreground (brush textForeground)
                ])
            // FuncUI keeps the FIRST subscription forever by default (SubPatchOptions.Never). The two
            // reset buttons change their ACTION in place (Reset→confirm, Reset All→cancel) when the
            // confirmation arms, so re-subscribe whenever the label flips — otherwise "Yes"/"No" would
            // still run the original Reset/Reset All (re-arming, and crossing over). Stable-label buttons
            // (+/- , the angle steps) are unaffected, so they keep their cheap one-time subscription.
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick e), SubPatchOptions.OnChangeOf (box caption.value))
        ] :> IView

    let private stepButton (handlers : Handlers) (axis : RotationAxis) (direction : StepDirection) (availability : ControlAvailability) (highlight : AxisHighlight) : IView =
        let id, caption =
            match direction with
            | StepDecrease -> axis.minusButtonId, ControlCaption $"{axis.name} −"
            | StepIncrease -> axis.plusButtonId, ControlCaption $"{axis.name} +"
        clickBox id caption availability highlight (fun e ->
            handlers.rotate axis ((buttonStepDegrees e.KeyModifiers).inDirection direction))

    let private field (handlers : Handlers) (axis : RotationAxis) (value : BarDegrees) (availability : ControlAvailability) : IView =
        let commit (src : obj) =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> BarDegrees.tryCreate tb.Text |> Option.iter (handlers.setAngle axis)
            | _ -> ()
        TextBox.create [
            TextBox.name axis.fieldId.value
            TextBox.width 60.0
            TextBox.isEnabled availability.isEnabled
            // The text comes from the model; while the user types nothing re-renders it (the model
            // value is unchanged), so the edit is uncontrolled until it is committed on Enter or
            // blur — then it is parsed (non-integers allowed) and pushed back through `setAngle`.
            TextBox.text value.text
            TextBox.onKeyDown (fun e -> if e.Key = Key.Enter then commit e.Source)
            TextBox.onLostFocus (fun e -> commit e.Source)
        ] :> IView

    let private axisGroup (handlers : Handlers) (axis : RotationAxis) (value : BarDegrees) (availability : ControlAvailability) (highlight : AxisHighlight) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 3.0
            StackPanel.verticalAlignment VerticalAlignment.Center
            StackPanel.children [
                stepButton handlers axis StepDecrease availability highlight
                stepButton handlers axis StepIncrease availability highlight
                field handlers axis value availability
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

    let mutable private armedAxis : RotationAxis option = None

    /// Paint one axis +/- button (a `clickBox` Border with a `TextBlock` child) armed or idle.
    let private styleButton (b : Border) (highlight : AxisHighlight) : unit =
        let background, border, weight =
            match highlight with
            | AxisArmed -> activeBackground, activeBorder, FontWeight.Bold
            | AxisIdle -> idleBackground, idleBorder, FontWeight.Normal
        b.Background <- brush background
        b.BorderBrush <- brush border
        match b.Child with
        | :? TextBlock as t -> t.FontWeight <- weight
        | _ -> ()

    /// Restyle every axis button currently in a window for the current `armedAxis`. The buttons are
    /// found by their `AutomationProperties.AutomationId` (the boxes carry no `Name` since the
    /// spec-0038 sweep).
    let private restyleBar (top : TopLevel) : unit =
        for v in top.GetVisualDescendants() do
            match v with
            | :? Border as b ->
                match RotationAxis.tryOfStepButton (ConstructorUiId (AutomationProperties.GetAutomationId b)) with
                | Some axis -> styleButton b (if b.IsEnabled && armedAxis = Some axis then AxisArmed else AxisIdle)
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

    /// How the bar lays its units out (spec 0046): on ONE row (the rotation test windows, whose layouts the
    /// headless tests pin), or on a row that WRAPS between the units when the host is narrower than the bar
    /// (the Main screen, where an element's three lock toggles outgrow the window).
    type private BarLayout =
        | SingleRow
        | WrapBetweenUnits

    /// The bar's visible tree: a pure projection of `state` plus which axis (if any) is armed.
    let private renderBar (state : State) (handlers : Handlers) (activeAxis : RotationAxis option) (layout : BarLayout) : IView =
        let unlessLocked (lock : AxisLock) : ControlAvailability =
            match lock with
            | AxisLocked -> ControlWithheld
            | AxisFree -> state.availability
        let offerAvailability (offer : AxisLockOffer) : ControlAvailability =
            match offer with
            | Lockable lock -> unlessLocked lock
            | NotLockable -> state.availability
        let r1Availability = offerAvailability state.r1Lock
        let r2Availability = offerAvailability state.r2Lock
        let r3Availability = unlessLocked state.r3Lock
        // An axis lights up only when its modifier is armed AND its buttons are actually live.
        let armed (axis : RotationAxis) (group : ControlAvailability) : AxisHighlight =
            match group with
            | ControlOffered when activeAxis = Some axis -> AxisArmed
            | ControlOffered | ControlWithheld -> AxisIdle
        let lockCaption (lock : AxisLock) (axis : RotationAxis) : ControlCaption =
            match lock with
            | AxisLocked -> ControlCaption $"Unlock {axis.name}"
            | AxisFree -> ControlCaption $"Lock {axis.name}"
        let lockButton =
            clickBox R3.lockButtonId (lockCaption state.r3Lock R3) state.availability AxisIdle (fun _ -> handlers.toggleLock R3)
        // Spec 0046: R1 / R2's toggles, right after their axis group, shown only where the host offers them.
        // The slot is ALWAYS present (hidden when not offered), so the bar's children keep their positions
        // and FuncUI never patches one axis group's named field into another's slot.
        let offeredLock (axis : RotationAxis) (offer : AxisLockOffer) : IView =
            let availability, caption =
                match offer with
                | NotLockable -> ControlWithheld, lockCaption AxisFree axis
                | Lockable lock -> state.availability, lockCaption lock axis
            Border.create [
                Border.isVisible (match offer with Lockable _ -> true | NotLockable -> false)
                Border.verticalAlignment VerticalAlignment.Center
                Border.child (clickBox axis.lockButtonId caption availability AxisIdle (fun _ -> handlers.toggleLock axis))
            ] :> IView
        // Reset / Reset All become a Yes / No confirmation IN PLACE: the same two buttons (plus a
        // prompt) keep their stable automation ids across the swap — only the label / action / prompt
        // change. A structural swap to differently-identified buttons made FuncUI reuse a Border and
        // re-set its then-`Name`, which Avalonia forbids once it is styled ("Cannot set Name : styled
        // element already styled") — the ids are AutomationIds now, but the in-place swap stays.
        let armedConfirm = state.confirm <> NoConfirm
        let prompt =
            match state.confirm with
            | ConfirmReset -> "Reset the selection's rotations?"
            | ConfirmResetAll -> "Reset ALL rotations?"
            | NoConfirm -> ""
        let resetLabel, resetAllLabel = if armedConfirm then "Yes", "No" else "Reset", "Reset All"
        let onReset () = match state.confirm with NoConfirm -> handlers.requestReset () | _ -> handlers.confirm ()
        let onResetAll () = match state.confirm with NoConfirm -> handlers.requestResetAll () | _ -> handlers.cancel ()
        // The bar is built of UNITS — an axis with its lock toggle, and the reset pair with its prompt — so a
        // wrapped line never separates a lock from its axis.
        let unit (children : IView list) : IView =
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 6.0
                StackPanel.verticalAlignment VerticalAlignment.Center
                StackPanel.children children
            ] :> IView
        let resetArea : IView =
            unit
                [ TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text prompt; TextBlock.isVisible armedConfirm ] :> IView
                  clickBox ConstructorUiId.rotationResetButton (ControlCaption resetLabel) state.availability AxisIdle (fun _ -> onReset ())
                  clickBox ConstructorUiId.rotationResetAllButton (ControlCaption resetAllLabel) state.availability AxisIdle (fun _ -> onResetAll ()) ]
        let units : IView list =
            [ unit
                  [ axisGroup handlers R1 state.r1 r1Availability (armed R1 r1Availability)
                    offeredLock R1 state.r1Lock ]
              unit
                  [ axisGroup handlers R2 state.r2 r2Availability (armed R2 r2Availability)
                    offeredLock R2 state.r2Lock ]
              unit
                  [ axisGroup handlers R3 state.r3 r3Availability (armed R3 r3Availability)
                    lockButton ]
              resetArea ]
        // Hook this window's keyboard once (guarded), so holding a rotate modifier lights up its axis.
        let hookKeys (e : RoutedEventArgs) : unit =
            match e.Source with
            | :? Visual as v ->
                let top = TopLevel.GetTopLevel v
                if not (isNull (box top)) then ensureKeyHook top
            | _ -> ()
        // The whole bar is disabled (and dimmed) when nothing is selected.
        match layout with
        | SingleRow ->
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 10.0
                StackPanel.margin (Thickness 8.0)
                StackPanel.onLoaded hookKeys
                StackPanel.isEnabled state.availability.isEnabled
                StackPanel.opacity (match state.availability with ControlOffered -> 1.0 | ControlWithheld -> 0.5)
                StackPanel.children units
            ] :> IView
        | WrapBetweenUnits ->
            WrapPanel.create [
                WrapPanel.orientation Orientation.Horizontal
                AttrBuilder<WrapPanel>.CreateProperty<float>(WrapPanel.ItemSpacingProperty, 10.0, ValueNone)
                AttrBuilder<WrapPanel>.CreateProperty<float>(WrapPanel.LineSpacingProperty, 6.0, ValueNone)
                WrapPanel.margin (Thickness 8.0)
                WrapPanel.onLoaded hookKeys
                WrapPanel.isEnabled state.availability.isEnabled
                WrapPanel.opacity (match state.availability with ControlOffered -> 1.0 | ControlWithheld -> 0.5)
                WrapPanel.children units
            ] :> IView

    /// The bar — a PURE projection of `state`/`handlers`, so it always reflects the host's CURRENT
    /// selection. Being pure, it re-renders on every host update (per-window: a different window's bar
    /// is a different render of a different model) and never freezes stale props — which is why the
    /// lock/angles track the selected element and why the Reset confirm-swap no longer re-enters the
    /// renderer. The keyboard highlight is layered on imperatively (`ensureKeyHook` / `renderBar`'s
    /// build-time read of `armedAxis`), so no component state is needed and the host screens stay
    /// untouched.
    let view (state : State) (handlers : Handlers) : IView =
        renderBar state handlers armedAxis SingleRow

    /// The same bar laid out on a row that WRAPS between its units when the host is narrower (spec 0046 —
    /// the Main screen, where an element's R1 / R2 / R3 lock toggles outgrow the window).
    let wrappingView (state : State) (handlers : Handlers) : IView =
        renderBar state handlers armedAxis WrapBetweenUnits
