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

/// A length in metres, as the position bars show and take it: a position, a bound or a step.
type BarMeters =
    | BarMeters of float

    member this.value = let (BarMeters m) = this in m

    /// Three decimals with an explicit sign, invariant culture: the text the position bars show.
    member this.signedText : string = System.String.Format(CultureInfo.InvariantCulture, "{0:+0.000;-0.000;0.000}", this.value)

    /// This step, signed for `direction`.
    member this.inDirection (direction : StepDirection) : BarMeters =
        match direction with
        | StepDecrease -> BarMeters -this.value
        | StepIncrease -> this

    /// A typed length (metres, non-integers allowed), invariant culture; `None` if malformed.
    static member tryCreate (text : string) : BarMeters option =
        match System.Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, v -> Some (BarMeters v)
        | _ -> None

/// The reusable "position-on-the-ray" control bar (Spec 0027, task 014): − / + step buttons, an editable
/// metres field, and a Reset, for sliding an element ALONG THE BEAM (§E.4 — the along-ray position is a
/// distance from the source; the host clamps it to the element's ray — a detector only from below). It is the
/// position analogue of the shared `RotationControls` bar: the visible controls are a pure projection of
/// the host `State`, and the host owns what "the selection" is and how the value is clamped. Generic — no
/// domain types — so any screen that moves something along the ray reuses ONE control.
module RayPositionControls =

    /// The two bounds of a `BoundedRange`.
    type PositionSpan =
        {
            lowest : BarMeters
            highest : BarMeters
        }

    /// How far the host lets the position go (spec 0046): between two bounds (an element between the
    /// start and the end of its ray), from a lower bound on (a detector — moving it extends its ray), or
    /// anywhere.
    type PositionRange =
        | BoundedRange of PositionSpan
        | AtLeast of lowest : BarMeters
        | UnboundedRange

    /// The current along-ray position the bar displays. `position` is in metres along the beam (0 = the
    /// host's chosen origin, e.g. table centre); `range` is the clamp the host applies.
    type RayPositionControlsState =
        {
            position : BarMeters
            range : PositionRange
            /// `ControlWithheld` when nothing is selected → the whole bar is disabled (nothing to move).
            availability : ControlAvailability
        }

        /// The readout line: the position and the range it may move in.
        member this.readoutText : string =
            let rangeText =
                match this.range with
                | BoundedRange span -> $"range {span.lowest.signedText} … {span.highest.signedText} m"
                | AtLeast lowest -> $"from {lowest.signedText} m, uncapped"
                | UnboundedRange -> "uncapped"
            $"x = {this.position.signedText} m along the beam ({rangeText})"

    /// What the bar does. The host owns the selection and the clamping.
    type RayPositionControlsHandlers =
        {
            moveBy : BarMeters -> unit          // nudge the position by N metres (the − / + buttons)
            setPosition : BarMeters -> unit     // set the position to exactly N metres (the field)
            reset : unit -> unit                // Reset → host returns the element to its origin
        }

    /// The step a − / + button applies: 0.05 m, or a larger 0.20 m with Shift held (matching the
    /// element-movement screen's arrow-key steps).
    let stepMeters (modifiers : KeyModifiers) : BarMeters =
        if modifiers.HasFlag KeyModifiers.Shift then BarMeters 0.20 else BarMeters 0.05

    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = Color.FromRgb(232uy, 232uy, 232uy)
    let private idleBorder = Color.FromRgb(120uy, 120uy, 120uy)
    let private textForeground = Color.FromRgb(0uy, 0uy, 0uy)

    /// A small clickable, button-styled Border, matching `RotationControls`, its id set as the
    /// `AutomationProperties.AutomationId` (a freely-mutable attached property — unlike `Control.Name`):
    /// the bar's boxes are regenerable per host re-render, and Avalonia forbids renaming a styled control
    /// (the `MaterialsControls` precedent). `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble
    /// invocation so one click is one action.
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

    let private stepButton (handlers : RayPositionControlsHandlers) (direction : StepDirection) (availability : ControlAvailability) : IView =
        let id, caption =
            match direction with
            | StepDecrease -> ConstructorUiId.rayPositionMinusButton, ControlCaption "−"
            | StepIncrease -> ConstructorUiId.rayPositionPlusButton, ControlCaption "+"
        clickBox id caption availability (fun e ->
            handlers.moveBy ((stepMeters e.KeyModifiers).inDirection direction))

    let private field (handlers : RayPositionControlsHandlers) (value : BarMeters) (availability : ControlAvailability) : IView =
        let commit (src : obj) =
            match src with
            | :? TextBox as tb when not (isNull tb.Text) -> BarMeters.tryCreate tb.Text |> Option.iter handlers.setPosition
            | _ -> ()
        TextBox.create [
            TextBox.name ConstructorUiId.rayPositionField.value
            TextBox.width 80.0
            TextBox.isEnabled availability.isEnabled
            TextBox.text value.signedText
            TextBox.onKeyDown (fun e -> if e.Key = Key.Enter then commit e.Source)
            TextBox.onLostFocus (fun e -> commit e.Source)
        ] :> IView

    /// The bar — a PURE projection of `state`/`handlers` (like `RotationControls.view`), so it always
    /// reflects the host's CURRENT selection / position and never freezes stale props.
    let rayPositionControlsView (state : RayPositionControlsState) (handlers : RayPositionControlsHandlers) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 6.0
            StackPanel.margin (Thickness 8.0)
            StackPanel.isEnabled state.availability.isEnabled
            StackPanel.opacity (match state.availability with ControlOffered -> 1.0 | ControlWithheld -> 0.5)
            StackPanel.children [
                TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text "Along beam:" ]
                stepButton handlers StepDecrease state.availability
                stepButton handlers StepIncrease state.availability
                field handlers state.position state.availability
                TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text "m" ]
                clickBox ConstructorUiId.rayPositionResetButton (ControlCaption "Reset position") state.availability (fun _ -> handlers.reset ())
                TextBlock.create [
                    TextBlock.name ConstructorUiId.rayPositionReadout.value
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (Thickness(10.0, 0.0, 0.0, 0.0))
                    TextBlock.text state.readoutText
                ]
            ]
        ] :> IView
