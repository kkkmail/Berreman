namespace OpticalConstructor.Controls

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// The reusable EXPERIMENTS control (Spec 0027, task 024 — the "Experiments" bay). The user picks WHICH
/// element currently present on the table is the swept element — the one whose R1 makes a full circle
/// (the `RotateR1FullCircle` experiment). A readout shows the built experiment. No solve yet (Phase 3).
/// This control is DOMAIN-FREE (matching `RendererControls` / `LibraryControls`): the host pre-flattens
/// the present elements into selectable `SweepCandidate`s and injects the behaviour as a `Handlers`
/// function record (the functional-proxy seam; a test substitutes stubs).
module ExperimentControls =

    /// One present, sweepable table element offered to the user (the host flattens the live scene into
    /// these; Controls carries no domain types). `elementId` is the element's serializable id; `label`
    /// is a human-readable display (e.g. "Linear polarizer #2").
    type SweepCandidate =
        {
            elementId : string
            label : string
        }

    /// The bay's pure, serializable state.
    type State =
        {
            /// The present elements, each offered as a candidate swept element.
            candidates : SweepCandidate list
            /// The currently-chosen swept element's id (the one whose R1 sweeps the full circle), if any.
            chosenId : string option
            /// The built experiment's readout text ("Experiment: rotate <label> R1 0…360°" or empty).
            experimentName : string
            /// `false` when there are no elements to sweep.
            enabled : bool
            /// Spec 0027 (024) Phase 3: the rotating-analyzer (angleDeg, intensity) curve, drawn inline as a
            /// polyline (the Malus proof). Empty when there is no run to show.
            chart : (float * float) list
            /// Spec 0027 (024) Phase 4: the ellipsometer Ψ/Δ readout (in DEGREES), shown instead of the
            /// intensity chart when the scene's detector is an ellipsometer. `None` for an intensity detector.
            psiDelta : (float * float) option
        }

    let empty : State =
        {
            candidates = []
            chosenId = None
            experimentName = ""
            enabled = false
            chart = []
            psiDelta = None
        }

    /// Behaviour injected by the host (the functional-proxy seam; tests pass stubs).
    type Handlers =
        {
            /// Pick which present element's R1 sweeps the full circle (by its id).
            chooseSwept : string -> unit
        }

    /// Stable automation ids (CLAUDE.md UI guidance).
    [<RequireQualifiedAccess>]
    module UiIds =
        let readout = "ExperimentReadout"
        let candidates = "ExperimentCandidates"
        /// A candidate's clickable id — the element id, prefixed so it cannot collide with other ids.
        let candidate (elementId : string) : string = "ExperimentCandidate_" + elementId
        /// The inline rotating-analyzer intensity polyline (Phase 3 — the Malus curve).
        let chart = "ExperimentChart"
        /// The ellipsometer Ψ/Δ readout text (Phase 4).
        let psiDelta = "EllipsometerReadout"

    // -- The button look, identical to the other bars' idle button (so they MATCH). --
    let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
    let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
    let private idleBackground = color 232 232 232
    let private chosenBackground = color 150 185 235
    let private idleBorder = color 120 120 120

    /// A clickable candidate (a styled, named Border), highlighted when it is the chosen swept element.
    let private candidateBox (id : string) (label : string) (chosen : bool) (enabled : bool) (onClick : unit -> unit) : IView =
        Border.create [
            Border.name id
            Border.isEnabled enabled
            Border.opacity (if enabled then 1.0 else 0.4)
            Border.background (brush (if chosen then chosenBackground else idleBackground))
            Border.borderBrush (brush idleBorder)
            Border.borderThickness 1.0
            Border.cornerRadius (CornerRadius 3.0)
            Border.padding (Thickness(10.0, 5.0))
            Border.margin (Thickness(0.0, 0.0, 8.0, 6.0))
            Border.verticalAlignment VerticalAlignment.Center
            Border.child (TextBlock.create [ TextBlock.text label ])
            // `e.Handled <- true` drops FuncUI's duplicate Tunnel|Bubble pass; re-subscribe when the
            // chosen flag flips so a reused box can't keep a stale handler.
            Border.onPointerPressed ((fun e -> e.Handled <- true; onClick ()), SubPatchOptions.OnChangeOf (box chosen))
        ] :> IView

    // -- The inline result area: a tiny rotating-analyzer intensity polyline, or the ellipsometer Ψ/Δ. --
    let private chartWidth = 240.0
    let private chartHeight = 120.0
    let private curveColor = color 30 90 200

    /// Map the (angleDeg, intensity) samples into the fixed chart box: θ across the width (0…360°),
    /// intensity normalized to the curve's own [min, max] across the height (inverted, screen y-down).
    let private chartPoints (samples : (float * float) list) : Point list =
        match samples with
        | [] -> []
        | _ ->
            let intensities = samples |> List.map snd
            let lo = List.min intensities
            let hi = List.max intensities
            let span = if hi - lo > 1e-12 then hi - lo else 1.0
            samples
            |> List.map (fun (deg, i) ->
                let x = chartWidth * (deg / 360.0)
                let y = chartHeight * (1.0 - (i - lo) / span)
                Point(x, y))

    /// The result block under the candidates: the ellipsometer Ψ/Δ text (Phase 4) when present, else the
    /// rotating-analyzer intensity polyline (Phase 3 — the Malus curve), else nothing.
    let private resultBlock (state : State) : IView list =
        match state.psiDelta with
        | Some (psiDeg, deltaDeg) ->
            [ TextBlock.create [
                  TextBlock.name UiIds.psiDelta
                  TextBlock.text (sprintf "Ψ = %.2f°   Δ = %.2f°" psiDeg deltaDeg)
              ] :> IView ]
        | None ->
            match chartPoints state.chart with
            | [] -> []
            | pts ->
                [ Border.create [
                      Border.borderBrush (brush idleBorder)
                      Border.borderThickness 1.0
                      Border.background (brush (color 248 248 248))
                      Border.width chartWidth
                      Border.height chartHeight
                      Border.horizontalAlignment HorizontalAlignment.Left
                      Border.child (
                          Canvas.create [
                              Canvas.width chartWidth
                              Canvas.height chartHeight
                              Canvas.children [
                                  Polyline.create [
                                      Polyline.name UiIds.chart
                                      Polyline.points pts
                                      Polyline.stroke (brush curveColor)
                                      Polyline.strokeThickness 1.5
                                  ]
                              ]
                          ])
                  ] :> IView ]

    /// The Experiments bay — a wrapping row of candidate elements (highlighted when chosen), a readout of
    /// the built experiment, and the inline result (the intensity curve or the ellipsometer Ψ/Δ).
    let view (state : State) (handlers : Handlers) : IView =
        let readoutText =
            if state.experimentName = "" then "Experiment: (choose the element to rotate full circle)"
            else state.experimentName
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 4.0
            StackPanel.children (
                [
                    TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.text readoutText ] :> IView
                    WrapPanel.create [
                        WrapPanel.name UiIds.candidates
                        WrapPanel.orientation Orientation.Horizontal
                        WrapPanel.children (
                            state.candidates
                            |> List.map (fun c ->
                                candidateBox
                                    (UiIds.candidate c.elementId)
                                    c.label
                                    (state.chosenId = Some c.elementId)
                                    state.enabled
                                    (fun () -> handlers.chooseSwept c.elementId)))
                    ] :> IView
                ]
                @ resultBlock state)
        ] :> IView
