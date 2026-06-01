/// Synthesis / Fit page view (spec 0024 Part U7 / R-1..R-4, AC-U7.1..AC-U7.3). The
/// fit page runs a long-duration optimization that MUST NOT freeze the UI, MUST report
/// per-iteration progress, MUST be cancellable, and MUST gate accept of a result. The
/// page model, the per-iteration `progressText`, the `canAccept`/`canRevert`/`canCancel`
/// flags, the `acceptConfirmationPrompt` gate, and the `comparisonOverlay` payload all
/// already exist and are frozen (`SynthesisFitPage`, §0.1); this view only renders them
/// and dispatches `SynthesisFitPage.Msg`. The background run itself (the marshaled
/// progress/completion `Cmd`, the host-held `CancellationTokenSource`) is wired in the
/// composition root (`Shell.fs`, §0.4/§0.5), not here.
///
/// Like the slice-002..006 view modules, the panel takes its sub-state (the
/// `SynthesisFitPage.Model`) plus a dispatch — NOT `RootModel`, which lives in
/// `Shell.fs` — so it composes under the root without a module cycle. `Shell` passes
/// `RootMsg.Fit >> dispatch`.
///
/// The fit-vs-measured overlay reuses the slice-002 `ChartHosts.scottPlotHost` (AvaPlot)
/// and the Part H §H.4 `Plot1DView.renderComparison`; it recomputes no geometry and
/// embeds no hosting of its own (Non-requirements).
///
/// New sibling `*View.fs` module per §0.1; authored against the public MIT
/// `Avalonia.FuncUI` 1.6.0 DSL surface (§0.2) — no clone reference.
module OpticalConstructor.Ui.FitView

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.FieldFunctions
open OpticalConstructor.Ui.Charts

/// `SynthesisFitPage` carries the page model, messages, and the Avalonia-free seam
/// helpers this view binds; the alias keeps the references short.
module Fit = OpticalConstructor.Ui.SynthesisFitPage

// ---------------------------------------------------------------------------
// Distinct CTA colours (UX commitment 5): positive (Accept / Confirm) vs negative
// (Cancel a run) vs neutral (Start / Revert).
// ---------------------------------------------------------------------------

let private positiveCta : IBrush = Brushes.SeaGreen :> IBrush
let private negativeCta : IBrush = Brushes.Firebrick :> IBrush
let private neutralCta : IBrush = Brushes.SteelBlue :> IBrush

// ---------------------------------------------------------------------------
// Per-iteration progress line (R-3 / commitment 2). The text comes from the frozen
// `SynthesisFitPage.progressText`; while running with no tick yet it shows a
// running state so the UI is never blank-but-busy.
// ---------------------------------------------------------------------------

let private progressLine (model : Fit.Model) : IView =
    let text =
        match Fit.progressText model with
        | Some t -> t
        | None -> if model.running then "Running…" else "Idle"
    TextBlock.create [ TextBlock.text text ] :> IView

// ---------------------------------------------------------------------------
// Same-row action bar (R-3 / R-4 / commitment 5): a single Start↔Cancel button with
// two states next to the Accept / Revert actions, with a visible gap and distinct
// CTA colours. Accept is gated by `canAccept`, Revert by `canRevert`, and the
// Start↔Cancel button's Cancel state by `canCancel` (= a run is in flight).
// ---------------------------------------------------------------------------

let private actionBar (model : Fit.Model) (dispatch : Fit.Msg -> unit) : IView =
    let running = model.running
    // The single Start↔Cancel control (commitment 5 — one button, two states).
    let startCancel =
        Button.create [
            Button.content (if running then "Cancel" else "Start")
            Button.background (if running then negativeCta else neutralCta)
            Button.foreground (Brushes.White :> IBrush)
            Button.onClick (fun _ -> dispatch (if running then Fit.CancelFit else Fit.StartFit))
        ] :> IView
    let accept =
        Button.create [
            Button.content "Accept"
            Button.isEnabled (Fit.canAccept model)
            Button.background positiveCta
            Button.foreground (Brushes.White :> IBrush)
            Button.onClick (fun _ -> dispatch Fit.RequestAccept)
        ] :> IView
    let revert =
        Button.create [
            Button.content "Revert"
            Button.isEnabled (Fit.canRevert model)
            Button.onClick (fun _ -> dispatch Fit.Revert)
        ] :> IView
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 12.0   // a visible gap between the CTAs (commitment 5)
        StackPanel.children [ startCancel; accept; revert ]
    ] :> IView

// ---------------------------------------------------------------------------
// The accept-confirmation gate (R-4 / commitment 3). Accept is NOT one-click: it
// routes through `acceptConfirmationPrompt`. When an Accept is pending the prompt
// renders same-row with explicit Confirm (dispatch `ConfirmAccept`) / Cancel
// (dispatch `CancelAccept`) controls; otherwise nothing renders.
// ---------------------------------------------------------------------------

let private confirmationGate (model : Fit.Model) (dispatch : Fit.Msg -> unit) : IView list =
    match Fit.acceptConfirmationPrompt model with
    | Some prompt ->
        [ StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.spacing 8.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text prompt; TextBlock.verticalAlignment VerticalAlignment.Center ]
                Button.create [
                    Button.content "Confirm"
                    Button.background positiveCta
                    Button.foreground (Brushes.White :> IBrush)
                    Button.onClick (fun _ -> dispatch Fit.ConfirmAccept)
                ]
                Button.create [
                    Button.content "Cancel"
                    Button.onClick (fun _ -> dispatch Fit.CancelAccept)
                ]
            ]
        ] :> IView ]
    | None -> []

// ---------------------------------------------------------------------------
// Fit-vs-measured overlay (R-4 / commitment 4 / AC-U7.1). On a completed run the
// frozen `comparisonOverlay` supplies the reference (pre-fit / measured) + fitted
// systems and the targets' photometric channels; the overlay is built by the Part H
// §H.4 `Plot1DView.renderComparison` and hosted in the slice-002 `ChartHosts`
// AvaPlot adapter — rebuilt each render so completion refreshes it with no manual
// reload, degrading to the §U1.8 placeholder if no native graphics surface exists.
// ---------------------------------------------------------------------------

let private overlaySection (model : Fit.Model) : IView =
    match Fit.comparisonOverlay model with
    | Some (systems, fns) ->
        let fn = match fns with | f :: _ -> f | [] -> OpticalFunction.R
        let x = Analytics.StandardLightVariables.wavelength200to800Range 12
        let plot = Plot1DView.renderComparison ChartSettings.ChartSettings.defaultValue systems fn x
        Border.create [ Border.height 220.0; Border.child (ChartHosts.scottPlotHost plot) ] :> IView
    | None ->
        TextBlock.create [
            TextBlock.text "Run a fit to compare the fitted result against the measured targets."
        ] :> IView

// ---------------------------------------------------------------------------
// The fit page (R-1): the per-iteration progress line over the Start↔Cancel +
// Accept/Revert action bar, the accept-confirmation gate when pending, and the
// fit-vs-measured overlay. Scrollable so the sections lay out independently of the
// page height.
// ---------------------------------------------------------------------------

let fitPanel (model : Fit.Model) (dispatch : Fit.Msg -> unit) : IView =
    let body =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 8.0
            StackPanel.margin 6.0
            StackPanel.children (
                [ TextBlock.create [ TextBlock.text "Synthesis / Fit"; TextBlock.fontWeight FontWeight.Bold ] :> IView
                  progressLine model
                  actionBar model dispatch ]
                @ confirmationGate model dispatch
                @ [ overlaySection model ])
        ]
    ScrollViewer.create [ ScrollViewer.content body ] :> IView
