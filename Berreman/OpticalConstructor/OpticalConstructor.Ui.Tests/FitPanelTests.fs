/// Headless synthesis/fit-page view tests (spec 0024 Part U7 §U7.1–U7.4, gate
/// `ui-tests`). Trait `Category=ui-tests` (the `ui-tests` gate runs
/// `--filter Category!=ui-smoke`).
///
/// Asserts the Part U7 fit page:
///   • R-1 / R-3 — the page renders the per-iteration progress line and a single
///     Start↔Cancel control; clicking Start dispatches `Fit StartFit`.
///   • R-2 / AC-U7.1 — `Fit StartFit` flips the page to running and attaches a `Cmd`
///     that runs the optimization off-thread, marshals per-iteration `ReportProgress`
///     and a completion `FitCompleted` back onto the UI thread (§0.4), and the
///     completion refreshes the fit-vs-measured overlay with no manual reload.
///   • AC-U7.2 — a cooperative Cancel resolves to `FitFailed "cancelled"`: the run
///     stops, the prior committed system is untouched, and Revert stays available.
///   • R-4 / AC-U7.3 — Accept routes through `acceptConfirmationPrompt` (it dispatches
///     `RequestAccept`, opening the gate; only `ConfirmAccept` replaces the system),
///     and Accept/Revert/Start render same-row.
///
/// The marshaled background `Cmd` is exercised exactly as `ConstructionEditTests`
/// exercises the node-solve `Cmd`: run the effect, pump the dispatcher until the
/// marshaled message lands. The seed target is one the system already satisfies, so a
/// real ALGLIB fit converges in ~1 iteration (deterministic + fast headlessly).
namespace OpticalConstructor.Ui.Tests

open System.Collections.Generic
open System.Threading
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open Berreman.FieldFunctions
open OpticalConstructor.Ui
open OpticalConstructor.Optimization
open OpticalConstructor.Optimization.OptimizationInterface

module FitPanelTests =

    let private seed = fst Shell.init

    let private baseSystem =
        match seed.workspace.project.systems with
        | s :: _ -> s
        | [] -> failwith "seed project must carry at least one system"

    /// One layer-thickness design variable and its start vector.
    let private param = DesignParameters.layerThickness 0 1.0e-6
    let private initial = [| 100.0e-9 |]

    /// A fully-populated fit model: one photometric target the seed system already
    /// satisfies at `initial` (desired = current model value), so the LM fit converges
    /// in ~1 iteration regardless of the start thickness.
    let private fitModel : SynthesisFitPage.Model =
        let probe : MeritFunction.FitTarget =
            {
                quantity = MeritFunction.Photometric OpticalFunction.R
                samplePoint = seed.source.light
                desiredValue = 0.0
                weight = 1.0
                tolerance = 1.0
                kind = MeritFunction.Equality
            }
        let current = MeritFunction.modelValue baseSystem [ param ] initial probe
        let target = { probe with desiredValue = current }
        SynthesisFitPage.init baseSystem [ param ] initial [ target ] LevenbergMarquardt "."

    let private mount (content : Avalonia.FuncUI.Types.IView) : Window =
        let window = Window()
        window.Content <- Component(fun _ctx -> content)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private buttonByContent (window : Window) (label : string) : Button =
        window.GetVisualDescendants()
        |> Seq.choose (fun v -> match v with | :? Button as b -> Some b | _ -> None)
        |> Seq.find (fun b -> string b.Content = label)

    let private hasButton (window : Window) (label : string) : bool =
        window.GetVisualDescendants()
        |> Seq.choose (fun v -> match v with | :? Button as b -> Some b | _ -> None)
        |> Seq.exists (fun b -> string b.Content = label)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``fit page renders the Start control and dispatches StartFit`` () =
        HeadlessSession.run (fun () ->
            // R-1 / R-3: the single Start↔Cancel control renders and clicking it
            // (the Start state, since the page is idle) dispatches StartFit.
            let captured = List<SynthesisFitPage.Msg>()
            let window = mount (FitView.fitPanel fitModel captured.Add)
            Assert.True(hasButton window "Start", "the Start↔Cancel control must render")
            (buttonByContent window "Start").RaiseEvent(RoutedEventArgs(Button.ClickEvent))
            Dispatcher.UIThread.RunJobs()
            Assert.Contains(captured, fun m -> match m with | SynthesisFitPage.StartFit -> true | _ -> false)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``StartFit runs off-thread and marshals per-iteration progress and completion`` () =
        HeadlessSession.run (fun () ->
            // R-2 / AC-U7.1: StartFit flips the page to running and attaches the
            // background-run Cmd; running it marshals ReportProgress + FitCompleted
            // back onto the UI thread.
            let model = { seed with fit = Some fitModel; page = Shell.Page.SynthesisFit }
            let m1, cmd = Shell.update (Shell.RootMsg.Fit SynthesisFitPage.StartFit) model
            Assert.True(m1.fit.Value.running, "StartFit must flip the page to running")
            Assert.NotEmpty(cmd)

            let dispatched = List<Shell.RootMsg>()
            for effect in cmd do effect dispatched.Add
            let completed () =
                dispatched
                |> Seq.exists (fun m -> match m with | Shell.RootMsg.Fit (SynthesisFitPage.FitCompleted _) -> true | _ -> false)
            // Pump the UI thread until the off-thread run posts FitCompleted back.
            let mutable spins = 0
            while not (completed ()) && spins < 500 do
                Dispatcher.UIThread.RunJobs()
                Thread.Sleep 10
                spins <- spins + 1
            Dispatcher.UIThread.RunJobs()

            // Per-iteration progress was marshaled (commitment 2).
            Assert.Contains(dispatched, fun m ->
                match m with | Shell.RootMsg.Fit (SynthesisFitPage.ReportProgress _) -> true | _ -> false)

            // Completion was marshaled; applying it refreshes the result charts with no
            // manual reload — a candidate + report land, so the comparison overlay is
            // now populated (AC-U7.1).
            let completedMsg =
                dispatched
                |> Seq.pick (fun m ->
                    match m with
                    | Shell.RootMsg.Fit (SynthesisFitPage.FitCompleted _ as fm) -> Some (Shell.RootMsg.Fit fm)
                    | _ -> None)
            let m2, _ = Shell.update completedMsg m1
            Assert.False(m2.fit.Value.running, "completion clears the running state")
            Assert.True(SynthesisFitPage.canAccept m2.fit.Value, "completion must produce an acceptable candidate")
            Assert.True((SynthesisFitPage.comparisonOverlay m2.fit.Value).IsSome,
                        "the fit-vs-measured overlay must refresh from the completed run"))

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``cancel during a run stops cooperatively and leaves the prior results`` () =
        // AC-U7.2: StartFit snapshots the working system for Revert; a cooperative
        // cancel resolves to FitFailed "cancelled" — the run stops, the prior committed
        // system is untouched, and Revert stays available.
        let started = SynthesisFitPage.update SynthesisFitPage.StartFit fitModel
        Assert.True(started.running, "the page is running after StartFit")
        Assert.True(SynthesisFitPage.canCancel started, "a Cancel control must be available while running")

        let cancelRequested = SynthesisFitPage.update SynthesisFitPage.CancelFit started
        Assert.True(cancelRequested.cancelRequested, "CancelFit sets the cooperative cancellation flag")

        let cancelled = SynthesisFitPage.update (SynthesisFitPage.FitFailed "cancelled") cancelRequested
        Assert.False(cancelled.running, "the run stops on the cancelled completion")
        Assert.True(cancelled.candidate.IsNone, "a cancelled run produces no candidate")
        Assert.True(System.Object.ReferenceEquals(started.workingSystem, cancelled.workingSystem),
                    "the prior committed system is untouched by a cancel")
        Assert.True(SynthesisFitPage.canRevert cancelled, "the pre-fit Revert remains available")

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``Accept routes through the confirmation gate before replacing the working system`` () =
        HeadlessSession.run (fun () ->
            // Build a completed candidate so Accept is enabled.
            let residual = MeritFunction.buildResidual baseSystem [ param ] fitModel.targets
            let report = FitQuality.reportFrom residual initial (residual initial)
            let withCandidate = SynthesisFitPage.update (SynthesisFitPage.FitCompleted (baseSystem, report)) fitModel
            Assert.True(SynthesisFitPage.canAccept withCandidate, "a completed run yields an acceptable candidate")

            // R-4 / commitment 5: Accept / Revert render same-row with the Start control;
            // clicking Accept dispatches RequestAccept (NOT a one-click replace).
            let captured = List<SynthesisFitPage.Msg>()
            let window = mount (FitView.fitPanel withCandidate captured.Add)
            Assert.True(hasButton window "Accept", "Accept renders")
            Assert.True(hasButton window "Revert", "Revert renders")
            Assert.True(hasButton window "Start", "the Start↔Cancel control renders")
            (buttonByContent window "Accept").RaiseEvent(RoutedEventArgs(Button.ClickEvent))
            Dispatcher.UIThread.RunJobs()
            Assert.Contains(captured, fun m -> match m with | SynthesisFitPage.RequestAccept -> true | _ -> false)
            window.Close()

            // RequestAccept opens the confirmation gate; the gate renders a Confirm
            // control that dispatches ConfirmAccept (commitment 3).
            let pending = SynthesisFitPage.update SynthesisFitPage.RequestAccept withCandidate
            Assert.True((SynthesisFitPage.acceptConfirmationPrompt pending).IsSome, "Accept must open the confirmation gate")
            let captured2 = List<SynthesisFitPage.Msg>()
            let gateWindow = mount (FitView.fitPanel pending captured2.Add)
            (buttonByContent gateWindow "Confirm").RaiseEvent(RoutedEventArgs(Button.ClickEvent))
            Dispatcher.UIThread.RunJobs()
            Assert.Contains(captured2, fun m -> match m with | SynthesisFitPage.ConfirmAccept -> true | _ -> false)
            gateWindow.Close()

            // Only ConfirmAccept replaces the working system; the candidate is consumed.
            let accepted = SynthesisFitPage.update SynthesisFitPage.ConfirmAccept pending
            Assert.False(SynthesisFitPage.canAccept accepted, "the candidate is consumed after a confirmed accept")
            Assert.True((SynthesisFitPage.acceptConfirmationPrompt accepted).IsNone, "the confirmation gate closes after accept"))
