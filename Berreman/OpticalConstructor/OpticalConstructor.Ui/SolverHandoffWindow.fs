namespace OpticalConstructor.Ui

open Avalonia.Automation
open OpticalConstructor.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain

/// Spec 0038 Part L (step 039) — the SolverHandoffWindow (UICOMP_XDUO_0011): a FuncUI `HostWindow`
/// mounting the pure `SolverHandoffView` MVU loop over the inverse flow's TERMINAL screen. The
/// composition root: the injected read-only `LibraryProxy` (detector-kind resolution) and the
/// `ExperimentDataProxy` (the step-36 measured-data load seam) validate the RECEIVED collection
/// snapshot; the window Close routes through the context's `requestClose = this.Close`. The window is a
/// single-instance one under `SolverHandoffWindowKey` (`WindowLauncher`) — but this step only DECLARES
/// the component (its surface + a headless test); the composition-root wiring that opens it from the
/// inverse constructor through the launcher lands at step 047 (WIRE_UI). Basic validation only — NO
/// solver math and NO normalization run here; the data is gathered raw for the future solver.
type SolverHandoffWindow(library : Library.LibraryProxy, experimentData : ExperimentData.ExperimentDataProxy, collection : ExperimentCollectionStore.ExperimentCollectionSnapshot) as this =
    inherit HostWindow()

    do
        this.Title <- "Solver handoff"
        this.Name <- UiIds.Handoff.window
        AutomationProperties.SetAutomationId(this, UiIds.Handoff.window)
        this.Width <- 920.0
        this.Height <- 760.0
        let context : SolverHandoffView.SolverHandoffContext =
            {
                library = library
                experimentData = experimentData
                collection = collection
                requestClose = fun () -> this.Close()
            }
        Program.mkSimple (fun () -> SolverHandoffView.init context) SolverHandoffView.update SolverHandoffView.view
        |> Program.withHost this
        |> Program.run
