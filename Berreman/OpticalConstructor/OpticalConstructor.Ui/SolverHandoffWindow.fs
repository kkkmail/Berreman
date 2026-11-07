namespace OpticalConstructor.Ui

open Avalonia.Automation
open OpticalConstructor.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain

/// Hosts the inverse collection summary and basic validation over the library and kit measured-data seam;
/// Close routes through the window context (spec 0038 steps 039/047; spec 0050 S4.2).
type SolverHandoffWindow(library : Library.LibraryProxy, experimentData : ExperimentData.ExperimentDataProxy, collection : ExperimentCollectionStore.ExperimentCollectionSnapshot) as this =
    inherit HostWindow()

    do
        this.Title <- "Solver handoff"
        this.Name <- ConstructorUiId.solverHandoffWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.solverHandoffWindow.value)
        this.Width <- 920.0
        this.Height <- 760.0
        // Spec 0046: never larger than the screen it opens on (its content scrolls).
        WindowFit.keepOnScreen this
        let context : SolverHandoffView.SolverHandoffContext =
            {
                library = library
                experimentData = experimentData
                collection = collection
                requestClose = fun () -> this.Close()
            }
        Program.mkSimple (fun () -> context.initSolverHandoff) SolverHandoffView.updateSolverHandoff SolverHandoffView.solverHandoffView
        |> Program.withHost this
        |> Program.run
