namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Elmish
open OpticalConstructor.Controls
open OpticalConstructor.Ui.ImportWizardView
open OpticalConstructor.Ui.TableAndElementRotationView

/// Hosts the column-mapping wizard over a picked file; only its Attach hands `onApplied` a choice (001 §K; spec 0050
/// S9.2).
type ImportWizardWindow(request : ImportWizardRequest, onApplied : ImportWizardApplied -> unit) as this =
    inherit HostWindow()

    do
        this.Title <- "Import data file"
        this.Name <- ConstructorUiId.importWizardWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.importWizardWindow.value)
        AutomationProperties.SetName(this, this.Title)
        this.Width <- 1000.0
        this.Height <- 700.0
        WindowFit.keepOnScreen this
        let context : ImportWizardContext =
            {
                request = request
                onApplied = onApplied
                requestClose = this.Close
            }
        Program.mkProgram
            (fun () -> context.initImportWizard, Cmd.none)
            (fun msg model -> updateImportWizard msg model, Cmd.none)
            importWizardView
        |> Program.withHost this
        |> Program.run

module ImportWizardOpening =

    type KitWindowShell with

        /// The shell whose `openImportWizard` shows an `ImportWizardWindow` owned by `owner`, its Attach dispatching `messages` in order (spec 0050 S9.2).
        member this.withImportWizard (owner : Window) (dispatch : TableAndElementRotationMsg -> unit) : KitWindowShell =
            let openImportWizard (request : ImportWizardRequest) : unit =
                let onApplied (applied : ImportWizardApplied) : unit = applied.messages |> List.iter dispatch
                ImportWizardWindow(request, onApplied).Show owner

            {
                this with openImportWizard = openImportWizard
            }
