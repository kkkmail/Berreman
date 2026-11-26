namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Elmish
open OpticalConstructor.Controls
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Ui.MaterialSourcesView

/// The Online sources surface in its own window, single-instance under `MaterialSourcesWindowKey`: search, pull and
/// Update snapshot, each pull's caption told to `pullsApplied`; closing it cancels a running update (spec 0050 S13.1).
type MaterialSourcesWindow(imports : CatalogueImportContext, pullsApplied : ControlCaption -> unit) as this =
    inherit HostWindow()

    // Captured by the init Cmd (Program.run is synchronous here), as MaterialsWindow does: the update's callbacks and the
    // Closed hook dispatch through it.
    let mutable dispatch : MaterialSourcesMsg -> unit = ignore

    do
        this.Title <- "Online sources"
        this.Name <- ConstructorUiId.materialSourcesWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.materialSourcesWindow.value)
        AutomationProperties.SetName(this, this.Title)
        this.Width <- 720.0
        this.Height <- 640.0
        WindowFit.keepOnScreen this
        let context : MaterialSourcesContext =
            {
                imports = imports
                // Each callback reads `dispatch` when it is called, after the init Cmd set it (spec 0050 S13.1).
                jobs =
                    SourceSnapshotJobHost.onUiThread
                        (fun progress -> dispatch (SourceSnapshotProgressed progress))
                        (fun ended -> dispatch (SourceSnapshotEnded ended))
                pullsApplied = pullsApplied
            }
        Program.mkProgram
            (fun () -> context.initMaterialSources, Cmd.ofEffect (fun d -> dispatch <- d))
            (fun msg m -> updateMaterialSources msg m, Cmd.none)
            materialSourcesView
        |> Program.withHost this
        |> Program.run
        // A closed window leaves no update running: the update stops before its next file (spec 0050 S13.1).
        this.Closed.Add (fun _ -> dispatch CancelSourceSnapshot)
