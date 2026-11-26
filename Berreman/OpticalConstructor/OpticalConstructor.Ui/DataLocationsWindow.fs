namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Elmish
open OpticalConstructor.Controls
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Ui.DataLocationsView

/// The Data locations screen in its own window, single-instance under `DataLocationsWindowKey`: every location with
/// its path and Open folder (001 §G, spec 0050 S5.3).
type DataLocationsWindow(locations : DataLocations, explorer : ExplorerProxy) as this =
    inherit HostWindow()

    do
        this.Title <- "Data locations"
        this.Name <- ConstructorUiId.dataLocationsWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.dataLocationsWindow.value)
        AutomationProperties.SetName(this, this.Title)
        this.Width <- 720.0
        this.Height <- 460.0
        WindowFit.keepOnScreen this
        let context : DataLocationsContext =
            {
                locations = locations
                explorer = explorer
            }
        Program.mkSimple (fun () -> context.initDataLocations) updateDataLocations dataLocationsView
        |> Program.withHost this
        |> Program.run
