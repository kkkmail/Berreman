namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Elmish
open OpticalConstructor.Controls
open OpticalConstructor.Ui.ItemModelChooserView
open OpticalConstructor.Ui.TableAndElementRotationView

/// Hosts the library's model chooser in a window as large as its screen allows; only Choose hands the request its offer
/// (spec 0051 `.manual/007` 4.3).
type ItemModelChooserWindow(request : ItemModelChooserRequest) as this =
    inherit HostWindow()

    do
        this.Title <- "Choose from the library"
        this.Name <- ConstructorUiId.itemModelChooserWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.itemModelChooserWindow.value)
        AutomationProperties.SetName(this, this.Title)
        // As large as the screen allows: `keepOnScreen` shrinks it to the working area (`006-2`).
        this.Width <- 1600.0
        this.Height <- 1200.0
        WindowFit.keepOnScreen this
        let confirm (offer : OpticalConstructor.Domain.Kit.ItemModelChoice) : unit =
            request.onChosen offer
            this.Close()
        Program.mkProgram
            (fun () -> ItemModelChooserModel.ofRequest request, Cmd.none)
            (fun msg model -> updateItemModelChooser msg model, Cmd.none)
            (fun model dispatch -> itemModelChooserView model dispatch confirm this.Close)
        |> Program.withHost this
        |> Program.run

module ItemModelChooserOpening =

    type KitWindowShell with

        /// The shell whose `chooseItemModel` shows an `ItemModelChooserWindow` owned by `owner` (spec 0051 `.manual/007`
        /// 4.3).
        member this.withItemModelChooser (owner : Window) : KitWindowShell =
            {
                this with chooseItemModel = fun request -> ItemModelChooserWindow(request).Show owner
            }
