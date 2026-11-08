namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Elmish
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Ui.KitOpening
open OpticalConstructor.Ui.KitStartView
open OpticalConstructor.Ui.TableAndElementRotationView

/// The kit start screen over the app scope: a chosen kit opens in the constructor of `mode`, through the import dialog
/// when its snapshot needs a decision, and the screen closes (001 §F, spec 0050 S5.2).
type KitStartWindow(context : AppContext, mode : ConstructorMode, pickerFor : TopLevel -> KitFolderPicker, showConstructor : ConstructorMode -> OpenedKit -> unit) as this =
    inherit HostWindow()

    // Captured by the init Cmd (Program.run is synchronous here), as LibraryWindow does: a failed re-binding after the
    // import dialog reaches the update through it.
    let mutable dispatch : KitStartMsg -> unit = ignore

    do
        this.Title <-
            match mode with
            | ForwardConstructor -> "Open a kit — Main"
            | InverseConstructor -> "Open a kit — Inverse"
        this.Name <- ConstructorUiId.kitStartWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.kitStartWindow.value)
        AutomationProperties.SetName(this, this.Title)
        this.Width <- 720.0
        this.Height <- 560.0
        WindowFit.keepOnScreen this
        // A cancelled import dialog has released the kit and leaves the screen open (spec 0050 S4.3, step 0174).
        let showKit (opened : OpenedKit) : unit =
            opened.resolveAndShow
                context.catalogueImports
                (fun shown ->
                    showConstructor mode shown
                    this.Close())
                (fun error -> dispatch (KitOpenFailed error))
        let kitStartContext : KitStartContext =
            {
                kitHistory = context.kitHistory
                limit = context.settings.kitHistoryLimit
                clock = ClockProvider.system
                author = KitAuthor System.Environment.UserName
                imports = context.catalogueImports
                picker = pickerFor this
                showKit = showKit
            }
        Program.mkProgram
            (fun () -> initKitStart kitStartContext mode, Cmd.ofEffect (fun d -> dispatch <- d))
            (fun msg m -> updateKitStart msg m, Cmd.none)
            kitStartView
        |> Program.withHost this
        |> Program.run
