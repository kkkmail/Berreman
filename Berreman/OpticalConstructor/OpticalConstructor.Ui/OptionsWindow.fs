namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Elmish
open OpticalConstructor.Controls
open OpticalConstructor.Ui.GuideView
open OpticalConstructor.Ui.OptionsView

/// Hosts the Options window over its proxy; its update closes it on Cancel or on a confirmed discard (spec 0051
/// `.manual/007` 1.5).
type OptionsWindow(proxy : OptionsProxy) as this =
    inherit HostWindow()

    do
        this.Title <- "Options"
        this.Name <- ConstructorUiId.optionsWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.optionsWindow.value)
        AutomationProperties.SetName(this, this.Title)
        this.Width <- 520.0
        this.Height <- 300.0
        WindowFit.keepOnScreen this
        let update (msg : OptionsMsg) (model : OptionsModel) : OptionsModel =
            match updateOptions msg model with
            | OptionsStayOpen next -> next
            | OptionsClose ->
                this.Close()
                model
        Program.mkSimple (fun () -> OptionsModel.ofProxy proxy) update optionsView
        |> Program.withHost this
        |> Program.run

/// Hosts the step-by-step guides (spec 0051 `.manual/007` 9.2).
type GuideWindow(first : GuideKind) as this =
    inherit HostWindow()

    do
        this.Title <- "Guide"
        this.Name <- ConstructorUiId.guideWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.guideWindow.value)
        AutomationProperties.SetName(this, this.Title)
        this.Width <- 820.0
        this.Height <- 720.0
        WindowFit.keepOnScreen this
        Program.mkSimple (fun () -> first) (fun (kind : GuideKind) (_ : GuideKind) -> kind) (fun kind dispatch -> guideView kind dispatch)
        |> Program.withHost this
        |> Program.run
