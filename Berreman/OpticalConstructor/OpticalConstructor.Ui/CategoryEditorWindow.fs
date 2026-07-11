namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.MaterialLibrary

/// Spec 0035 (006) — the Category editor window (UICOMP_XDUO_0006): a FuncUI `HostWindow`
/// mounting the pure `CategoryEditorView` MVU loop over the step-4 Domain `CategoryEditor` edit
/// model and the step-5 `CategoryControls` surface. The composition root: the injected
/// `CategoryEditorContext` routes every inline verb and the window Save through the given
/// `CategoryProxy` (`addCategory` / `updateCategory` / `removeCategory`), closing the window on the
/// window-level Save/Cancel. Declared, not wired: no parent view or launcher opens it in this slice.
type CategoryEditorWindow(categories : CategoryProxy) as this =
    inherit HostWindow()

    do
        this.Title <- "Category Editor"
        this.Name <- CategoryEditorView.UiIds.window
        AutomationProperties.SetAutomationId(this, CategoryEditorView.UiIds.window)
        this.Width <- 720.0
        this.Height <- 640.0
        let context : CategoryEditorView.CategoryEditorContext =
            {
                categories = categories
                requestClose = fun () -> this.Close()
            }
        Program.mkSimple (fun () -> CategoryEditorView.init context) CategoryEditorView.update CategoryEditorView.view
        |> Program.withHost this
        |> Program.run
