namespace OpticalConstructor.Ui

open Avalonia.Automation
open OpticalConstructor.Controls
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.MaterialLibrary

/// Material editor over caller-supplied `materials` and `categories` stores.
/// Save adds or updates the intent's entry and closes on success; Cancel closes without writing.
/// Entries whose physics is not editable open view-only, with no ladder or Save affordance (spec 0050 S11.1).
type MaterialEditorWindow(materials : MaterialProxy, intent : MaterialEditorView.MaterialEditorIntent, categories : CategoryProxy) as this =
    inherit HostWindow()

    // The Elmish dispatch, captured by the init Cmd the moment the loop starts, and the latest
    // model, mirrored on every step — the `OnClosing` chrome gate reads dirtiness off this model
    // (spec 0038 step 033).
    let mutable dispatch : MaterialEditorView.MaterialEditorMsg -> unit = ignore
    let mutable latestModel : MaterialEditorView.MaterialEditorModel option = None

    do
        this.Title <-
            match intent with
            | MaterialEditorView.EditMaterial e -> $"Material Editor — %s{e.name.value}"
            | MaterialEditorView.NewMaterial _ -> "Material Editor — new material"
        this.Name <- ConstructorUiId.materialEditorWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.materialEditorWindow.value)
        // Spec 0038 (032): the editor is now a two-pane split (identity + ladder | tabbed preview).
        // Sized so the left ladder pane, narrower than the former full width, still fits its
        // ScrollViewer viewport for a single-aspect ladder without scrolling.
        this.Width <- 1400.0
        this.Height <- 1000.0
        // Spec 0046: never larger than the screen it opens on (its content scrolls).
        WindowFit.keepOnScreen this
        let context : MaterialEditorView.MaterialEditorContext =
            {
                materials = materials
                categories = categories
                requestClose = fun () -> this.Close()
            }
        let initModel = MaterialEditorView.initMaterialEditor context intent
        latestModel <- Some initModel
        Program.mkProgram
            (fun () -> initModel, Cmd.ofEffect (fun d -> dispatch <- d))
            (fun msg m -> let next = MaterialEditorView.updateMaterialEditor msg m in latestModel <- Some next; next, Cmd.none)
            MaterialEditorView.materialEditorView
        |> Program.withHost this
        |> Program.run

    /// Spec 0038 (033): the window-chrome close gate, shared by `OnClosing` and its headless proof
    /// (the OS title-bar X is not reachable through the headless input surface). Returns true — and
    /// dispatches the pure `CancelClicked`, surfacing the discard confirm — when a dirty editor must
    /// NOT close through the chrome; false when the close may proceed. It routes through the SAME
    /// `CancelClicked` the Cancel button hits, so both exits are equally gated.
    member _.ChromeCloseIntercepted() : bool =
        match latestModel with
        | Some m when m.isDirty ->
            dispatch MaterialEditorView.MaterialCancelClicked
            true
        | _ -> false

    /// Gate only the genuine window chrome (title-bar X — a NON-programmatic close). An app or
    /// launcher `Close()` is programmatic and proceeds untouched (`base.OnClosing` raises the
    /// `Closing` event so the FuncUI host tears down on the real close); Save / Discard / a pristine
    /// Cancel all close through the programmatic `requestClose`.
    override this.OnClosing(e : WindowClosingEventArgs) =
        if not e.IsProgrammatic && this.ChromeCloseIntercepted() then e.Cancel <- true
        else base.OnClosing(e)
