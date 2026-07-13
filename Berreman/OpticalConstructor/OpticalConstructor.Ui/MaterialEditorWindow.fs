namespace OpticalConstructor.Ui

open Avalonia.Automation
open OpticalConstructor.Controls
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.MaterialLibrary

/// Spec 0033 (023) — the Material editor window (UICOMP_XDUO_0004): a FuncUI `HostWindow`
/// mounting the pure `MaterialEditorView` MVU loop over the Domain `MaterialComplexityEditor`
/// ladder. The composition root: the injected `MaterialEditorContext` routes Save through
/// `MaterialProxy` on the intent's freshness (spec 0038 step 008) — `addMaterial` for a NEW
/// entry (whose `MaterialId` the `MaterialEditorIntent` minted AT WINDOW OPEN),
/// `updateMaterial` for an existing one — closing the window on success; Cancel closes
/// without writing. An entry with `complexity = None` (engine-coded physics) opens
/// VIEW-ONLY: no ladder, no Save affordance. Opened by the workbench verbs through the
/// SVC_XDUO_0001 window launcher, keyed by the intent's id.
type MaterialEditorWindow(materials : MaterialProxy, intent : MaterialEditorView.MaterialEditorIntent, ?categories : CategoryProxy) as this =
    inherit HostWindow()

    // The Elmish dispatch, captured by the init Cmd the moment the loop starts, and the latest
    // model, mirrored on every step — the `OnClosing` chrome gate reads dirtiness off this model
    // (spec 0038 step 033).
    let mutable dispatch : MaterialEditorView.Msg -> unit = ignore
    let mutable latestModel : MaterialEditorView.Model option = None

    do
        this.Title <-
            match intent with
            | MaterialEditorView.EditMaterial e -> $"Material Editor — %s{e.name}"
            | MaterialEditorView.NewMaterial _ -> "Material Editor — new material"
        this.Name <- UiIds.MaterialEditor.window
        AutomationProperties.SetAutomationId(this, UiIds.MaterialEditor.window)
        // Spec 0038 (032): the editor is now a two-pane split (identity + ladder | tabbed preview).
        // Sized so the left ladder pane, narrower than the former full width, still fits its
        // ScrollViewer viewport for a single-aspect ladder without scrolling.
        this.Width <- 1400.0
        this.Height <- 1000.0
        // Spec 0035 (009): the create picker's live catalogue seam. The Main-screen launcher passes
        // the SHARED store (so a rename in the Category editor re-labels this picker); a standalone
        // open (no `categories` argument) defaults to a fresh in-memory catalogue of the built-ins.
        let categoryProxy = defaultArg categories (CategoryProxy.createInMemory (fun _ -> []))
        let context : MaterialEditorView.MaterialEditorContext =
            {
                materials = materials
                categories = categoryProxy
                requestClose = fun () -> this.Close()
            }
        let initModel = MaterialEditorView.init context intent
        latestModel <- Some initModel
        Program.mkProgram
            (fun () -> initModel, Cmd.ofEffect (fun d -> dispatch <- d))
            (fun msg m -> let next = MaterialEditorView.update msg m in latestModel <- Some next; next, Cmd.none)
            MaterialEditorView.view
        |> Program.withHost this
        |> Program.run

    /// Spec 0038 (033): the window-chrome close gate, shared by `OnClosing` and its headless proof
    /// (the OS title-bar X is not reachable through the headless input surface). Returns true — and
    /// dispatches the pure `CancelClicked`, surfacing the discard confirm — when a dirty editor must
    /// NOT close through the chrome; false when the close may proceed. It routes through the SAME
    /// `CancelClicked` the Cancel button hits, so both exits are equally gated.
    member _.ChromeCloseIntercepted() : bool =
        match latestModel with
        | Some m when MaterialEditorView.isDirty m ->
            dispatch MaterialEditorView.CancelClicked
            true
        | _ -> false

    /// Gate only the genuine window chrome (title-bar X — a NON-programmatic close). An app or
    /// launcher `Close()` is programmatic and proceeds untouched (`base.OnClosing` raises the
    /// `Closing` event so the FuncUI host tears down on the real close); Save / Discard / a pristine
    /// Cancel all close through the programmatic `requestClose`.
    override this.OnClosing(e : WindowClosingEventArgs) =
        if not e.IsProgrammatic && this.ChromeCloseIntercepted() then e.Cancel <- true
        else base.OnClosing(e)
