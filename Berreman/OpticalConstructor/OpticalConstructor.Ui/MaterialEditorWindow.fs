namespace OpticalConstructor.Ui

open Avalonia.Automation
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

    do
        this.Title <-
            match intent with
            | MaterialEditorView.EditMaterial e -> $"Material Editor — %s{e.name}"
            | MaterialEditorView.NewMaterial _ -> "Material Editor — new material"
        this.Name <- MaterialEditorView.UiIds.window
        AutomationProperties.SetAutomationId(this, MaterialEditorView.UiIds.window)
        this.Width <- 1150.0
        this.Height <- 980.0
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
        Program.mkSimple (fun () -> MaterialEditorView.init context intent) MaterialEditorView.update MaterialEditorView.view
        |> Program.withHost this
        |> Program.run
