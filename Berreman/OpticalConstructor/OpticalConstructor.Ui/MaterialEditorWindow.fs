namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.MaterialLibrary

/// Spec 0033 (023) — the Material editor window (UICOMP_XDUO_0004): a FuncUI `HostWindow`
/// mounting the pure `MaterialEditorView` MVU loop over the Domain `MaterialComplexityEditor`
/// ladder. The composition root: the injected `MaterialEditorContext` routes Save through
/// `MaterialProxy` — `addMaterial` for a new entry (`existing = None`, minting
/// `MaterialId.create`), `updateMaterial` for an existing one — closing the window on success;
/// Cancel closes without writing. An entry with `complexity = None` (engine-coded physics)
/// opens VIEW-ONLY: no ladder, no Save affordance. Declared, not wired: no parent view or
/// launcher opens it in this slice.
type MaterialEditorWindow(materials : MaterialProxy, existing : MaterialEntry option, ?categories : CategoryProxy) as this =
    inherit HostWindow()

    do
        this.Title <-
            match existing with
            | Some e -> $"Material Editor — %s{e.name}"
            | None -> "Material Editor — new material"
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
        Program.mkSimple (fun () -> MaterialEditorView.init context existing) MaterialEditorView.update MaterialEditorView.view
        |> Program.withHost this
        |> Program.run
