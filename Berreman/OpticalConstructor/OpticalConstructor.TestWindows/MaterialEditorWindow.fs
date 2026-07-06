namespace OpticalConstructor.TestWindows

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
type MaterialEditorWindow(materials : MaterialProxy, existing : MaterialEntry option) as this =
    inherit HostWindow()

    do
        this.Title <-
            match existing with
            | Some e -> sprintf "Material editor — %s" e.name
            | None -> "Material editor — new material"
        this.Name <- MaterialEditorView.UiIds.window
        AutomationProperties.SetAutomationId(this, MaterialEditorView.UiIds.window)
        this.Width <- 1150.0
        this.Height <- 980.0
        let context : MaterialEditorView.MaterialEditorContext =
            {
                materials = materials
                requestClose = fun () -> this.Close()
            }
        Program.mkSimple (fun () -> MaterialEditorView.init context existing) MaterialEditorView.update MaterialEditorView.view
        |> Program.withHost this
        |> Program.run
