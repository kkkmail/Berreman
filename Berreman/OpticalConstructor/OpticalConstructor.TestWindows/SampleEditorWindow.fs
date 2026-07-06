namespace OpticalConstructor.TestWindows

open Avalonia.Automation
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library

/// Spec 0033 (022) — the Sample editor window (UICOMP_XDUO_0003): a FuncUI `HostWindow`
/// mounting the pure `SampleEditorView` MVU loop over the step-21 Domain `SampleStackEditor`.
/// The composition root: the material choices are resolved ONCE from
/// `MaterialProxy.listMaterials`, and the injected `SampleEditorContext` routes Save through
/// `SampleProxy` — `addSample` for a new sample (`existing = None`, minting `SampleId.create`),
/// `updateSample` for an existing one — closing the window on success; Cancel closes without
/// writing. Declared, not wired: no parent view or launcher opens it in this slice.
type SampleEditorWindow(materials : MaterialProxy, samples : SampleProxy, existing : Sample option) as this =
    inherit HostWindow()

    do
        this.Title <-
            match existing with
            | Some s -> sprintf "Sample editor — %s" s.name
            | None -> "Sample editor — new sample"
        this.Name <- SampleEditorView.UiIds.window
        AutomationProperties.SetAutomationId(this, SampleEditorView.UiIds.window)
        this.Width <- 1080.0
        this.Height <- 900.0
        let entries =
            match materials.listMaterials () with
            | Ok list -> list
            | Error _ -> []
        let context : SampleEditorView.SampleEditorContext =
            {
                samples = samples
                requestClose = fun () -> this.Close()
            }
        Program.mkSimple (fun () -> SampleEditorView.init context entries existing) SampleEditorView.update SampleEditorView.view
        |> Program.withHost this
        |> Program.run
