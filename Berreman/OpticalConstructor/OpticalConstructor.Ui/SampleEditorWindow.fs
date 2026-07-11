namespace OpticalConstructor.Ui

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
/// `SampleProxy` — `addSample` for a NEW sample (either NEW `SampleEditorIntent`, whose
/// `SampleId` was minted AT WINDOW OPEN — spec 0038 step 008), `updateSample` for an existing
/// one — closing the window on success; Cancel closes without writing. The
/// `SampleEditorIntent` (spec 0035 step 014) selects the open mode: a blank new sample, a new
/// sample pre-seeded with a foldable starter multilayer period, or an existing sample updated
/// in place.
type SampleEditorWindow(materials : MaterialProxy, samples : SampleProxy, intent : SampleEditorView.SampleEditorIntent) as this =
    inherit HostWindow()

    do
        this.Title <-
            match intent with
            | SampleEditorView.EditSample s -> $"Sample Editor — %s{s.name}"
            | SampleEditorView.NewSeededMultilayer _ -> "Sample Editor — new multilayer"
            | SampleEditorView.NewBlankSample _ -> "Sample Editor — new sample"
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
        Program.mkSimple (fun () -> SampleEditorView.init context entries intent) SampleEditorView.update SampleEditorView.view
        |> Program.withHost this
        |> Program.run
