namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings

/// Spec 0033 (022) — the Sample editor window (UICOMP_XDUO_0003): a FuncUI `HostWindow`
/// mounting the pure `SampleEditorView` MVU loop over the step-21 Domain `SampleStackEditor`.
/// The composition root: the material list is resolved from `MaterialProxy.listMaterials` at
/// open and RE-QUERIED on every Select-session return and window activation (spec 0038 step
/// 019 — the load-once snapshot is gone; live cross-window notifications stay out of scope);
/// the injected `SampleEditorContext` routes Save through `SampleProxy` — `addSample` for a
/// NEW sample (either NEW `SampleEditorIntent`, whose `SampleId` was minted AT WINDOW OPEN —
/// spec 0038 step 008), `updateSample` for an existing one — closing the window on success;
/// Cancel closes without writing. The `SampleEditorIntent` (spec 0035 step 014) selects the
/// open mode: a blank new sample, a new sample pre-seeded with a foldable starter multilayer
/// period, or an existing sample updated in place. The per-layer Choose material… verb's
/// `openMaterialsSelect` composes HERE (step 019) over the step-008 SVC_XDUO_0001
/// `WindowLauncher` under `MaterialsWindowKey`: a LIVE Materials window is RE-TARGETED
/// through its step-016 `Retarget` seam, a missing one opens fresh in Select state, shown per
/// the step-005 modality switch (the Domain default unless the caller threads the
/// app-configured value — step 47 owns the composition acceptance).
type SampleEditorWindow(materials : MaterialProxy, samples : SampleProxy, categories : CategoryProxy, intent : SampleEditorView.SampleEditorIntent, ?selectWindowModality : SelectWindowModality) as this =
    inherit HostWindow()

    // The Elmish dispatch, captured by the init Cmd the moment the loop starts (Program.run is
    // synchronous in this constructor) — the host's Activated hook dispatches the step-019
    // material re-query through it into the pure update.
    let mutable dispatch : SampleEditorView.Msg -> unit = ignore
    // The latest model, mirrored on every step — the `OnClosing` chrome gate reads dirtiness off
    // it (spec 0038 step 033).
    let mutable latestModel : SampleEditorView.Model option = None

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
            match materials.listMaterials ActiveOnly with
            | Ok list -> list
            | Error _ -> []
        // The per-layer Choose material… verb's Select-state open (spec 0038 step 019): the
        // SAME registry key as every other Materials-window open, so this verb and the
        // workbench strip button meet in one open-or-activate space — a LIVE window is
        // re-pointed at the new session through the step-016 `Retarget` seam (the superseded
        // session is cancelled by its pure update), a missing one opens fresh in Select state.
        let openMaterialsSelect (owner : Window) (selectContext : SelectionContext<MaterialEntry>) : unit =
            let retargetWindow (live : Window) : unit =
                match live with
                | :? MaterialsWindow as materialsWindow -> materialsWindow.Retarget selectContext
                | _ -> ()
            let launcher =
                WindowLauncher.WindowLauncher.create
                    (fun (_ : WindowLauncher.WindowKey) -> MaterialsWindow(materials, categories, mode = Select selectContext) :> Window |> Ok)
                    (defaultArg selectWindowModality SelectWindowModality.defaultValue)
                    (WindowLauncher.SelectOpen (owner, retargetWindow))
            // The typed open error is deliberately dropped at this unit seam — a failed open
            // leaves no window, exactly the user-visible outcome (the EditorLaunchers shape).
            launcher.openOrActivate WindowLauncher.MaterialsWindowKey |> ignore
        let context : SampleEditorView.SampleEditorContext =
            {
                materials = materials
                samples = samples
                openMaterialsSelect = openMaterialsSelect
                requestClose = fun () -> this.Close()
            }
        let initModel = SampleEditorView.init context entries intent
        latestModel <- Some initModel
        Program.mkProgram
            (fun () -> initModel, Cmd.ofEffect (fun d -> dispatch <- d))
            (fun msg m -> let next = SampleEditorView.update msg m in latestModel <- Some next; next, Cmd.none)
            SampleEditorView.view
        |> Program.withHost this
        |> Program.run
        // Window activation re-queries the material list (spec 0038 step 019): a material
        // saved through another window appears on the next activation — the second of the two
        // re-query triggers (the first is a Select-session return, wired in the view's
        // session callbacks).
        this.Activated.Add (fun _ -> dispatch SampleEditorView.RefreshMaterials)

    /// Spec 0038 (033): the window-chrome close gate, shared by `OnClosing` and its headless proof
    /// (the OS title-bar X is not reachable through the headless input surface). Returns true — and
    /// dispatches the pure `CancelClicked`, surfacing the discard confirm — when a dirty editor must
    /// NOT close through the chrome; false when the close may proceed. It routes through the SAME
    /// `CancelClicked` the Cancel button hits, so both exits are equally gated.
    member _.ChromeCloseIntercepted() : bool =
        match latestModel with
        | Some m when SampleEditorView.isDirty m ->
            dispatch SampleEditorView.CancelClicked
            true
        | _ -> false

    /// Gate only the genuine window chrome (title-bar X — a NON-programmatic close). An app or
    /// launcher `Close()` is programmatic and proceeds untouched (`base.OnClosing` raises the
    /// `Closing` event so the FuncUI host tears down on the real close); Save / Discard / a pristine
    /// Cancel all close through the programmatic `requestClose`.
    override this.OnClosing(e : WindowClosingEventArgs) =
        if not e.IsProgrammatic && this.ChromeCloseIntercepted() then e.Cancel <- true
        else base.OnClosing(e)
