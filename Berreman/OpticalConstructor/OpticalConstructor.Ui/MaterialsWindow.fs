namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings

/// Spec 0038 Part E (step 013) — the Materials window (UICOMP_XDUO_0009): a FuncUI `HostWindow`
/// mounting the pure `MaterialsWindowView` MVU loop — the step-012 `FacetedTreeControls` over
/// the step-011 material facets, a view panel, and the Add / Edit / Remove / Categories… verbs
/// — over the injected app-scope material + category stores. The composition root: the
/// context's launchers open the Material / Category editors through the step-008 SVC_XDUO_0001
/// `WindowLauncher` under the editor keys (`MaterialEditorKey` — an Add-minted or edited
/// entity id — and the single-instance `CategoryEditorKey`), Browse-mode over the ONE
/// host-layer `WindowRegistry`, so an editor opened from THIS window and the same editor
/// opened from anywhere else meet in one open-or-activate space. The window itself is opened
/// single-instance under `MaterialsWindowKey` by ITS callers (the workbench's `Materials…`
/// strip button through `EditorLaunchers.defaults`; the launcher-form button lands in step 45).
/// Step 016: `mode` opens the window Browse (the default) or Select — the SAME window with the
/// Select/Close pair and the fixed-constraint banner; `Retarget` is the launcher's re-target
/// seam (a Select-state open of the LIVE instance re-points its session), and the `Closed`
/// hook cancels a still-pending session exactly once (the title-bar X and a staleness
/// `Close()` from the requesting surface both land there).
type MaterialsWindow(materials : MaterialProxy, categories : CategoryProxy, ?mode : LibraryWindowMode<MaterialEntry>, ?treeAutoBuildThreshold : TreeAutoBuildThreshold) as this =
    inherit HostWindow()

    // The Elmish dispatch, captured by the init Cmd the moment the loop starts (Program.run is
    // synchronous in this constructor) — the host's Closed hook and the public Retarget seam
    // dispatch through it into the pure update.
    let mutable dispatch : MaterialsWindowView.Msg -> unit = ignore

    do
        this.Title <- "Materials"
        this.Name <- MaterialsWindowView.UiIds.window
        AutomationProperties.SetAutomationId(this, MaterialsWindowView.UiIds.window)
        this.Width <- 1150.0
        this.Height <- 800.0
        // Browse-mode opens always `Show` — the Select-state modality switch is never consulted
        // on this path (the step-008 `EditorLaunchers.defaults` precedent), so the baked
        // `defaultValue` is inert here; the step-019 sample-editor picking composes its own
        // `SelectOpen` launcher (`SampleEditorWindow.openMaterialsSelect`) to open THIS window
        // in Select state (step 47 threads the app-configured modality).
        let openBrowse (key : WindowLauncher.WindowKey) (build : unit -> Window) : unit =
            let launcher =
                WindowLauncher.WindowLauncher.create
                    (fun (_ : WindowLauncher.WindowKey) -> build () |> Ok)
                    SelectWindowModality.defaultValue
                    WindowLauncher.BrowseOpen
            launcher.openOrActivate key |> ignore
        let context : MaterialsWindowView.MaterialsWindowContext =
            {
                materials = materials
                categories = categories
                // The Domain default unless a caller threads the app-configured value in (the
                // step-008 defaults precedent — step 47 owns the composition acceptance).
                treeAutoBuildThreshold = defaultArg treeAutoBuildThreshold TreeAutoBuildThreshold.defaultValue
                openMaterialEditor =
                    fun intent ->
                        let key =
                            match intent with
                            | MaterialEditorView.NewMaterial mintedId -> WindowLauncher.MaterialEditorKey mintedId
                            | MaterialEditorView.EditMaterial entry -> WindowLauncher.MaterialEditorKey entry.id
                        openBrowse key (fun () -> MaterialEditorWindow(materials, intent, categories = categories) :> Window)
                openCategoryEditor =
                    fun () ->
                        openBrowse WindowLauncher.CategoryEditorKey (fun () -> CategoryEditorWindow(categories) :> Window)
                requestClose = this.Close
            }
        Program.mkProgram
            (fun () -> MaterialsWindowView.init context (defaultArg mode Browse), Cmd.ofEffect (fun d -> dispatch <- d))
            (fun msg m -> MaterialsWindowView.update msg m, Cmd.none)
            MaterialsWindowView.view
        |> Program.withHost this
        |> Program.run
        // ANY close path funnels here (title-bar X, a staleness Close(), the Select/Close
        // verbs' requestClose): a still-pending Select session cancels exactly once — the
        // verbs flip the mode to Browse in the same update that fires their callback, so the
        // queued dismissal is a no-op after them (MaterialsWindowView.SelectDismissed).
        this.Closed.Add (fun _ -> dispatch MaterialsWindowView.SelectDismissed)

    /// The launcher's re-target seam (step 016): a Select-state open that found THIS window
    /// live re-points its session at the new context — the superseded session is cancelled by
    /// the pure update; the window instance stays (single-instance semantics).
    member _.Retarget (context : SelectionContext<MaterialEntry>) : unit =
        dispatch (MaterialsWindowView.RetargetSelect context)
