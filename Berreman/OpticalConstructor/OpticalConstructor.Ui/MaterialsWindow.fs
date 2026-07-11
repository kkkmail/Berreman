namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.MaterialLibrary
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
type MaterialsWindow(materials : MaterialProxy, categories : CategoryProxy, ?treeAutoBuildThreshold : TreeAutoBuildThreshold) as this =
    inherit HostWindow()

    do
        this.Title <- "Materials"
        this.Name <- MaterialsWindowView.UiIds.window
        AutomationProperties.SetAutomationId(this, MaterialsWindowView.UiIds.window)
        this.Width <- 1150.0
        this.Height <- 800.0
        // Browse-mode opens always `Show` — the Select-state modality switch is never consulted
        // on this path (the step-008 `EditorLaunchers.defaults` precedent), so the baked
        // `defaultValue` is inert here; step 016 composes Select launchers over
        // `AppContext.settings`.
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
            }
        Program.mkSimple (fun () -> MaterialsWindowView.init context) MaterialsWindowView.update MaterialsWindowView.view
        |> Program.withHost this
        |> Program.run
