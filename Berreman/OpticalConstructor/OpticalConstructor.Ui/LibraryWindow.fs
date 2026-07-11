namespace OpticalConstructor.Ui

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.WorkbenchSettings

/// Spec 0038 Part F (step 015) — the Library window (UICOMP_XDUO_0010): a FuncUI `HostWindow`
/// mounting the pure `LibraryWindowView` MVU loop — the step-012 `FacetedTreeControls` over the
/// step-011 library facet catalogue (seeded default representation "By kind"), a view panel,
/// and the Add sample / Make multilayer / Edit / Remove verbs — over the injected read-only
/// `LibraryProxy` (the preset entries) and the app-scope samples + materials stores. The
/// composition root: the context's launcher opens the Sample editor through the step-008
/// SVC_XDUO_0001 `WindowLauncher` under `SampleEditorKey` (an Add-minted or edited entity id),
/// Browse-mode over the ONE host-layer `WindowRegistry`, so an editor opened from THIS window
/// and the same editor opened from anywhere else meet in one open-or-activate space. The window
/// itself is opened single-instance under `LibraryWindowKey` by ITS callers (the workbench's
/// `Library…` strip button through `EditorLaunchers.defaults`; the launcher-form button lands
/// in step 45; the Browse+Select mode DU lands in step 016).
type LibraryWindow(library : LibraryProxy, samples : SampleProxy, materials : MaterialProxy, ?treeAutoBuildThreshold : TreeAutoBuildThreshold, ?thicknessBucketCap : ThicknessBucketCap) as this =
    inherit HostWindow()

    do
        this.Title <- "Library"
        this.Name <- LibraryWindowView.UiIds.window
        AutomationProperties.SetAutomationId(this, LibraryWindowView.UiIds.window)
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
        let context : LibraryWindowView.LibraryWindowContext =
            {
                library = library
                samples = samples
                materials = materials
                // The Domain defaults unless a caller threads the app-configured values in (the
                // step-008 defaults precedent — step 47 owns the composition acceptance).
                treeAutoBuildThreshold = defaultArg treeAutoBuildThreshold TreeAutoBuildThreshold.defaultValue
                thicknessBucketCap = defaultArg thicknessBucketCap ThicknessBucketCap.defaultValue
                openSampleEditor =
                    fun intent ->
                        let key =
                            match intent with
                            | SampleEditorView.NewBlankSample mintedId
                            | SampleEditorView.NewSeededMultilayer mintedId -> WindowLauncher.SampleEditorKey mintedId
                            | SampleEditorView.EditSample sample -> WindowLauncher.SampleEditorKey sample.id
                        openBrowse key (fun () -> SampleEditorWindow(materials, samples, intent) :> Window)
            }
        Program.mkSimple (fun () -> LibraryWindowView.init context) LibraryWindowView.update LibraryWindowView.view
        |> Program.withHost this
        |> Program.run
