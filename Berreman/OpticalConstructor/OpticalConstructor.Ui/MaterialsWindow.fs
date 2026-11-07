namespace OpticalConstructor.Ui

open Avalonia.Automation
open OpticalConstructor.Controls
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Softellect.Sys.Primitives
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Ui.ImportDialogView

/// Browses or selects live materials, launches editors, and previews dropped entry files for import.
/// Retarget replaces the Select session; closing cancels a pending session exactly once.
/// Spec 0038 steps 013/016 (UICOMP_XDUO_0009); spec 0050 S3.2.
type MaterialsWindow(materials : MaterialProxy, categories : CategoryProxy, catalogueImports : CatalogueImportContext, ?mode : LibraryWindowMode<MaterialEntry>, ?treeAutoBuildThreshold : TreeAutoBuildThreshold) as this =
    inherit HostWindow()

    // The Elmish dispatch, captured by the init Cmd the moment the loop starts (Program.run is
    // synchronous in this constructor) — the host's Closed hook and the public Retarget seam
    // dispatch through it into the pure update.
    let mutable dispatch : MaterialsWindowView.MaterialsWindowMsg -> unit = ignore

    do
        this.Title <- "Materials"
        this.Name <- ConstructorUiId.materialsWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.materialsWindow.value)
        this.Width <- 1150.0
        this.Height <- 800.0
        // Spec 0046: never larger than the screen it opens on (its content scrolls).
        WindowFit.keepOnScreen this
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
                catalogueImports = catalogueImports
                openImportDialog =
                    fun decisions ->
                        ImportDialogWindow(catalogueImports, ApplyDroppedEntries decisions,
                            fun applied -> dispatch (MaterialsWindowView.MaterialsImportApplied applied.summary)).Show()
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
            (fun () -> MaterialsWindowView.initMaterialsWindow context (defaultArg mode Browse), Cmd.ofEffect (fun d -> dispatch <- d))
            (fun msg m -> MaterialsWindowView.updateMaterialsWindow msg m, Cmd.none)
            MaterialsWindowView.materialsWindowView
        |> Program.withHost this
        |> Program.run
        EntryFileDrop.acceptEntryFiles this (fun files -> dispatch (MaterialsWindowView.DropMaterialEntryFiles files))
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

    /// Decide dropped entry files and open their import choices without applying them.
    member _.DropEntryFiles (files : FileName list) : unit =
        dispatch (MaterialsWindowView.DropMaterialEntryFiles files)
