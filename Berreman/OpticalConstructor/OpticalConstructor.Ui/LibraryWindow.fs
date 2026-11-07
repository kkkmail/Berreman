namespace OpticalConstructor.Ui

open Avalonia.Automation
open OpticalConstructor.Controls
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.LibraryBackup
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.WindowMode
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Ui.ImportDialogView
open Softellect.Sys.Primitives

/// Hosts the live Library in Browse or Select mode, with shared stores and editor launchers
/// (spec 0038 Part F). Dropped entry files and pending registrations open import choices;
/// only Apply writes them (spec 0050 S3.2).
type LibraryWindow(library : LibraryProxy, samples : SampleProxy, sources : SourceProxy, materials : MaterialProxy, categories : CategoryProxy, libraryBackups : LibraryBackupProxy, catalogueImports : CatalogueImportContext, ?mode : LibraryWindowMode<LibraryEntry>, ?treeAutoBuildThreshold : TreeAutoBuildThreshold, ?thicknessBucketCap : ThicknessBucketCap) as this =
    inherit HostWindow()

    // The Elmish dispatch, captured by the init Cmd the moment the loop starts (Program.run is
    // synchronous in this constructor) — the host's Closed hook and the public Retarget seam
    // dispatch through it into the pure update.
    let mutable dispatch : LibraryWindowView.LibraryWindowMsg -> unit = ignore

    do
        this.Title <- "Library"
        this.Name <- ConstructorUiId.libraryWindow.value
        AutomationProperties.SetAutomationId(this, ConstructorUiId.libraryWindow.value)
        this.Width <- 1150.0
        this.Height <- 800.0
        // Spec 0046: never larger than the screen it opens on (its content scrolls).
        WindowFit.keepOnScreen this
        // Browse-mode opens always `Show` — the Select-state modality switch is never consulted
        // on this path (the step-008 `EditorLaunchers.defaults` precedent), so the baked
        // `defaultValue` is inert here; the step-017 Selector flow composes `SelectOpen`
        // launchers over `AppContext.settings` to open THIS window in Select state.
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
                categories = categories
                libraryBackups = libraryBackups
                catalogueImports = catalogueImports
                openImportDialog =
                    fun request ->
                        ImportDialogWindow(catalogueImports, request,
                            fun applied -> dispatch (LibraryWindowView.LibraryImportApplied applied)).Show()
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
                        openBrowse key (fun () -> SampleEditorWindow(materials, samples, categories, catalogueImports, intent) :> Window)
                // Spec 0046: the light-source editor, keyed by the source's entry id (a new source's id
                // was minted by the Add source dispatch), Browse-mode over the same registry.
                sources = sources
                openSourceEditor =
                    fun intent ->
                        let key =
                            match intent with
                            | LightSourceEditorView.NewSource mintedId -> WindowLauncher.SourceEditorKey mintedId
                            | LightSourceEditorView.EditSource source -> WindowLauncher.SourceEditorKey source.id
                        openBrowse key (fun () -> LightSourceEditorWindow(sources, intent) :> Window)
                requestClose = this.Close
            }
        Program.mkProgram
            (fun () -> LibraryWindowView.initLibraryWindow context (defaultArg mode Browse), Cmd.ofEffect (fun d -> dispatch <- d))
            (fun msg m -> LibraryWindowView.updateLibraryWindow msg m, Cmd.none)
            LibraryWindowView.libraryWindowView
        |> Program.withHost this
        |> Program.run
        EntryFileDrop.acceptEntryFiles this (fun files -> dispatch (LibraryWindowView.DropLibraryEntryFiles files))
        // ANY close path funnels here (title-bar X, a staleness Close(), the Select/Close
        // verbs' requestClose): a still-pending Select session cancels exactly once — the
        // verbs flip the mode to Browse in the same update that fires their callback, so the
        // queued dismissal is a no-op after them (LibraryWindowView.LibrarySelectDismissed).
        this.Closed.Add (fun _ -> dispatch LibraryWindowView.LibrarySelectDismissed)

    /// The launcher's re-target seam (step 016): a Select-state open that found THIS window
    /// live re-points its session at the new context — the superseded session is cancelled by
    /// the pure update; the window instance stays (single-instance semantics).
    member _.Retarget (context : SelectionContext<LibraryEntry>) : unit =
        dispatch (LibraryWindowView.RetargetLibrarySelect context)

    /// Decide dropped entry files and open their import choices without applying them.
    member _.DropEntryFiles (files : FileName list) : unit =
        dispatch (LibraryWindowView.DropLibraryEntryFiles files)
