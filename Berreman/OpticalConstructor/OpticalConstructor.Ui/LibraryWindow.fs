namespace OpticalConstructor.Ui

open Avalonia.Automation
open OpticalConstructor.Controls
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Domain.WindowMode
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
/// in step 45). Step 016: `mode` opens the window Browse (the default) or Select — the SAME
/// window pre-constrained to the session's kind with the Select/Close pair; `Retarget` is the
/// launcher's re-target seam (a Select-state open of the LIVE instance re-points its session),
/// and the `Closed` hook cancels a still-pending session exactly once (the title-bar X and a
/// staleness `Close()` from the requesting surface both land there). Step 019: the app-scope
/// `categories` store threads through to the Sample editor, whose per-layer Choose material…
/// verb composes the Materials window (its category facet reads the LIVE store) in Select
/// state.
type LibraryWindow(library : LibraryProxy, samples : SampleProxy, materials : MaterialProxy, categories : CategoryProxy, ?mode : LibraryWindowMode<LibraryEntry>, ?treeAutoBuildThreshold : TreeAutoBuildThreshold, ?thicknessBucketCap : ThicknessBucketCap) as this =
    inherit HostWindow()

    // The Elmish dispatch, captured by the init Cmd the moment the loop starts (Program.run is
    // synchronous in this constructor) — the host's Closed hook and the public Retarget seam
    // dispatch through it into the pure update.
    let mutable dispatch : LibraryWindowView.Msg -> unit = ignore

    do
        this.Title <- "Library"
        this.Name <- UiIds.LibraryWindow.window
        AutomationProperties.SetAutomationId(this, UiIds.LibraryWindow.window)
        this.Width <- 1150.0
        this.Height <- 800.0
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
                        openBrowse key (fun () -> SampleEditorWindow(materials, samples, categories, intent) :> Window)
                requestClose = this.Close
            }
        Program.mkProgram
            (fun () -> LibraryWindowView.init context (defaultArg mode Browse), Cmd.ofEffect (fun d -> dispatch <- d))
            (fun msg m -> LibraryWindowView.update msg m, Cmd.none)
            LibraryWindowView.view
        |> Program.withHost this
        |> Program.run
        // ANY close path funnels here (title-bar X, a staleness Close(), the Select/Close
        // verbs' requestClose): a still-pending Select session cancels exactly once — the
        // verbs flip the mode to Browse in the same update that fires their callback, so the
        // queued dismissal is a no-op after them (LibraryWindowView.SelectDismissed).
        this.Closed.Add (fun _ -> dispatch LibraryWindowView.SelectDismissed)

    /// The launcher's re-target seam (step 016): a Select-state open that found THIS window
    /// live re-points its session at the new context — the superseded session is cancelled by
    /// the pure update; the window instance stays (single-instance semantics).
    member _.Retarget (context : SelectionContext<LibraryEntry>) : unit =
        dispatch (LibraryWindowView.RetargetSelect context)
