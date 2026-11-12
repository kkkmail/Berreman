/// Runnable composition root (§A.1 / §A.6). Hosts the Avalonia `Application` +
/// FuncUI `HostWindow` bootstrap: the launcher is the startup window, and its Main
/// button opens the kit start screen for the `MainConstructorWindow` scene. The old Elmish
/// constructor shell (`MainWindow` hosting `Shell.init/update/view`) was dead code
/// for the Main flow and is retired (spec 0038 Part B.1), along with the
/// localization startup-error surface it carried.
///
/// The bootstrap uses the public MIT `Avalonia.FuncUI` NuGet surface — the same
/// buildable interpretation of §A.6 the Ui project compiles against. The
/// audit-gated clone at `C:\GitHub\Avalonia.FuncUI.Clone\` stays UNREFERENCED and
/// its linking mechanism UNRESOLVED (constraint 5 / §A.9 / AC-A8): nothing here
/// references it.
namespace OpticalConstructor.App

open System

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Media
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Softellect.Sys.Logging

open OpticalConstructor.Controls
open OpticalConstructor.Domain.KitArchive
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Storage.KitArchiveStore
open OpticalConstructor.Ui
open OpticalConstructor.Ui.AppShell
open OpticalConstructor.Ui.ImportDialogView
open OpticalConstructor.Ui.ImportWizardOpening
open OpticalConstructor.Ui.KitArchiveJob
open OpticalConstructor.Ui.KitOpening

/// Load the persisted user environment once at startup (J.6). `load` is total and
/// falls back to the built-in `defaults` on a missing/invalid settings file, so the
/// app always has a usable theme to apply (AC-J6).
module private Startup =
    let settings = UserEnvironment.EnvironmentSettings.load (UserEnvironment.settingsPath ())

    /// Load typed workbench settings and resolved locations once, with defaults on configuration failure
    /// (spec 0038 Part C, step 005; spec 0050 §3.9).
    let workbenchSettings, dataLocations = AppConfig.loadWorkbenchSettings ()

    /// Share one app scope, composed over configured locations and settings, across every product window
    /// (spec 0038 Part C, step 006; spec 0050 §3.9).
    let context = AppContext.create dataLocations workbenchSettings

/// Mounts each constructor over its own kit and the shared app library (spec 0050 S4.3).
module private ConstructorScene =
    let storesOf (context : AppContext) (opened : OpenedKit) : KitLibraryStores =
        let stores : KitLibraryStores =
            {
                library = context.library
                materials = context.materials
                samples = context.samples
                categories = context.categories
            }
        stores.overlaidWith opened.overlay

    /// Runs the kit window over `seed`: the kit bar above the workbench, and an outside-change check of the kit on every
    /// activation (spec 0050 S5.3). Its Kit menu's shell is the system explorer, this window's close, the real archive on
    /// UI-thread jobs, and this window's zip and kit dialogs starting in `context`'s exports and kits folders (spec 0050 S6).
    /// Its Attach data file… opens the column-mapping wizard over this window (spec 0050 S9.2).
    let mount (window : HostWindow) (context : AppContext) (opened : OpenedKit) (seed : unit -> TableAndElementRotationView.TableAndElementRotationModel) : unit =
        window.Width <- TableAndElementRotationView.tableAndElementRotationCanvasWidth
        window.Height <- TableAndElementRotationView.tableAndElementRotationCanvasHeight + 210.0
        // Spec 0046: never larger than the screen it opens on — on a laptop the design height can exceed
        // the working area, which put the bottom of the scrolling page (the table) off-screen.
        OpticalConstructor.Controls.WindowFit.keepOnScreen window
        window.Closed.Add(fun _ ->
            match opened.library.kit.close () with
            | Ok () -> ()
            | Error error -> Logger.logError $"ConstructorScene: could not close the kit: {error.describe}")
        // The init Cmd stores the Elmish dispatch (Program.run is synchronous), so a host event can send a message.
        let hostDispatch : (TableAndElementRotationView.TableAndElementRotationMsg -> unit) ref = ref ignore
        let kitBarShell : TableAndElementRotationView.KitWindowShell =
            {
                TableAndElementRotationView.KitWindowShell.inert with
                    explorer = ExplorerProxy.system
                    requestClose = window.Close
                    zipPicker = KitZipPicker.ofWindow window context.locations.exportsFolder
                    archive = KitArchiveProxy.create ()
                    archiveJobs =
                        KitArchiveJobHost.onUiThread
                            (fun progress -> hostDispatch.Value (TableAndElementRotationView.SendKitProgressed progress))
                            (fun ended -> hostDispatch.Value (TableAndElementRotationView.SendKitEnded ended))
                    kitPicker = KitFolderPicker.ofWindow window context.locations.kitsFolder
            }
        let shell : TableAndElementRotationView.KitWindowShell =
            kitBarShell.withImportWizard window (fun msg -> hostDispatch.Value msg)
        let init () : TableAndElementRotationView.TableAndElementRotationModel * Cmd<TableAndElementRotationView.TableAndElementRotationMsg> =
            let seeded = seed ()
            {
                seeded with kitShell = shell
            },
            Cmd.ofEffect (fun dispatch -> hostDispatch.Value <- dispatch)
        // Spec 0050 S5.3: the title follows the model on every render, so a mode switch or a reload's new name shows.
        let titledView
            (model : TableAndElementRotationView.TableAndElementRotationModel)
            (dispatch : TableAndElementRotationView.TableAndElementRotationMsg -> unit)
            : Avalonia.FuncUI.Types.IView =
            window.Title <- model.kitWindowTitle.value
            KitWindowView.kitWindowView model dispatch
        Program.mkProgram
            init
            (fun msg model -> TableAndElementRotationView.updateTableAndElementRotation msg model, Cmd.none)
            titledView
        |> Program.withHost window
        |> Program.run
        // Spec 0050 S5.3; .manual/001 §E: every activation, the first included, offers the kit files changed outside the app.
        window.Activated.Add(fun _ ->
            hostDispatch.Value (TableAndElementRotationView.CheckKitChanges TableAndElementRotationView.KitCheckOnActivation))

    let withAppContext
        (context : AppContext)
        (opened : OpenedKit)
        (model : TableAndElementRotationView.TableAndElementRotationModel)
        : TableAndElementRotationView.TableAndElementRotationModel =
        {
            model with
                sources = context.sources
                libraryBackups = context.libraryBackups
                catalogueImports = context.catalogueImports
                // Spec 0050 S7.2; 001 §E: the kit's snapshots answer for models this machine's library lacks.
                itemModels = context.itemModels.overlaidWith opened.overlay
                experimentExport = context.experimentExport
        }

/// Works in the kit the start screen opened, with its snapshot overlay, and releases it on close (spec 0050 S4.3, S5.2).
/// It runs the kit window, titled by the kit, its access, its folder and the constructor (spec 0050 S5.3).
/// Its kit bar sends and copies through the real dialogs (spec 0050 S6).
/// Its Items panel reads the app's item models (spec 0050 S7.2).
type MainConstructorWindow(context : AppContext, opened : OpenedKit) as this =
    inherit HostWindow()
    let stores = ConstructorScene.storesOf context opened

    do
        let model =
            TableAndElementRotationView.initMainWithKit stores.library context.experiments stores.materials stores.samples stores.categories opened.library.kit
            |> ConstructorScene.withAppContext context opened
        ConstructorScene.mount this context opened (fun () -> model.loadKit opened)

    /// The kit-overlaid stores supplied to this constructor's scene (spec 0050 S4.3).
    member _.sessionLibraryStores : KitLibraryStores = stores

/// Opens the inverse seed in the kit the start screen opened and releases it on close (spec 0050 S4.3, S5.2).
/// It runs the kit window, titled by the kit, its access, its folder and the constructor (spec 0050 S5.3).
/// Its kit bar sends and copies through the real dialogs (spec 0050 S6).
/// Its Items panel reads the app's item models (spec 0050 S7.2).
type InverseConstructorWindow(context : AppContext, opened : OpenedKit) as this =
    inherit HostWindow()
    do
        let stores = ConstructorScene.storesOf context opened
        let model =
            TableAndElementRotationView.initInverseWithKit stores.library context.experiments stores.materials stores.samples stores.categories opened.library.kit
            |> ConstructorScene.withAppContext context opened
        ConstructorScene.mount this context opened (fun () -> model.loadKit opened)

module private KitConstructorWindows =
    /// Opens the constructor window of `mode` over the kit the start screen opened, registered under the kit's folder
    /// (spec 0050 S4.3, S5.2, S5.3). A window that already holds the kit keeps it, and a failed open logs its reason;
    /// either way the opened kit is closed again.
    let showConstructorIn (context : AppContext) (mode : TableAndElementRotationView.ConstructorMode) (opened : OpenedKit) : unit =
        let constructorWindow (_ : WindowLauncher.WindowKey) : Result<Window, WindowLauncher.WindowLauncherError> =
            match mode with
            | TableAndElementRotationView.ForwardConstructor -> MainConstructorWindow(context, opened) :> Window |> Ok
            | TableAndElementRotationView.InverseConstructor -> InverseConstructorWindow(context, opened) :> Window |> Ok
        let closeOpenedKit () : unit =
            match opened.library.kit.close () with
            | Ok () -> ()
            | Error error -> Logger.logError $"KitConstructorWindows.showConstructorIn: could not close the kit: {error.describe}"
        let launcher =
            WindowLauncher.WindowLauncher.create constructorWindow context.settings.selectWindowModality WindowLauncher.BrowseOpen
        match launcher.openOrActivate opened.folder.windowKey with
        | Ok (WindowLauncher.CreatedWindow _) -> ()
        | Ok (WindowLauncher.ActivatedWindow _)
        | Ok (WindowLauncher.RetargetedWindow _) -> closeOpenedKit ()
        | Error (WindowLauncher.WindowFactoryFailedErr (_, reason)) ->
            closeOpenedKit ()
            Logger.logError $"KitConstructorWindows.showConstructorIn: could not open the kit window: {reason}"
        | Error (WindowLauncher.WindowNotRegisteredErr _) ->
            closeOpenedKit ()
            Logger.logError $"KitConstructorWindows.showConstructorIn: no kit window was registered for {opened.folder.value.value}"

/// Main and Inverse open the kit start screen of their mode; Materials and Library open over the one app scope, sharing
/// editor-window launchers. A library-open failure disables every surface action, never the footer's Data locations link
/// (spec 0038 step 045; spec 0050 S2.4, S5.2, S5.3). A start screen's Open kit from zip… dialog starts in the exports
/// folder, where Send kit… writes (spec 0050 S6).
type LauncherWindow(locations : DataLocations, settings : WorkbenchSettings, opened : Result<AppContext, StorageError>, pickerFor : TopLevel -> KitFolderPicker) as this =
    inherit Window()

    let mutable pendingImports : PendingCatalogueImport list =
        match opened with
        | Ok context -> context.pendingImports
        | Error _ -> []

    do
        this.Title <- "Optical Constructor — Launcher"
        this.Width <- 380.0
        this.Height <- 300.0
        this.CanResize <- false
        OpticalConstructor.Controls.WindowFit.keepOnScreen this
        let dataLocationsLauncher =
            WindowLauncher.WindowLauncher.create
                (fun (_ : WindowLauncher.WindowKey) -> DataLocationsWindow(locations, ExplorerProxy.system) :> Window |> Ok)
                settings.selectWindowModality
                WindowLauncher.BrowseOpen
        let openDataLocations () : unit =
            match dataLocationsLauncher.openOrActivate WindowLauncher.DataLocationsWindowKey with
            | Ok _ -> ()
            | Error (WindowLauncher.WindowFactoryFailedErr (_, reason)) ->
                Logger.logError $"LauncherWindow: could not open the Data locations window: {reason}"
            | Error (WindowLauncher.WindowNotRegisteredErr _) ->
                Logger.logError "LauncherWindow: no Data locations window was registered"
        let rec launcherContent (outcome : Result<AppContext, StorageError>) : Control =
            let title =
                TextBlock(
                    Text = "Optical Constructor",
                    FontSize = 18.0,
                    FontWeight = FontWeight.SemiBold,
                    Margin = Thickness(0.0, 0.0, 0.0, 14.0))
            let surfaceButton (id : ConstructorUiId) (label : string) (onClick : AppContext -> unit) : Button =
                let button =
                    Button(
                        Name = id.value,
                        Content = label,
                        HorizontalAlignment = Layout.HorizontalAlignment.Stretch)
                AutomationProperties.SetAutomationId(button, id.value)
                AutomationProperties.SetName(button, label)
                match outcome with
                | Ok context -> button.Click.Add(fun _ -> onClick context)
                | Error _ -> button.IsEnabled <- false
                button
            // Spec 0038 step 045: share the constructor strip's single-instance window registry.
            let launchers = TableAndElementRotationView.EditorLaunchers.defaults
            let zipPickerFor (owner : TopLevel) : KitZipPicker = KitZipPicker.ofWindow owner locations.exportsFolder
            let panel = StackPanel(Margin = Thickness 20.0, Spacing = 8.0)
            panel.Children.Add title
            let mainButton =
                surfaceButton ConstructorUiId.openMainButton "Main" (fun context ->
                    KitStartWindow(context, TableAndElementRotationView.ForwardConstructor, pickerFor, zipPickerFor, KitConstructorWindows.showConstructorIn context).Show())
            let inverseButton =
                surfaceButton ConstructorUiId.openInverseButton "Inverse" (fun context ->
                    KitStartWindow(context, TableAndElementRotationView.InverseConstructor, pickerFor, zipPickerFor, KitConstructorWindows.showConstructorIn context).Show())
            let materialsButton =
                surfaceButton ConstructorUiId.openMaterialsButton "Materials" (fun context ->
                    launchers.openMaterialsWindow context.materials context.categories context.catalogueImports)
            let libraryButton =
                surfaceButton ConstructorUiId.openLibraryButton "Library" (fun context ->
                    launchers.openLibraryWindow context.library context.samples context.sources context.materials context.categories context.libraryBackups context.catalogueImports)
            let versionText = $"Version {Berreman.BuildInfo.version.value}"
            let version =
                TextBlock(
                    Name = ConstructorUiId.launcherVersionText.value,
                    Text = versionText,
                    FontSize = 11.0,
                    Opacity = 0.7,
                    HorizontalAlignment = Layout.HorizontalAlignment.Right,
                    Margin = Thickness(20.0, 0.0, 20.0, 10.0))
            AutomationProperties.SetAutomationId(version, ConstructorUiId.launcherVersionText.value)
            AutomationProperties.SetName(version, versionText)
            // Spec 0050 S5.3: the locations do not depend on the library, so the link is enabled in every outcome.
            let linkText = "Data locations"
            let dataLocationsLink =
                Button(
                    Name = ConstructorUiId.launcherDataLocationsLink.value,
                    Content = TextBlock(Text = linkText, TextDecorations = TextDecorations.Underline),
                    Background = Brushes.Transparent,
                    BorderThickness = Thickness 0.0,
                    Padding = Thickness 0.0,
                    FontSize = 11.0,
                    VerticalAlignment = Layout.VerticalAlignment.Center,
                    Margin = Thickness(20.0, 0.0, 0.0, 10.0))
            AutomationProperties.SetAutomationId(dataLocationsLink, ConstructorUiId.launcherDataLocationsLink.value)
            AutomationProperties.SetName(dataLocationsLink, linkText)
            dataLocationsLink.Click.Add(fun _ -> openDataLocations ())
            DockPanel.SetDock(dataLocationsLink, Dock.Left)
            let footer = DockPanel()
            footer.Children.Add dataLocationsLink
            footer.Children.Add version
            DockPanel.SetDock(footer, Dock.Bottom)
            match outcome with
            | Ok _ ->
                match pendingImports with
                | [] -> ()
                | _ ->
                    let count = pendingImports |> List.sumBy (fun pending -> pending.decisions.Length)
                    let message = $"Library entries waiting for an import decision: {count}."
                    let notice =
                        TextBlock(
                            Name = ConstructorUiId.launcherPendingImportsText.value,
                            Text = message,
                            TextWrapping = TextWrapping.Wrap)
                    AutomationProperties.SetAutomationId(notice, ConstructorUiId.launcherPendingImportsText.value)
                    AutomationProperties.SetName(notice, message)
                    panel.Children.Add notice
                    panel.Children.Add(
                        surfaceButton ConstructorUiId.launcherReviewImportsButton "Review imports…" reviewPendingImports)
            | Error error ->
                let message = $"The library could not be opened: {error.describe}"
                let errorText =
                    TextBlock(
                        Name = ConstructorUiId.launcherLibraryErrorText.value,
                        Text = message,
                        TextWrapping = TextWrapping.Wrap)
                AutomationProperties.SetAutomationId(errorText, ConstructorUiId.launcherLibraryErrorText.value)
                AutomationProperties.SetName(errorText, message)
                panel.Children.Add errorText
                // Spec 0050 S2.4: the original mismatch authorizes retry even after a backup or IO failure.
                match opened with
                | Error (LibrarySchemaMismatchErr _) ->
                    let label = "Recreate library from catalogues"
                    let recreateButton =
                        Button(
                            Name = ConstructorUiId.launcherRecreateLibraryButton.value,
                            Content = label,
                            HorizontalAlignment = Layout.HorizontalAlignment.Stretch)
                    AutomationProperties.SetAutomationId(recreateButton, ConstructorUiId.launcherRecreateLibraryButton.value)
                    AutomationProperties.SetName(recreateButton, label)
                    recreateButton.Click.Add(fun _ ->
                        let recreated = AppContext.recreateLibrary locations settings
                        pendingImports <-
                            match recreated with
                            | Ok context -> context.pendingImports
                            | Error _ -> []
                        this.Content <- launcherContent recreated)
                    panel.Children.Add recreateButton
                | _ -> ()
            panel.Children.Add mainButton
            panel.Children.Add inverseButton
            panel.Children.Add materialsButton
            panel.Children.Add libraryButton
            let form = DockPanel()
            form.Children.Add footer
            // Spec 0046; spec 0050 S2.4: a wrapped open error must not push the buttons out of reach.
            form.Children.Add(ScrollViewer(
                Content = panel,
                HorizontalScrollBarVisibility = Primitives.ScrollBarVisibility.Disabled,
                VerticalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto))
            form

        and reviewPendingImports (context : AppContext) : unit =
            match pendingImports with
            | [] -> ()
            | head :: _ ->
                let onApplied (applied : ImportDialogApplied) : unit =
                    match applied.request with
                    | SettlePendingCatalogue pending ->
                        pendingImports <- pendingImports |> List.filter (fun waiting -> waiting.entry.catalogueId <> pending.entry.catalogueId)
                        this.Content <- launcherContent (Ok context)
                    | ApplyDroppedEntries _ | ResolveKitSnapshot _ -> ()
                ImportDialogWindow(context.catalogueImports, SettlePendingCatalogue head, onApplied).Show(this)

        this.Content <- launcherContent opened
        let mutable startupShown = false
        this.Opened.Add(fun _ ->
            if not startupShown then
                startupShown <- true
                match opened with
                | Ok context -> reviewPendingImports context
                | Error _ -> ())

/// The Avalonia application: Fluent theme plus the persisted light/dark variant
/// (§J.8 — `Theme.themeVariant` is the only theme-label → Avalonia seam).
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Startup.settings.theme.themeVariant

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            // The launcher is the startup window. Closing it after opening Main / a test
            // window must NOT quit the app, so shut down only when the last window closes.
            desktop.ShutdownMode <- ShutdownMode.OnLastWindowClose
            desktop.MainWindow <-
                LauncherWindow(Startup.dataLocations, Startup.workbenchSettings, Startup.context,
                    fun owner -> KitFolderPicker.ofWindow owner Startup.dataLocations.kitsFolder)
        | _ -> ()

module Program =

    [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.IoBoundary, "the process entry point - argv in, exit code out, per the .NET contract")>]
    [<EntryPoint; STAThread>]
    let main (argv : string[]) : int =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .StartWithClassicDesktopLifetime(argv)
