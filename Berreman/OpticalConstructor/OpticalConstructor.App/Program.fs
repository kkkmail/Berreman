/// Runnable composition root (§A.1 / §A.6). Hosts the Avalonia `Application` +
/// FuncUI `HostWindow` bootstrap: the launcher is the startup window, and its Main
/// button opens the `MainConstructorWindow` workbench scene. The old Elmish
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

open OpticalConstructor.Controls
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Ui
open OpticalConstructor.Ui.AppShell
open OpticalConstructor.Ui.ImportDialogView

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
    let mount (window : HostWindow) (seed : unit -> TableAndElementRotationView.TableAndElementRotationModel) : unit =
        window.Width <- TableAndElementRotationView.tableAndElementRotationCanvasWidth
        window.Height <- TableAndElementRotationView.tableAndElementRotationCanvasHeight + 210.0
        // Spec 0046: never larger than the screen it opens on — on a laptop the design height can exceed
        // the working area, which put the bottom of the scrolling page (the table) off-screen.
        OpticalConstructor.Controls.WindowFit.keepOnScreen window
        Program.mkSimple seed TableAndElementRotationView.updateTableAndElementRotation TableAndElementRotationView.mainView
        |> Program.withHost window
        |> Program.run

    let withAppContext (context : AppContext) (model : TableAndElementRotationView.TableAndElementRotationModel) : TableAndElementRotationView.TableAndElementRotationModel =
        {
            model with
                sources = context.sources
                libraryBackups = context.libraryBackups
                catalogueImports = context.catalogueImports
                experimentExport = context.experimentExport
        }

/// The forward constructor works in its own initialized scratch kit (spec 0050 S4.3; .manual/010).
type MainConstructorWindow private (model : TableAndElementRotationView.TableAndElementRotationModel) as this =
    inherit HostWindow()
    do
        this.Title <- "Optical Constructor — Main"
        ConstructorScene.mount this (fun () -> model)

    /// Creates the window only after its kit initializes successfully (spec 0050 .manual/010).
    static member tryCreate (context : AppContext) : Result<MainConstructorWindow, KitError> =
        TableAndElementRotationView.initMainWith context.library context.experiments context.materials context.samples context.categories
        |> Result.map (fun model -> MainConstructorWindow(ConstructorScene.withAppContext context model))

/// The inverse constructor opens an unbound sample in its own scratch kit (spec 0050 S4.3; .manual/010).
type InverseConstructorWindow private (model : TableAndElementRotationView.TableAndElementRotationModel) as this =
    inherit HostWindow()
    do
        this.Title <- "Optical Constructor — Inverse"
        ConstructorScene.mount this (fun () -> model)

    /// Creates the window only after its kit initializes successfully (spec 0050 .manual/010).
    static member tryCreate (context : AppContext) : Result<InverseConstructorWindow, KitError> =
        TableAndElementRotationView.initInverse context.library context.experiments context.materials context.samples context.categories
        |> Result.map (fun model -> InverseConstructorWindow(ConstructorScene.withAppContext context model))

/// Opens Main, Inverse, Materials and Library over one app scope, sharing editor-window launchers.
/// A library-open failure is displayed with all surface actions disabled (spec 0038 step 045; spec 0050 S2.4).
/// Startup catalogue decisions remain pending until their import dialog applies them (spec 0050 S3.2).
type LauncherWindow(locations : DataLocations, settings : WorkbenchSettings, opened : Result<AppContext, StorageError>) as this =
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
            let kitStatus = TextBlock(TextWrapping = TextWrapping.Wrap, IsVisible = false)
            AutomationProperties.SetAutomationId(kitStatus, ConstructorUiId.kitStatus.value)
            AutomationProperties.SetName(kitStatus, "Kit status")
            let openConstructor (created : Result<Window, KitError>) : unit =
                match created with
                | Ok window ->
                    kitStatus.IsVisible <- false
                    window.Show()
                | Error error ->
                    kitStatus.Text <- $"The constructor could not be opened: {error.describe}"
                    kitStatus.IsVisible <- true
            let mainButton =
                surfaceButton ConstructorUiId.openMainButton "Main" (fun context ->
                    MainConstructorWindow.tryCreate context |> Result.map (fun window -> window :> Window) |> openConstructor)
            let inverseButton =
                surfaceButton ConstructorUiId.openInverseButton "Inverse" (fun context ->
                    InverseConstructorWindow.tryCreate context |> Result.map (fun window -> window :> Window) |> openConstructor)
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
            DockPanel.SetDock(version, Dock.Bottom)
            let panel = StackPanel(Margin = Thickness 20.0, Spacing = 8.0)
            panel.Children.Add title
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
            panel.Children.Add kitStatus
            panel.Children.Add mainButton
            panel.Children.Add inverseButton
            panel.Children.Add materialsButton
            panel.Children.Add libraryButton
            let form = DockPanel()
            form.Children.Add version
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
            desktop.MainWindow <- LauncherWindow(Startup.dataLocations, Startup.workbenchSettings, Startup.context)
        | _ -> ()

module Program =

    [<Softellect.Sys.ElevationBoundary(nameof Softellect.Sys.IoBoundary, "the process entry point - argv in, exit code out, per the .NET contract")>]
    [<EntryPoint; STAThread>]
    let main (argv : string[]) : int =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .StartWithClassicDesktopLifetime(argv)
