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
open Softellect.Sys.Logging

open OpticalConstructor.Controls
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.KitFolders
open OpticalConstructor.Domain.KitLibrary
open OpticalConstructor.Domain.LibraryEntryFiles
open OpticalConstructor.Domain.CatalogueImport
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Storage.Errors
open OpticalConstructor.Ui
open OpticalConstructor.Ui.AppShell
open OpticalConstructor.Ui.ImportDialogView
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

    let mount (window : HostWindow) (opened : OpenedKit) (seed : unit -> TableAndElementRotationView.TableAndElementRotationModel) : unit =
        window.Width <- TableAndElementRotationView.tableAndElementRotationCanvasWidth
        window.Height <- TableAndElementRotationView.tableAndElementRotationCanvasHeight + 210.0
        // Spec 0046: never larger than the screen it opens on — on a laptop the design height can exceed
        // the working area, which put the bottom of the scrolling page (the table) off-screen.
        OpticalConstructor.Controls.WindowFit.keepOnScreen window
        window.Closed.Add(fun _ ->
            match opened.library.kit.close () with
            | Ok () -> ()
            | Error error -> Logger.logError $"ConstructorScene: could not close the kit: {error.describe}")
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

/// Works in the opened kit with its snapshot overlay and releases it on close (spec 0050 S4.3).
type MainConstructorWindow(context : AppContext, opened : OpenedKit) as this =
    inherit HostWindow()
    let stores = ConstructorScene.storesOf context opened

    do
        this.Title <- "Optical Constructor — Main"
        let model =
            TableAndElementRotationView.initMainWithKit stores.library context.experiments stores.materials stores.samples stores.categories opened.library.kit
            |> ConstructorScene.withAppContext context
        ConstructorScene.mount this opened (fun () -> model.loadKit opened)

    /// The kit-overlaid stores supplied to this constructor's scene (spec 0050 S4.3).
    member _.sessionLibraryStores : KitLibraryStores = stores

/// Opens the inverse seed in the supplied kit and releases it on close (spec 0050 S4.3).
type InverseConstructorWindow(context : AppContext, opened : OpenedKit) as this =
    inherit HostWindow()
    do
        this.Title <- "Optical Constructor — Inverse"
        let stores = ConstructorScene.storesOf context opened
        let model =
            TableAndElementRotationView.initInverseWithKit stores.library context.experiments stores.materials stores.samples stores.categories opened.library.kit
            |> ConstructorScene.withAppContext context
        ConstructorScene.mount this opened (fun () -> model.loadKit opened)

/// Opens Main, Inverse, Materials and Library over one app scope, sharing editor-window launchers.
/// A library-open failure is displayed with all surface actions disabled (spec 0038 step 045; spec 0050 S2.4).
/// Confirms picked-kit snapshots before opening a constructor; catalogue decisions remain pending (spec 0050 S3.2, S4.3).
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
            let panel = StackPanel(Margin = Thickness 20.0, Spacing = 8.0)
            panel.Children.Add title
            let mutable kitErrorOpt : TextBlock option = None
            let mutable kitOfferOpt : StackPanel option = None
            let clearKitError () : unit =
                match kitErrorOpt with
                | Some errorText ->
                    panel.Children.Remove errorText |> ignore
                    kitErrorOpt <- None
                | None -> ()
                match kitOfferOpt with
                | Some row ->
                    panel.Children.Remove row |> ignore
                    kitOfferOpt <- None
                | None -> ()
            let showKitError (error : KitLibraryError) : unit =
                clearKitError ()
                let caption =
                    match error with
                    | KitLibraryKitErr (KitLockedErr holder) -> holder.openElsewhereCaption
                    | _ -> error.openFailedCaption
                let errorText = TextBlock(Name = ConstructorUiId.launcherKitErrorText.value, Text = caption.value, TextWrapping = TextWrapping.Wrap)
                AutomationProperties.SetAutomationId(errorText, ConstructorUiId.launcherKitErrorText.value)
                AutomationProperties.SetName(errorText, caption.value)
                kitErrorOpt <- Some errorText
                panel.Children.Insert(1, errorText)
            let rec openConstructorKit (context : AppContext) (folder : KitFolder) (mode : TableAndElementRotationView.ConstructorMode) (access : KitAccess) : unit =
                let author = KitAuthor Environment.UserName
                match KitOpening.openKitAt folder access ClockProvider.system author context.catalogueImports with
                | Ok kit ->
                    clearKitError ()
                    let showConstructor (resolved : OpenedKit) : unit =
                        let window : Window =
                            match mode with
                            | TableAndElementRotationView.ForwardConstructor -> MainConstructorWindow(context, resolved) :> Window
                            | TableAndElementRotationView.InverseConstructor -> InverseConstructorWindow(context, resolved) :> Window
                        window.Show()
                    kit.resolveAndShow context.catalogueImports showConstructor showKitError
                | Error error ->
                    showKitError error
                    match error with
                    | KitLibraryKitErr (KitLockedErr _) ->
                        let readOnlyButton =
                            surfaceButton ConstructorUiId.launcherOpenKitReadOnlyButton "Open read-only" (fun _ ->
                                openConstructorKit context folder mode ReadOnlyKitAccess)
                        readOnlyButton.Background <- SolidColorBrush MaterialsWindowView.positiveBackground
                        let cancelButton =
                            surfaceButton ConstructorUiId.launcherCancelKitOpenButton "Cancel" (fun _ -> clearKitError ())
                        cancelButton.Background <- SolidColorBrush MaterialsWindowView.negativeBackground
                        let row = StackPanel(Orientation = Layout.Orientation.Horizontal, Spacing = 12.0)
                        row.Children.Add readOnlyButton
                        row.Children.Add cancelButton
                        kitOfferOpt <- Some row
                        panel.Children.Insert(2, row)
                    | _ -> ()
            let mainButton =
                surfaceButton ConstructorUiId.openMainButton "Main" (fun context ->
                    (pickerFor this).pickThen (fun folder ->
                        openConstructorKit context folder TableAndElementRotationView.ForwardConstructor ReadWriteKitAccess))
            let inverseButton =
                surfaceButton ConstructorUiId.openInverseButton "Inverse" (fun context ->
                    (pickerFor this).pickThen (fun folder ->
                        openConstructorKit context folder TableAndElementRotationView.InverseConstructor ReadWriteKitAccess))
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
