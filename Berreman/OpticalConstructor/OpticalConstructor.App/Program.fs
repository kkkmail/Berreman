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

open OpticalConstructor.Ui

/// Load the persisted user environment once at startup (J.6). `load` is total and
/// falls back to the built-in `defaults` on a missing/invalid settings file, so the
/// app always has a usable theme to apply (AC-J6).
module private Startup =
    let settings = UserEnvironment.load (UserEnvironment.settingsPath ())

    /// The typed appsettings.json values (spec 0038 Part C, step 005): the provider
    /// is created ONCE here — `AppConfig.loadWorkbenchSettings` opens the
    /// build-copied appsettings.json through `AppSettingsProvider`, `SetOnMissing`
    /// writes every missing key's default back (the first run self-documents), and
    /// the values flow inward only as this elevated Domain record (no view or Domain
    /// module reads the provider ambiently). The binding initializes with the module
    /// when `App.Initialize` reads `settings` above. Consumers arrive with the
    /// Part C window-policy seam; `environment.json` (the user preferences above)
    /// and appsettings.json never merge. Total — a missing/unreadable file falls
    /// back to `WorkbenchSettings.defaults`, so startup (and the headless ui-smoke
    /// session) never blocks on configuration.
    let workbenchSettings = AppConfig.loadWorkbenchSettings ()

    /// The ONE app-scope composition value (spec 0038 Part C, step 006): the five
    /// domain proxies — library / experiments / materials / samples / categories —
    /// built HERE, once, bundled with the step-005 settings record above. The module
    /// initializes when `App.Initialize` reads `settings`, i.e. BEFORE any window
    /// opens; the launcher, the Main workbench window, and every window opened later
    /// receive THIS value, so they all observe the same in-memory stores. (The
    /// in-window five-proxy composition this replaces lived in
    /// `MainConstructorWindow`'s constructor; the test scenes keep their own
    /// parameterless `DefaultStores` default in `TableAndElementRotationView`.)
    let context = AppContext.create workbenchSettings

/// Spec 0038 step 045: the stable automation ids for the product launcher's FOUR surface buttons —
/// Main / Inverse / Materials / Library. Centralized here (the Controls `UiIds` / workbench
/// `WorkbenchIds` precedent) so the `LauncherWindow` and its headless tests share ONE source of
/// truth. The product-launcher ids live with the App composition root that OWNS the launcher; the
/// in-window ids stay in the Controls `UiIds` module. Each id is set as BOTH the button's `Name`
/// and its `AutomationProperties.AutomationId`, so meaning-based automation finds it by a
/// layout-independent handle ("automate by meaning, not by pixels"). Distinct from the workbench
/// strip's `OpenMaterialsWindowButton` / `OpenLibraryWindowButton` — those are different controls
/// that open the SAME single-instance windows over the shared registry.
[<RequireQualifiedAccess>]
module LauncherIds =
    [<Literal>]
    let openMainButton = "OpenMainButton"
    [<Literal>]
    let openInverseButton = "OpenInverseButton"
    [<Literal>]
    let openMaterialsButton = "OpenMaterialsButton"
    [<Literal>]
    let openLibraryButton = "OpenLibraryButton"

/// Spec 0038 step 045: the shared body of the two constructor windows. The forward Main scene and
/// the inverse scene are the SAME dynamic "Lego constructor" (`mainView` over
/// `TableAndElementRotationView.update`) run over the SAME app scope — they differ ONLY in the seed
/// the Elmish loop starts from (forward `initMainWith` vs inverse `initInverse`, step 037) and their
/// title. `mount` sizes the window like the Main scene has since spec 0027 task 018 (the ribbon's tab
/// strip + the tallest bay — Render's three rows — need more headroom than the flat bar) and runs the
/// scene from `seed`. Both window types below thread the app-scope experiment-data / experiment-
/// collection proxies (STORE_XDUO_0006 / 0005) onto the seed by record update, so every constructor
/// window the launcher or the strip opens shares the ONE collection store built in `AppContext.create`.
module private ConstructorScene =
    let mount (window : HostWindow) (seed : unit -> TableAndElementRotationView.Model) : unit =
        window.Width <- TableAndElementRotationView.canvasWidth
        window.Height <- TableAndElementRotationView.canvasHeight + 210.0
        Program.mkSimple seed TableAndElementRotationView.update TableAndElementRotationView.mainView
        |> Program.withHost window
        |> Program.run

/// The Main screen (Spec 0027): the dynamic "Lego constructor" — the SAME table + element scene as
/// "Test Table + Element Rotations" (`TableAndElementRotationView`), seeded with a light source and a
/// detector and given an add/remove palette. Spec 0038 Part C (step 006): the five app-scope proxies —
/// library / experiments / materials / samples / categories (STORE_XDUO_0001/0002/0003, coupled in
/// `AppContext.create`) — are NO LONGER built here; they arrive injected as `context` built once at
/// startup (`Startup.context`), so every Main window the launcher opens shares the same stores. All
/// five inject through `initMainWith`; the app-scope experiment-data + experiment-collection proxies
/// (STORE_XDUO_0006 / 0005, Part L step 037) thread on by record update, so every Main window shares
/// the ONE collection store. `WireUiCompositionTests` (and the step-006 `AppContextTests` two-surface
/// proof) drive THIS window headless over a context composed the same way.
type MainConstructorWindow(context : AppContext) as this =
    inherit HostWindow()
    do
        this.Title <- "Optical Constructor — Main"
        ConstructorScene.mount this (fun () ->
            { TableAndElementRotationView.initMainWith context.library context.experiments context.materials context.samples context.categories with
                experimentData = context.experimentData
                experimentCollections = context.experimentCollections })

/// Spec 0038 step 045: the INVERSE constructor window (`initInverse`, Part L step 037). The SAME
/// dynamic workbench as `MainConstructorWindow` opened in the inverse-problem flow: seeded with a light
/// source, an UNBOUND sample (the unknown — drawn dashed), and a detector; with the sample unbound the
/// forward experiment chart is absent until a HINT sample is bound through the Library window's Select
/// state. Shares the SAME app scope as the launcher and the Main window (the step-006 `context`), so the
/// stores and the ONE saved-collection store are shared; the app-scope experiment proxies thread on by
/// record update exactly as the Main window does.
type InverseConstructorWindow(context : AppContext) as this =
    inherit HostWindow()
    do
        this.Title <- "Optical Constructor — Inverse"
        ConstructorScene.mount this (fun () ->
            { TableAndElementRotationView.initInverse context.library context.experiments context.materials context.samples context.categories with
                experimentData = context.experimentData
                experimentCollections = context.experimentCollections })

/// The product launcher form (Spec 0027 / step 045): FOUR buttons — Main / Inverse / Materials /
/// Library, in that order — over the ONE injected app scope. This is the app's startup window.
///
/// - **Main** opens the forward constructor (`MainConstructorWindow`); opening it twice yields two
///   views over one set of stores (the step-006 app scope).
/// - **Inverse** opens the inverse-problem constructor (`InverseConstructorWindow`, step 037).
/// - **Materials / Library** open the single-instance step-013/015 windows through the SAME
///   `EditorLaunchers.defaults` launcher seam the constructor strip uses, so a window opened from the
///   launcher and the same window opened from the constructor strip meet in ONE open-or-activate space
///   (the shared host-layer `WindowRegistry` keyed by `MaterialsWindowKey` / `LibraryWindowKey`).
///
/// The seven diagnostic test buttons live in the standalone `OpticalConstructor.TestWindows.App`
/// executable's `TestLauncherWindow` (spec 0038 Part C, step 004), so test code stays out of the
/// product dependency graph.
type LauncherWindow(context : AppContext) as this =
    inherit Window()

    do
        this.Title <- "Optical Constructor — Launcher"
        this.Width <- 380.0
        this.Height <- 300.0
        this.CanResize <- false
        let title =
            TextBlock(
                Text = "Optical Constructor",
                FontSize = 18.0,
                FontWeight = FontWeight.SemiBold,
                Margin = Thickness(0.0, 0.0, 0.0, 14.0))
        // Every surface button carries its stable id as BOTH its `Name` and its
        // `AutomationProperties.AutomationId` (meaning-based automation, not pixels), plus the human
        // label as its accessible name.
        let surfaceButton (id : string) (label : string) (onClick : unit -> unit) : Button =
            let button =
                Button(
                    Name = id,
                    Content = label,
                    HorizontalAlignment = Layout.HorizontalAlignment.Stretch)
            AutomationProperties.SetAutomationId(button, id)
            AutomationProperties.SetName(button, label)
            button.Click.Add(fun _ -> onClick ())
            button
        // Materials / Library go through the SAME launcher seam as the constructor strip
        // (`EditorLaunchers.defaults`) over the ONE shared `WindowRegistry`, so both callers share one
        // single-instance window.
        let launchers = TableAndElementRotationView.EditorLaunchers.defaults
        let mainButton =
            surfaceButton LauncherIds.openMainButton "Main" (fun () -> MainConstructorWindow(context).Show())
        let inverseButton =
            surfaceButton LauncherIds.openInverseButton "Inverse" (fun () -> InverseConstructorWindow(context).Show())
        let materialsButton =
            surfaceButton LauncherIds.openMaterialsButton "Materials" (fun () ->
                launchers.openMaterialsWindow context.materials context.categories)
        let libraryButton =
            surfaceButton LauncherIds.openLibraryButton "Library" (fun () ->
                launchers.openLibraryWindow context.library context.samples context.materials context.categories)
        let panel = StackPanel(Margin = Thickness 20.0, Spacing = 8.0)
        panel.Children.Add title
        panel.Children.Add mainButton
        panel.Children.Add inverseButton
        panel.Children.Add materialsButton
        panel.Children.Add libraryButton
        this.Content <- panel

/// The Avalonia application: Fluent theme plus the persisted light/dark variant
/// (§J.8 — `AppShell.themeVariant` is the only theme-label → Avalonia seam).
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- AppShell.themeVariant Startup.settings.theme

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            // The launcher is the startup window. Closing it after opening Main / a test
            // window must NOT quit the app, so shut down only when the last window closes.
            desktop.ShutdownMode <- ShutdownMode.OnLastWindowClose
            desktop.MainWindow <- LauncherWindow(Startup.context)
        | _ -> ()

module Program =

    [<EntryPoint; STAThread>]
    let main (argv : string[]) : int =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .StartWithClassicDesktopLifetime(argv)
