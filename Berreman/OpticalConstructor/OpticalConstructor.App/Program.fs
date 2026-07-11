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

/// The Main screen (Spec 0027): the dynamic "Lego constructor". It is the SAME table + element scene
/// as "Test Table + Element Rotations" (`TableAndElementRotationView` — same table, same initial zoom,
/// same select/unselect + rotation/zoom/pan logic), seeded with a light source and a detector and given
/// an add/remove palette so elements can be added and removed at runtime. The ONLY difference from the
/// test scene is that palette (`initMain`); the scene logic is shared. (The old Elmish constructor
/// shell that used to sit beside this window was retired in spec 0038 Part B.1.)
type MainConstructorWindow(context : AppContext) as this =
    inherit HostWindow()
    do
        this.Title <- "Optical Constructor — Main"
        this.Width <- TableAndElementRotationView.canvasWidth
        // Spec 0027 task 018: the Main screen is now the ribbon of "large controls" (`mainView`) — the
        // ribbon's tab strip + the tallest bay (Render, three rows) need more headroom than the flat bar.
        this.Height <- TableAndElementRotationView.canvasHeight + 210.0
        // Spec 0038 Part C (step 006): the five proxies — library / experiments / materials /
        // samples / categories (STORE_XDUO_0001/0002/0003, coupled in `AppContext.create`:
        // materials consult the LIVE samples, categories the LIVE materials) — are NO LONGER
        // built here; they arrive as the injected app-scope `context` built once at startup
        // (`Startup.context`), so every Main window the launcher opens shares the same stores.
        // All five inject through `initMainWith` (which seeds the REAL editor launchers,
        // `EditorLaunchers.defaults` — since spec 0038 step 008 those open every verb window
        // through the SVC_XDUO_0001 `WindowLauncher` over the host-layer WindowRegistry, so a
        // second Edit of the same entity ACTIVATES its live editor window), and the editor
        // windows the workbench verbs open write to the SAME app-scope stores. The step-013
        // Materials-window factory threads the same way: the ribbon strip's `Materials…` button
        // opens the single-instance `MaterialsWindow` through `defaults.openMaterialsWindow`
        // over `context.materials` / `context.categories` under `MaterialsWindowKey` — no
        // extra composition here (step 47 owns the composition acceptance). The step-015
        // Library-window factory likewise: the strip's `Library…` button opens the
        // single-instance `LibraryWindow` through `defaults.openLibraryWindow` over
        // `context.library` / `context.samples` / `context.materials` under
        // `LibraryWindowKey` — the retired samples-workbench bay's editors now open from
        // inside that window over the same stores. The ui-smoke
        // composition acceptance (`WireUiCompositionTests`, and the step-006 two-surface proof
        // in `AppContextTests`) drives THIS window headless over a context composed the same way.
        Program.mkSimple (fun () -> TableAndElementRotationView.initMainWith context.library context.experiments context.materials context.samples context.categories) TableAndElementRotationView.update TableAndElementRotationView.mainView
        |> Program.withHost this
        |> Program.run

/// The simple launcher form (Spec 0027): `Main` opens the constructor workbench scene
/// (the `MainConstructorWindow` above). This is the app's startup window. The seven
/// diagnostic test buttons moved to the standalone `OpticalConstructor.TestWindows.App`
/// executable's `TestLauncherWindow` (spec 0038 Part C, step 004), so test code no
/// longer sits in the product dependency graph; the product launcher keeps Main only
/// (Inverse / Materials / Library land in step 45).
type LauncherWindow(context : AppContext) as this =
    inherit Window()

    do
        this.Title <- "Optical Constructor — Launcher"
        this.Width <- 380.0
        this.Height <- 180.0
        this.CanResize <- false
        let title =
            TextBlock(
                Text = "Optical Constructor",
                FontSize = 18.0,
                FontWeight = FontWeight.SemiBold,
                Margin = Thickness(0.0, 0.0, 0.0, 14.0))
        let mainButton =
            Button(
                Name = "OpenMainButton",
                Content = "Main",
                HorizontalAlignment = Layout.HorizontalAlignment.Stretch)
        // Spec 0038 Part C (step 006): every Main window opens over the SAME injected
        // app scope — opening Main twice yields two views over one set of stores.
        mainButton.Click.Add(fun _ -> MainConstructorWindow(context).Show())
        let panel = StackPanel(Margin = Thickness 20.0)
        panel.Children.Add title
        panel.Children.Add mainButton
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
