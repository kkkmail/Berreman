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
open OpticalConstructor.TestWindows
// Spec 0033 (024): the samples-store module — also brings the optional
// `MaterialProxy.createInMemory` type extension (declared in `Library`, after `Sample`)
// into scope for the Main-scene composition below.
open OpticalConstructor.Domain.Library

/// Load the persisted user environment once at startup (J.6). `load` is total and
/// falls back to the built-in `defaults` on a missing/invalid settings file, so the
/// app always has a usable theme to apply (AC-J6).
module private Startup =
    let settings = UserEnvironment.load (UserEnvironment.settingsPath ())

/// The Main screen (Spec 0027): the dynamic "Lego constructor". It is the SAME table + element scene
/// as "Test Table + Element Rotations" (`TableAndElementRotationView` — same table, same initial zoom,
/// same select/unselect + rotation/zoom/pan logic), seeded with a light source and a detector and given
/// an add/remove palette so elements can be added and removed at runtime. The ONLY difference from the
/// test scene is that palette (`initMain`); the scene logic is shared. (The old Elmish constructor
/// shell that used to sit beside this window was retired in spec 0038 Part B.1.)
type MainConstructorWindow() as this =
    inherit HostWindow()
    do
        this.Title <- "Optical Constructor — Main"
        this.Width <- TableAndElementRotationView.canvasWidth
        // Spec 0027 task 018: the Main screen is now the ribbon of "large controls" (`mainView`) — the
        // ribbon's tab strip + the tallest bay (Render, three rows) need more headroom than the flat bar.
        this.Height <- TableAndElementRotationView.canvasHeight + 210.0
        // Spec 0027 (024): build the mock Library + Experiments proxies at the composition root and inject
        // them into the Main scene. Real, disk-backed proxies would later be built here instead (in
        // `OpticalConstructor.Storage`), leaving the scene/bay logic unchanged.
        let library = OpticalConstructor.Domain.Library.createInMemory ()
        let experiments = OpticalConstructor.Domain.Experiments.createInMemory ()
        // Spec 0033 (024/026): the material / sample WRITE stores (STORE_XDUO_0001/0002) join the
        // composition — the samples store first, then the materials store whose remove-block
        // consults the LIVE samples through `samplesReferencing`. All FIVE proxies (library /
        // experiments / materials / samples / categories) inject through `initMainWith` (which seeds
        // the REAL editor launchers, `EditorLaunchers.defaults`); the ui-smoke composition acceptance
        // (`WireUiCompositionTests`) drives THIS window headless — the reordered full-surface
        // Materials / Library bays render over the wired stores and Add / Edit / Categories… open the
        // three real editor windows.
        let samples = SampleProxy.createInMemory ()
        let materials = OpticalConstructor.Domain.MaterialLibrary.MaterialProxy.createInMemory (samplesReferencing samples)
        // Spec 0035 (009/019): the category WRITE store (STORE_XDUO_0003) joins the composition LAST —
        // its remove-block consults the live materials store through `materialsReferencingCategory`.
        // Step 009 threaded it through `initMainWith`; the step-019 WIRE_UI slice OWNS the composition
        // acceptance — `WireUiCompositionTests` drives THIS window headless and the Materials bay's
        // "Categories…" verb opens the real Category editor over this root-wired proxy, beside the
        // Material / Sample editors.
        let categories = OpticalConstructor.Domain.MaterialLibrary.CategoryProxy.createInMemory (OpticalConstructor.Domain.MaterialLibrary.materialsReferencingCategory materials)
        Program.mkSimple (fun () -> TableAndElementRotationView.initMainWith library experiments materials samples categories) TableAndElementRotationView.update TableAndElementRotationView.mainView
        |> Program.withHost this
        |> Program.run

/// The simple launcher form (Spec 0027): `Main` opens the constructor workbench scene
/// (the `MainConstructorWindow` above); the test buttons open the diagnostic test windows
/// (`OpticalConstructor.TestWindows`). This is the app's startup window so every path is one
/// click away; further test windows are added as buttons here and live in the TestWindows project.
type LauncherWindow() as this =
    inherit Window()

    do
        this.Title <- "Optical Constructor — Launcher"
        this.Width <- 380.0
        this.Height <- 520.0
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
                HorizontalAlignment = Layout.HorizontalAlignment.Stretch,
                Margin = Thickness(0.0, 0.0, 0.0, 8.0))
        mainButton.Click.Add(fun _ -> MainConstructorWindow().Show())
        let tableTestButton =
            Button(
                Name = "OpenTableRotationTestButton",
                Content = "Test Optical Table Rotations",
                HorizontalAlignment = Layout.HorizontalAlignment.Stretch,
                Margin = Thickness(0.0, 0.0, 0.0, 8.0))
        tableTestButton.Click.Add(fun _ -> TableRotationWindow().Show())
        let elementTestButton =
            Button(
                Name = "OpenElementRotationTestButton",
                Content = "Test Optical Element Rotations",
                HorizontalAlignment = Layout.HorizontalAlignment.Stretch,
                Margin = Thickness(0.0, 0.0, 0.0, 8.0))
        elementTestButton.Click.Add(fun _ -> ElementRotationWindow().Show())
        let tableElementTestButton =
            Button(
                Name = "OpenTableAndElementRotationTestButton",
                Content = "Test Table + Element Rotations",
                HorizontalAlignment = Layout.HorizontalAlignment.Stretch,
                Margin = Thickness(0.0, 0.0, 0.0, 8.0))
        tableElementTestButton.Click.Add(fun _ -> TableAndElementRotationWindow().Show())
        let elementMovementTestButton =
            Button(
                Name = "OpenElementMovementTestButton",
                Content = "Test Element Movement",
                HorizontalAlignment = Layout.HorizontalAlignment.Stretch,
                Margin = Thickness(0.0, 0.0, 0.0, 8.0))
        elementMovementTestButton.Click.Add(fun _ -> ElementMovementWindow().Show())
        let rendererTestButton =
            Button(
                Name = "OpenRendererTestButton",
                Content = "Test Renderers",
                HorizontalAlignment = Layout.HorizontalAlignment.Stretch,
                Margin = Thickness(0.0, 0.0, 0.0, 8.0))
        rendererTestButton.Click.Add(fun _ -> RendererTestWindow().Show())
        let snapTestButton =
            Button(
                Name = "OpenSnapToBeamTestButton",
                Content = "Test Snap to Beam",
                HorizontalAlignment = Layout.HorizontalAlignment.Stretch,
                Margin = Thickness(0.0, 0.0, 0.0, 8.0))
        snapTestButton.Click.Add(fun _ -> SnapToBeamWindow().Show())
        let snapReflectedTestButton =
            Button(
                Name = "OpenSnapToReflectedTestButton",
                Content = "Test Snap to Reflected Light",
                HorizontalAlignment = Layout.HorizontalAlignment.Stretch)
        snapReflectedTestButton.Click.Add(fun _ -> SnapToReflectedWindow().Show())
        let panel = StackPanel(Margin = Thickness 20.0)
        panel.Children.Add title
        panel.Children.Add mainButton
        panel.Children.Add tableTestButton
        panel.Children.Add elementTestButton
        panel.Children.Add tableElementTestButton
        panel.Children.Add elementMovementTestButton
        panel.Children.Add rendererTestButton
        panel.Children.Add snapTestButton
        panel.Children.Add snapReflectedTestButton
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
            desktop.MainWindow <- LauncherWindow()
        | _ -> ()

module Program =

    [<EntryPoint; STAThread>]
    let main (argv : string[]) : int =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .StartWithClassicDesktopLifetime(argv)
