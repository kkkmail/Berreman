/// Headless launch/render smoke test (spec 0024 §U1.7 / AC-U1.3, gate `ui-smoke`).
/// Trait `Category=ui-smoke` so the gate command filters to this test.
///
/// Drives the REAL `App` lifetime headlessly rather than constructing a window
/// directly. The shared headless session has already built and `Initialize()`d
/// `OpticalConstructor.App.App` — running its theme seam (`App.Initialize` ->
/// `AppShell.themeVariant`), so the persisted environment.json theme is applied.
/// The test then attaches a `ClassicDesktopStyleApplicationLifetime` and invokes
/// `App.OnFrameworkInitializationCompleted`, so the framework-init path that
/// constructs and opens the launcher (`LauncherWindow`, the startup window whose
/// Main button opens the constructor scene) actually executes. The retired Elmish
/// shell's per-page renders (spec 0038 Part B.1) are gone with the shell; the Main
/// constructor scene render is pinned by `WireUiCompositionTests`.
namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Threading
open Xunit
open OpticalConstructor.Ui
open OpticalConstructor.App

module SmokeTests =

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``app host opens the launcher with the persisted theme without throwing`` () =
        HeadlessSession.run (fun () ->
            // 1) The shared headless session built and Initialize()'d the real
            //    `OpticalConstructor.App.App` (TestApp.fs points BuildAvaloniaApp at it),
            //    so its theme seam (App.Initialize -> AppShell.themeVariant) has already
            //    run on the current application. Assert it executed: RequestedThemeVariant
            //    is the persisted theme's mapped variant, not the null default an
            //    Application carries before Initialize runs.
            let sessionApp = Application.Current :?> App
            let expectedVariant =
                AppShell.themeVariant (UserEnvironment.load (UserEnvironment.settingsPath ())).theme
            Assert.Equal(expectedVariant, sessionApp.RequestedThemeVariant)

            // 2) Drive framework-init through the REAL App lifetime. The session app's
            //    ApplicationLifetime is fixed (null, and immutable once AppBuilder-initialized),
            //    so attach a ClassicDesktopStyleApplicationLifetime to a fresh, not-yet-
            //    initialized App and invoke App.OnFrameworkInitializationCompleted. That runs
            //    the real desktop branch which constructs and sets the launcher as MainWindow
            //    on the headless platform.
            let desktop = new ClassicDesktopStyleApplicationLifetime()
            let app = App()
            app.ApplicationLifetime <- desktop
            app.Initialize()
            app.OnFrameworkInitializationCompleted()

            Assert.NotNull(desktop.MainWindow)
            desktop.MainWindow.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(desktop.MainWindow.IsVisible)
            desktop.MainWindow.Close())
