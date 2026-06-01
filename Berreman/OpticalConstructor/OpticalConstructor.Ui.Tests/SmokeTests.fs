/// Headless launch/render smoke test (spec 0024 §U1.7 / AC-U1.3, gate `ui-smoke`).
/// Trait `Category=ui-smoke` so the gate command filters to exactly this test.
///
/// Rather than constructing `MainWindow()` directly (which bypasses the App's own
/// framework-init), this drives the REAL `App` lifetime headlessly. The shared headless
/// session has already built and `Initialize()`d `OpticalConstructor.App.App` — running
/// its theme seam (`App.Initialize` -> `AppShell.themeVariant`, Program.fs:52-54). The
/// test then attaches a `ClassicDesktopStyleApplicationLifetime` and invokes
/// `App.OnFrameworkInitializationCompleted` (Program.fs:56-60), so the framework-init
/// path that constructs and opens `MainWindow` (which mounts the Elmish `Shell` program)
/// actually executes. It asserts the desktop lifetime's `MainWindow` was set, shows it,
/// confirms the theme seam ran, then renders each page body (Construction + Synthesis/Fit)
/// one frame without throwing.
namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Threading
open Avalonia.FuncUI
open Avalonia.FuncUI.Types
open Xunit
open OpticalConstructor.Ui
open OpticalConstructor.App

module SmokeTests =

    /// Host a pre-built view in a headless window, show it, and flush the dispatcher
    /// so the view materializes and a frame lays out. Throws if the view body throws.
    let private renderInWindow (content : IView) : unit =
        let window = Window()
        window.Content <- Component(fun _ctx -> content)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        Assert.True(window.IsVisible)
        window.Close()

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``app host opens and renders every panel and page without throwing`` () =
        HeadlessSession.run (fun () ->
            // 1) The shared headless session built and Initialize()'d the real
            //    `OpticalConstructor.App.App` (TestApp.fs points BuildAvaloniaApp at it),
            //    so its theme seam (App.Initialize -> AppShell.themeVariant, Program.fs:52-54)
            //    has already run on the current application. Assert it executed:
            //    RequestedThemeVariant is the persisted theme's mapped variant, not the
            //    null default an Application carries before Initialize runs.
            let sessionApp = Application.Current :?> App
            let expectedVariant =
                AppShell.themeVariant (UserEnvironment.load (UserEnvironment.settingsPath ())).theme
            Assert.Equal(expectedVariant, sessionApp.RequestedThemeVariant)

            // 2) Drive framework-init through the REAL App lifetime. The session app's
            //    ApplicationLifetime is fixed (null, and immutable once AppBuilder-initialized),
            //    so attach a ClassicDesktopStyleApplicationLifetime to a fresh, not-yet-
            //    initialized App and invoke App.OnFrameworkInitializationCompleted
            //    (Program.fs:56-60). That runs the real desktop branch which constructs and
            //    sets MainWindow (mounting the Elmish Shell program — Shell.init/update/view —
            //    and rendering every visible panel) on the headless platform.
            let desktop = new ClassicDesktopStyleApplicationLifetime()
            let app = App()
            app.ApplicationLifetime <- desktop
            app.Initialize()
            app.OnFrameworkInitializationCompleted()

            Assert.NotNull(desktop.MainWindow)
            desktop.MainWindow.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(desktop.MainWindow.IsVisible)
            desktop.MainWindow.Close()

            // 3) Each page body renders one frame without throwing.
            let model = fst Shell.init
            for page in [ Shell.Page.Construction; Shell.Page.SynthesisFit ] do
                renderInWindow (Shell.view { model with page = page } ignore))
