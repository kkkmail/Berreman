/// Runnable composition root for the DIAGNOSTIC test-window launcher (spec 0038
/// Part C, step 004): its own executable, mirroring the product bootstrap (Fluent
/// theme, classic desktop lifetime) so the diagnostic scenes run without touching
/// the product dependency graph. No persisted-environment theme seam here — the
/// diagnostics always start under the default Fluent variant.
namespace OpticalConstructor.TestWindows.App

open System

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent

/// The Avalonia application hosting the test launcher as its startup window.
type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            // Closing the launcher after opening a diagnostic scene must NOT quit
            // the app, so shut down only when the last window closes.
            desktop.ShutdownMode <- ShutdownMode.OnLastWindowClose
            desktop.MainWindow <- TestLauncherWindow()
        | _ -> ()

module Program =

    [<EntryPoint; STAThread>]
    let main (argv : string[]) : int =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .StartWithClassicDesktopLifetime(argv)
