/// Runnable composition root (§A.1 / §A.6). The slice-001 scaffold left this as a
/// stub `main` that returned 0 without starting Avalonia, so the built exe exited
/// immediately. The UI slices (002–016) authored `OpticalConstructor.Ui` as a
/// library of FuncUI views (`AppShell.shellView`, etc.) but never wired a host;
/// this module supplies the missing Avalonia `Application` + FuncUI `HostWindow`
/// bootstrap so the window actually opens.
///
/// The bootstrap uses the public MIT `Avalonia.FuncUI` 1.6.0 NuGet surface — the
/// same buildable interpretation of §A.6 the Ui project compiles against. The
/// audit-gated clone at `C:\GitHub\Avalonia.FuncUI.Clone\` stays UNREFERENCED and
/// its linking mechanism UNRESOLVED (constraint 5 / §A.9 / AC-A8): nothing here
/// references it.
namespace OpticalConstructor.App

open System

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

open OpticalConstructor.Ui

/// Load the persisted user environment once at startup (J.6). `load` is total and
/// falls back to the built-in `defaults` on a missing/invalid settings file, so the
/// shell always has a usable layout/theme to render (AC-J6).
module private Startup =
    let settings = UserEnvironment.load (UserEnvironment.settingsPath ())

/// The main application window: a FuncUI `HostWindow` hosting the spec-0024 root MVU
/// loop. The static `Component` mount the slice-001 scaffold carried is replaced by
/// an `Avalonia.FuncUI.Elmish` program (spec 0024 §U1.3 / R-3): `Shell.init`/`update`/
/// `view` are mounted on this host, seeded from the already-loaded `Startup.settings`
/// so the persisted theme/layout still drive the first frame.
type MainWindow() as this =
    inherit HostWindow()
    do
        this.Title <- "Optical Constructor"
        this.Width <- 1200.0
        this.Height <- 800.0
        Program.mkProgram (fun () -> Shell.initFrom Startup.settings) Shell.update Shell.view
        |> Program.withHost this
        |> Program.run

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
            desktop.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint; STAThread>]
    let main (argv : string[]) : int =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .StartWithClassicDesktopLifetime(argv)
