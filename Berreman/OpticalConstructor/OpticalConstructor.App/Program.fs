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
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Media
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

open OpticalConstructor.Ui
open OpticalConstructor.TestWindows

/// Load the persisted user environment once at startup (J.6). `load` is total and
/// falls back to the built-in `defaults` on a missing/invalid settings file, so the
/// shell always has a usable layout/theme to render (AC-J6).
module private Startup =
    let settings = UserEnvironment.load (UserEnvironment.settingsPath ())

    /// Run the localization completeness check once at startup (Spec 0026 §I.3.1)
    /// against the persisted language. `Localization.startupCheck` loads the shipped
    /// `strings.json` from beside the assembly and returns a copyable message when the
    /// resource is missing, malformed, or incomplete for the active language; `None`
    /// when it loads and is complete. The app runs on the English fallback either way
    /// (never crash, never silent) — this value only drives whether the copyable error
    /// surface is shown. The shipped resource is complete, so it is `None` in a normal
    /// run and under the headless smoke host.
    let localizationError : string option =
        Localization.startupCheck settings.language

/// The copyable localization startup-error surface (Spec 0026 §I.3.1). A small window
/// whose read-only, multiline `TextBox` holds the completeness/load message so the user
/// can select and copy it (Ctrl+C, or the Copy button) into a bug report or translation
/// fix — rather than the app failing silently or crashing. Shown only when
/// `Startup.localizationError` is `Some`, so it never appears for the complete shipped
/// resource (and so never under the smoke host). The Copy button guards a null
/// clipboard, so it is harmless on the headless platform.
type private LocalizationErrorWindow(message : string) as this =
    inherit Window()
    do
        this.Title <- "Optical Constructor — Localization"
        this.Width <- 640.0
        this.Height <- 420.0
        let box =
            TextBox(
                Text = message,
                IsReadOnly = true,
                AcceptsReturn = true,
                TextWrapping = TextWrapping.Wrap,
                VerticalAlignment = Layout.VerticalAlignment.Stretch)
        let copy = Button(Content = "Copy to clipboard", Margin = Thickness(0.0, 8.0, 0.0, 0.0))
        copy.Click.Add(fun _ ->
            match this.Clipboard with
            | null -> ()
            // Avalonia 12 moved `SetTextAsync` to an extension on `IClipboard` (ClipboardExtensions),
            // called statically since this file does not `open Avalonia.Input.Platform`.
            | clip -> Avalonia.Input.Platform.ClipboardExtensions.SetTextAsync(clip, message) |> ignore)
        let panel = StackPanel(Margin = Thickness 12.0)
        panel.Children.Add box
        panel.Children.Add copy
        this.Content <- panel

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
        // spec 0024 Part U8 / R-1 (§0.5): register this window's storage provider with the
        // shell host layer once it is available (on open), so the Open/Save file pickers
        // have an `IStorageProvider` — held OUTSIDE the root model.
        this.Opened.Add(fun _ -> Shell.setStorageProvider this.StorageProvider)
        // Spec 0026 §I.3.1: once the main window is open, surface the copyable
        // localization startup error (if any) in a separate window. Deferring to
        // `Opened` keeps it additive — the shell still renders on the English fallback,
        // so a missing translation neither blocks startup nor crashes (AC-I3).
        match Startup.localizationError with
        | Some message -> this.Opened.Add(fun _ -> LocalizationErrorWindow(message).Show())
        | None -> ()
        // Spec 0026 D.6.1: the constructor is the default landing page. That landing is
        // realised in `Shell.initFrom` (which seeds `page = Page.Constructor`), so the
        // composition root mounts it for free here — no separate page selection at the
        // host. The localization completeness surface above is unchanged (slice 003).
        Program.mkProgram (fun () -> Shell.initFrom Startup.settings) Shell.update Shell.view
        |> Program.withHost this
        |> Program.run

/// The Main screen (Spec 0027): the dynamic "Lego constructor". It is the SAME table + element scene
/// as "Test Table + Element Rotations" (`TableAndElementRotationView` — same table, same initial zoom,
/// same select/unselect + rotation/zoom/pan logic), seeded with a light source and a detector and given
/// an add/remove palette so elements can be added and removed at runtime. The ONLY difference from the
/// test scene is that palette (`initMain`); the scene logic is shared. (The old constructor shell —
/// `MainWindow` above — is kept but no longer opened by the launcher's Main button.)
type MainConstructorWindow() as this =
    inherit HostWindow()
    do
        this.Title <- "Optical Constructor — Main"
        this.Width <- TableAndElementRotationView.canvasWidth
        this.Height <- TableAndElementRotationView.canvasHeight + 170.0
        Program.mkSimple TableAndElementRotationView.initMain TableAndElementRotationView.update TableAndElementRotationView.view
        |> Program.withHost this
        |> Program.run

/// The simple launcher form (Spec 0027): `Main` opens the existing Optical Constructor window
/// (the `MainWindow` above, unchanged); the test buttons open the diagnostic test windows
/// (`OpticalConstructor.TestWindows`). This is the app's startup window so every path is one
/// click away; further test windows are added as buttons here and live in the TestWindows project.
type LauncherWindow() as this =
    inherit Window()

    do
        this.Title <- "Optical Constructor — Launcher"
        this.Width <- 380.0
        this.Height <- 420.0
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
                HorizontalAlignment = Layout.HorizontalAlignment.Stretch)
        rendererTestButton.Click.Add(fun _ -> RendererTestWindow().Show())
        let panel = StackPanel(Margin = Thickness 20.0)
        panel.Children.Add title
        panel.Children.Add mainButton
        panel.Children.Add tableTestButton
        panel.Children.Add elementTestButton
        panel.Children.Add tableElementTestButton
        panel.Children.Add elementMovementTestButton
        panel.Children.Add rendererTestButton
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
