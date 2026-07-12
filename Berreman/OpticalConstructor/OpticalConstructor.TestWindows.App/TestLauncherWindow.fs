/// Spec 0038 Part C (step 004, UICOMP_XDUO_0007): the diagnostic test launcher.
/// It lives in the SEPARATE `OpticalConstructor.TestWindows.App` executable so the
/// seven diagnostic scenes stay one click away for a developer while leaving the
/// product dependency graph entirely (`OpticalConstructor.App` no longer references
/// `OpticalConstructor.TestWindows`; its launcher keeps Main only). One button per
/// diagnostic scene, each carrying a stable intent-named `Name` AND the matching
/// `AutomationProperties.AutomationId` / `AutomationProperties.Name`, so headless
/// tests and external UI automation find every scene by meaning, not by pixels.
namespace OpticalConstructor.TestWindows.App

open Avalonia
open OpticalConstructor.Controls
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Media
open OpticalConstructor.TestWindows

/// The test-launcher form: the diagnostic counterpart of the product launcher.
/// Every diagnostic scene window (hosted in `OpticalConstructor.TestWindows`) hangs
/// off one stable-named button; the window itself owns no scene state.
type TestLauncherWindow() as this =
    inherit Window()

    do
        this.Title <- "Optical Constructor — Test Launcher"
        this.Width <- 380.0
        this.Height <- 480.0
        this.CanResize <- false
        let title =
            TextBlock(
                Text = "Optical Constructor — Diagnostics",
                FontSize = 18.0,
                FontWeight = FontWeight.SemiBold,
                Margin = Thickness(0.0, 0.0, 0.0, 14.0))
        // One button per diagnostic scene: stable Name + AutomationId (same string)
        // and the human label as the accessible name.
        let sceneButton (id : string) (label : string) (openScene : unit -> Window) : Button =
            let button =
                Button(
                    Name = id,
                    Content = label,
                    HorizontalAlignment = Layout.HorizontalAlignment.Stretch,
                    Margin = Thickness(0.0, 0.0, 0.0, 8.0))
            AutomationProperties.SetAutomationId(button, id)
            AutomationProperties.SetName(button, label)
            button.Click.Add(fun _ -> (openScene ()).Show())
            button
        let scenes : (string * string * (unit -> Window)) list =
            [ UiIds.TestLauncher.openTableRotationTestButton, "Test Optical Table Rotations", fun () -> TableRotationWindow() :> Window
              UiIds.TestLauncher.openElementRotationTestButton, "Test Optical Element Rotations", fun () -> ElementRotationWindow() :> Window
              UiIds.TestLauncher.openTableAndElementRotationTestButton, "Test Table + Element Rotations", fun () -> TableAndElementRotationWindow() :> Window
              UiIds.TestLauncher.openElementMovementTestButton, "Test Element Movement", fun () -> ElementMovementWindow() :> Window
              UiIds.TestLauncher.openRendererTestButton, "Test Renderers", fun () -> RendererTestWindow() :> Window
              UiIds.TestLauncher.openSnapToBeamTestButton, "Test Snap to Beam", fun () -> SnapToBeamWindow() :> Window
              UiIds.TestLauncher.openSnapToReflectedTestButton, "Test Snap to Reflected Light", fun () -> SnapToReflectedWindow() :> Window ]
        let panel = StackPanel(Margin = Thickness 20.0)
        panel.Children.Add title
        for (id, label, openScene) in scenes do
            panel.Children.Add(sceneButton id label openScene)
        this.Content <- panel
