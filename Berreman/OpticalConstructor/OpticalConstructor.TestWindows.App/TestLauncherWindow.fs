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
open OpticalConstructor.Domain.Kit
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
        // Spec 0046: never larger than the screen it opens on (its content scrolls).
        WindowFit.keepOnScreen this
        this.CanResize <- false
        let title =
            TextBlock(
                Text = "Optical Constructor — Diagnostics",
                FontSize = 18.0,
                FontWeight = FontWeight.SemiBold,
                Margin = Thickness(0.0, 0.0, 0.0, 14.0))
        // One button per diagnostic scene: stable Name + AutomationId (same string)
        // and the human label as the accessible name.
        let kitStatus = TextBlock(TextWrapping = TextWrapping.Wrap, IsVisible = false)
        AutomationProperties.SetAutomationId(kitStatus, ConstructorUiId.kitStatus.value)
        AutomationProperties.SetName(kitStatus, "Kit status")
        let sceneButton (id : string) (label : string) (openScene : unit -> Result<Window, KitError>) : Button =
            let button =
                Button(
                    Name = id,
                    Content = label,
                    HorizontalAlignment = Layout.HorizontalAlignment.Stretch,
                    Margin = Thickness(0.0, 0.0, 0.0, 8.0))
            AutomationProperties.SetAutomationId(button, id)
            AutomationProperties.SetName(button, label)
            button.Click.Add(fun _ ->
                match openScene () with
                | Ok window ->
                    kitStatus.IsVisible <- false
                    window.Show()
                | Error error ->
                    kitStatus.Text <- $"The diagnostic could not be opened: {error.describe}"
                    kitStatus.IsVisible <- true)
            button
        let scenes : (string * string * (unit -> Result<Window, KitError>)) list =
            [ ConstructorUiId.openTableRotationTestButton.value, "Test Optical Table Rotations", fun () -> TableRotationWindow() :> Window |> Ok
              ConstructorUiId.openElementRotationTestButton.value, "Test Optical Element Rotations", fun () -> ElementRotationWindow() :> Window |> Ok
              ConstructorUiId.openTableAndElementRotationTestButton.value, "Test Table + Element Rotations", fun () -> TableAndElementRotationWindow.tryCreate () |> Result.map (fun window -> window :> Window)
              ConstructorUiId.openElementMovementTestButton.value, "Test Element Movement", fun () -> ElementMovementWindow() :> Window |> Ok
              ConstructorUiId.openRendererTestButton.value, "Test Renderers", fun () -> RendererTestWindow() :> Window |> Ok
              ConstructorUiId.openSnapToBeamTestButton.value, "Test Snap to Beam", fun () -> SnapToBeamWindow() :> Window |> Ok
              ConstructorUiId.openSnapToReflectedTestButton.value, "Test Snap to Reflected Light", fun () -> SnapToReflectedWindow() :> Window |> Ok ]
        let panel = StackPanel(Margin = Thickness 20.0)
        panel.Children.Add title
        panel.Children.Add kitStatus
        for (id, label, openScene) in scenes do
            panel.Children.Add(sceneButton id label openScene)
        this.Content <- ScrollViewer(
            Content = panel,
            HorizontalScrollBarVisibility = Primitives.ScrollBarVisibility.Disabled,
            VerticalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto)
