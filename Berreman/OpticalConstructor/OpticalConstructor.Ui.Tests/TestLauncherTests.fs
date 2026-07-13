/// Spec 0038 Part C (step 004, UICOMP_XDUO_0007): the TestLauncherWindow contract.
/// The seven diagnostic scene buttons left the product launcher for the standalone
/// `OpticalConstructor.TestWindows.App` executable; this suite pins that window's
/// surface. Structure (gate `ui-tests`): every scene button present by stable `Name`
/// with its label AND its `AutomationProperties.AutomationId` / accessible name, in
/// the declared order. Behaviour (gate `ui-smoke`): the window renders all seven
/// buttons under a headless frame, clicking EVERY button opens its diagnostic scene
/// window (observed through the public `Window.WindowOpenedEvent` seam), and the new
/// executable's own `App` lifetime makes TestLauncherWindow its startup `MainWindow`
/// (the SmokeTests pattern against the new composition root).
namespace OpticalConstructor.Ui.Tests

open Avalonia.Automation
open OpticalConstructor.Controls
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.App

module TestLauncherTests =

    let private buttons (w : Window) : Button list =
        w.GetVisualDescendants()
        |> Seq.choose (function :? Button as b -> Some b | _ -> None)
        |> List.ofSeq

    let private tryFindButton (name : string) (w : Window) : Button option =
        buttons w |> List.tryFind (fun b -> b.Name = name)

    /// Open a fresh test launcher on the shared headless session and run `check` over it.
    let private withTestLauncher (check : TestLauncherWindow -> unit) : unit =
        HeadlessSession.run (fun () ->
            let launcher = TestLauncherWindow()
            launcher.Show()
            Dispatcher.UIThread.RunJobs()
            try check launcher
            finally launcher.Close())

    /// The component's declared button order — one id per diagnostic scene.
    let private sceneButtonIds : string list =
        [ UiIds.TestLauncher.openTableRotationTestButton
          UiIds.TestLauncher.openElementRotationTestButton
          UiIds.TestLauncher.openTableAndElementRotationTestButton
          UiIds.TestLauncher.openElementMovementTestButton
          UiIds.TestLauncher.openRendererTestButton
          UiIds.TestLauncher.openSnapToBeamTestButton
          UiIds.TestLauncher.openSnapToReflectedTestButton ]

    // ------------------------- structure (gate `ui-tests`) -------------------------

    [<Fact>]
    let ``the test launcher carries its title and fixed, non-resizable size`` () =
        withTestLauncher (fun launcher ->
            Assert.Equal("Optical Constructor — Test Launcher", launcher.Title)
            Assert.Equal(380.0, launcher.Width)
            Assert.Equal(480.0, launcher.Height)
            Assert.False(launcher.CanResize, "the test launcher is a fixed-size form"))

    [<Theory>]
    [<InlineData(UiIds.TestLauncher.openTableRotationTestButton, "Test Optical Table Rotations")>]
    [<InlineData(UiIds.TestLauncher.openElementRotationTestButton, "Test Optical Element Rotations")>]
    [<InlineData(UiIds.TestLauncher.openTableAndElementRotationTestButton, "Test Table + Element Rotations")>]
    [<InlineData(UiIds.TestLauncher.openElementMovementTestButton, "Test Element Movement")>]
    [<InlineData(UiIds.TestLauncher.openRendererTestButton, "Test Renderers")>]
    [<InlineData(UiIds.TestLauncher.openSnapToBeamTestButton, "Test Snap to Beam")>]
    [<InlineData(UiIds.TestLauncher.openSnapToReflectedTestButton, "Test Snap to Reflected Light")>]
    let ``the test launcher hosts every scene button by stable name with its label`` (name : string) (label : string) =
        withTestLauncher (fun launcher ->
            match tryFindButton name launcher with
            | Some button -> Assert.Equal(label, string button.Content)
            | None -> Assert.Fail($"test-launcher button '{name}' was not found"))

    [<Theory>]
    [<InlineData(UiIds.TestLauncher.openTableRotationTestButton, "Test Optical Table Rotations")>]
    [<InlineData(UiIds.TestLauncher.openElementRotationTestButton, "Test Optical Element Rotations")>]
    [<InlineData(UiIds.TestLauncher.openTableAndElementRotationTestButton, "Test Table + Element Rotations")>]
    [<InlineData(UiIds.TestLauncher.openElementMovementTestButton, "Test Element Movement")>]
    [<InlineData(UiIds.TestLauncher.openRendererTestButton, "Test Renderers")>]
    [<InlineData(UiIds.TestLauncher.openSnapToBeamTestButton, "Test Snap to Beam")>]
    [<InlineData(UiIds.TestLauncher.openSnapToReflectedTestButton, "Test Snap to Reflected Light")>]
    let ``every scene button carries its automation id and accessible name`` (name : string) (label : string) =
        withTestLauncher (fun launcher ->
            match tryFindButton name launcher with
            | Some button ->
                Assert.Equal(name, AutomationProperties.GetAutomationId(button))
                Assert.Equal(label, AutomationProperties.GetName(button))
            | None -> Assert.Fail($"test-launcher button '{name}' was not found"))

    [<Fact>]
    let ``the test launcher hosts exactly the seven scene buttons in declared order`` () =
        withTestLauncher (fun launcher ->
            let names = buttons launcher |> List.map (fun b -> b.Name)
            Assert.Equal<string list>(sceneButtonIds, names))

    // ---------------------- behaviour (gate `ui-smoke`) ----------------------

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the test launcher renders all seven scene buttons under a headless frame`` () =
        withTestLauncher (fun launcher ->
            Assert.True(launcher.IsVisible, "the test launcher must be shown")
            for id in sceneButtonIds do
                match tryFindButton id launcher with
                | Some button -> Assert.True(button.IsVisible, $"scene button '{id}' must be visible in the rendered frame")
                | None -> Assert.Fail($"test-launcher button '{id}' was not found"))

    // Click each scene button headlessly and observe the diagnostic window it opens
    // via `Window.WindowOpenedEvent` (the LauncherTests / WireUiCompositionTests seam).

    let private clickOpens (buttonName : string) (isExpected : Window -> bool) : unit =
        HeadlessSession.run (fun () ->
            let launcher = TestLauncherWindow()
            launcher.Show()
            Dispatcher.UIThread.RunJobs()
            try
                let opened = ResizeArray<Window>()
                use _sub =
                    Window.WindowOpenedEvent.Raised
                    |> Observable.subscribe (fun (struct (sender, _args)) ->
                        match sender with
                        | :? Window as w when not (obj.ReferenceEquals(w, launcher)) -> opened.Add w
                        | _ -> ())
                (match tryFindButton buttonName launcher with
                 | Some button -> button.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
                 | None -> Assert.Fail($"test-launcher button '{buttonName}' was not found"))
                Dispatcher.UIThread.RunJobs()
                Assert.Equal(1, opened.Count)
                Assert.True(isExpected opened.[0], $"'{buttonName}' opened a {opened.[0].GetType().Name}")
                Assert.True(opened.[0].IsVisible, "the opened scene window must be shown")
                opened.[0].Close()
            finally launcher.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the table-rotation button opens its test window`` () =
        clickOpens UiIds.TestLauncher.openTableRotationTestButton (fun w -> w :? TableRotationWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the element-rotation button opens its test window`` () =
        clickOpens UiIds.TestLauncher.openElementRotationTestButton (fun w -> w :? ElementRotationWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the table-and-element-rotation button opens its test window`` () =
        clickOpens UiIds.TestLauncher.openTableAndElementRotationTestButton (fun w -> w :? TableAndElementRotationWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the element-movement button opens its test window`` () =
        clickOpens UiIds.TestLauncher.openElementMovementTestButton (fun w -> w :? ElementMovementWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the renderer button opens its test window`` () =
        clickOpens UiIds.TestLauncher.openRendererTestButton (fun w -> w :? RendererTestWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the snap-to-beam button opens its test window`` () =
        clickOpens UiIds.TestLauncher.openSnapToBeamTestButton (fun w -> w :? SnapToBeamWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the snap-to-reflected button opens its test window`` () =
        clickOpens UiIds.TestLauncher.openSnapToReflectedTestButton (fun w -> w :? SnapToReflectedWindow)

    // The new executable's own composition root (the SmokeTests pattern): a fresh
    // `OpticalConstructor.TestWindows.App.App` with an attached desktop lifetime must
    // make TestLauncherWindow its startup MainWindow and keep running until the last
    // window closes.

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the test-windows app host opens the test launcher as its startup window`` () =
        HeadlessSession.run (fun () ->
            let desktop = new ClassicDesktopStyleApplicationLifetime()
            let app = App()
            app.ApplicationLifetime <- desktop
            app.Initialize()
            app.OnFrameworkInitializationCompleted()

            Assert.Equal(ShutdownMode.OnLastWindowClose, desktop.ShutdownMode)
            Assert.NotNull(desktop.MainWindow)
            Assert.True(desktop.MainWindow :? TestLauncherWindow, $"the startup window is a {desktop.MainWindow.GetType().Name}")
            desktop.MainWindow.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(desktop.MainWindow.IsVisible)
            desktop.MainWindow.Close())
