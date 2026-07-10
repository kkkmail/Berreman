/// Spec 0038 Part B.1 (step 002): the launcher contract. After the Elmish shell's
/// retirement the launcher IS the app's startup window; its Main button is the ONLY
/// path into the Main constructor scene, and every diagnostic scene hangs off one of
/// its stable-named buttons. Structure (each scene button present by `Name` with its
/// label — gate `ui-tests`) and behaviour (clicking EVERY button opens its scene
/// window headlessly, observed through the public `Window.WindowOpenedEvent` seam —
/// gate `ui-smoke`) are pinned here; `SmokeTests` separately proves the REAL `App`
/// lifetime makes this window the startup `MainWindow` with the persisted theme.
namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.App
open OpticalConstructor.TestWindows

module LauncherTests =

    let private buttons (w : Window) : Button list =
        w.GetVisualDescendants()
        |> Seq.choose (function :? Button as b -> Some b | _ -> None)
        |> List.ofSeq

    let private tryFindButton (name : string) (w : Window) : Button option =
        buttons w |> List.tryFind (fun b -> b.Name = name)

    /// Open a fresh launcher on the shared headless session and run `check` over it.
    let private withLauncher (check : LauncherWindow -> unit) : unit =
        HeadlessSession.run (fun () ->
            let launcher = LauncherWindow()
            launcher.Show()
            Dispatcher.UIThread.RunJobs()
            try check launcher
            finally launcher.Close())

    // ------------------------- structure (gate `ui-tests`) -------------------------

    [<Fact>]
    let ``the launcher carries its title and fixed, non-resizable size`` () =
        withLauncher (fun launcher ->
            Assert.Equal("Optical Constructor — Launcher", launcher.Title)
            Assert.Equal(380.0, launcher.Width)
            Assert.Equal(520.0, launcher.Height)
            Assert.False(launcher.CanResize, "the launcher is a fixed-size form"))

    [<Theory>]
    [<InlineData("OpenMainButton", "Main")>]
    [<InlineData("OpenTableRotationTestButton", "Test Optical Table Rotations")>]
    [<InlineData("OpenElementRotationTestButton", "Test Optical Element Rotations")>]
    [<InlineData("OpenTableAndElementRotationTestButton", "Test Table + Element Rotations")>]
    [<InlineData("OpenElementMovementTestButton", "Test Element Movement")>]
    [<InlineData("OpenRendererTestButton", "Test Renderers")>]
    [<InlineData("OpenSnapToBeamTestButton", "Test Snap to Beam")>]
    [<InlineData("OpenSnapToReflectedTestButton", "Test Snap to Reflected Light")>]
    let ``the launcher hosts every scene button by stable name with its label`` (name : string) (label : string) =
        withLauncher (fun launcher ->
            match tryFindButton name launcher with
            | Some button -> Assert.Equal(label, string button.Content)
            | None -> Assert.Fail($"launcher button '{name}' was not found"))

    [<Fact>]
    let ``Main is the first scene button, one click from startup`` () =
        withLauncher (fun launcher ->
            match buttons launcher with
            | first :: _ -> Assert.Equal("OpenMainButton", first.Name)
            | [] -> Assert.Fail "the launcher hosts no buttons")

    // ---------------------- behaviour (gate `ui-smoke`) ----------------------
    // Click each launcher button headlessly and observe the scene window it opens
    // via `Window.WindowOpenedEvent` (the WireUiCompositionTests seam).

    let private clickOpens (buttonName : string) (isExpected : Window -> bool) : unit =
        HeadlessSession.run (fun () ->
            let launcher = LauncherWindow()
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
                 | None -> Assert.Fail($"launcher button '{buttonName}' was not found"))
                Dispatcher.UIThread.RunJobs()
                Assert.Equal(1, opened.Count)
                Assert.True(isExpected opened.[0], $"'{buttonName}' opened a {opened.[0].GetType().Name}")
                Assert.True(opened.[0].IsVisible, "the opened scene window must be shown")
                opened.[0].Close()
            finally launcher.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main button opens the Main constructor scene`` () =
        clickOpens "OpenMainButton" (fun w -> w :? MainConstructorWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the table-rotation button opens its test window`` () =
        clickOpens "OpenTableRotationTestButton" (fun w -> w :? TableRotationWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the element-rotation button opens its test window`` () =
        clickOpens "OpenElementRotationTestButton" (fun w -> w :? ElementRotationWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the table-and-element-rotation button opens its test window`` () =
        clickOpens "OpenTableAndElementRotationTestButton" (fun w -> w :? TableAndElementRotationWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the element-movement button opens its test window`` () =
        clickOpens "OpenElementMovementTestButton" (fun w -> w :? ElementMovementWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the renderer button opens its test window`` () =
        clickOpens "OpenRendererTestButton" (fun w -> w :? RendererTestWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the snap-to-beam button opens its test window`` () =
        clickOpens "OpenSnapToBeamTestButton" (fun w -> w :? SnapToBeamWindow)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the snap-to-reflected button opens its test window`` () =
        clickOpens "OpenSnapToReflectedTestButton" (fun w -> w :? SnapToReflectedWindow)
