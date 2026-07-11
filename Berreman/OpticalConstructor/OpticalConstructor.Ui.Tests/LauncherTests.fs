/// Spec 0038 Part B.1 (step 002) / Part C (step 004): the PRODUCT launcher contract.
/// After the Elmish shell's retirement the launcher IS the app's startup window and
/// its Main button is the ONLY path into the Main constructor scene. Step 004 moved
/// the seven diagnostic scene buttons to the standalone
/// `OpticalConstructor.TestWindows.App` executable (`TestLauncherTests` pins that
/// window), so the product launcher hosts Main ALONE — structure (gate `ui-tests`)
/// pins that the diagnostic buttons are gone, and behaviour (gate `ui-smoke`) pins
/// the Main click opening its scene headlessly through the public
/// `Window.WindowOpenedEvent` seam; `SmokeTests` separately proves the REAL `App`
/// lifetime makes this window the startup `MainWindow` with the persisted theme.
namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.App

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
            Assert.Equal(180.0, launcher.Height)
            Assert.False(launcher.CanResize, "the launcher is a fixed-size form"))

    [<Fact>]
    let ``the launcher hosts the Main button by stable name with its label`` () =
        withLauncher (fun launcher ->
            match tryFindButton "OpenMainButton" launcher with
            | Some button -> Assert.Equal("Main", string button.Content)
            | None -> Assert.Fail "launcher button 'OpenMainButton' was not found")

    [<Fact>]
    let ``the diagnostic scene buttons are gone — Main is the launcher's only button`` () =
        withLauncher (fun launcher ->
            match buttons launcher with
            | [ only ] -> Assert.Equal("OpenMainButton", only.Name)
            | others -> Assert.Fail($"expected exactly one launcher button, found {others.Length}"))

    // ---------------------- behaviour (gate `ui-smoke`) ----------------------
    // Click the Main button headlessly and observe the scene window it opens
    // via `Window.WindowOpenedEvent` (the WireUiCompositionTests seam).

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main button opens the Main constructor scene`` () =
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
                (match tryFindButton "OpenMainButton" launcher with
                 | Some button -> button.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
                 | None -> Assert.Fail "launcher button 'OpenMainButton' was not found")
                Dispatcher.UIThread.RunJobs()
                Assert.Equal(1, opened.Count)
                Assert.True(opened.[0] :? MainConstructorWindow, $"'OpenMainButton' opened a {opened.[0].GetType().Name}")
                Assert.True(opened.[0].IsVisible, "the opened scene window must be shown")
                opened.[0].Close()
            finally launcher.Close())
