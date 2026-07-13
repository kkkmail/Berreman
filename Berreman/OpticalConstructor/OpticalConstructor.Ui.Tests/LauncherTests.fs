/// Spec 0038 Part B.1 (step 002) / Part C (step 004) / step 045: the PRODUCT launcher contract.
/// After the Elmish shell's retirement the launcher IS the app's startup window. Step 045 finalizes
/// it to FOUR surface buttons — Main / Inverse / Materials / Library, in that order, each by a stable
/// `Name` + `AutomationId`: Main opens the forward constructor, Inverse the step-037 inverse
/// constructor, and Materials / Library the single-instance step-013/015 windows through the SAME
/// app-scoped launcher seam the constructor strip uses (`EditorLaunchers.defaults`), so a window
/// opened from the launcher and the same window opened from the constructor strip meet in ONE
/// open-or-activate space. Structure (gate `ui-tests`) pins the four buttons + their ids; behaviour
/// (gate `ui-smoke`) clicks each button headlessly and observes the surface it opens through the
/// public `Window.WindowOpenedEvent` seam (the `WireUiCompositionTests` seam), and proves the
/// launcher/constructor single-instance sharing. `SmokeTests` separately proves the REAL `App`
/// lifetime makes this window the startup `MainWindow` with the persisted theme.
namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Interactivity
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open OpticalConstructor.Domain.WorkbenchSettings
open OpticalConstructor.Controls
open OpticalConstructor.Ui
open OpticalConstructor.App

module LauncherTests =

    let private buttons (w : Window) : Button list =
        w.GetVisualDescendants()
        |> Seq.choose (function :? Button as b -> Some b | _ -> None)
        |> List.ofSeq

    let private tryFindButton (name : string) (w : Window) : Button option =
        buttons w |> List.tryFind (fun b -> b.Name = name)

    /// A control matches `id` by its `Name` OR its `AutomationProperties.AutomationId` (the windows
    /// the launcher opens set both — the `WireUiCompositionTests` precedent).
    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || AutomationProperties.GetAutomationId c = id

    /// A fresh app-scope context per launcher (spec 0038 step 006: the launcher receives the
    /// ONE `Startup.context` in the app; each test isolates its own mutable stores the same way).
    let private freshContext () : AppContext = AppContext.create WorkbenchSettings.defaults

    /// Open a fresh launcher on the shared headless session and run `check` over it.
    let private withLauncher (check : LauncherWindow -> unit) : unit =
        HeadlessSession.run (fun () ->
            let launcher = LauncherWindow(freshContext ())
            launcher.Show()
            Dispatcher.UIThread.RunJobs()
            try check launcher
            finally launcher.Close())

    /// Click the launcher button `id` and return every NEW window opened (via the public
    /// `Window.WindowOpenedEvent`), excluding the launcher itself.
    let private clickAndObserve (launcher : LauncherWindow) (id : string) : Window list =
        let opened = ResizeArray<Window>()
        use _sub =
            Window.WindowOpenedEvent.Raised
            |> Observable.subscribe (fun (struct (sender, _args)) ->
                match sender with
                | :? Window as w when not (obj.ReferenceEquals(w, launcher)) -> opened.Add w
                | _ -> ())
        (match tryFindButton id launcher with
         | Some button -> button.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
         | None -> Assert.Fail($"launcher button '%s{id}' was not found"))
        Dispatcher.UIThread.RunJobs()
        List.ofSeq opened

    /// Click the centre of the clickable Border carrying `id` (by Name or AutomationId) — the
    /// constructor strip's Materials button is a Border, not a Button (the `WireUiCompositionTests`
    /// `clickOn` seam).
    let private clickBorder (window : Window) (id : string) : unit =
        let found =
            window.GetVisualDescendants()
            |> Seq.tryPick (function :? Border as b when matchesId id b && b.IsEffectivelyVisible -> Some b | _ -> None)
        match found with
        | None -> Assert.Fail($"%s{id} was not found (or not visible)")
        | Some b ->
            let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
            if c.HasValue then
                window.MouseDown(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
                if window.IsVisible then
                    window.MouseUp(c.Value, Avalonia.Input.MouseButton.Left, Avalonia.Input.RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
            else Assert.Fail($"%s{id} has no on-screen position")

    // ------------------------- structure (gate `ui-tests`) -------------------------

    [<Fact>]
    let ``the launcher carries its title and fixed, non-resizable size`` () =
        withLauncher (fun launcher ->
            Assert.Equal("Optical Constructor — Launcher", launcher.Title)
            Assert.Equal(380.0, launcher.Width)
            Assert.Equal(300.0, launcher.Height)
            Assert.False(launcher.CanResize, "the launcher is a fixed-size form"))

    [<Fact>]
    let ``the launcher hosts Main / Inverse / Materials / Library in that order by stable name`` () =
        withLauncher (fun launcher ->
            let names = buttons launcher |> List.map (fun b -> b.Name)
            Assert.Equal<string list>(
                [ LauncherIds.openMainButton
                  LauncherIds.openInverseButton
                  LauncherIds.openMaterialsButton
                  LauncherIds.openLibraryButton ],
                names))

    [<Fact>]
    let ``each launcher button carries its stable AutomationId and label`` () =
        withLauncher (fun launcher ->
            let expected =
                [ LauncherIds.openMainButton, "Main"
                  LauncherIds.openInverseButton, "Inverse"
                  LauncherIds.openMaterialsButton, "Materials"
                  LauncherIds.openLibraryButton, "Library" ]
            for id, label in expected do
                match tryFindButton id launcher with
                | Some button ->
                    Assert.Equal(id, AutomationProperties.GetAutomationId button)
                    Assert.Equal(label, string button.Content)
                | None -> Assert.Fail($"launcher button '%s{id}' was not found"))

    // ---------------------- behaviour (gate `ui-smoke`) ----------------------
    // Click each launcher button headlessly and observe the surface it opens via
    // `Window.WindowOpenedEvent` (the WireUiCompositionTests seam).

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main button opens the forward Main constructor scene`` () =
        withLauncher (fun launcher ->
            match clickAndObserve launcher LauncherIds.openMainButton with
            | [ opened ] ->
                Assert.True(opened :? MainConstructorWindow, $"'OpenMainButton' opened a {opened.GetType().Name}")
                Assert.True(opened.IsVisible, "the opened scene window must be shown")
                opened.Close()
            | others -> Assert.Fail($"expected exactly one window, found {others.Length}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Inverse button opens the inverse constructor scene`` () =
        withLauncher (fun launcher ->
            match clickAndObserve launcher LauncherIds.openInverseButton with
            | [ opened ] ->
                Assert.True(opened :? InverseConstructorWindow, $"'OpenInverseButton' opened a {opened.GetType().Name}")
                Assert.True(opened.IsVisible, "the opened inverse scene window must be shown")
                opened.Close()
            | others -> Assert.Fail($"expected exactly one window, found {others.Length}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Materials button opens the single-instance Materials window`` () =
        withLauncher (fun launcher ->
            match clickAndObserve launcher LauncherIds.openMaterialsButton with
            | [ opened ] ->
                Assert.True(matchesId UiIds.MaterialsWindow.window opened, "'OpenMaterialsButton' must open the Materials window")
                Assert.True(opened.IsVisible, "the Materials window must be shown")
                // Close it so the shared WindowRegistry does not leak the MaterialsWindowKey registration.
                opened.Close()
                Dispatcher.UIThread.RunJobs()
            | others -> Assert.Fail($"expected exactly one window, found {others.Length}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Library button opens the single-instance Library window`` () =
        withLauncher (fun launcher ->
            match clickAndObserve launcher LauncherIds.openLibraryButton with
            | [ opened ] ->
                Assert.True(matchesId UiIds.LibraryWindow.window opened, "'OpenLibraryButton' must open the Library window")
                Assert.True(opened.IsVisible, "the Library window must be shown")
                // Close it so the shared WindowRegistry does not leak the LibraryWindowKey registration.
                opened.Close()
                Dispatcher.UIThread.RunJobs()
            | others -> Assert.Fail($"expected exactly one window, found {others.Length}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Materials from the launcher then the constructor strip activates ONE single instance`` () =
        // The launcher and the constructor share the step-006 app scope AND the host-layer
        // WindowRegistry: opening Materials from the launcher then from the constructor strip must
        // meet in ONE window (the second open ACTIVATES, it does not stack a copy).
        HeadlessSession.run (fun () ->
            let context = freshContext ()
            let launcher = LauncherWindow(context)
            launcher.Show()
            Dispatcher.UIThread.RunJobs()
            let constructorWindow = MainConstructorWindow(context)
            constructorWindow.Show()
            Dispatcher.UIThread.RunJobs()
            try
                // Count ONLY Materials-window opens across both actions.
                let materialsOpens = ResizeArray<Window>()
                use _sub =
                    Window.WindowOpenedEvent.Raised
                    |> Observable.subscribe (fun (struct (sender, _args)) ->
                        match sender with
                        | :? Window as w when matchesId UiIds.MaterialsWindow.window w -> materialsOpens.Add w
                        | _ -> ())
                // 1) launcher Materials button → CREATES the single instance.
                (match tryFindButton LauncherIds.openMaterialsButton launcher with
                 | Some button -> button.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
                 | None -> Assert.Fail "launcher button 'OpenMaterialsButton' was not found")
                Dispatcher.UIThread.RunJobs()
                Assert.Equal(1, materialsOpens.Count)
                // 2) constructor strip Materials button → ACTIVATES the SAME instance (no new window).
                clickBorder constructorWindow TableAndElementRotationView.WorkbenchIds.openMaterialsButton
                Dispatcher.UIThread.RunJobs()
                Assert.Equal(1, materialsOpens.Count)
                materialsOpens.[0].Close()
                Dispatcher.UIThread.RunJobs()
            finally
                constructorWindow.Close()
                launcher.Close())
