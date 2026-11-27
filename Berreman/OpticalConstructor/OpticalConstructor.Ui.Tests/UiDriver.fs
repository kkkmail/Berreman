namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Automation
open Avalonia.Automation.Peers
open Avalonia.Automation.Provider
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit

open OpticalConstructor.Controls

/// The one driver of the UI tests (spec 0051 `.manual/003` 1.5): it finds a control by the id UI Automation reports and
/// drives it through its automation pattern, or presses it at its centre with the headless mouse where a test checks
/// layout or hit-testing.
module UiDriver =

    /// The id a control answers to: its AutomationId, else its `Name`, as Avalonia's automation peer reports it.
    let idOf (control : Control) : string =
        match AutomationProperties.GetAutomationId control with
        | null | "" -> control.Name
        | id -> id

    /// Whether `control` answers to `id`.
    let hasId (id : string) (control : Control) : bool = idOf control = id

    /// Every control under `root` answering to `id`, in visual order.
    let controlsWithId (root : Visual) (id : string) : Control list =
        root.GetVisualDescendants()
        |> Seq.choose (function :? Control as control when idOf control = id -> Some control | _ -> None)
        |> List.ofSeq

    let tryFindControl (window : Window) (id : string) : Control option = controlsWithId window id |> List.tryHead

    let findControl (window : Window) (id : string) : Control =
        match tryFindControl window id with
        | Some control -> control
        | None -> failwith $"{id} must be in the window"

    let isPresent (window : Window) (id : string) : bool =
        match tryFindControl window id with
        | Some _ -> true
        | None -> false

    let assertAbsent (window : Window) (id : string) : unit = Assert.False(isPresent window id, $"{id} must not be in the window")

    /// Whether a choice is checked: a toggle's own state; any other control is unchecked.
    let isChecked (control : Control) : bool =
        match control with
        | :? Avalonia.Controls.Primitives.ToggleButton as toggle -> toggle.IsChecked = System.Nullable true
        | _ -> false

    /// The first text a control shows: a text block's or a text box's own, else the first text block inside it.
    let textOfControl (control : Control) : string =
        match control with
        | :? TextBlock as text -> text.Text
        | :? TextBox as box -> box.Text
        | _ ->
            control.GetVisualDescendants()
            |> Seq.tryPick (function :? TextBlock as text -> Some text.Text | _ -> None)
            |> Option.defaultValue ""

    let textOf (window : Window) (id : string) : string = textOfControl (findControl window id)

    /// The caption of a command control: its automation Name, else the text it shows.
    let captionOf (window : Window) (id : string) : string =
        let control = findControl window id
        match AutomationProperties.GetName control with
        | null | "" -> textOfControl control
        | name -> name

    /// The fill a control is drawn with: a command control's look (its chosen look while chosen), else a border's,
    /// panel's or text block's background.
    let fillOfControl (control : Control) : Color =
        let solid (brush : IBrush) : Color =
            match brush with
            | :? ISolidColorBrush as solid -> solid.Color
            | _ -> Colors.Transparent
        match control with
        | :? CommandButton as button -> button.Emphasis.fill
        | :? ChoiceToggle as choice ->
            match choice.IsChecked |> Option.ofNullable with
            | Some true -> choice.ChosenEmphasis.fill
            | Some false | None -> choice.Emphasis.fill
        | :? Border as border -> solid border.Background
        | :? Panel as panel -> solid panel.Background
        | :? TemplatedControl as templated -> solid templated.Background
        | _ -> Colors.Transparent

    let fillOf (window : Window) (id : string) : Color = fillOfControl (findControl window id)

    /// Whether the choice answering to `id` is the chosen one.
    let isChosen (window : Window) (id : string) : bool =
        match findControl window id with
        | :? ToggleButton as toggle -> toggle.IsChecked = System.Nullable true
        | other -> failwith $"{id} is a {other.GetType().Name}, not a choice"

    /// Renders until `centre` hit-tests to `pressed` or one of its parts, for at most 50 frames, and answers whether it
    /// does: hit testing reads the last rendered frame, which can trail a scroll (spec 0050 S11.5).
    let settlesOn (window : Window) (pressed : Control) (centre : Point) : bool =
        let hitsPressed () : bool =
            match window.InputHitTest centre with
            | :? Visual as hit -> obj.ReferenceEquals(pressed, hit) || pressed.IsVisualAncestorOf hit
            | _ -> false
        let mutable ticks = 0
        while not (hitsPressed ()) && ticks < 50 do
            AvaloniaHeadlessPlatform.ForceRenderTimerTick()
            Dispatcher.UIThread.RunJobs()
            ticks <- ticks + 1
        hitsPressed ()

    /// Presses `pressed` at its centre with the headless mouse, after bringing it into view; a press that closes the
    /// window gets no release.
    let pressAtCentre (window : Window) (pressed : Control) : unit =
        Assert.True(pressed.IsEffectivelyVisible, $"{idOf pressed} must be visible")
        pressed.BringIntoView() |> ignore
        Dispatcher.UIThread.RunJobs()
        match pressed.TranslatePoint(Point(pressed.Bounds.Width / 2.0, pressed.Bounds.Height / 2.0), window) |> Option.ofNullable with
        | Some centre ->
            settlesOn window pressed centre |> ignore
            window.MouseDown(centre, MouseButton.Left, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            if window.IsVisible then
                window.MouseUp(centre, MouseButton.Left, RawInputModifiers.None)
                Dispatcher.UIThread.RunJobs()
        | None -> Assert.Fail $"{idOf pressed} has no position in its window"

    /// Presses the control answering to `id` at its centre.
    let clickOn (window : Window) (id : string) : unit = pressAtCentre window (findControl window id)

    /// Drives the control answering to `id` through its automation pattern, as UI Automation would: Invoke for a
    /// button, Toggle for a choice.
    let invoke (window : Window) (id : string) : unit =
        let control = findControl window id
        match box (ControlAutomationPeer.CreatePeerForElement control) with
        | :? IToggleProvider as toggle -> toggle.Toggle()
        | :? IInvokeProvider as invokable -> invokable.Invoke()
        | peer -> Assert.Fail $"{id} offers no Invoke or Toggle pattern ({peer.GetType().Name})"
        Dispatcher.UIThread.RunJobs()

    /// The same driver over the typed id.
    module ById =
        let controlsWithId (root : Visual) (id : ConstructorUiId) : Control list = controlsWithId root id.value
        let tryFindControl (window : Window) (id : ConstructorUiId) : Control option = tryFindControl window id.value
        let assertAbsent (window : Window) (id : ConstructorUiId) : unit = assertAbsent window id.value
        let findControl (window : Window) (id : ConstructorUiId) : Control = findControl window id.value
        let isPresent (window : Window) (id : ConstructorUiId) : bool = isPresent window id.value
        let textOf (window : Window) (id : ConstructorUiId) : string = textOf window id.value
        let captionOf (window : Window) (id : ConstructorUiId) : string = captionOf window id.value
        let fillOf (window : Window) (id : ConstructorUiId) : Color = fillOf window id.value
        let isChosen (window : Window) (id : ConstructorUiId) : bool = isChosen window id.value
        let clickOn (window : Window) (id : ConstructorUiId) : unit = clickOn window id.value
        let invoke (window : Window) (id : ConstructorUiId) : unit = invoke window id.value
