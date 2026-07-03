namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open OpticalConstructor.Controls

/// Tests for the shared rotation-controls bar's task-009 behaviour: an axis's +/- buttons "light
/// up" (accent background) while its rotate modifier is held. Two layers:
///   * pure — the modifier → armed-axis map (`activeAxisForModifiers`) matches the wheel-rotate map;
///   * headless — a real Shift key press, routed through the live input pipeline, turns the R1
///     buttons' background to the accent (and leaves the others alone), and releasing reverts it.
module RotationControlsTests =

    // ---- pure: the modifier → armed-axis map (drives the highlight) ----

    [<Fact>]
    let ``the armed axis follows the wheel-rotate modifier map`` () =
        Assert.Equal(Some RotationControls.R1, RotationControls.activeAxisForModifiers KeyModifiers.Shift)
        Assert.Equal(Some RotationControls.R2, RotationControls.activeAxisForModifiers (KeyModifiers.Control ||| KeyModifiers.Shift))
        Assert.Equal(Some RotationControls.R3, RotationControls.activeAxisForModifiers KeyModifiers.Alt)
        // Anything that is not exactly a rotate combo arms nothing (e.g. the zoom combos, no modifier).
        Assert.Equal(None, RotationControls.activeAxisForModifiers KeyModifiers.None)
        Assert.Equal(None, RotationControls.activeAxisForModifiers KeyModifiers.Control)
        Assert.Equal(None, RotationControls.activeAxisForModifiers (KeyModifiers.Control ||| KeyModifiers.Alt))                       // zoom element
        Assert.Equal(None, RotationControls.activeAxisForModifiers (KeyModifiers.Control ||| KeyModifiers.Shift ||| KeyModifiers.Alt)) // zoom all
        Assert.Equal(None, RotationControls.activeAxisForModifiers (KeyModifiers.Shift ||| KeyModifiers.Alt))

    // ---- headless: a real key press drives the live highlight ----

    let private noopHandlers : RotationControls.Handlers =
        { rotate = fun _ _ -> ()
          setAngle = fun _ _ -> ()
          toggleR3Lock = fun () -> ()
          requestReset = fun () -> ()
          requestResetAll = fun () -> ()
          confirm = fun () -> ()
          cancel = fun () -> () }

    let private enabledState : RotationControls.State =
        { r1 = 0.0; r2 = 0.0; r3 = 0.0; r3Locked = false; enabled = true; confirm = RotationControls.NoConfirm }

    /// The light-accent fill the bar paints onto an armed axis's buttons (#CCE8FF, from the control).
    let private accent : Color = Color.FromRgb(204uy, 232uy, 255uy)

    let private borderNamed (w : Window) (name : string) : Border option =
        w.GetVisualDescendants()
        |> Seq.choose (fun v -> match v with | :? Border as b when b.Name = name -> Some b | _ -> None)
        |> Seq.tryHead

    let private backgroundColor (w : Window) (name : string) : Color option =
        match borderNamed w name with
        | Some b -> (match b.Background with | :? SolidColorBrush as s -> Some s.Color | _ -> None)
        | None -> None

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``holding an axis modifier lights up that axis's buttons only, and releasing reverts`` () =
        HeadlessSession.run (fun () ->
            let window = Window(Width = 760.0, Height = 200.0)
            window.Content <- Component(fun _ -> RotationControls.view enabledState noopHandlers)
            window.Show()
            Dispatcher.UIThread.RunJobs()

            // Idle: no axis is armed.
            Assert.NotEqual(Some accent, backgroundColor window RotationControls.UiIds.r1Plus)

            // Hold Shift → R1 arms (R1−/R1+ both accent), R3 stays idle.
            window.KeyPressQwerty(PhysicalKey.ShiftLeft, RawInputModifiers.Shift)
            Dispatcher.UIThread.RunJobs()
            Assert.Equal(Some accent, backgroundColor window RotationControls.UiIds.r1Plus)
            Assert.Equal(Some accent, backgroundColor window RotationControls.UiIds.r1Minus)
            Assert.NotEqual(Some accent, backgroundColor window RotationControls.UiIds.r3Plus)

            // Release Shift → back to idle.
            window.KeyReleaseQwerty(PhysicalKey.ShiftLeft, RawInputModifiers.None)
            Dispatcher.UIThread.RunJobs()
            Assert.NotEqual(Some accent, backgroundColor window RotationControls.UiIds.r1Plus)

            window.Close())

    /// The "Lock R3" / "Unlock R3" button's current label.
    let private lockLabel (window : Window) : string =
        match borderNamed window RotationControls.UiIds.lockR3 with
        | Some b -> (match b.Child with | :? TextBlock as t -> t.Text | _ -> "<no text>")
        | None -> "<not found>"

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the bar reflects a changed r3Locked prop from the host (no stale per-element lock)`` () =
        // Reproduces the task-009 regression: the bar is a stateful Component, so it must still pick
        // up new props when the host re-renders (e.g. selecting a different element with its OWN R3
        // lock). A parent component flips `r3Locked`; the lock button label must follow.
        HeadlessSession.run (fun () ->
            let setLocked : (bool -> unit) ref = ref (fun _ -> ())
            let host =
                Component(fun ctx ->
                    let locked = ctx.useState false
                    setLocked.Value <- locked.Set
                    RotationControls.view { enabledState with r3Locked = locked.Current } noopHandlers)
            let window = Window(Width = 760.0, Height = 200.0)
            window.Content <- host
            window.Show()
            Dispatcher.UIThread.RunJobs()

            // Unlocked element → "Lock R3".
            Assert.Equal("Lock R3", lockLabel window)
            // The host now supplies a LOCKED element (as if a different element were selected).
            setLocked.Value true
            Dispatcher.UIThread.RunJobs()
            Assert.Equal("Unlock R3", lockLabel window)
            // And back to unlocked.
            setLocked.Value false
            Dispatcher.UIThread.RunJobs()
            Assert.Equal("Lock R3", lockLabel window)

            window.Close())

    let private buttonLabel (window : Window) (name : string) : string =
        match borderNamed window name with
        | Some b -> (match b.Child with | :? TextBlock as t -> t.Text | _ -> "<no text>")
        | None -> "<not found>"

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``arming Reset turns the two buttons into the Yes/No confirmation in place (no rename exception)`` () =
        // Regression: a structural swap to differently-named confirm buttons made FuncUI reuse a Border
        // and re-set its Name, throwing "Cannot set Name : styled element already styled". The two reset
        // buttons now keep their ids and just change label/action, so the confirm flow renders cleanly.
        HeadlessSession.run (fun () ->
            let setConfirm : (RotationControls.ResetConfirm -> unit) ref = ref (fun _ -> ())
            let host =
                Component(fun ctx ->
                    let confirm = ctx.useState RotationControls.NoConfirm
                    setConfirm.Value <- confirm.Set
                    RotationControls.view { enabledState with confirm = confirm.Current } noopHandlers)
            let window = Window(Width = 760.0, Height = 200.0)
            window.Content <- host
            window.Show()
            Dispatcher.UIThread.RunJobs()

            Assert.Equal("Reset", buttonLabel window RotationControls.UiIds.reset)
            Assert.Equal("Reset All", buttonLabel window RotationControls.UiIds.resetAll)
            // Arm the confirmation — must not throw, and the same buttons become Yes / No.
            setConfirm.Value RotationControls.ConfirmReset
            Dispatcher.UIThread.RunJobs()
            Assert.Equal("Yes", buttonLabel window RotationControls.UiIds.reset)
            Assert.Equal("No", buttonLabel window RotationControls.UiIds.resetAll)
            // Cancelling back returns them to Reset / Reset All.
            setConfirm.Value RotationControls.NoConfirm
            Dispatcher.UIThread.RunJobs()
            Assert.Equal("Reset", buttonLabel window RotationControls.UiIds.reset)

            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``clicking the armed buttons runs confirm (Yes) / cancel (No), not the original Reset actions`` () =
        // Reproduces the reported bug: after arming, the SAME reused button must run the NEW action.
        // FuncUI keeps the first handler by default, so without re-subscribing, "Yes" re-ran requestReset
        // (dialog stuck) and "No" ran requestResetAll (Reset / Reset All crossing over).
        HeadlessSession.run (fun () ->
            let mutable resets = 0
            let mutable cancels = 0
            let host =
                Component(fun ctx ->
                    let confirm = ctx.useState RotationControls.NoConfirm
                    let handlers : RotationControls.Handlers =
                        { rotate = fun _ _ -> ()
                          setAngle = fun _ _ -> ()
                          toggleR3Lock = fun () -> ()
                          requestReset = fun () -> confirm.Set RotationControls.ConfirmReset
                          requestResetAll = fun () -> confirm.Set RotationControls.ConfirmResetAll
                          confirm = fun () -> resets <- resets + 1; confirm.Set RotationControls.NoConfirm
                          cancel = fun () -> cancels <- cancels + 1; confirm.Set RotationControls.NoConfirm }
                    RotationControls.view { enabledState with confirm = confirm.Current } handlers)
            // Wide enough that the reset buttons (far right of the bar) are actually on-screen to click.
            let window = Window(Width = 1500.0, Height = 200.0)
            window.Content <- host
            window.Show()
            Dispatcher.UIThread.RunJobs()

            let click (name : string) : unit =
                match borderNamed window name with
                | Some b ->
                    let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                    if c.HasValue then
                        window.MouseDown(c.Value, MouseButton.Left, RawInputModifiers.None)
                        Dispatcher.UIThread.RunJobs()
                        window.MouseUp(c.Value, MouseButton.Left, RawInputModifiers.None)
                        Dispatcher.UIThread.RunJobs()
                    else Assert.Fail(sprintf "%s has no on-screen position" name)
                | None -> Assert.Fail(sprintf "%s not found" name)

            // Reset → arms; the SAME button now reads "Yes" and clicking it CONFIRMS (one reset, no cancel).
            click RotationControls.UiIds.reset
            Assert.Equal("Yes", buttonLabel window RotationControls.UiIds.reset)
            click RotationControls.UiIds.reset
            Assert.Equal(1, resets)
            Assert.Equal(0, cancels)
            Assert.Equal("Reset", buttonLabel window RotationControls.UiIds.reset)

            // Reset All → arms; the OTHER button now reads "No" and clicking it CANCELS (no extra reset).
            click RotationControls.UiIds.resetAll
            Assert.Equal("No", buttonLabel window RotationControls.UiIds.resetAll)
            click RotationControls.UiIds.resetAll
            Assert.Equal(1, cancels)
            Assert.Equal(1, resets)
            Assert.Equal("Reset All", buttonLabel window RotationControls.UiIds.resetAll)

            window.Close())
