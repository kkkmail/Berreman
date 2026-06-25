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
