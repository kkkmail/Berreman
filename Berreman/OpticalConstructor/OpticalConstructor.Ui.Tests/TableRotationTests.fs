namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.TableView
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.TableRotationView

/// Tests for the table-rotation test window (Spec 0027, task 002-rotate-table). Two layers:
///   * pure MVU `update` proves the rotation maths — button steps (15° / Shift 5°), mod-360,
///     drag-to-tumble, wheel-spin, and click-select — with no window at all;
///   * **real headless pointer injection** (`Avalonia.Headless` `MouseDown/Move/Up/Wheel`)
///     drives ACTUAL mouse events through the live input pipeline into the real FuncUI view
///     and asserts the model rotated — i.e. it tests "rotation by mouse", the thing that was
///     broken, end to end. The handlers are dumb event→message translators, so a manual MVU
///     loop (mutable model + capturing dispatch) lets the test read the model after injection.
module TableRotationTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9

    // ============================ pure MVU ============================

    [<Fact>]
    let ``init starts straight top-down and unselected`` () =
        let m = TableRotationView.init ()
        Assert.True(close m.view.r1.degrees 0.0 && close m.view.r2.degrees 0.0 && close m.view.r3.degrees 0.0)
        Assert.Equal(TableUnselected, m.selection)

    [<Fact>]
    let ``the button step is 15 degrees normally and 5 degrees with Shift`` () =
        Assert.True(close (buttonStepDegrees false) 15.0)
        Assert.True(close (buttonStepDegrees true) 5.0)

    [<Fact>]
    let ``a button rotation advances exactly its axis by the given degrees`` () =
        let init = TableRotationView.init ()
        let r2 = TableRotationView.update (RotateR2By 5.0) init
        Assert.True(close r2.view.r2.degrees 5.0, $"R2 = {r2.view.r2.degrees}")
        Assert.True(close r2.view.r1.degrees 0.0 && close r2.view.r3.degrees 0.0, "only R2 should move")

    [<Fact>]
    let ``angles wrap mod 360 (no values over 360, negatives wrap up)`` () =
        Assert.True(close (normalizeDegrees 370.0) 10.0)
        Assert.True(close (normalizeDegrees -15.0) 345.0)
        // 350 + 15 -> 5, not 365.
        let m = TableRotationView.init () |> TableRotationView.update (RotateR1By 350.0) |> TableRotationView.update (RotateR1By 15.0)
        Assert.True(close m.view.r1.degrees 5.0, $"R1 wrapped to {m.view.r1.degrees}")

    [<Fact>]
    let ``reset returns a tumbled view to straight top-down`` () =
        let tumbled =
            TableRotationView.init ()
            |> TableRotationView.update (RotateR1By 30.0)
            |> TableRotationView.update (RotateR2By 20.0)
            |> TableRotationView.update (RotateR3By -45.0)
        let reset = TableRotationView.update ResetView tumbled
        Assert.True(close reset.view.r1.degrees 0.0 && close reset.view.r2.degrees 0.0 && close reset.view.r3.degrees 0.0)

    [<Fact>]
    let ``a pointer drag tumbles the table (horizontal yaws R3, vertical pitches R2)`` () =
        let p0 : ScreenPoint = TableRotationView.center
        let p1 : ScreenPoint = { sx = p0.sx + 100.0; sy = p0.sy + 50.0 }
        let m =
            TableRotationView.init ()
            |> TableRotationView.update (PointerDown p0)
            |> TableRotationView.update (PointerMove p1)
        Assert.True(close m.view.r3.degrees (orbitDegreesPerPixel * 100.0), $"R3 = {m.view.r3.degrees}")
        Assert.True(close m.view.r2.degrees (orbitDegreesPerPixel * 50.0), $"R2 = {m.view.r2.degrees}")

    [<Fact>]
    let ``a press that does not move past the threshold is a click, not a drag`` () =
        let p0 : ScreenPoint = TableRotationView.center
        let nudge : ScreenPoint = { sx = p0.sx + 1.0; sy = p0.sy + 1.0 }
        let m =
            TableRotationView.init ()
            |> TableRotationView.update (PointerDown p0)
            |> TableRotationView.update (PointerMove nudge)
        // Below threshold: no rotation yet.
        Assert.True(close m.view.r2.degrees 0.0 && close m.view.r3.degrees 0.0, "sub-threshold nudge rotated the view")
        // ...and releasing selects the table (it was a click).
        let released = TableRotationView.update (PointerUp p0) m
        Assert.Equal(TableSelected, released.selection)

    [<Fact>]
    let ``clicking empty space (off the plate) unselects`` () =
        let far : ScreenPoint = { sx = TableRotationView.center.sx + 100000.0; sy = TableRotationView.center.sy }
        let m =
            TableRotationView.init ()
            |> TableRotationView.update (PointerDown TableRotationView.center)
            |> TableRotationView.update (PointerUp TableRotationView.center)   // select first
            |> TableRotationView.update (PointerDown far)
            |> TableRotationView.update (PointerUp far)
        Assert.Equal(TableUnselected, m.selection)

    [<Fact>]
    let ``the mouse wheel spins R1 in-plane`` () =
        let m = TableRotationView.update (WheelBy 1) (TableRotationView.init ())
        Assert.True(close m.view.r1.degrees wheelSpinDegrees, $"R1 = {m.view.r1.degrees}")

    // ================== real headless pointer injection ==================

    /// Build the real FuncUI view over a manual MVU loop (mutable model + capturing dispatch),
    /// show it headlessly, run `inject` (which fires real pointer events), and return the final
    /// model. The handlers carry no model state, so they stay valid across the whole gesture
    /// without re-rendering the tree (which would disturb pointer routing).
    let private withMouseHarness (inject : Window -> unit) : Model =
        let mutable model = TableRotationView.init ()
        let dispatch (m : Msg) = model <- TableRotationView.update m model
        let window = Window(Width = 920.0, Height = 760.0)
        window.Content <- Component(fun _ -> TableRotationView.view model dispatch)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        inject window
        Dispatcher.UIThread.RunJobs()
        window.Close()
        model

    let private pt (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``dragging the mouse across the table tumbles it (real pointer input)`` () =
        HeadlessSession.run (fun () ->
            let p0 = TableRotationView.center
            let p1 : ScreenPoint = { sx = p0.sx + 120.0; sy = p0.sy + 40.0 }
            let model =
                withMouseHarness (fun w ->
                    w.MouseDown(pt p0, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseMove(pt p1, RawInputModifiers.LeftMouseButton)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseUp(pt p1, MouseButton.Left, RawInputModifiers.None))
            Assert.True(model.view.r3.degrees > 1.0, $"drag did not yaw R3: {model.view.r3.degrees}")
            Assert.True(model.view.r2.degrees > 1.0, $"drag did not pitch R2: {model.view.r2.degrees}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real mouse click selects the table, a click off it deselects`` () =
        HeadlessSession.run (fun () ->
            let onPlate =
                withMouseHarness (fun w ->
                    w.MouseDown(pt TableRotationView.center, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseUp(pt TableRotationView.center, MouseButton.Left, RawInputModifiers.None))
            Assert.Equal(TableSelected, onPlate.selection)
            let offPlate =
                withMouseHarness (fun w ->
                    let corner : ScreenPoint = { sx = 5.0; sy = 5.0 }
                    w.MouseDown(pt corner, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseUp(pt corner, MouseButton.Left, RawInputModifiers.None))
            Assert.Equal(TableUnselected, offPlate.selection))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real mouse wheel notch spins the table (R1)`` () =
        HeadlessSession.run (fun () ->
            let model =
                withMouseHarness (fun w ->
                    w.MouseWheel(pt TableRotationView.center, Vector(0.0, 1.0), RawInputModifiers.None))
            Assert.True(model.view.r1.degrees > 0.0, $"wheel did not spin R1: {model.view.r1.degrees}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Shift + a real button click rotates by 5 degrees, not 15`` () =
        HeadlessSession.run (fun () ->
            let model =
                withMouseHarness (fun w ->
                    let button =
                        w.GetVisualDescendants()
                        |> Seq.choose (fun v -> match v with | :? Border as b when b.Name = UiIds.rotateR2Plus -> Some b | _ -> None)
                        |> Seq.tryHead
                    match button with
                    | Some b ->
                        let centre = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), w)
                        if centre.HasValue then
                            w.MouseDown(centre.Value, MouseButton.Left, RawInputModifiers.Shift)
                            Dispatcher.UIThread.RunJobs()
                            w.MouseUp(centre.Value, MouseButton.Left, RawInputModifiers.Shift)
                        else Assert.Fail("could not locate the R2+ button on screen")
                    | None -> Assert.Fail("R2+ button not found in the visual tree"))
            Assert.True(close model.view.r2.degrees 5.0, $"Shift+button gave {model.view.r2.degrees}, expected 5"))

    // ===================== headless render proof =====================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the table rotation window host opens and renders headlessly`` () =
        HeadlessSession.run (fun () ->
            let window = TableRotationWindow()
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
