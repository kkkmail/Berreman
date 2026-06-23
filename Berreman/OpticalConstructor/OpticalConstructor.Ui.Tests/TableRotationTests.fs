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

/// Tests for the table-rotation test window (Spec 0027, task 002-rotate-table). The rotation is
/// CONSTRAINED to one axis at a time by the documented modifier+wheel gestures (§E.3/§E.5); a
/// plain drag does NOTHING. Two layers:
///   * pure MVU `update` proves the gesture map, the one-axis constraint, mod-360, and that a
///     plain drag is inert;
///   * **real `Avalonia.Headless` pointer injection** drives actual wheel/drag/click events
///     through the live input pipeline and asserts the model — so "rotation by mouse" and
///     "plain drag does nothing" are proved end to end, and one notch = exactly one 5° step.
module TableRotationTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9
    let private isTopDown (m : Model) : bool =
        close m.view.r1.degrees 0.0 && close m.view.r2.degrees 0.0 && close m.view.r3.degrees 0.0

    /// `init` with the table selected (the table is only rotatable when selected, task 006 #1):
    /// a clean click on the plate centre selects it.
    let private selectedModel () : Model =
        TableRotationView.init ()
        |> TableRotationView.update (PointerDown TableRotationView.center)
        |> TableRotationView.update (PointerUp TableRotationView.center)

    // ============================ pure MVU ============================

    [<Fact>]
    let ``the table is rotatable ONLY when it is selected`` () =
        // Unselected: button and wheel rotation are both inert.
        Assert.True(isTopDown (TableRotationView.update (RotateR1By 15.0) (TableRotationView.init ())), "an unselected table must not rotate by button")
        Assert.True(isTopDown (TableRotationView.update (Wheel (Set.ofList [ ModShift ], 1)) (TableRotationView.init ())), "an unselected table must not rotate by wheel")
        // Selected: it rotates.
        let sel = TableRotationView.update (RotateR1By 15.0) (selectedModel ())
        Assert.True(close sel.view.r1.degrees 15.0, $"a selected table should rotate, got {sel.view.r1.degrees}")

    [<Fact>]
    let ``init starts straight top-down and unselected`` () =
        let m = TableRotationView.init ()
        Assert.True(isTopDown m && close m.view.zoom 1.0)
        Assert.Equal(TableUnselected, m.selection)

    [<Fact>]
    let ``the documented wheel map sends each modifier combo to exactly one action`` () =
        Assert.Equal(RotateView1, wheelAction (Set.ofList [ ModShift ]))
        Assert.Equal(RotateView2, wheelAction (Set.ofList [ ModCtrl; ModShift ]))
        Assert.Equal(RotateView3, wheelAction (Set.ofList [ ModAlt ]))
        Assert.Equal(ZoomView, wheelAction Set.empty)
        Assert.Equal(ZoomView, wheelAction (Set.ofList [ ModCtrl ]))
        // Undocumented combinations do NOTHING (one thing at a time, or nothing).
        Assert.Equal(NoWheelAction, wheelAction (Set.ofList [ ModCtrl; ModAlt ]))
        Assert.Equal(NoWheelAction, wheelAction (Set.ofList [ ModShift; ModAlt ]))
        Assert.Equal(NoWheelAction, wheelAction (Set.ofList [ ModCtrl; ModShift; ModAlt ]))

    [<Fact>]
    let ``Shift+wheel rotates ONLY R1 by one 5 degree step`` () =
        let m = TableRotationView.update (Wheel (Set.ofList [ ModShift ], 1)) (selectedModel ())
        Assert.True(close m.view.r1.degrees 5.0, $"R1 = {m.view.r1.degrees}")
        Assert.True(close m.view.r2.degrees 0.0 && close m.view.r3.degrees 0.0 && close m.view.zoom 1.0, "only R1 should move")

    [<Fact>]
    let ``Ctrl+Shift+wheel rotates ONLY R2, Alt+wheel rotates ONLY R3`` () =
        let r2 = TableRotationView.update (Wheel (Set.ofList [ ModCtrl; ModShift ], 1)) (selectedModel ())
        Assert.True(close r2.view.r2.degrees 5.0 && close r2.view.r1.degrees 0.0 && close r2.view.r3.degrees 0.0)
        let r3 = TableRotationView.update (Wheel (Set.ofList [ ModAlt ], -1)) (selectedModel ())
        Assert.True(close r3.view.r3.degrees 355.0 && close r3.view.r1.degrees 0.0 && close r3.view.r2.degrees 0.0, $"R3 = {r3.view.r3.degrees}")

    [<Fact>]
    let ``plain wheel zooms and changes no rotation`` () =
        let m = TableRotationView.update (Wheel (Set.empty, 1)) (TableRotationView.init ())
        Assert.True(m.view.zoom > 1.0, $"zoom = {m.view.zoom}")
        Assert.True(isTopDown m, "zoom must not rotate")

    [<Fact>]
    let ``an undocumented modifier combo does nothing`` () =
        let init = TableRotationView.init ()
        let m = TableRotationView.update (Wheel (Set.ofList [ ModCtrl; ModAlt ], 1)) init
        Assert.Equal<Model>(init, m)

    [<Fact>]
    let ``angles wrap mod 360`` () =
        Assert.True(close (normalizeDegrees 370.0) 10.0 && close (normalizeDegrees -15.0) 345.0)
        let m = selectedModel () |> TableRotationView.update (RotateR1By 350.0) |> TableRotationView.update (RotateR1By 15.0)
        Assert.True(close m.view.r1.degrees 5.0, $"R1 wrapped to {m.view.r1.degrees}")

    [<Fact>]
    let ``the button step is 15 degrees normally and 5 degrees with Shift`` () =
        Assert.True(close (buttonStepDegrees false) 15.0 && close (buttonStepDegrees true) 5.0)

    [<Fact>]
    let ``a plain press - drag - release PANS the table (no rotation, no selection)`` () =
        let p0 = TableRotationView.center
        let p1 : ScreenPoint = { sx = p0.sx + 150.0; sy = p0.sy + 80.0 }
        let m =
            TableRotationView.init ()
            |> TableRotationView.update (PointerDown p0)
            |> TableRotationView.update (PointerMove p1)
            |> TableRotationView.update (PointerUp p1)
        // The drag pans by the screen delta; it must NOT rotate, zoom, or select.
        Assert.True(close m.view.panX 150.0 && close m.view.panY 80.0, $"pan = ({m.view.panX}, {m.view.panY})")
        Assert.True(isTopDown m && close m.view.zoom 1.0, "a drag must not rotate or zoom")
        Assert.Equal(TableUnselected, m.selection)

    [<Fact>]
    let ``a clean click (no drag) selects the table, a click off it deselects`` () =
        let selected =
            TableRotationView.init ()
            |> TableRotationView.update (PointerDown TableRotationView.center)
            |> TableRotationView.update (PointerUp TableRotationView.center)
        Assert.Equal(TableSelected, selected.selection)
        let far : ScreenPoint = { sx = 3.0; sy = 3.0 }
        let unselected =
            selected
            |> TableRotationView.update (PointerDown far)
            |> TableRotationView.update (PointerUp far)
        Assert.Equal(TableUnselected, unselected.selection)

    [<Fact>]
    let ``reset returns a rotated view to straight top-down`` () =
        let tumbled =
            selectedModel ()
            |> TableRotationView.update (RotateR1By 30.0)
            |> TableRotationView.update (RotateR2By 20.0)
            |> TableRotationView.update (RotateR3By -45.0)
        Assert.False(isTopDown tumbled, "the selected table should have tumbled before the reset")
        Assert.True(isTopDown (TableRotationView.update ResetView tumbled))

    // ================== real headless pointer injection ==================

    /// Build the real FuncUI view over a manual MVU loop (mutable model + capturing dispatch),
    /// show it headlessly, run `inject` (real pointer events), and return the final model. The
    /// handlers carry no model state, so they stay valid across a gesture without re-rendering.
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

    let private at (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)

    /// Select the table by a real click on its centre (rotation needs the table selected, #1).
    let private selectTable (w : Window) : unit =
        w.MouseDown(at TableRotationView.center, MouseButton.Left, RawInputModifiers.None)
        Dispatcher.UIThread.RunJobs()
        w.MouseUp(at TableRotationView.center, MouseButton.Left, RawInputModifiers.None)
        Dispatcher.UIThread.RunJobs()

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real Shift+wheel notch rotates ONLY R1 by exactly 5 degrees`` () =
        HeadlessSession.run (fun () ->
            let model =
                withMouseHarness (fun w ->
                    selectTable w
                    w.MouseWheel(at TableRotationView.center, Vector(0.0, 1.0), RawInputModifiers.Shift))
            // Exactly 5°, NOT 10° — proves one notch = one step (FuncUI's duplicate pass is dropped).
            Assert.True(close model.view.r1.degrees 5.0, $"R1 = {model.view.r1.degrees} (expected exactly 5)")
            Assert.True(close model.view.r2.degrees 0.0 && close model.view.r3.degrees 0.0, "only R1 should move"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real Ctrl+Shift+wheel rotates ONLY R2 and Alt+wheel rotates ONLY R3`` () =
        HeadlessSession.run (fun () ->
            let r2 =
                withMouseHarness (fun w ->
                    selectTable w
                    w.MouseWheel(at TableRotationView.center, Vector(0.0, 1.0), RawInputModifiers.Control ||| RawInputModifiers.Shift))
            Assert.True(close r2.view.r2.degrees 5.0 && close r2.view.r1.degrees 0.0 && close r2.view.r3.degrees 0.0, $"R2 = {r2.view.r2.degrees}")
            let r3 =
                withMouseHarness (fun w ->
                    selectTable w
                    w.MouseWheel(at TableRotationView.center, Vector(0.0, 1.0), RawInputModifiers.Alt))
            Assert.True(close r3.view.r3.degrees 5.0 && close r3.view.r1.degrees 0.0 && close r3.view.r2.degrees 0.0, $"R3 = {r3.view.r3.degrees}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real click-and-drag PANS the table across the screen`` () =
        HeadlessSession.run (fun () ->
            let p0 = TableRotationView.center
            let p1 : ScreenPoint = { sx = p0.sx + 160.0; sy = p0.sy + 70.0 }
            let model =
                withMouseHarness (fun w ->
                    w.MouseDown(at p0, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseMove(at p1, RawInputModifiers.LeftMouseButton)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseUp(at p1, MouseButton.Left, RawInputModifiers.None))
            // The drag pans by the pointer delta — and must not rotate, zoom, or select.
            Assert.True(close model.view.panX 160.0 && close model.view.panY 70.0, $"pan = ({model.view.panX}, {model.view.panY})")
            Assert.True(isTopDown model && close model.view.zoom 1.0, $"a drag changed rotation/zoom: R1={model.view.r1.degrees} zoom={model.view.zoom}")
            Assert.Equal(TableUnselected, model.selection))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real clean click selects the table`` () =
        HeadlessSession.run (fun () ->
            let model =
                withMouseHarness (fun w ->
                    w.MouseDown(at TableRotationView.center, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseUp(at TableRotationView.center, MouseButton.Left, RawInputModifiers.None))
            Assert.Equal(TableSelected, model.selection))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Shift + a real button click rotates by 5 degrees, not 15`` () =
        HeadlessSession.run (fun () ->
            let model =
                withMouseHarness (fun w ->
                    selectTable w
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

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the table rotation window host opens and renders headlessly`` () =
        HeadlessSession.run (fun () ->
            let window = TableRotationWindow()
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
