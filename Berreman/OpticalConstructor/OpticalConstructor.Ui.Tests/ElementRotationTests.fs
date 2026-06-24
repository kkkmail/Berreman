namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.TableView
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.ElementRotationView

/// Tests for the optical-element-rotation test window (Spec 0027). The table is fixed top-down;
/// the SELECTED element rotates per the spec model (`Placement.orientedBasis`/`withR1/R2/R3`,
/// R3 locked by default) and carries a per-element draw zoom (default 5×, with the Ctrl+Alt /
/// Ctrl+Alt+Shift wheel gestures). Pure MVU proves the rules; real `Avalonia.Headless` pointer
/// injection proves the mouse path end to end.
module ElementRotationTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9
    let private elem (i : int) (m : Model) : TestElement = List.item i m.elements
    let private deg (a : Angle) : float = a.degrees

    // ============================ pure MVU ============================

    [<Fact>]
    let ``init places three elements on the central ray, middle selected, 5x zoom, R3 locked`` () =
        let m = ElementRotationView.init ()
        Assert.Equal(3, List.length m.elements)
        Assert.Equal(1, m.selected)
        // All on the central ray (y = 0) at the expected x, default zoom, R3 locked, rest pose.
        let xs = m.elements |> List.map (fun e -> e.placement.placementPoint.x / 1.0<meter>)
        Assert.True(close (List.item 0 xs) -0.5 && close (List.item 1 xs) 0.0 && close (List.item 2 xs) 0.5)
        for e in m.elements do
            Assert.True(close (e.placement.placementPoint.y / 1.0<meter>) 0.0, "elements sit on the central ray (y = 0)")
            Assert.True(close e.zoom defaultElementZoom)
            Assert.True(e.placement.r3Locked && not e.placement.r1Locked && not e.placement.r2Locked)
            Assert.True(close (deg e.placement.r1) 0.0 && close (deg e.placement.r2) 0.0 && close (deg e.placement.r3) 0.0)

    [<Fact>]
    let ``a fresh element's rest pose has N1 along the central ray and N2 along the table normal`` () =
        // The spec-correct rest pose (AC-A1), read straight off the test element via orientedBasis.
        let (n1, n2, _) = orientedBasis (elem 1 (ElementRotationView.init ())).placement
        Assert.True(close n1.x 1.0 && close n1.y 0.0 && close n1.z 0.0, $"N1 = ({n1.x}, {n1.y}, {n1.z})")
        Assert.True(close n2.x 0.0 && close n2.y 0.0 && close n2.z 1.0, $"N2 = ({n2.x}, {n2.y}, {n2.z})")

    [<Fact>]
    let ``the wheel map sends each modifier combo to exactly one action`` () =
        Assert.Equal(RotateSelectedR1, wheelAction (Set.ofList [ ModShift ]))
        Assert.Equal(RotateSelectedR2, wheelAction (Set.ofList [ ModCtrl; ModShift ]))
        Assert.Equal(RotateSelectedR3, wheelAction (Set.ofList [ ModAlt ]))
        Assert.Equal(ZoomSelected, wheelAction (Set.ofList [ ModCtrl; ModAlt ]))
        Assert.Equal(ZoomAll, wheelAction (Set.ofList [ ModCtrl; ModShift; ModAlt ]))
        // Plain / Ctrl wheel zooms the TABLE (#2).
        Assert.Equal(ZoomTable, wheelAction Set.empty)
        Assert.Equal(ZoomTable, wheelAction (Set.ofList [ ModCtrl ]))
        // Shift+Alt is still undocumented → nothing.
        Assert.Equal(NoWheelAction, wheelAction (Set.ofList [ ModShift; ModAlt ]))

    [<Fact>]
    let ``Shift+wheel rotates ONLY the selected element's R1`` () =
        let m = ElementRotationView.update (Wheel (Set.ofList [ ModShift ], 1)) (ElementRotationView.init ())
        Assert.True(close (deg (elem 1 m).placement.r1) 5.0, $"selected R1 = {deg (elem 1 m).placement.r1}")
        Assert.True(close (deg (elem 0 m).placement.r1) 0.0 && close (deg (elem 2 m).placement.r1) 0.0, "other elements must not rotate")

    [<Fact>]
    let ``R3 is locked by default and rotates only after unlocking (spec A.1.2 / A.4.5)`` () =
        let m0 = ElementRotationView.init ()
        // Alt+wheel is inert while R3 is locked.
        let locked = ElementRotationView.update (Wheel (Set.ofList [ ModAlt ], 1)) m0
        Assert.True(close (deg (elem 1 locked).placement.r3) 0.0, "R3 must be inert while locked")
        // Unlock, then Alt+wheel turns R3 by the step.
        let unlocked = ElementRotationView.update ToggleR3Lock m0
        let turned = ElementRotationView.update (Wheel (Set.ofList [ ModAlt ], 1)) unlocked
        Assert.True(close (deg (elem 1 turned).placement.r3) 5.0, $"R3 after unlock = {deg (elem 1 turned).placement.r3}")

    [<Fact>]
    let ``Ctrl+Shift+wheel rotates R2 by the step`` () =
        let m = ElementRotationView.update (Wheel (Set.ofList [ ModCtrl; ModShift ], 1)) (ElementRotationView.init ())
        Assert.True(close (deg (elem 1 m).placement.r2) 5.0)

    [<Fact>]
    let ``Ctrl+Alt+wheel zooms ONLY the selected element; Ctrl+Alt+Shift+wheel zooms ALL`` () =
        let one = ElementRotationView.update (Wheel (Set.ofList [ ModCtrl; ModAlt ], 1)) (ElementRotationView.init ())
        Assert.True((elem 1 one).zoom > defaultElementZoom, "selected element zoomed")
        Assert.True(close (elem 0 one).zoom defaultElementZoom && close (elem 2 one).zoom defaultElementZoom, "others unchanged")
        let all = ElementRotationView.update (Wheel (Set.ofList [ ModCtrl; ModShift; ModAlt ], 1)) (ElementRotationView.init ())
        Assert.True((elem 0 all).zoom > defaultElementZoom && (elem 1 all).zoom > defaultElementZoom && (elem 2 all).zoom > defaultElementZoom,
                    "all elements zoomed")

    [<Fact>]
    let ``setting an axis to an exact non-integer angle works, lock-respecting`` () =
        let m = ElementRotationView.update (RotSetAxis (RotationControls.R1, 42.5)) (ElementRotationView.init ())
        Assert.True(close (deg (elem 1 m).placement.r1) 42.5, $"R1 = {deg (elem 1 m).placement.r1}")
        // R3 is locked by default, so setting it is inert until unlocked.
        let r3 = ElementRotationView.update (RotSetAxis (RotationControls.R3, 30.0)) (ElementRotationView.init ())
        Assert.True(close (deg (elem 1 r3).placement.r3) 0.0, "setting a locked R3 must be inert")

    [<Fact>]
    let ``Reset (confirmed) zeros ONLY the selected element's rotations (not its zoom)`` () =
        let dirtied =
            ElementRotationView.init ()
            |> ElementRotationView.update (RotateR1By 30.0)
            |> ElementRotationView.update ToggleR3Lock
            |> ElementRotationView.update (RotateR3By 20.0)
            |> ElementRotationView.update (ZoomSelectedBy 3)
        let armed = ElementRotationView.update RotRequestReset dirtied
        Assert.Equal(RotationControls.ConfirmReset, armed.rotationConfirm)
        Assert.True(close (deg (elem 1 armed).placement.r1) 30.0, "not reset until confirmed")
        let m = ElementRotationView.update RotConfirm armed
        let e = elem 1 m
        Assert.True(close (deg e.placement.r1) 0.0 && close (deg e.placement.r2) 0.0 && close (deg e.placement.r3) 0.0, "rotations zeroed")
        Assert.True(e.zoom > defaultElementZoom, "Reset must not touch the element's zoom")

    [<Fact>]
    let ``Reset All (confirmed) zeros every element's rotations`` () =
        let m =
            ElementRotationView.init ()
            |> ElementRotationView.update (RotateR1By 30.0)                          // element 1 (selected)
            |> ElementRotationView.update (PointerDown { sx = 310.0; sy = 280.0 })   // select element 0
            |> ElementRotationView.update (PointerUp { sx = 310.0; sy = 280.0 })
            |> ElementRotationView.update (RotateR2By 25.0)                          // element 0
            |> ElementRotationView.update RotRequestResetAll
            |> ElementRotationView.update RotConfirm
        Assert.True(m.elements |> List.forall (fun e -> close (deg e.placement.r1) 0.0 && close (deg e.placement.r2) 0.0 && close (deg e.placement.r3) 0.0))

    [<Fact>]
    let ``rotation and zoom act on whichever element is selected`` () =
        // Select element 0, rotate it; element 1 (previously selected) must be untouched.
        let m =
            ElementRotationView.init ()
            |> ElementRotationView.update (PointerDown { sx = 310.0; sy = 280.0 })   // element 0's projected centre
            |> ElementRotationView.update (PointerUp { sx = 310.0; sy = 280.0 })
            |> ElementRotationView.update (RotateR2By 15.0)
        Assert.Equal(0, m.selected)
        Assert.True(close (deg (elem 0 m).placement.r2) 15.0 && close (deg (elem 1 m).placement.r2) 0.0)

    [<Fact>]
    let ``a clean click selects the nearest element; a drag pans the table (no reselect)`` () =
        let m0 = ElementRotationView.init ()
        let clicked =
            m0
            |> ElementRotationView.update (PointerDown { sx = 510.0; sy = 280.0 })   // element 2
            |> ElementRotationView.update (PointerUp { sx = 510.0; sy = 280.0 })
        Assert.Equal(2, clicked.selected)
        // A drag pans the table by the screen delta and leaves the selection unchanged.
        let dragged =
            clicked
            |> ElementRotationView.update (PointerDown { sx = 310.0; sy = 280.0 })
            |> ElementRotationView.update (PointerMove { sx = 460.0; sy = 360.0 })
            |> ElementRotationView.update (PointerUp { sx = 460.0; sy = 360.0 })
        Assert.Equal(2, dragged.selected)
        Assert.True(close dragged.view.panX 150.0 && close dragged.view.panY 80.0, $"pan = ({dragged.view.panX}, {dragged.view.panY})")

    [<Fact>]
    let ``plain wheel zooms the table view, not the elements`` () =
        let m = ElementRotationView.update (Wheel (Set.empty, 1)) (ElementRotationView.init ())
        Assert.True(m.view.zoom > 1.0, $"table zoom = {m.view.zoom}")
        Assert.True(close (elem 1 m).zoom defaultElementZoom, "the per-element zoom is unchanged by a table-zoom wheel")

    [<Fact>]
    let ``the button step is 15 degrees normally and 5 with Shift; angles wrap mod 360`` () =
        Assert.True(close (buttonStepDegrees false) 15.0 && close (buttonStepDegrees true) 5.0)
        Assert.True(close (normalizeDegrees 370.0) 10.0 && close (normalizeDegrees -15.0) 345.0)

    // ================== real headless pointer injection ==================

    let private withMouseHarness (inject : Window -> unit) : Model =
        let mutable model = ElementRotationView.init ()
        let dispatch (m : Msg) = model <- ElementRotationView.update m model
        let window = Window(Width = 920.0, Height = 780.0)
        window.Content <- Component(fun _ -> ElementRotationView.view model dispatch)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        inject window
        Dispatcher.UIThread.RunJobs()
        window.Close()
        model

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real Shift+wheel rotates ONLY the selected element's R1 by exactly 5 degrees`` () =
        HeadlessSession.run (fun () ->
            let m = withMouseHarness (fun w -> w.MouseWheel(Point(410.0, 280.0), Vector(0.0, 1.0), RawInputModifiers.Shift))
            Assert.True(close (deg (elem 1 m).placement.r1) 5.0, $"selected R1 = {deg (elem 1 m).placement.r1} (expected exactly 5)")
            Assert.True(close (deg (elem 0 m).placement.r1) 0.0 && close (deg (elem 2 m).placement.r1) 0.0, "others must not rotate"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real Ctrl+Alt+wheel zooms only the selected element, Ctrl+Alt+Shift zooms all`` () =
        HeadlessSession.run (fun () ->
            let one = withMouseHarness (fun w -> w.MouseWheel(Point(410.0, 280.0), Vector(0.0, 1.0), RawInputModifiers.Control ||| RawInputModifiers.Alt))
            Assert.True((elem 1 one).zoom > defaultElementZoom && close (elem 0 one).zoom defaultElementZoom, "only the selected element zoomed")
            let all = withMouseHarness (fun w -> w.MouseWheel(Point(410.0, 280.0), Vector(0.0, 1.0), RawInputModifiers.Control ||| RawInputModifiers.Alt ||| RawInputModifiers.Shift))
            Assert.True((elem 0 all).zoom > defaultElementZoom && (elem 2 all).zoom > defaultElementZoom, "all elements zoomed"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real click selects the nearest element`` () =
        HeadlessSession.run (fun () ->
            let m =
                withMouseHarness (fun w ->
                    w.MouseDown(Point(310.0, 280.0), MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseUp(Point(310.0, 280.0), MouseButton.Left, RawInputModifiers.None))
            Assert.Equal(0, m.selected))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Shift + a real button click rotates the selected element by 5 degrees, not 15`` () =
        HeadlessSession.run (fun () ->
            let m =
                withMouseHarness (fun w ->
                    let button =
                        w.GetVisualDescendants()
                        |> Seq.choose (fun v -> match v with | :? Border as b when b.Name = RotationControls.UiIds.r2Plus -> Some b | _ -> None)
                        |> Seq.tryHead
                    match button with
                    | Some b ->
                        let centre = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), w)
                        if centre.HasValue then
                            w.MouseDown(centre.Value, MouseButton.Left, RawInputModifiers.Shift)
                            Dispatcher.UIThread.RunJobs()
                            w.MouseUp(centre.Value, MouseButton.Left, RawInputModifiers.Shift)
                        else Assert.Fail("could not locate the R2+ button")
                    | None -> Assert.Fail("R2+ button not found"))
            Assert.True(close (deg (elem 1 m).placement.r2) 5.0, $"Shift+button gave {deg (elem 1 m).placement.r2}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the element rotation window host opens and renders headlessly`` () =
        HeadlessSession.run (fun () ->
            let window = ElementRotationWindow()
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
