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
open OpticalConstructor.Domain.Table
open OpticalConstructor.Domain.TableView
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.TableAndElementRotationView

/// Tests for the combined table + element rotation window (Spec 0027, task 006 #3). The rotation
/// gestures act on whatever is selected (the table view, or an element); rotating the table leaves
/// the elements' own angles unchanged but moves how they project (they are snapped to the table).
module TableAndElementRotationTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9
    let private elem (i : int) (m : Model) : TestElement = List.item i m.elements
    let private deg (a : Angle) : float = a.degrees
    let private d2 (a : ScreenPoint) (b : ScreenPoint) : float = sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

    /// The on-screen projected centre of an element under a given view (mirrors the view's own
    /// private hit-test projection, using the public scale/centre).
    let private centreOf (view : TableViewState) (e : TestElement) : ScreenPoint =
        TableView.project pixelsPerMeter center view
            (Vector3.create (e.placement.placementPoint.x / 1.0<meter>) (e.placement.placementPoint.y / 1.0<meter>) 0.0)

    /// A click (no drag) at a screen point.
    let private clickAt (sp : ScreenPoint) (m : Model) : Model =
        m |> update (PointerDown sp) |> update (PointerUp sp)

    // ============================ pure MVU ============================

    [<Fact>]
    let ``init: table selected, three elements on the central ray, top-down view`` () =
        let m = init ()
        Assert.Equal(TableSelected, m.selection)
        Assert.Equal(3, List.length m.elements)
        Assert.True(close m.view.r1.degrees 0.0 && close m.view.zoom 1.0)
        for e in m.elements do
            Assert.True(close (e.placement.placementPoint.y / 1.0<meter>) 0.0)
            Assert.True(close e.zoom defaultElementZoom && e.placement.r3Locked)

    // ===================== Lego constructor (Main scene) =====================
    // The Main screen is the SAME scene as the test window, seeded differently (`initMain`) with an
    // add/remove palette. The static test scene (`init`) has an empty palette and is unchanged.

    [<Fact>]
    let ``the static test scene has no palette (add/remove is off), behaviour unchanged`` () =
        Assert.True(List.isEmpty (init ()).palette)

    [<Fact>]
    let ``initMain: a light source and a detector on the beam, table selected, same zoom, with a palette`` () =
        let m = initMain ()
        Assert.Equal(TableSelected, m.selection)
        Assert.Equal(2, List.length m.elements)
        Assert.Equal(LightSource, (elem 0 m).placement.catalogueKind)
        Assert.Equal(Detector, (elem 1 m).placement.catalogueKind)
        Assert.False(List.isEmpty m.palette, "the Main scene exposes an add palette")
        Assert.True(close m.view.zoom 1.0 && close m.view.r1.degrees 0.0, "same table / initial zoom as the test scene")

    [<Fact>]
    let ``AddElement appends a catalogue element on the beam and selects it (so the bar acts on it)`` () =
        let m1 = update (AddElement Sample) (initMain ())
        Assert.Equal(3, List.length m1.elements)
        Assert.Equal(ElementSelected 2, m1.selection)
        Assert.Equal(Sample, (elem 2 m1).placement.catalogueKind)
        Assert.True(close ((elem 2 m1).placement.placementPoint.y / 1.0<meter>) 0.0, "the new element sits on the beam")
        let rotated = update (RotateR1By 15.0) m1
        Assert.True(close (deg (elem 2 rotated).placement.r1) 15.0, "the added, selected element rotates")

    [<Fact>]
    let ``RemoveSelected removes the selected element; it is inert when the table is selected`` () =
        let withSample = update (AddElement Sample) (initMain ())   // 3 elements, element 2 selected
        let removed = update RemoveSelected withSample
        Assert.Equal(2, List.length removed.elements)
        Assert.Equal(NothingSelected, removed.selection)
        let tableSel = { (initMain ()) with selection = TableSelected }
        Assert.Equal<Model>(tableSel, update RemoveSelected tableSel)

    [<Fact>]
    let ``the wheel map sends each modifier combo to one action`` () =
        Assert.Equal(RotateSel1, wheelAction (Set.ofList [ ModShift ]))
        Assert.Equal(RotateSel2, wheelAction (Set.ofList [ ModCtrl; ModShift ]))
        Assert.Equal(RotateSel3, wheelAction (Set.ofList [ ModAlt ]))
        Assert.Equal(ZoomElementSelected, wheelAction (Set.ofList [ ModCtrl; ModAlt ]))
        Assert.Equal(ZoomElementsAll, wheelAction (Set.ofList [ ModCtrl; ModShift; ModAlt ]))
        Assert.Equal(ZoomTable, wheelAction Set.empty)
        Assert.Equal(ZoomTable, wheelAction (Set.ofList [ ModCtrl ]))
        Assert.Equal(NoWheelAction, wheelAction (Set.ofList [ ModShift; ModAlt ]))

    [<Fact>]
    let ``rotation acts on the TABLE when the table is selected`` () =
        // init has the table selected.
        let m = update (Wheel (Set.ofList [ ModShift ], 1)) (init ())
        Assert.True(close m.view.r1.degrees 5.0, $"table R1 = {m.view.r1.degrees}")
        // No element rotated.
        Assert.True(m.elements |> List.forall (fun e -> close (deg e.placement.r1) 0.0))

    [<Fact>]
    let ``rotation acts on the ELEMENT when an element is selected`` () =
        let m0 = init ()
        // Element 1 sits at table x = 0 → screen centre.
        let selected = clickAt { sx = center.sx; sy = center.sy } m0
        Assert.Equal(ElementSelected 1, selected.selection)
        let m = update (Wheel (Set.ofList [ ModShift ], 1)) selected
        Assert.True(close (deg (elem 1 m).placement.r1) 5.0, $"element R1 = {deg (elem 1 m).placement.r1}")
        // The table view did NOT rotate, and the other elements did not either.
        Assert.True(close m.view.r1.degrees 0.0)
        Assert.True(close (deg (elem 0 m).placement.r1) 0.0 && close (deg (elem 2 m).placement.r1) 0.0)

    [<Fact>]
    let ``rotating the table does NOT change element angles but moves how they project (snapped to table)`` () =
        let m0 = init ()                                  // table selected
        let before = centreOf m0.view (elem 0 m0)
        let m1 = update (RotateR1By 90.0) m0              // rotate the TABLE view
        // The element's own rotation angles are unchanged...
        let e0 = elem 0 m1
        Assert.True(close (deg e0.placement.r1) 0.0 && close (deg e0.placement.r2) 0.0 && close (deg e0.placement.r3) 0.0,
                    "an element's own angles must not change when the table rotates")
        // ...the table view rotated...
        Assert.True(close m1.view.r1.degrees 90.0)
        // ...and the element's projection moved with the rotated table.
        let after = centreOf m1.view (elem 0 m1)
        Assert.True(d2 before after > 1.0, "the element should project to a new place after the table rotates")

    [<Fact>]
    let ``R3 starts locked on elements and unlocked on the table; the toggle targets the selection`` () =
        let m0 = init ()
        // Table selected → ToggleR3Lock toggles the TABLE's R3 lock (task 008 — the table now has one).
        Assert.True((update ToggleR3Lock m0).tableR3Locked)
        // Select an element: R3 locked, so Alt+wheel is inert; after unlocking it turns.
        let sel = clickAt { sx = center.sx; sy = center.sy } m0
        Assert.True(close (deg (elem 1 (update (Wheel (Set.ofList [ ModAlt ], 1)) sel)).placement.r3) 0.0, "R3 inert while locked")
        let turned = sel |> update ToggleR3Lock |> update (Wheel (Set.ofList [ ModAlt ], 1))
        Assert.True(close (deg (elem 1 turned).placement.r3) 5.0)

    [<Fact>]
    let ``Ctrl+Alt+wheel zooms the selected element (nothing when the table is selected); Ctrl+Alt+Shift zooms all`` () =
        let m0 = init ()   // table selected
        // Table selected → element-zoom does nothing.
        Assert.True(m0.elements |> List.forall (fun e -> close e.zoom defaultElementZoom))
        let tableSelZoom = update (Wheel (Set.ofList [ ModCtrl; ModAlt ], 1)) m0
        Assert.True(tableSelZoom.elements |> List.forall (fun e -> close e.zoom defaultElementZoom), "no element zoom while the table is selected")
        // Element selected → Ctrl+Alt zooms only it.
        let sel = clickAt { sx = center.sx; sy = center.sy } m0
        let one = update (Wheel (Set.ofList [ ModCtrl; ModAlt ], 1)) sel
        Assert.True((elem 1 one).zoom > defaultElementZoom && close (elem 0 one).zoom defaultElementZoom)
        // Ctrl+Alt+Shift zooms all (regardless of selection).
        let all = update (Wheel (Set.ofList [ ModCtrl; ModShift; ModAlt ], 1)) m0
        Assert.True(all.elements |> List.forall (fun e -> e.zoom > defaultElementZoom))

    [<Fact>]
    let ``plain wheel zooms the table; a drag pans it; a click selects table or element`` () =
        let m0 = init ()
        Assert.True((update (Wheel (Set.empty, 1)) m0).view.zoom > 1.0, "plain wheel zooms the table")
        let dragged = m0 |> update (PointerDown center) |> update (PointerMove { sx = center.sx + 120.0; sy = center.sy + 40.0 }) |> update (PointerUp { sx = center.sx + 120.0; sy = center.sy + 40.0 })
        Assert.True(close dragged.view.panX 120.0 && close dragged.view.panY 40.0, "a drag pans the table")
        // Click an element vs the empty plate vs off the plate entirely.
        Assert.Equal(ElementSelected 0, (clickAt { sx = center.sx - 100.0; sy = center.sy } m0).selection)   // element 0 at x=-0.5 → screen -100
        Assert.Equal(TableSelected, (clickAt { sx = center.sx; sy = center.sy + 100.0 } m0).selection)        // on the plate (y≈0.5 m), clear of every element
        Assert.Equal(NothingSelected, (clickAt { sx = center.sx; sy = center.sy + 200.0 } m0).selection)      // off the plate (y≈1.0 m > 0.6 m half-width) → unselects

    [<Fact>]
    let ``clicking off the plate unselects the table, then rotation gestures are inert`` () =
        let off = clickAt { sx = center.sx; sy = center.sy + 200.0 } (init ())   // init has the table selected
        Assert.Equal(NothingSelected, off.selection)
        // Nothing selected ⇒ nothing to rotate (button delta and wheel both no-ops).
        let after = off |> update (RotateR1By 15.0) |> update (Wheel (Set.ofList [ ModShift ], 1))
        Assert.True(close after.view.r1.degrees 0.0, "the table is not rotated while unselected")
        Assert.True(after.elements |> List.forall (fun e -> close e.placement.r1.degrees 0.0), "no element is rotated while unselected")

    [<Fact>]
    let ``the table R3 is unlocked by default; locking it makes table R3 rotation inert`` () =
        let m0 = init ()   // table selected
        Assert.False(m0.tableR3Locked)
        let locked = update ToggleR3Lock m0
        Assert.True(locked.tableR3Locked)
        Assert.True(close (update (RotateR3By 15.0) locked).view.r3.degrees 0.0, "a locked table R3 ignores rotation")

    [<Fact>]
    let ``setting an axis to an exact angle acts on the current selection`` () =
        let t = update (RotSetAxis (RotationControls.R1, 42.5)) (init ())   // table selected
        Assert.True(close t.view.r1.degrees 42.5, $"table R1 = {t.view.r1.degrees}")
        let e = init () |> clickAt { sx = center.sx; sy = center.sy } |> update (RotSetAxis (RotationControls.R1, 33.0))
        Assert.True(close (deg (elem 1 e).placement.r1) 33.0, "element R1 set")

    [<Fact>]
    let ``Reset (selection, confirmed) zeros only the selected object's rotations`` () =
        let m =
            init ()
            |> update (RotateR1By 30.0)                       // table R1 = 30
            |> clickAt { sx = center.sx; sy = center.sy }     // select element 1
            |> update (RotateR2By 20.0)                        // element 1 R2 = 20
            |> update RotRequestReset
            |> update RotConfirm
        Assert.True(close (deg (elem 1 m).placement.r2) 0.0, "the selected element is reset")
        Assert.True(close m.view.r1.degrees 30.0, "the table's rotation is NOT reset by Reset (selection)")

    [<Fact>]
    let ``Reset All (confirmed) zeros the table's AND every element's rotations`` () =
        let dirtied =
            init ()
            |> update (RotateR1By 40.0)                       // rotate the table
            |> clickAt { sx = center.sx; sy = center.sy }     // select element 1
            |> update ToggleR3Lock                            // unlock element R3
            |> update (RotateR3By 25.0)                        // rotate the element
        let m = dirtied |> update RotRequestResetAll |> update RotConfirm
        Assert.True(close m.view.r1.degrees 0.0 && close m.view.r2.degrees 0.0 && close m.view.r3.degrees 0.0, "table rotations zeroed")
        Assert.True(m.elements |> List.forall (fun e -> close (deg e.placement.r1) 0.0 && close (deg e.placement.r2) 0.0 && close (deg e.placement.r3) 0.0), "all element rotations zeroed")

    [<Fact>]
    let ``the button step is 15 normally and 5 with Shift; angles wrap mod 360`` () =
        Assert.True(close (buttonStepDegrees false) 15.0 && close (buttonStepDegrees true) 5.0)
        Assert.True(close (normalizeDegrees 370.0) 10.0 && close (normalizeDegrees -15.0) 345.0)

    // ================== real headless pointer injection ==================

    let private withMouseHarness (inject : Window -> unit) : Model =
        let mutable model = init ()
        let dispatch (m : Msg) = model <- update m model
        let window = Window(Width = 920.0, Height = 780.0)
        window.Content <- Component(fun _ -> view model dispatch)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        inject window
        Dispatcher.UIThread.RunJobs()
        window.Close()
        model

    let private pt (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real Shift+wheel rotates the table (table selected at start)`` () =
        HeadlessSession.run (fun () ->
            let m = withMouseHarness (fun w -> w.MouseWheel(pt center, Vector(0.0, 1.0), RawInputModifiers.Shift))
            Assert.True(close m.view.r1.degrees 5.0, $"table R1 = {m.view.r1.degrees}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``clicking an element then Shift+wheel rotates that element, not the table`` () =
        HeadlessSession.run (fun () ->
            let m =
                withMouseHarness (fun w ->
                    w.MouseDown(pt center, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseUp(pt center, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseWheel(pt center, Vector(0.0, 1.0), RawInputModifiers.Shift))
            Assert.Equal(ElementSelected 1, m.selection)
            Assert.True(close (deg (elem 1 m).placement.r1) 5.0, $"element R1 = {deg (elem 1 m).placement.r1}")
            Assert.True(close m.view.r1.degrees 0.0, "the table must not have rotated"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real drag pans the table`` () =
        HeadlessSession.run (fun () ->
            let p1 : ScreenPoint = { sx = center.sx + 140.0; sy = center.sy + 60.0 }
            let m =
                withMouseHarness (fun w ->
                    w.MouseDown(pt center, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseMove(pt p1, RawInputModifiers.LeftMouseButton)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseUp(pt p1, MouseButton.Left, RawInputModifiers.None))
            Assert.True(close m.view.panX 140.0 && close m.view.panY 60.0, $"pan = ({m.view.panX}, {m.view.panY})"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Shift + a real button click rotates the selection by 5 degrees`` () =
        HeadlessSession.run (fun () ->
            let m =
                withMouseHarness (fun w ->
                    let button =
                        w.GetVisualDescendants()
                        |> Seq.choose (fun v -> match v with | :? Border as b when b.Name = RotationControls.UiIds.r2Plus -> Some b | _ -> None)
                        |> Seq.tryHead
                    match button with
                    | Some b ->
                        let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), w)
                        if c.HasValue then
                            w.MouseDown(c.Value, MouseButton.Left, RawInputModifiers.Shift)
                            Dispatcher.UIThread.RunJobs()
                            w.MouseUp(c.Value, MouseButton.Left, RawInputModifiers.Shift)
                        else Assert.Fail("could not locate the R2+ button")
                    | None -> Assert.Fail("R2+ button not found"))
            // Table is selected at start, so the table view's R2 turned by 5°.
            Assert.True(close m.view.r2.degrees 5.0, $"table R2 = {m.view.r2.degrees}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the table+element window host opens and renders headlessly`` () =
        HeadlessSession.run (fun () ->
            let window = TableAndElementRotationWindow()
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main scene renders its add/remove palette (the only UI difference from the test scene)`` () =
        HeadlessSession.run (fun () ->
            let mutable model = initMain ()
            let dispatch (m : Msg) = model <- update m model
            let window = Window(Width = canvasWidth, Height = canvasHeight + 170.0)
            window.Content <- Component(fun _ -> view model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The palette row renders real `Button`s; the static test scene (empty palette) renders none.
            let buttons = window.GetVisualDescendants() |> Seq.choose (function :? Button as b -> Some b | _ -> None) |> Seq.toList
            Assert.True(List.length buttons >= 1, "the Main scene shows add/remove buttons")
            window.Close())
