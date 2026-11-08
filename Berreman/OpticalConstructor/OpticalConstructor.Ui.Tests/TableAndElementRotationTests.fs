namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI
open Xunit
open Softellect.Sys.Primitives
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table
open OpticalConstructor.Domain.TableView
open OpticalConstructor.Tests
open OpticalConstructor.TestWindows
open OpticalConstructor.Ui.TableAndElementRotationView

/// Tests for the combined table + element rotation window (Spec 0027, task 006 #3). The rotation
/// gestures act on whatever is selected (the table view, or an element); rotating the table leaves
/// the elements' own angles unchanged but moves how they project (they are snapped to the table).
module TableAndElementRotationTests =

    let private owner = FolderName "TableAndElementRotationTests"

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9
    let private elem (i : int) (m : TableAndElementRotationModel) : TestElement = List.item i m.elements
    let private deg (a : Angle) : float = a.degrees
    let private d2 (a : ScreenPoint) (b : ScreenPoint) : float = sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

    /// The on-screen projected centre of an element under a given view (mirrors the view's own
    /// private hit-test projection, using the public scale/centre).
    let private centreOf (view : TableViewState) (e : TestElement) : ScreenPoint =
        TableView.project OpticalConstructor.Ui.TableScene.pixelsPerMeter OpticalConstructor.Ui.TableScene.center view
            (Vector3.create (e.placement.placementPoint.x / 1.0<meter>) (e.placement.placementPoint.y / 1.0<meter>) 0.0)

    /// A click (no drag) at a screen point.
    let private clickAt (sp : ScreenPoint) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
        m |> updateTableAndElementRotation (PointerDown sp) |> updateTableAndElementRotation (PointerUp sp)

    // ============================ pure MVU ============================

    [<Fact>]
    let ``init: table selected, three elements on the central ray, top-down view`` () =
        let m = SceneTestFixture.staticScene ()
        Assert.Equal(TableSelected, m.selection)
        Assert.Equal(3, List.length m.elements)
        Assert.True(close m.view.r1.degrees 0.0 && close m.view.zoom.value 1.0)
        for e in m.elements do
            Assert.True(close (e.placement.placementPoint.y / 1.0<meter>) 0.0)
            Assert.True(close e.zoom.value defaultElementZoom.value && e.placement.r3Locked = RotationLocked)

    // ===================== Lego constructor (Main scene) =====================
    // The Main screen is the SAME scene as the test window, seeded differently (`initMain`) with an
    // add/remove palette. The static test scene (`init`) has an empty palette and is unchanged.

    [<Fact>]
    let ``the static test scene has no palette (add/remove is off), behaviour unchanged`` () =
        Assert.True(List.isEmpty (SceneTestFixture.staticScene ()).palette)

    [<Fact>]
    let ``initMain: a light source and a detector on the beam, table selected, same zoom, with a palette`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal(TableSelected, m.selection)
        Assert.Equal(2, List.length m.elements)
        Assert.Equal(LightSource, (elem 0 m).placement.catalogueKind)
        Assert.Equal(Detector, (elem 1 m).placement.catalogueKind)
        Assert.False(List.isEmpty m.palette, "the Main scene exposes an add palette")
        Assert.True(close m.view.zoom.value 1.0 && close m.view.r1.degrees 0.0, "same table / initial zoom as the test scene")

    [<Fact>]
    let ``the default test scene composes its stores from the Built-in catalogue`` () : unit =
        let model = SceneTestFixture.mainScene ()
        Assert.Equal<Result<Library.Sample list, Library.SampleError>>(
            Ok (BuiltinCatalogue.samples () |> List.sortBy _.id),
            model.samples.listSamples MaterialLibrary.IncludeInactive |> Result.map (List.sortBy _.id))
        Assert.Equal<Result<MaterialLibrary.MaterialEntry list, MaterialLibrary.MaterialError>>(
            Ok (BuiltinCatalogue.materials () |> List.sortBy _.id),
            model.materials.listMaterials MaterialLibrary.IncludeInactive |> Result.map (List.sortBy _.id))
        Assert.Equal<Result<MaterialLibrary.MaterialCategory list, MaterialLibrary.CategoryError>>(
            Ok (BuiltinCatalogue.categories () |> List.sortBy _.id),
            model.categories.listCategories () |> Result.map (List.sortBy _.id))
        for preset in BuiltinCatalogue.presets () do
            Assert.Equal<Result<Library.LibraryEntry option, Library.LibraryError>>(
                Ok (Some preset), model.library.tryGetEntry preset.entryId)

    [<Fact>]
    let ``AddElement appends a catalogue element on the beam and selects it (so the bar acts on it)`` () =
        let m1 = updateTableAndElementRotation (AddElement Sample) (SceneTestFixture.mainScene ())
        Assert.Equal(3, List.length m1.elements)
        Assert.Equal(ElementSelected (SceneElementIndex 2), m1.selection)
        Assert.Equal(Sample, (elem 2 m1).placement.catalogueKind)
        Assert.True(close ((elem 2 m1).placement.placementPoint.y / 1.0<meter>) 0.0, "the new element sits on the beam")
        let rotated = updateTableAndElementRotation (RotateR1By (BarDegrees 15.0)) m1
        Assert.True(close (deg (elem 2 rotated).placement.r1) 15.0, "the added, selected element rotates")

    [<Fact>]
    let ``RemoveSelected removes the selected element; it is inert when the table is selected`` () =
        let withSample = updateTableAndElementRotation (AddElement Sample) (SceneTestFixture.mainScene ())   // 3 elements, element 2 selected
        let removed = updateTableAndElementRotation RemoveSelectedElement withSample
        Assert.Equal(2, List.length removed.elements)
        Assert.Equal(NothingSelected, removed.selection)
        let tableSel = { (SceneTestFixture.mainScene ()) with selection = TableSelected }
        Assert.Equal<TableAndElementRotationModel>(tableSel, updateTableAndElementRotation RemoveSelectedElement tableSel)

    // ===================== Main-screen large controls: Move / Render / Ribbon (task 018) =====================

    [<Fact>]
    let ``the Move bay slides the SELECTED element along the beam, clamped to the plate; inert otherwise`` () =
        let m = updateTableAndElementRotation (AddElement Sample) (SceneTestFixture.mainScene ())   // element 2 selected, at x = -0.3
        let slid = updateTableAndElementRotation (SlideSelectedBy (BarMeters 0.3)) m
        Assert.True(close ((elem 2 slid).elementX) 0.0, $"slid to x = {(elem 2 slid).elementX}")
        // Clamp to ±half the plate (default table is 2 m → ±1.0 m).
        Assert.True(close ((elem 2 (updateTableAndElementRotation (SlideSelectedTo (BarMeters 5.0)) m)).elementX) 1.0, "clamps to +1.0")
        Assert.True(close ((elem 2 (updateTableAndElementRotation (SlideSelectedTo (BarMeters -5.0)) m)).elementX) -1.0, "clamps to -1.0")
        Assert.True(close ((elem 2 (updateTableAndElementRotation ResetSelectedPosition slid)).elementX) 0.0, "reset → table centre")
        // With the TABLE selected, sliding is a no-op (nothing on the beam is selected).
        let tableSel = { (SceneTestFixture.mainScene ()) with selection = TableSelected }
        Assert.Equal<TableAndElementRotationModel>(tableSel, updateTableAndElementRotation (SlideSelectedBy (BarMeters 0.5)) tableSel)

    [<Fact>]
    let ``the Render bay tunes how elements are drawn (swap + the serializable config)`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal(RendererControls.Wireframe, m.render.kind)
        Assert.Equal(72, m.render.rails.value)
        Assert.Equal(RendererControls.Shapes, (updateTableAndElementRotation RenderSwap m).render.kind)
        Assert.Equal(4, (updateTableAndElementRotation (RenderSetRailsIndex (RendererControls.RendererPresetIndex 0)) m).render.rails.value)
        Assert.Equal(8, (updateTableAndElementRotation (RenderSetCircles (RendererControls.CapCircles 99)) m).render.circles.value)       // clamps to 8
        Assert.Equal(36, (updateTableAndElementRotation (RenderSetRadialsIndex (RendererControls.RendererPresetIndex 4)) m).render.radials.value)
        Assert.Equal(1.0, (updateTableAndElementRotation (RenderSetFaceOpacity (RendererControls.RendererOpacity 2.0)) m).render.faceOpacity.value)   // clamps to 1
        Assert.Equal(0.0, (updateTableAndElementRotation (RenderSetRailOpacity (RendererControls.RendererOpacity -1.0)) m).render.railOpacity.value)  // clamps to 0

    [<Fact>]
    let ``Main: adding a flat mirror reflects the beam, so a downstream element leaves the straight ray`` () =
        // initMain = [source(0); detector(1)]. Add a flat mirror (index 2, selected) and tilt it to R2 = 45°.
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement FlatMirror) |> updateTableAndElementRotation (RotSetAxis (RotationControls.R2, BarDegrees 45.0))
        let centres = m.snappedCentres
        // The DETECTOR (index 1) is now OFF the straight +X ray (y ≠ 0) — the beam reflected off the mirror,
        // figured out from the mirror tracking REFLECTED light (not hardcoded).
        Assert.True(abs (centres.[SceneElementIndex 1]).y > 1.0e-3, $"the detector snapped off the straight ray (y = {(centres.[SceneElementIndex 1]).y})")
        // A transmissive element (a polarizer) instead keeps the beam straight.
        let straight = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement LinearPolarizer)
        Assert.True(abs ((straight.snappedCentres).[SceneElementIndex 1]).y < 1.0e-9, "a polarizer keeps the beam straight")

    [<Fact>]
    let ``Main: tilting a mirror's R3 snaps downstream elements OUT of the table plane (z changes)`` () =
        // A flat mirror tilted out of plane (R3 = 30°) sends the reflected beam out of the table plane, so
        // the downstream detector snaps to a position with a non-zero z (perpendicular to the table). R3
        // starts locked on a fresh element, so unlock it first (as the user does).
        let m =
            SceneTestFixture.mainScene ()
            |> updateTableAndElementRotation (AddElement FlatMirror)                            // mirror (index 2) selected, R3 locked
            |> updateTableAndElementRotation ToggleR3Lock                                       // unlock the mirror's R3
            |> updateTableAndElementRotation (RotSetAxis (RotationControls.R3, BarDegrees 30.0))
        let det = (m.snappedCentres).[SceneElementIndex 1]
        Assert.True(abs det.z > 1.0e-3, $"the detector snapped out of the table plane (z = {det.z})")
        // In plane (no R3) the detector stays on the table (z = 0).
        let flat = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement FlatMirror) |> updateTableAndElementRotation (RotSetAxis (RotationControls.R2, BarDegrees 45.0))
        Assert.True(abs ((flat.snappedCentres).[SceneElementIndex 1]).z < 1.0e-9, "an in-plane reflection keeps z = 0")

    [<Fact>]
    let ``Main: a transmissive downstream element auto-orients to face the reflected beam`` () =
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement FlatMirror) |> updateTableAndElementRotation (RotSetAxis (RotationControls.R2, BarDegrees 45.0))
        let incoming = ((fst (m.mainSnap)).[SceneElementIndex 1]).incoming |> Option.get   // the reflected beam at the detector
        Assert.True(abs incoming.y > 1.0e-3, "the beam at the detector is reflected (bent off +X)")
        // The detector is drawn FACING that reflected beam (its N1 = the incoming direction), as in the test.
        let { primaryNormal = n1 } = orientedBasis (drawnPlacement m (SceneElementIndex 1))
        Assert.True(abs (n1.x - incoming.x) < 1.0e-6 && abs (n1.y - incoming.y) < 1.0e-6 && abs (n1.z - incoming.z) < 1.0e-6,
                    $"the detector faces the reflected beam (n1 = {n1.x}, {n1.y}, {n1.z})")
        // Spec 0046: every element but the SOURCE (the ray root) is oriented on its beam — mirrors too,
        // so the face a ray reflects off is always the drawn one.
        Assert.False(LightSource.autoOrientsToBeam)
        Assert.True(FlatMirror.autoOrientsToBeam && CurvedMirror.autoOrientsToBeam)
        Assert.True(Detector.autoOrientsToBeam && LinearPolarizer.autoOrientsToBeam)

    [<Fact>]
    let ``the ribbon offers every large control as a Bay; selecting one is pure state`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal(BayNames.add, m.ribbon)                           // the first tab, Add, is open at start (spec 0046)
        let bays = mainBays m ignore
        Assert.Equal<Ribbon.BayName list>(BayNames.allBayNames, bays |> List.map (fun b -> b.name))
        Assert.Equal(BayNames.render, (updateTableAndElementRotation (SelectBay BayNames.render) m).ribbon)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main ribbon shows the bay tabs and reveals ONLY the selected bay's controls`` () =
        HeadlessSession.run (fun () ->
            // The ribbon realizes ONLY the active bay's content in one keyed slot (step 007), so a
            // non-selected bay's controls are absent from the tree; effective visibility of the shown
            // names therefore isolates exactly the selected bay's controls.
            let visibleNames (model : TableAndElementRotationModel) : Set<string> =
                let mutable m = model
                let dispatch (msg : TableAndElementRotationMsg) = m <- updateTableAndElementRotation msg m
                let window = Window(Width = 980.0, Height = tableAndElementRotationCanvasHeight + 230.0)
                window.Content <- Component(fun _ -> mainView m dispatch)
                window.Show()
                Dispatcher.UIThread.RunJobs()
                // The clickable boxes carry an `AutomationProperties.AutomationId` (spec 0038 sweep);
                // sliders / panels keep their `Name` — collect BOTH id kinds into the visible set.
                let ns =
                    window.GetVisualDescendants()
                    |> Seq.collect (fun v ->
                        match v with
                        | :? Control as c when c.IsEffectivelyVisible ->
                            seq {
                                if not (isNull c.Name) then yield c.Name
                                let autoId = Avalonia.Automation.AutomationProperties.GetAutomationId c
                                if not (isNull autoId) then yield autoId
                            }
                        | _ -> Seq.empty)
                    |> Set.ofSeq
                window.Close()
                ns
            // Every bay has a (visible) ribbon tab.
            // Main opens on Add (spec 0046): the palette shows, the rotation buttons do not.
            let addView = visibleNames (SceneTestFixture.mainScene ())
            Assert.DoesNotContain(ConstructorUiId.rotationR2PlusButton.value, addView)
            let rotationView = visibleNames (updateTableAndElementRotation (SelectBay BayNames.rotation) (SceneTestFixture.mainScene ()))
            for bay in BayNames.allBayNames do
                Assert.Contains((ConstructorUiId.ribbonTab (ConstructorUiKey bay.value)).value, rotationView)
            // The Rotate bay shows the rotation buttons; the Move bay's controls are hidden.
            Assert.Contains(ConstructorUiId.rotationR2PlusButton.value, rotationView)
            Assert.DoesNotContain(ConstructorUiId.rayPositionMinusButton.value, rotationView)
            // Selecting the Render bay reveals the renderer sliders (and hides the rotation buttons).
            let renderView = visibleNames (updateTableAndElementRotation (SelectBay BayNames.render) (SceneTestFixture.mainScene ()))
            Assert.Contains(ConstructorUiId.rendererRailsSlider.value, renderView)
            Assert.DoesNotContain(ConstructorUiId.rotationR2PlusButton.value, renderView)
            // Selecting the Move bay reveals the along-beam controls.
            Assert.Contains(ConstructorUiId.rayPositionMinusButton.value, visibleNames (updateTableAndElementRotation (SelectBay BayNames.move) (SceneTestFixture.mainScene ()))))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Main: Render bay, swap the renderer, then back to Rotation does not crash (ribbon recycling)`` () =
        // Regression for "Cannot set Name : styled element already styled": swapping the single ribbon
        // content node between two different controls made FuncUI recycle a styled, named control. Driven
        // through the REAL Main window (its Elmish loop runs the incremental virtual-DOM patch, as in app).
        HeadlessSession.run (fun () ->
            // Spec 0038 step 006: the real window takes the injected app scope — a fresh one here
            // (this file opens the view module, not `OpticalConstructor.Ui`, so qualify the type).
            let window = (OpticalConstructor.App.MainConstructorWindow.tryCreate (TestAppScope.freshAppContext owner) |> SceneTestFixture.kitSuccess)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let click (name : string) : unit =
                match window.GetVisualDescendants() |> Seq.tryPick (function :? Border as b when b.Name = name -> Some b | _ -> None) with
                | Some b ->
                    let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                    if c.HasValue then
                        window.MouseDown(c.Value, MouseButton.Left, RawInputModifiers.None)
                        Dispatcher.UIThread.RunJobs()
                        window.MouseUp(c.Value, MouseButton.Left, RawInputModifiers.None)
                        Dispatcher.UIThread.RunJobs()
                    else Assert.Fail($"%s{name} has no on-screen position")
                | None -> Assert.Fail($"%s{name} not found")
            click (ConstructorUiId.ribbonTab (ConstructorUiKey BayNames.render.value)).value     // show the Render bay
            click ConstructorUiId.rendererSwapButton.value     // swap the renderer (wireframe → shapes)
            click (ConstructorUiId.ribbonTab (ConstructorUiKey BayNames.rotation.value)).value    // back to Rotation — must NOT throw
            Assert.True(window.IsVisible)
            window.Close())

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
        let m = updateTableAndElementRotation (Wheel (Set.ofList [ ModShift ], OpticalConstructor.Ui.WheelNotches 1)) (SceneTestFixture.staticScene ())
        Assert.True(close m.view.r1.degrees 5.0, $"table R1 = {m.view.r1.degrees}")
        // No element rotated.
        Assert.True(m.elements |> List.forall (fun e -> close (deg e.placement.r1) 0.0))

    [<Fact>]
    let ``rotation acts on the ELEMENT when an element is selected`` () =
        let m0 = SceneTestFixture.staticScene ()
        // Element 1 sits at table x = 0 → screen centre.
        let selected = clickAt { sx = OpticalConstructor.Ui.TableScene.center.sx; sy = OpticalConstructor.Ui.TableScene.center.sy } m0
        Assert.Equal(ElementSelected (SceneElementIndex 1), selected.selection)
        let m = updateTableAndElementRotation (Wheel (Set.ofList [ ModShift ], OpticalConstructor.Ui.WheelNotches 1)) selected
        Assert.True(close (deg (elem 1 m).placement.r1) 5.0, $"element R1 = {deg (elem 1 m).placement.r1}")
        // The table view did NOT rotate, and the other elements did not either.
        Assert.True(close m.view.r1.degrees 0.0)
        Assert.True(close (deg (elem 0 m).placement.r1) 0.0 && close (deg (elem 2 m).placement.r1) 0.0)

    [<Fact>]
    let ``rotating the table does NOT change element angles but moves how they project (snapped to table)`` () =
        let m0 = SceneTestFixture.staticScene ()                                  // table selected
        let before = centreOf m0.view (elem 0 m0)
        let m1 = updateTableAndElementRotation (RotateR1By (BarDegrees 90.0)) m0              // rotate the TABLE view
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
        let m0 = SceneTestFixture.staticScene ()
        // Table selected → ToggleR3Lock toggles the TABLE's R3 lock (task 008 — the table now has one).
        Assert.Equal(RotationLocked, (updateTableAndElementRotation ToggleR3Lock m0).tableR3Locked)
        // Select an element: R3 locked, so Alt+wheel is inert; after unlocking it turns.
        let sel = clickAt { sx = OpticalConstructor.Ui.TableScene.center.sx; sy = OpticalConstructor.Ui.TableScene.center.sy } m0
        Assert.True(close (deg (elem 1 (updateTableAndElementRotation (Wheel (Set.ofList [ ModAlt ], OpticalConstructor.Ui.WheelNotches 1)) sel)).placement.r3) 0.0, "R3 inert while locked")
        let turned = sel |> updateTableAndElementRotation ToggleR3Lock |> updateTableAndElementRotation (Wheel (Set.ofList [ ModAlt ], OpticalConstructor.Ui.WheelNotches 1))
        Assert.True(close (deg (elem 1 turned).placement.r3) 5.0)

    [<Fact>]
    let ``Ctrl+Alt+wheel zooms the selected element (nothing when the table is selected); Ctrl+Alt+Shift zooms all`` () =
        let m0 = SceneTestFixture.staticScene ()   // table selected
        // Table selected → element-zoom does nothing.
        Assert.True(m0.elements |> List.forall (fun e -> close e.zoom.value defaultElementZoom.value))
        let tableSelZoom = updateTableAndElementRotation (Wheel (Set.ofList [ ModCtrl; ModAlt ], OpticalConstructor.Ui.WheelNotches 1)) m0
        Assert.True(tableSelZoom.elements |> List.forall (fun e -> close e.zoom.value defaultElementZoom.value), "no element zoom while the table is selected")
        // Element selected → Ctrl+Alt zooms only it.
        let sel = clickAt { sx = OpticalConstructor.Ui.TableScene.center.sx; sy = OpticalConstructor.Ui.TableScene.center.sy } m0
        let one = updateTableAndElementRotation (Wheel (Set.ofList [ ModCtrl; ModAlt ], OpticalConstructor.Ui.WheelNotches 1)) sel
        Assert.True((elem 1 one).zoom.value > defaultElementZoom.value && close (elem 0 one).zoom.value defaultElementZoom.value)
        // Ctrl+Alt+Shift zooms all (regardless of selection).
        let all = updateTableAndElementRotation (Wheel (Set.ofList [ ModCtrl; ModShift; ModAlt ], OpticalConstructor.Ui.WheelNotches 1)) m0
        Assert.True(all.elements |> List.forall (fun e -> e.zoom > defaultElementZoom))

    [<Fact>]
    let ``plain wheel zooms the table; a drag pans it; a click selects table or element`` () =
        let m0 = SceneTestFixture.staticScene ()
        Assert.True((updateTableAndElementRotation (Wheel (Set.empty, OpticalConstructor.Ui.WheelNotches 1)) m0).view.zoom.value > 1.0, "plain wheel zooms the table")
        let dragged = m0 |> updateTableAndElementRotation (PointerDown OpticalConstructor.Ui.TableScene.center) |> updateTableAndElementRotation (PointerMove { sx = OpticalConstructor.Ui.TableScene.center.sx + 120.0; sy = OpticalConstructor.Ui.TableScene.center.sy + 40.0 }) |> updateTableAndElementRotation (PointerUp { sx = OpticalConstructor.Ui.TableScene.center.sx + 120.0; sy = OpticalConstructor.Ui.TableScene.center.sy + 40.0 })
        Assert.True(close dragged.view.panX.value 120.0 && close dragged.view.panY.value 40.0, "a drag pans the table")
        // Click an element vs the empty plate vs off the plate entirely.
        Assert.Equal(ElementSelected (SceneElementIndex 0), (clickAt { sx = OpticalConstructor.Ui.TableScene.center.sx - 100.0; sy = OpticalConstructor.Ui.TableScene.center.sy } m0).selection)   // element 0 at x=-0.5 → screen -100
        Assert.Equal(TableSelected, (clickAt { sx = OpticalConstructor.Ui.TableScene.center.sx; sy = OpticalConstructor.Ui.TableScene.center.sy + 100.0 } m0).selection)        // on the plate (y≈0.5 m), clear of every element
        Assert.Equal(NothingSelected, (clickAt { sx = OpticalConstructor.Ui.TableScene.center.sx; sy = OpticalConstructor.Ui.TableScene.center.sy + 200.0 } m0).selection)      // off the plate (y≈1.0 m > 0.6 m half-width) → unselects

    [<Fact>]
    let ``clicking off the plate unselects the table, then rotation gestures are inert`` () =
        let off = clickAt { sx = OpticalConstructor.Ui.TableScene.center.sx; sy = OpticalConstructor.Ui.TableScene.center.sy + 200.0 } (SceneTestFixture.staticScene ())   // init has the table selected
        Assert.Equal(NothingSelected, off.selection)
        // Nothing selected ⇒ nothing to rotate (button delta and wheel both no-ops).
        let after = off |> updateTableAndElementRotation (RotateR1By (BarDegrees 15.0)) |> updateTableAndElementRotation (Wheel (Set.ofList [ ModShift ], OpticalConstructor.Ui.WheelNotches 1))
        Assert.True(close after.view.r1.degrees 0.0, "the table is not rotated while unselected")
        Assert.True(after.elements |> List.forall (fun e -> close e.placement.r1.degrees 0.0), "no element is rotated while unselected")

    [<Fact>]
    let ``the table R3 is unlocked by default; locking it makes table R3 rotation inert`` () =
        let m0 = SceneTestFixture.staticScene ()   // table selected
        Assert.Equal(RotationFree, m0.tableR3Locked)
        let locked = updateTableAndElementRotation ToggleR3Lock m0
        Assert.Equal(RotationLocked, locked.tableR3Locked)
        Assert.True(close (updateTableAndElementRotation (RotateR3By (BarDegrees 15.0)) locked).view.r3.degrees 0.0, "a locked table R3 ignores rotation")

    [<Fact>]
    let ``setting an axis to an exact angle acts on the current selection`` () =
        let t = updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 42.5)) (SceneTestFixture.staticScene ())   // table selected
        Assert.True(close t.view.r1.degrees 42.5, $"table R1 = {t.view.r1.degrees}")
        let e = SceneTestFixture.staticScene () |> clickAt { sx = OpticalConstructor.Ui.TableScene.center.sx; sy = OpticalConstructor.Ui.TableScene.center.sy } |> updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 33.0))
        Assert.True(close (deg (elem 1 e).placement.r1) 33.0, "element R1 set")

    [<Fact>]
    let ``Reset (selection, confirmed) zeros only the selected object's rotations`` () =
        let m =
            SceneTestFixture.staticScene ()
            |> updateTableAndElementRotation (RotateR1By (BarDegrees 30.0))                       // table R1 = 30
            |> clickAt { sx = OpticalConstructor.Ui.TableScene.center.sx; sy = OpticalConstructor.Ui.TableScene.center.sy }     // select element 1
            |> updateTableAndElementRotation (RotateR2By (BarDegrees 20.0))                        // element 1 R2 = 20
            |> updateTableAndElementRotation RotRequestReset
            |> updateTableAndElementRotation RotConfirm
        Assert.True(close (deg (elem 1 m).placement.r2) 0.0, "the selected element is reset")
        Assert.True(close m.view.r1.degrees 30.0, "the table's rotation is NOT reset by Reset (selection)")

    [<Fact>]
    let ``Reset All (confirmed) zeros the table's AND every element's rotations`` () =
        let dirtied =
            SceneTestFixture.staticScene ()
            |> updateTableAndElementRotation (RotateR1By (BarDegrees 40.0))                       // rotate the table
            |> clickAt { sx = OpticalConstructor.Ui.TableScene.center.sx; sy = OpticalConstructor.Ui.TableScene.center.sy }     // select element 1
            |> updateTableAndElementRotation ToggleR3Lock                            // unlock element R3
            |> updateTableAndElementRotation (RotateR3By (BarDegrees 25.0))                        // rotate the element
        let m = dirtied |> updateTableAndElementRotation RotRequestResetAll |> updateTableAndElementRotation RotConfirm
        Assert.True(close m.view.r1.degrees 0.0 && close m.view.r2.degrees 0.0 && close m.view.r3.degrees 0.0, "table rotations zeroed")
        Assert.True(m.elements |> List.forall (fun e -> close (deg e.placement.r1) 0.0 && close (deg e.placement.r2) 0.0 && close (deg e.placement.r3) 0.0), "all element rotations zeroed")

    [<Fact>]
    let ``the button step is 15 normally and 5 with Shift; angles wrap mod 360`` () =
        Assert.True(close (RotationControls.buttonStepDegrees KeyModifiers.None).value 15.0 && close (RotationControls.buttonStepDegrees KeyModifiers.Shift).value 5.0)
        Assert.True(close ((BarDegrees 370.0).normalized.value) 10.0 && close ((BarDegrees -15.0).normalized.value) 345.0)

    // ================== real headless pointer injection ==================

    let private withMouseHarness (inject : Window -> unit) : TableAndElementRotationModel =
        let mutable model = SceneTestFixture.staticScene ()
        let dispatch (m : TableAndElementRotationMsg) = model <- updateTableAndElementRotation m model
        let window = Window(Width = 920.0, Height = 780.0)
        window.Content <- Component(fun _ -> tableAndElementRotationView model dispatch)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        inject window
        Dispatcher.UIThread.RunJobs()
        window.Close()
        model

    let private pt (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)

    /// A canvas point in WINDOW coordinates — the controls are docked above the canvas, so the canvas is
    /// offset down by the control-bar height; a selecting click must target the element's real position.
    let private atCanvas (w : Window) (sp : ScreenPoint) : Point =
        let off =
            w.GetVisualDescendants()
            |> Seq.tryPick (function :? Canvas as c -> Some c | _ -> None)
            |> Option.bind (fun c -> c.TranslatePoint(Point(0.0, 0.0), w) |> Option.ofNullable)
            |> Option.defaultValue (Point(0.0, 0.0))
        Point(off.X + sp.sx, off.Y + sp.sy)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real Shift+wheel rotates the table (table selected at start)`` () =
        HeadlessSession.run (fun () ->
            let m = withMouseHarness (fun w -> w.MouseWheel(pt OpticalConstructor.Ui.TableScene.center, Vector(0.0, 1.0), RawInputModifiers.Shift))
            Assert.True(close m.view.r1.degrees 5.0, $"table R1 = {m.view.r1.degrees}"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``clicking an element then Shift+wheel rotates that element, not the table`` () =
        HeadlessSession.run (fun () ->
            let m =
                withMouseHarness (fun w ->
                    w.MouseDown(atCanvas w OpticalConstructor.Ui.TableScene.center, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseUp(atCanvas w OpticalConstructor.Ui.TableScene.center, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseWheel(pt OpticalConstructor.Ui.TableScene.center, Vector(0.0, 1.0), RawInputModifiers.Shift))
            Assert.Equal(ElementSelected (SceneElementIndex 1), m.selection)
            Assert.True(close (deg (elem 1 m).placement.r1) 5.0, $"element R1 = {deg (elem 1 m).placement.r1}")
            Assert.True(close m.view.r1.degrees 0.0, "the table must not have rotated"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``a real drag pans the table`` () =
        HeadlessSession.run (fun () ->
            let p1 : ScreenPoint = { sx = OpticalConstructor.Ui.TableScene.center.sx + 140.0; sy = OpticalConstructor.Ui.TableScene.center.sy + 60.0 }
            let m =
                withMouseHarness (fun w ->
                    w.MouseDown(pt OpticalConstructor.Ui.TableScene.center, MouseButton.Left, RawInputModifiers.None)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseMove(pt p1, RawInputModifiers.LeftMouseButton)
                    Dispatcher.UIThread.RunJobs()
                    w.MouseUp(pt p1, MouseButton.Left, RawInputModifiers.None))
            Assert.True(close m.view.panX.value 140.0 && close m.view.panY.value 60.0, $"pan = ({m.view.panX.value}, {m.view.panY.value})"))

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``Shift + a real button click rotates the selection by 5 degrees`` () =
        HeadlessSession.run (fun () ->
            let m =
                withMouseHarness (fun w ->
                    let button =
                        w.GetVisualDescendants()
                        |> Seq.choose (fun v -> match v with | :? Border as b when Avalonia.Automation.AutomationProperties.GetAutomationId(b) = ConstructorUiId.rotationR2PlusButton.value -> Some b | _ -> None)
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
            let window = (TableAndElementRotationWindow.tryCreate () |> SceneTestFixture.kitSuccess)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main scene renders its add/remove palette (the only UI difference from the test scene)`` () =
        HeadlessSession.run (fun () ->
            let mutable model = SceneTestFixture.mainScene ()
            let dispatch (m : TableAndElementRotationMsg) = model <- updateTableAndElementRotation m model
            let window = Window(Width = tableAndElementRotationCanvasWidth, Height = tableAndElementRotationCanvasHeight + 170.0)
            window.Content <- Component(fun _ -> tableAndElementRotationView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // The shared palette control renders its "Remove selected" button (a styled Border with a
            // stable id); the static test scene (empty palette) renders none.
            let removeButtons =
                window.GetVisualDescendants()
                |> Seq.choose (function :? Border as b when Avalonia.Automation.AutomationProperties.GetAutomationId(b) = ConstructorUiId.paletteRemoveSelectedButton.value -> Some b | _ -> None)
                |> Seq.toList
            Assert.Equal(1, List.length removeButtons)
            window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the palette's polarizer trio and sample buttons add pre-bound / unbound elements (018)`` () =
        // Spec 0038 (018): the palette exposes THREE polarizer buttons with stable ids — LP / CPL /
        // CPR — over the UNCHANGED CatalogueKinds; each add lands PRE-BOUND to its seeded entry,
        // while a sample add stays UNBOUND (the inverse hook).
        HeadlessSession.run (fun () ->
            let mutable model = SceneTestFixture.mainScene ()
            let dispatch (m : TableAndElementRotationMsg) = model <- updateTableAndElementRotation m model
            let window = Window(Width = tableAndElementRotationCanvasWidth, Height = tableAndElementRotationCanvasHeight + 260.0)
            window.Content <- Component(fun _ -> tableAndElementRotationView model dispatch)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            let clickAdd (code : string) : unit =
                let id = (ConstructorUiId.paletteAddButton (ConstructorUiKey code)).value
                let button =
                    window.GetVisualDescendants()
                    |> Seq.tryPick (function
                        | :? Border as b when Avalonia.Automation.AutomationProperties.GetAutomationId(b) = id -> Some b
                        | _ -> None)
                match button with
                | Some b ->
                    let c = b.TranslatePoint(Point(b.Bounds.Width / 2.0, b.Bounds.Height / 2.0), window)
                    if c.HasValue then
                        window.MouseDown(c.Value, MouseButton.Left, RawInputModifiers.None)
                        Dispatcher.UIThread.RunJobs()
                        window.MouseUp(c.Value, MouseButton.Left, RawInputModifiers.None)
                        Dispatcher.UIThread.RunJobs()
                    else Assert.Fail($"%s{id} has no on-screen position")
                | None -> Assert.Fail($"%s{id} was not rendered")
            clickAdd "LP"
            clickAdd "CPL"
            clickAdd "CPR"
            clickAdd "Sa"
            let added = model.elements |> List.skip 2   // past the seeded source / detector
            Assert.Equal<CatalogueKind list>(
                [ LinearPolarizer; CircularPolarizer; CircularPolarizer; Sample ],
                added |> List.map (fun e -> e.placement.catalogueKind))
            Assert.Equal<ElementValueId option list>(
                [ Some Library.PresetIds.polarizerLp; Some Library.PresetIds.polarizerCpLeft; Some Library.PresetIds.polarizerCpRight; None ],
                added |> List.map (fun e -> e.placement.valueId))
            window.Close())
