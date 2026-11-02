namespace OpticalConstructor.Ui.Tests

open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Threading
open Avalonia.VisualTree
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.TableView
open OpticalConstructor.Controls
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0046 — the light source is the one element placed freely: it is selected like any element, moves
/// in table x, y and z (each with its own move lock) and starts with every move and rotation locked, and the
/// beam follows it. A detector slides along its ray without the plate cap and Resets to 1 m. The Move bay
/// shows the x / y / z bar for the source and the along-beam bar for everything else; the Rotation bay offers
/// the R1 / R2 locks for elements only. Behaviour is proven through `update`; the headless tests prove the
/// bays render it and survive switching between the two kinds of selection.
module SourceAndDetectorMoveTests =

    let private tol = 1.0e-9
    let private close (a : float) (b : float) : bool = abs (a - b) <= tol
    let private elem (i : int) (m : TableAndElementRotationModel) : TestElement = List.item i m.elements
    let private boundedRange (lowest : float) (highest : float) : RayPositionControls.PositionRange =
        RayPositionControls.BoundedRange { lowest = BarMeters lowest; highest = BarMeters highest }

    /// The canvas point an element is drawn at (its snapped centre through the live view).
    let private canvasPointOf (i : int) (m : TableAndElementRotationModel) : ScreenPoint =
        TableView.project pixelsPerMeter center m.view ((snappedCentres m).[i])

    let private clickAt (sp : ScreenPoint) (m : TableAndElementRotationModel) : TableAndElementRotationModel = m |> updateTableAndElementRotation (PointerDown sp) |> updateTableAndElementRotation (PointerUp sp)

    /// The Main scene with the light source (element 0) selected by a click on it.
    let private sourceSelected () : TableAndElementRotationModel =
        let m = initMain ()
        clickAt (canvasPointOf 0 m) m

    /// The light source's x, y and z in metres.
    let private sourceAt (m : TableAndElementRotationModel) : float * float * float =
        let p = (elem 0 m).placement
        positionOn TableX p / 1.0<meter>, positionOn TableY p / 1.0<meter>, positionOn TableZ p / 1.0<meter>

    let private unlockAllMoves (m : TableAndElementRotationModel) : TableAndElementRotationModel =
        m |> updateTableAndElementRotation (ToggleMoveLock TableX) |> updateTableAndElementRotation (ToggleMoveLock TableY) |> updateTableAndElementRotation (ToggleMoveLock TableZ)

    // ------------------------------ the light source (update) ------------------------------

    [<Fact>]
    let ``a click on the light source selects it`` () =
        Assert.Equal(ElementSelected 0, (sourceSelected ()).selection)

    [<Fact>]
    let ``the light source starts with every move and every rotation locked, so nothing moves it`` () =
        let p = (elem 0 (initMain ())).placement
        Assert.True(p.r1Locked = RotationLocked && p.r2Locked = RotationLocked && p.r3Locked = RotationLocked, "every rotation starts locked")
        Assert.Equal<MoveLocks>({ xLock = MoveLocked; yLock = MoveLocked; zLock = MoveLocked }, p.moveLocks)
        let m = sourceSelected ()
        let tried =
            m
            |> updateTableAndElementRotation (MoveSelectedBy (TableX, 0.2))
            |> updateTableAndElementRotation (MoveSelectedBy (TableY, 0.2))
            |> updateTableAndElementRotation (MoveSelectedBy (TableZ, 0.2))
            |> updateTableAndElementRotation (SlideSelectedBy 0.2)
            |> updateTableAndElementRotation (RotateR1By 15.0)
            |> updateTableAndElementRotation (RotateR2By 15.0)
            |> updateTableAndElementRotation (RotateR3By 15.0)
        Assert.Equal<TestElement>(elem 0 m, elem 0 tried)

    [<Fact>]
    let ``every other element keeps the old defaults: R3 locked, R1 / R2 and its moves free`` () =
        let p = (elem 2 (updateTableAndElementRotation (AddElement Sample) (initMain ()))).placement
        Assert.True(p.r3Locked = RotationLocked && p.r1Locked = RotationFree && p.r2Locked = RotationFree)
        Assert.Equal<MoveLocks>({ xLock = MoveFree; yLock = MoveFree; zLock = MoveFree }, p.moveLocks)

    [<Fact>]
    let ``an unlocked light source moves freely in x, y and z — nothing checks the table`` () =
        let m = unlockAllMoves (sourceSelected ())
        let moved = m |> updateTableAndElementRotation (MoveSelectedBy (TableX, 0.25)) |> updateTableAndElementRotation (MoveSelectedTo (TableY, 0.1)) |> updateTableAndElementRotation (MoveSelectedTo (TableZ, 0.15))
        let x, y, z = sourceAt moved
        Assert.True(close x -0.75 && close y 0.1 && close z 0.15, $"source at (%g{x}, %g{y}, %g{z})")
        let far = m |> updateTableAndElementRotation (MoveSelectedTo (TableX, -9.0)) |> updateTableAndElementRotation (MoveSelectedTo (TableY, 9.0)) |> updateTableAndElementRotation (MoveSelectedTo (TableZ, 3.0))
        let fx, fy, fz = sourceAt far
        Assert.True(close fx -9.0 && close fy 9.0 && close fz 3.0, $"source at (%g{fx}, %g{fy}, %g{fz})")

    [<Fact>]
    let ``re-locking a move axis freezes it again; a snapped element only slides along its beam`` () =
        let m = sourceSelected () |> updateTableAndElementRotation (ToggleMoveLock TableZ) |> updateTableAndElementRotation (MoveSelectedTo (TableZ, 0.2)) |> updateTableAndElementRotation (ToggleMoveLock TableZ)
        let _, _, z = sourceAt (updateTableAndElementRotation (MoveSelectedTo (TableZ, 0.5)) m)
        Assert.True(close z 0.2, $"the re-locked height stayed (z = %g{z})")
        // A sample rides its beam: y / z moves and the move locks are inert, x slides it along the beam.
        let s = updateTableAndElementRotation (AddElement Sample) (initMain ())
        Assert.Equal<TableAndElementRotationModel>(s, updateTableAndElementRotation (MoveSelectedBy (TableY, 0.2)) s)
        Assert.Equal<TableAndElementRotationModel>(s, updateTableAndElementRotation (MoveSelectedBy (TableZ, 0.2)) s)
        Assert.Equal<TableAndElementRotationModel>(s, updateTableAndElementRotation (ToggleMoveLock TableX) s)
        Assert.True(close (elementX (elem 2 (updateTableAndElementRotation (MoveSelectedBy (TableX, 0.2)) s))) -0.1)

    [<Fact>]
    let ``moving the light source on x carries the ray and every element with it, like y and z`` () =
        // source (-1) → sample (-0.3) → detector (+1), plus a detector on the sample's reflected ray.
        let plateId = (Library.SampleItem Library.SeedSamples.glassPlate1mm).entryId
        let scene =
            initMain ()
            |> updateTableAndElementRotation (AddElement Sample)
            |> fun m -> updateTableAndElementRotation (BindValueIdTo ((elem 2 m).id, plateId)) m
            |> updateTableAndElementRotation (RotSetAxis (RotationControls.R2, 30.0))
            |> updateTableAndElementRotation (AddElement Detector)
        let before = snappedCentres scene
        let gaps (m : TableAndElementRotationModel) : float list = m.elements |> List.map (fun e -> elementX e - elementX (elem 0 m))
        let source = { scene with selection = ElementSelected 0 } |> updateTableAndElementRotation (ToggleMoveLock TableX)
        let movedX = updateTableAndElementRotation (MoveSelectedBy (TableX, 0.4)) source
        // Every along-beam gap is kept, so every element is drawn exactly 0.4 m further along x.
        Assert.Equal<float list>(gaps scene, gaps movedX)
        let after = snappedCentres movedX
        for i in 0 .. List.length scene.elements - 1 do
            Assert.True(abs ((after.[i]).x - (before.[i]).x - 0.4) < 1.0e-9 && abs ((after.[i]).y - (before.[i]).y) < 1.0e-9,
                        $"element %d{i} moved by (%g{(after.[i]).x - (before.[i]).x}, %g{(after.[i]).y - (before.[i]).y})")
        // The open-ray end and a Reset follow the source too; resetting the source carries the ray back.
        Assert.True(close (openRayEndX movedX) 1.4)
        let back = updateTableAndElementRotation ResetSelectedPosition movedX
        Assert.Equal<float list>(gaps scene, gaps back)
        Assert.True(close (elementX (elem 0 back)) -1.0 && close (elementX (elem 1 back)) 1.0)
        // A locked x moves nothing at all.
        Assert.Equal<TableAndElementRotationModel>(scene, { (updateTableAndElementRotation (MoveSelectedBy (TableX, 0.4)) { scene with selection = ElementSelected 0 }) with selection = scene.selection })

    [<Fact>]
    let ``the beam follows the light source: raised, every element and ray segment rides at its height`` () =
        let m = sourceSelected () |> updateTableAndElementRotation (ToggleMoveLock TableZ) |> updateTableAndElementRotation (MoveSelectedTo (TableZ, 0.12)) |> updateTableAndElementRotation (AddElement Sample)
        let centres = snappedCentres m
        Assert.True(close (centres.[0]).z 0.12)
        Assert.True(close (centres.[1]).z 0.12, $"the detector rides the raised beam (z = %g{(centres.[1]).z})")
        Assert.True(close (centres.[2]).z 0.12, $"the sample rides the raised beam (z = %g{(centres.[2]).z})")
        Assert.All(snd (mainSnap m), fun s -> Assert.True(abs (s.start.z - 0.12) < 1.0e-9 && abs (s.finish.z - 0.12) < 1.0e-9))

    [<Fact>]
    let ``an unlocked light source rotates and swings the beam; the table has no R1 / R2 lock`` () =
        let m = sourceSelected () |> updateTableAndElementRotation ToggleR2Lock |> updateTableAndElementRotation (RotateR2By 15.0)
        Assert.True(close (elem 0 m).placement.r2.degrees 15.0)
        Assert.True(abs ((snappedCentres m).[1]).y > 1.0e-3, "the detector follows the steered beam off the x axis")
        Assert.False((elem 0 (updateTableAndElementRotation ToggleR1Lock m)).placement.r1Locked = RotationLocked)
        let t = { initMain () with selection = TableSelected }
        Assert.Equal<TableAndElementRotationModel>(t, updateTableAndElementRotation ToggleR1Lock t)
        Assert.Equal<TableAndElementRotationModel>(t, updateTableAndElementRotation ToggleR2Lock t)

    [<Fact>]
    let ``Reset returns the light source to its default point even while its moves are locked`` () =
        let m =
            unlockAllMoves (sourceSelected ())
            |> updateTableAndElementRotation (MoveSelectedTo (TableX, 0.0))
            |> updateTableAndElementRotation (MoveSelectedTo (TableZ, 0.3))
            |> unlockAllMoves
        let reset = updateTableAndElementRotation ResetSelectedPosition m
        let x, y, z = sourceAt reset
        Assert.True(close x (RayModel.defaultSourcePoint.x / 1.0<meter>) && close y 0.0 && close z 0.0, $"source at (%g{x}, %g{y}, %g{z})")
        Assert.Equal(MoveLocked, moveLockOf TableX (elem 0 reset).placement)

    // ------------------------------ detectors (update) ------------------------------

    [<Fact>]
    let ``a detector slides anywhere past the start of its ray, uncapped, and Reset puts it 1 m along`` () =
        let m = { initMain () with selection = ElementSelected 1 }
        Assert.Equal(RayPositionControls.AtLeast (BarMeters -1.0), alongBeamRange m 1)
        let far = updateTableAndElementRotation (SlideSelectedTo 7.5) m
        Assert.True(close (elementX (elem 1 far)) 7.5)
        Assert.True(abs (((snappedCentres far).[1]).x - 7.5) < 1.0e-9, "laid out 7.5 m along the straight beam")
        Assert.True(close (elementX (elem 1 (m |> updateTableAndElementRotation (SlideSelectedBy 5.0) |> updateTableAndElementRotation (SlideSelectedBy 5.0)))) 11.0)
        Assert.True(close (elementX (elem 1 (updateTableAndElementRotation (SlideSelectedTo -4.0) m))) -1.0, "never before the source")
        Assert.True(close (elementX (elem 1 (updateTableAndElementRotation ResetSelectedPosition far))) 1.0)

    // ------------------------------ the ray rule (update) ------------------------------

    [<Fact>]
    let ``an element is held between the source and the detector, and moving the detector out extends its ray`` () =
        // initMain = [ source (x = -1); detector (x = +1) ] + a sample (2, selected).
        let s = updateTableAndElementRotation (AddElement Sample) (initMain ())
        Assert.Equal(boundedRange -1.0 1.0, alongBeamRange s 2)
        Assert.True(close (elementX (elem 2 (updateTableAndElementRotation (SlideSelectedTo 7.5) s))) 1.0, "stops at the detector")
        Assert.True(close (elementX (elem 2 (updateTableAndElementRotation (SlideSelectedTo -7.5) s))) -1.0, "stops at the source")
        // Move the detector 5 m out: the sample may now follow it.
        let detectorOut = { s with selection = ElementSelected 1 } |> updateTableAndElementRotation (SlideSelectedTo 5.0)
        let s' = { detectorOut with selection = ElementSelected 2 }
        Assert.Equal(boundedRange -1.0 5.0, alongBeamRange s' 2)
        Assert.True(close (elementX (elem 2 (updateTableAndElementRotation (SlideSelectedTo 4.0) s'))) 4.0)
        // A moved source carries its ray: the span moves with it, its length unchanged.
        let shifted =
            { s with selection = ElementSelected 0 }
            |> unlockAllMoves
            |> updateTableAndElementRotation (MoveSelectedTo (TableX, -3.0))
        Assert.Equal(boundedRange -3.0 -1.0, alongBeamRange shifted 2)

    [<Fact>]
    let ``with no detector on it a ray ends 1 m along; a reflected ray starts at its host`` () =
        // Remove the detector: the main beam is open and ends 1 m along.
        let open' =
            { initMain () with selection = ElementSelected 1 }
            |> updateTableAndElementRotation RemoveSelected
            |> updateTableAndElementRotation (AddElement Sample)
        Assert.Equal(boundedRange -1.0 (openRayEndX open'), alongBeamRange open' 1)
        // A sample bound to a glass plate (emits R and T) opens a reflected ray; a lens moved onto it is
        // held between the plate and 1 m.
        let plateId = (Library.SampleItem Library.SeedSamples.glassPlate1mm).entryId
        let scene =
            initMain ()
            |> updateTableAndElementRotation (AddElement Sample)
            |> fun m -> updateTableAndElementRotation (BindValueIdTo ((elem 2 m).id, plateId)) m
            |> updateTableAndElementRotation (RotSetAxis (RotationControls.R2, 30.0))
            |> updateTableAndElementRotation (AddElement Lens)
        let onReflected = updateTableAndElementRotation (PlaceOnBeam ((elem 3 scene).id, BeamLayout.OnReflectedBeamOf (elem 2 scene).id)) scene
        let hostX = elementX (elem 2 onReflected)
        Assert.Equal(boundedRange hostX (openRayEndX onReflected), alongBeamRange onReflected 3)
        Assert.True(close (elementX (elem 3 onReflected)) (elementX (elem 3 scene)), "already past its host, it keeps its place")

    // ------------------------------ the bays (headless) ------------------------------

    /// Mount the Main workbench through a re-rendering FuncUI Component (the `LibraryWindowTests` drive
    /// pattern), returning the window, a dispatch, and the latest model.
    let private mount (model0 : TableAndElementRotationModel) : Window * (TableAndElementRotationMsg -> unit) * (unit -> TableAndElementRotationModel) =
        let latest : TableAndElementRotationModel ref = ref model0
        let dispatchRef : (TableAndElementRotationMsg -> unit) ref = ref ignore
        let comp =
            Avalonia.FuncUI.Component(fun ctx ->
                let state = ctx.useState model0
                let dispatch (msg : TableAndElementRotationMsg) =
                    let m = updateTableAndElementRotation msg latest.Value
                    latest.Value <- m
                    state.Set m
                dispatchRef.Value <- dispatch
                mainView state.Current dispatch)
        let window = Window(Width = 1200.0, Height = 1050.0, Content = comp)
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window, (fun msg -> dispatchRef.Value msg; Dispatcher.UIThread.RunJobs()), (fun () -> latest.Value)

    /// The Names and AutomationIds of every effectively visible control.
    let private visibleIds (window : Window) : Set<string> =
        window.GetVisualDescendants()
        |> Seq.collect (fun v ->
            match v with
            | :? Control as c when c.IsEffectivelyVisible ->
                seq {
                    if not (isNull c.Name) then yield c.Name
                    let autoId = AutomationProperties.GetAutomationId c
                    if not (isNull autoId) then yield autoId
                }
            | _ -> Seq.empty)
        |> Set.ofSeq

    /// The text of the TextBlock named `name`.
    let private textNamed (window : Window) (name : string) : string =
        window.GetVisualDescendants()
        |> Seq.tryPick (fun v ->
            match v with
            | :? TextBlock as t when t.Name = name -> Some t.Text
            | _ -> None)
        |> Option.defaultValue ""

    /// Select element `i` through a click at its drawn point.
    let private select (dispatch : TableAndElementRotationMsg -> unit) (getModel : unit -> TableAndElementRotationModel) (i : int) : unit =
        let sp = canvasPointOf i (getModel ())
        dispatch (PointerDown sp)
        dispatch (PointerUp sp)

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Move bay shows the x / y / z bar for the source and the uncapped along-beam bar for a detector`` () =
        HeadlessSession.run (fun () ->
            let window, dispatch, getModel = mount { sourceSelected () with ribbon = BayNames.move }
            let sourceIds = visibleIds window
            Assert.Contains(ConstructorUiId.tablePositionXLockButton.value, sourceIds)
            Assert.Contains(ConstructorUiId.tablePositionZField.value, sourceIds)
            Assert.DoesNotContain(ConstructorUiId.rayPositionMinusButton.value, sourceIds)
            // Switching the selection swaps the bar (keyed, so no named field is patched across).
            select dispatch getModel 1
            Assert.Equal(ElementSelected 1, (getModel ()).selection)
            let detectorIds = visibleIds window
            Assert.Contains(ConstructorUiId.rayPositionMinusButton.value, detectorIds)
            Assert.DoesNotContain(ConstructorUiId.tablePositionXLockButton.value, detectorIds)
            Assert.Contains("uncapped", textNamed window ConstructorUiId.rayPositionReadout.value)
            select dispatch getModel 0
            Assert.Contains(ConstructorUiId.tablePositionXLockButton.value, visibleIds window)
            window.Close()
            Dispatcher.UIThread.RunJobs())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Rotation bay offers the R1 / R2 locks for an element, not for the table, and survives switching`` () =
        HeadlessSession.run (fun () ->
            // Main opens on Add (spec 0046) — open the Rotate bay.
            let window, dispatch, getModel = mount { sourceSelected () with ribbon = BayNames.rotation }
            let elementIds = visibleIds window
            Assert.Contains(ConstructorUiId.rotationLockR1Button.value, elementIds)
            Assert.Contains(ConstructorUiId.rotationLockR2Button.value, elementIds)
            // A click on the empty plate selects the table: the R1 / R2 locks hide, R3's stays.
            dispatch (PointerDown { sx = center.sx; sy = center.sy + 60.0 })
            dispatch (PointerUp { sx = center.sx; sy = center.sy + 60.0 })
            Assert.Equal(TableSelected, (getModel ()).selection)
            let tableIds = visibleIds window
            Assert.DoesNotContain(ConstructorUiId.rotationLockR1Button.value, tableIds)
            Assert.DoesNotContain(ConstructorUiId.rotationLockR2Button.value, tableIds)
            Assert.Contains(ConstructorUiId.rotationLockR3Button.value, tableIds)
            select dispatch getModel 1
            Assert.Contains(ConstructorUiId.rotationLockR1Button.value, visibleIds window)
            window.Close()
            Dispatcher.UIThread.RunJobs())
