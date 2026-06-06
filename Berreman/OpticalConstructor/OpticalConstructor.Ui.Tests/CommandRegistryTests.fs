/// Centralized-command + constructor-interaction tests (Spec 0026 Part E + the Part C
/// view interactions, slice 005, gate `ui-tests` / `ui-smoke`). The pure cases carry
/// `Category=ui-tests` (the `ui-tests` gate runs `--filter Category!=ui-smoke`); the one
/// mount-and-render case carries `Category=ui-smoke`.
///
/// The interaction acceptance criteria are proved against the PURE `ConstructorView.update`
/// (the headless harness does NOT fire real pointer/wheel events), and the command model
/// against the `Commands` registry. Coverage:
///   * AC-E1 — every key map / mouse map control derives from the ONE registry; a command
///             reachable both ways declares both bindings on one definition.
///   * AC-E8 — the key map is configurable (overrides applied over the default registry).
///   * AC-E2 — Shift/Ctrl+Shift/Alt wheel change R1/R2/R3 by the configured step and are
///             inert on a locked axis; R3 requires unlocking.
///   * AC-E3 — Shift+drag slides bounded by neighbours; Ctrl+drag reassigns the ray; a
///             plain left-drag on an element is inert and hints; the model redraws.
///   * AC-E4 — a ribbon drop snaps to the middle of the nearest central-ray path.
///   * AC-E5 — Ctrl+Z/Ctrl+Y/Ctrl+S/Esc perform undo/redo/save/cancel.
///   * AC-C2 — plain wheel and Ctrl+wheel both zoom; reset view returns to top-down.
///   * AC-C3 — clicking empty table selects the table; clicking an element selects it.
namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Threading
open Avalonia.FuncUI
open Avalonia.FuncUI.Types
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Project
open OpticalConstructor.Ui
open OpticalConstructor.Ui.Commands

module CommandRegistryTests =

    // --- Fixtures -----------------------------------------------------------

    let private at (x : float) (y : float) : TablePoint = { x = x * 1.0<meter>; y = y * 1.0<meter> }

    let private sampleAt (x : float) (y : float) : ElementPlacement =
        ElementPlacement.create Sample (at x y)

    /// A page model seeded over the canonical template project with `ps` as the on-table
    /// placements (the rest of the project is irrelevant to the interaction logic).
    let private model (ps : ElementPlacement list) : ConstructorView.Model =
        let proj = { Templates.bandpassFilter () with placements = ps; table = Table.defaultTable }
        ConstructorView.init UserEnvironment.defaults proj

    let private select (i : int) (m : ConstructorView.Model) : ConstructorView.Model =
        { m with selection = ConstructorView.ElementSelected i }

    let private placementAt (i : int) (m : ConstructorView.Model) : ElementPlacement =
        List.item i m.project.placements

    // =======================================================================
    // AC-E1 — the single source of commands (constraint 0.4).
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E1 every key and mouse binding is sourced once from the one registry`` () =
        // No gesture is bound to two commands — the key/mouse maps are the registry,
        // projected, with no second binding site.
        let keyGestures = Commands.keyBindings |> List.map fst
        let mouseGestures = Commands.mouseBindings |> List.map fst
        Assert.Equal(List.length keyGestures, List.length (List.distinct keyGestures))
        Assert.Equal(List.length mouseGestures, List.length (List.distinct mouseGestures))
        // Every registry binding resolves back to its command through the projected map.
        for (g, c) in Commands.keyBindings do Assert.Equal(Some c, Commands.lookupKey g)
        for (g, c) in Commands.mouseBindings do Assert.Equal(Some c, Commands.lookupMouse g)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E1 a command reachable by key AND mouse declares both bindings on one definition`` () =
        // Slide is the canonical keyboard-plus-mouse command (arrow keys OR Shift+drag).
        match (Commands.defOf SlideAlongRay).Value.binding with
        | KeyboardAndMouse (ks, ms) ->
            Assert.False(List.isEmpty ks, "slide must declare its keyboard gestures")
            Assert.False(List.isEmpty ms, "slide must declare its mouse gesture")
        | other -> Assert.Fail(sprintf "slide must be keyboard-plus-mouse, was %A" other)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E1 the declared key bindings resolve to the right commands`` () =
        Assert.Equal(Some ResetRotation, Commands.lookupKey (keyWith [ Shift ] (Letter 'R')))
        Assert.Equal(Some DeleteElement, Commands.lookupKey (key Delete))
        Assert.Equal(Some DeleteElement, Commands.lookupKey (key Backspace))
        Assert.Equal(Some DuplicateElement, Commands.lookupKey (keyWith [ Ctrl ] (Letter 'D')))
        Assert.Equal(Some Undo, Commands.lookupKey (keyWith [ Ctrl ] (Letter 'Z')))
        Assert.Equal(Some Redo, Commands.lookupKey (keyWith [ Ctrl ] (Letter 'Y')))
        Assert.Equal(Some Redo, Commands.lookupKey (keyWith [ Ctrl; Shift ] (Letter 'Z')))
        Assert.Equal(Some SaveProject, Commands.lookupKey (keyWith [ Ctrl ] (Letter 'S')))
        Assert.Equal(Some ResetView, Commands.lookupKey (keyWith [ Ctrl ] (Digit 0)))
        Assert.Equal(Some CancelOrDeselect, Commands.lookupKey (key Escape))
        Assert.Equal(Some NextElement, Commands.lookupKey (key Tab))
        Assert.Equal(Some PreviousElement, Commands.lookupKey (keyWith [ Shift ] Tab))

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E1 the declared mouse bindings resolve to the right commands`` () =
        // Zoom: plain wheel OR Ctrl+wheel; the three rotations on Shift/Ctrl+Shift/Alt.
        Assert.Equal(Some ZoomView, Commands.lookupMouse (WheelGesture Set.empty))
        Assert.Equal(Some ZoomView, Commands.lookupMouse (WheelGesture (set [ Ctrl ])))
        Assert.Equal(Some RotateR1, Commands.lookupMouse (WheelGesture (set [ Shift ])))
        Assert.Equal(Some RotateR2, Commands.lookupMouse (WheelGesture (set [ Ctrl; Shift ])))
        Assert.Equal(Some RotateR3, Commands.lookupMouse (WheelGesture (set [ Alt ])))
        // Drags: plain = pan, Shift = slide, Ctrl = reassign.
        Assert.Equal(Some PanView, Commands.lookupMouse (DragGesture (LeftButton, Set.empty)))
        Assert.Equal(Some SlideAlongRay, Commands.lookupMouse (DragGesture (LeftButton, set [ Shift ])))
        Assert.Equal(Some MoveToRay, Commands.lookupMouse (DragGesture (LeftButton, set [ Ctrl ])))
        // The element open/menu gestures, and the ribbon drop.
        Assert.Equal(Some ElementContextMenu, Commands.lookupMouse (Press (RightButton, Set.empty)))
        Assert.Equal(Some OpenElementDialog, Commands.lookupMouse (DoublePress LeftButton))
        Assert.Equal(Some PlaceFromRibbon, Commands.lookupMouse RibbonDropGesture)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E1 the context-menu controls project from the one registry`` () =
        // The element context menu surfaces the registry commands flagged inContextMenu.
        Assert.Contains(ResetRotation, Commands.contextMenuCommands)
        Assert.Contains(DeleteElement, Commands.contextMenuCommands)
        Assert.Contains(OpenElementDialog, Commands.contextMenuCommands)
        Assert.Contains(LocalHelp, Commands.contextMenuCommands)

    // =======================================================================
    // AC-E8 — the configurable key map (overrides applied over the default registry).
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E8 a key-binding override rebinds the command over the default registry`` () =
        let overrides = [ "reset-rotation", "Ctrl+R" ]
        // The override moves Reset rotation onto Ctrl+R and frees Shift+R.
        Assert.Equal(Some ResetRotation, Commands.resolveKey overrides (keyWith [ Ctrl ] (Letter 'R')))
        let customized = Commands.keyLookupOf (Commands.withKeyOverrides overrides)
        Assert.Equal(None, Map.tryFind (keyWith [ Shift ] (Letter 'R')) customized)
        // Every OTHER command is untouched by the single override.
        Assert.Equal(Some Undo, Commands.resolveKey overrides (keyWith [ Ctrl ] (Letter 'Z')))

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E8 a gesture string round-trips through parse and format`` () =
        let g = keyWith [ Ctrl; Shift ] (Letter 'Z')
        Assert.Equal("Ctrl+Shift+Z", Commands.formatKeyGesture g)
        Assert.Equal(Some g, Commands.parseKeyGesture "Ctrl+Shift+Z")
        Assert.Equal(Some (key Delete), Commands.parseKeyGesture "Delete")
        Assert.Equal(Some (keyWith [ Ctrl ] (Digit 0)), Commands.parseKeyGesture "Ctrl+0")
        // A malformed override is ignored (the default binding then stands).
        Assert.Equal(None, Commands.parseKeyGesture "Ctrl+Nonsense")

    // =======================================================================
    // AC-E2 — rotations by the configured step, inert on a locked axis.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E2 Shift, Ctrl+Shift and Alt wheel rotate R1, R2, R3 by the step`` () =
        let m0 = select 0 (model [ sampleAt 0.0 0.0 ])
        // R1 (unlocked by default): Shift+wheel turns it by the 5° step.
        let m1 = ConstructorView.update (ConstructorView.WheelAt ([ Shift ], 1, at 0.0 0.0)) m0
        Assert.True(abs ((placementAt 0 m1).r1.degrees - 5.0) < 1e-9)
        // R2 (unlocked by default): Ctrl+Shift+wheel turns it by the step.
        let m2 = ConstructorView.update (ConstructorView.WheelAt ([ Ctrl; Shift ], 1, at 0.0 0.0)) m0
        Assert.True(abs ((placementAt 0 m2).r2.degrees - 5.0) < 1e-9)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E2 R3 is inert until unlocked, then rotates by the step`` () =
        let m0 = select 0 (model [ sampleAt 0.0 0.0 ])
        // R3 starts LOCKED (A.1.2), so Alt+wheel is inert.
        let mLocked = ConstructorView.update (ConstructorView.WheelAt ([ Alt ], 1, at 0.0 0.0)) m0
        Assert.Equal(0.0, (placementAt 0 mLocked).r3.value)
        // Unlock R3 from the context menu, then Alt+wheel turns it by the step.
        let mUnlocked = ConstructorView.update (ConstructorView.MenuToggleLock ConstructorView.AxisR3) m0
        let mTurned = ConstructorView.update (ConstructorView.WheelAt ([ Alt ], 1, at 0.0 0.0)) mUnlocked
        Assert.True(abs ((placementAt 0 mTurned).r3.degrees - 5.0) < 1e-9)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E2 a locked R1 is inert and the rotation step is configurable`` () =
        // Locking R1 makes Shift+wheel inert (A.4.5).
        let mLocked = { select 0 (model [ sampleAt 0.0 0.0 ]) with selection = ConstructorView.ElementSelected 0 }
        let mLocked = ConstructorView.update (ConstructorView.MenuToggleLock ConstructorView.AxisR1) mLocked
        let mInert = ConstructorView.update (ConstructorView.WheelAt ([ Shift ], 1, at 0.0 0.0)) mLocked
        Assert.Equal(0.0, (placementAt 0 mInert).r1.value)
        // A configured 10° step rotates by 10° per notch (E.3.1 configurability).
        let m10 =
            let m = select 0 (model [ sampleAt 0.0 0.0 ])
            { m with keyMap = { UserEnvironment.defaultKeyMap with rotationStepDegrees = 10.0 } }
        let mTurned = ConstructorView.update (ConstructorView.WheelAt ([ Shift ], 1, at 0.0 0.0)) m10
        Assert.True(abs ((placementAt 0 mTurned).r1.degrees - 10.0) < 1e-9)

    // =======================================================================
    // AC-E3 — slide bounded by neighbours / reassign / inert plain-drag + hint.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E3 Shift+drag slides the element bounded by its neighbours and never past them`` () =
        // Three elements on the central ray; the middle one slides between the outer two.
        let m1 = select 1 (model [ sampleAt -0.5 0.0; sampleAt 0.0 0.0; sampleAt 0.5 0.0 ])
        let mBegin = ConstructorView.update (ConstructorView.BeginDrag ([ Shift ], at 0.0 0.0)) m1
        // Sliding far right clamps to the right neighbour (never passes it).
        let mRight = ConstructorView.update (ConstructorView.SlideTo (at 10.0 0.0)) mBegin
        Assert.Equal(0.5<meter>, (placementAt 1 mRight).placementPoint.x)
        // Sliding far left clamps to the left neighbour.
        let mLeft = ConstructorView.update (ConstructorView.SlideTo (at -10.0 0.0)) mBegin
        Assert.Equal(-0.5<meter>, (placementAt 1 mLeft).placementPoint.x)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E3 Ctrl+drag reassigns the element to a different ray`` () =
        let m1 = select 0 (model [ sampleAt 0.0 0.0 ])
        Assert.Equal(RayModel.CentralRay, ConstructorView.rayOfIndex 0 m1)
        let mBegin = ConstructorView.update (ConstructorView.BeginDrag ([ Ctrl ], at 0.0 0.0)) m1
        let mEnd = ConstructorView.update ConstructorView.EndDrag mBegin
        Assert.Equal(RayModel.ReflectedBranch 0, ConstructorView.rayOfIndex 0 mEnd)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E3 a plain left-drag on an element does nothing and shows the movable hint`` () =
        let m1 = select 0 (model [ sampleAt 0.0 0.0 ])
        let mPlain = ConstructorView.update (ConstructorView.BeginDrag ([], at 0.0 0.0)) m1
        Assert.True(mPlain.movableHint, "the movable hint must show on a plain drag over an element")
        // The element is untouched (the inert guard against accidental moves, E.4.4).
        Assert.Equal((placementAt 0 m1).placementPoint.x, (placementAt 0 mPlain).placementPoint.x)
        match mPlain.drag with
        | ConstructorView.NoDrag -> ()
        | other -> Assert.Fail(sprintf "a plain element drag must not start a move, was %A" other)

    // =======================================================================
    // AC-E4 — drag-to-place snaps to the middle of the nearest central-ray path.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E4 a ribbon drop snaps the new element to the central-ray middle`` () =
        let m0 = model []
        let dropped = ConstructorView.update (ConstructorView.RibbonDrop (Sample, at 0.4 0.3)) m0
        // The new element lands attached to the CR (its middle), not at the free drop point.
        let mid = ConstructorView.centralRayMiddle m0
        Assert.Equal(1, List.length dropped.project.placements)
        Assert.Equal(mid.x, (placementAt 0 dropped).placementPoint.x)
        Assert.Equal(mid.y, (placementAt 0 dropped).placementPoint.y)
        Assert.Equal(ConstructorView.ElementSelected 0, dropped.selection)

    // =======================================================================
    // AC-E5 — undo / redo / save / cancel.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E5 Ctrl+Z and Ctrl+Y undo and redo the last edit`` () =
        let m1 = select 0 (model [ sampleAt 0.0 0.0 ])
        let mRot = ConstructorView.update (ConstructorView.WheelAt ([ Shift ], 1, at 0.0 0.0)) m1
        Assert.True(abs ((placementAt 0 mRot).r1.degrees - 5.0) < 1e-9)
        let mUndo = ConstructorView.update (ConstructorView.KeyPress (keyWith [ Ctrl ] (Letter 'Z'))) mRot
        Assert.Equal(0.0, (placementAt 0 mUndo).r1.value)
        let mRedo = ConstructorView.update (ConstructorView.KeyPress (keyWith [ Ctrl ] (Letter 'Y'))) mUndo
        Assert.True(abs ((placementAt 0 mRedo).r1.degrees - 5.0) < 1e-9)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-E5 Ctrl+S requests a save and Esc deselects to the table`` () =
        let m0 = model [ sampleAt 0.0 0.0 ]
        let mSaved = ConstructorView.update (ConstructorView.KeyPress (keyWith [ Ctrl ] (Letter 'S'))) m0
        Assert.Equal(1, mSaved.saveRequests)
        let mEsc = ConstructorView.update (ConstructorView.KeyPress (key Escape)) (select 0 m0)
        Assert.Equal(ConstructorView.TableSelected, mEsc.selection)

    // =======================================================================
    // AC-C2 — zoom (plain + Ctrl wheel) and reset view.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C2 plain wheel and Ctrl+wheel both zoom`` () =
        let m0 = model []
        let mIn = ConstructorView.update (ConstructorView.WheelAt ([], 1, at 0.0 0.0)) m0
        Assert.True(mIn.view.zoom > m0.view.zoom, "plain wheel up must zoom in")
        let mOut = ConstructorView.update (ConstructorView.WheelAt ([], -1, at 0.0 0.0)) m0
        Assert.True(mOut.view.zoom < m0.view.zoom, "plain wheel down must zoom out")
        let mCtrl = ConstructorView.update (ConstructorView.WheelAt ([ Ctrl ], 1, at 0.0 0.0)) m0
        Assert.True(mCtrl.view.zoom > m0.view.zoom, "Ctrl+wheel up must also zoom in")

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C2 reset view returns to straight top-down at the default zoom`` () =
        let m0 = model []
        let dirty = { m0 with view = { m0.view with r2 = Angle.degree 30.0; panX = 50.0; panY = -20.0; zoom = 3.0 } }
        // Reset view is confirmation-gated in slice 007 (K.2 / AC-K2 — it is ephemeral view state
        // NOT captured by the project-snapshot EditHistory, so it confirms rather than undoes):
        // `Invoke ResetView` arms a pending confirm and leaves the (dirty) view untouched...
        let armed = ConstructorView.update (ConstructorView.Invoke ResetView) dirty
        Assert.True(abs ((armed.view.r2.degrees) - 30.0) < 1e-9, "the view stays dirty until the reset is confirmed")
        Assert.Equal(Some "confirm.resetView", ConstructorView.pendingPromptKey armed)
        // ...then confirming through the same-row gate performs the reset to straight top-down.
        let reset = ConstructorView.update ConstructorView.ConfirmPending armed
        Assert.Equal(0.0, reset.view.r2.value)
        Assert.Equal(0.0, reset.view.panX)
        Assert.Equal(0.0, reset.view.panY)
        Assert.Equal(Table.defaultZoom, reset.view.zoom)
        Assert.Equal(None, reset.pending)

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``C2-4 rotating the view rotates every element together (elements travel with the table)`` () =
        // The table's R1 is measured relative to the screen (C.2.4): rotating the view
        // spins the WHOLE table about the canvas centre, so every element's screen vector
        // is its top-down vector rotated by the same angle.
        let v0 = (model []).view
        let vRot = { v0 with r1 = Angle.degree 90.0 }
        // The table origin maps to the canvas centre under any rotation.
        let center = ConstructorView.projectToCanvas v0 (at 0.0 0.0)
        let vecFrom (v : Table.TableViewState) (p : TablePoint) =
            let (x, y) = ConstructorView.projectToCanvas v p
            (x - fst center, y - snd center)
        // A +90° in-plane rotation sends (vx, vy) -> (-vy, vx).
        let rot90 (vx, vy) = (-vy, vx)
        for p in [ at 0.5 0.0; at 0.0 0.5; at -0.3 0.2 ] do
            let (ex, ey) = rot90 (vecFrom v0 p)
            let (rx, ry) = vecFrom vRot p
            Assert.True(abs (ex - rx) < 1e-6 && abs (ey - ry) < 1e-6,
                        sprintf "element at %A must travel with the rotated table" p)

    // =======================================================================
    // AC-C3 — table selection.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-tests")>]
    let ``AC-C3 clicking an element selects it and clicking empty table selects the table`` () =
        let m0 = model [ sampleAt 0.3 0.0 ]
        let onElement = ConstructorView.update (ConstructorView.SelectAt (at 0.3 0.0)) m0
        Assert.Equal(ConstructorView.ElementSelected 0, onElement.selection)
        let onEmpty = ConstructorView.update (ConstructorView.SelectAt (at 0.9 0.4)) onElement
        Assert.Equal(ConstructorView.TableSelected, onEmpty.selection)

    // =======================================================================
    // ui-smoke — the constructor surface mounts and renders one frame.
    // =======================================================================

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the constructor surface renders one frame without throwing`` () =
        HeadlessSession.run (fun () ->
            let m = select 0 (model [ sampleAt -0.3 0.0; sampleAt 0.3 0.0 ])
            let window = Window()
            window.Content <- Component(fun _ctx -> ConstructorView.view m ignore)
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
