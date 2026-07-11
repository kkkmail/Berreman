/// Spec 0038 Part B.1 (step 002): pure-MVU coverage of the Main "Lego constructor"
/// scene's `update` arms that `TableAndElementRotationTests` does not pin — the scene
/// that replaced the retired Elmish shell as the app's Main screen. Everything here is
/// windowless model logic (gate `ui-tests`): the add/remove palette per catalogue
/// kind, the Move-bay clamps, the Render-bay config clamps, exact-angle setting with
/// its normalization and R3-lock semantics, the confirm-gated rotation resets, the
/// wheel zoom clamps, the confirm-gated Library bind, pointer drag-threshold
/// behaviour, the Experiments-bay host guards, and the Library (samples) workbench
/// bay's disarm-on-query-change discipline. (The Materials bay's twin discipline moved
/// with the bay into the step-013 Materials window — see `MaterialsWindowTests`.)
namespace OpticalConstructor.Ui.Tests

open Xunit
open Berreman.Constants
open OpticalConstructor.Controls
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.TableView
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

module MainSceneMsgTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9
    let private elem (i : int) (m : Model) : TestElement = List.item i m.elements
    let private clickAt (sp : ScreenPoint) (m : Model) : Model =
        m |> update (PointerDown sp) |> update (PointerUp sp)

    /// A screen point clearly off the plate (the existing off-plate click precedent).
    let private offPlate : ScreenPoint = { sx = center.sx; sy = center.sy + 200.0 }

    /// Every catalogue kind, resolvable from its short palette code.
    let private allKinds : CatalogueKind list =
        [ LightSource; LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ]

    let private kindOfCode (code : string) : CatalogueKind =
        match allKinds |> List.filter (fun k -> Catalogue.kindCode k = code) with
        | [ k ] -> k
        | matches -> failwith $"catalogue code '{code}' matched {List.length matches} kinds"

    /// The common Lego fixture: the Main scene with one added Sample, selected (index 2).
    let private withSample () : Model = update (AddElement Sample) (initMain ())

    // ======================= add / remove (the Lego palette) =======================

    [<Theory>]
    [<InlineData("S")>]
    [<InlineData("LP")>]
    [<InlineData("CP")>]
    [<InlineData("Sa")>]
    [<InlineData("L")>]
    [<InlineData("FM")>]
    [<InlineData("CM")>]
    [<InlineData("D")>]
    let ``AddElement appends each catalogue kind on the beam, selected, at the default zoom`` (code : string) =
        let kind = kindOfCode code
        let m = update (AddElement kind) (initMain ())
        Assert.Equal(3, List.length m.elements)
        Assert.Equal(ElementSelected 2, m.selection)
        Assert.Equal(kind, (elem 2 m).placement.catalogueKind)
        Assert.True(close ((elem 2 m).placement.placementPoint.y / 1.0<meter>) 0.0, "the new element sits on the beam")
        Assert.True(close (elem 2 m).zoom defaultElementZoom)

    [<Fact>]
    let ``successive adds spread the middle elements along the beam`` () =
        let m = initMain () |> update (AddElement Sample) |> update (AddElement Lens)
        Assert.True(elementX (elem 2 m) < elementX (elem 3 m), "later adds land further along the beam")
        Assert.False(close (elementX (elem 2 m)) (elementX (elem 3 m)), "no two adds land on the same spot")

    [<Fact>]
    let ``adding an element leaves the existing elements and the table view untouched`` () =
        let before = initMain ()
        let after = update (AddElement Lens) before
        Assert.Equal<TestElement list>(before.elements, after.elements |> List.take (List.length before.elements))
        Assert.Equal(before.view, after.view)

    [<Fact>]
    let ``adding an element preserves the palette and the snap chain`` () =
        let before = initMain ()
        let after = update (AddElement FlatMirror) before
        Assert.Equal<CatalogueKind list>(before.palette, after.palette)
        Assert.Equal(before.snapChain, after.snapChain)

    [<Fact>]
    let ``RemoveSelected with nothing selected is inert`` () =
        let m = { initMain () with selection = NothingSelected }
        Assert.Equal<Model>(m, update RemoveSelected m)

    [<Fact>]
    let ``removing a middle element preserves the survivors' order`` () =
        let m = initMain () |> update (AddElement Sample) |> update (AddElement Lens)
        let removed = update RemoveSelected { m with selection = ElementSelected 2 }   // drop the Sample
        Assert.Equal<CatalogueKind list>(
            [ LightSource; Detector; Lens ],
            removed.elements |> List.map (fun e -> e.placement.catalogueKind))
        Assert.Equal(NothingSelected, removed.selection)

    [<Fact>]
    let ``an add after a remove appends at the end and selects the new element`` () =
        let m = withSample () |> update RemoveSelected |> update (AddElement CircularPolarizer)
        Assert.Equal(3, List.length m.elements)
        Assert.Equal(ElementSelected 2, m.selection)
        Assert.Equal(CircularPolarizer, (elem 2 m).placement.catalogueKind)

    // ============================== the Move bay ==============================

    [<Fact>]
    let ``SlideSelectedBy accumulates across dispatches`` () =
        let m = withSample () |> update (SlideSelectedBy 0.1) |> update (SlideSelectedBy 0.1)
        Assert.True(close (elementX (elem 2 m)) -0.1, $"expected -0.1, got {elementX (elem 2 m)}")

    [<Fact>]
    let ``SlideSelectedBy clamps at both plate edges and stays clamped`` () =
        let m = withSample ()
        let atPlus = m |> update (SlideSelectedBy 10.0) |> update (SlideSelectedBy 10.0)
        Assert.True(close (elementX (elem 2 atPlus)) 1.0, "clamped at +half the plate")
        let atMinus = atPlus |> update (SlideSelectedBy -10.0)
        Assert.True(close (elementX (elem 2 atMinus)) -1.0, "one big negative slide runs to -half")

    [<Fact>]
    let ``sliding moves the element only along the beam`` () =
        let m = update (SlideSelectedTo 0.4) (withSample ())
        Assert.True(close (elementX (elem 2 m)) 0.4)
        Assert.True(close ((elem 2 m).placement.placementPoint.y / 1.0<meter>) 0.0, "y stays on the beam")

    [<Fact>]
    let ``slide messages are inert with nothing selected`` () =
        let m = { withSample () with selection = NothingSelected }
        Assert.Equal<Model>(m, update (SlideSelectedBy 0.5) m)
        Assert.Equal<Model>(m, update (SlideSelectedTo 0.5) m)
        Assert.Equal<Model>(m, update ResetSelectedPosition m)

    [<Fact>]
    let ``ResetSelectedPosition is inert when the table is selected`` () =
        let m = { withSample () with selection = TableSelected }
        Assert.Equal<Model>(m, update ResetSelectedPosition m)

    [<Fact>]
    let ``the along-beam clamp is half the plate length`` () =
        Assert.True(close (plateHalfLength (initMain ())) 1.0, "the default 2 m table clamps sliding to ±1 m")

    // ============================= the Render bay =============================

    [<Fact>]
    let ``a double renderer swap round-trips the renderer kind`` () =
        let m = initMain ()
        Assert.Equal(m.render.kind, (m |> update RenderSwap |> update RenderSwap).render.kind)

    [<Fact>]
    let ``RenderSetRailsIndex clamps to the option ends`` () =
        let m = initMain ()
        Assert.Equal(4, (update (RenderSetRailsIndex -5) m).render.rails)
        Assert.Equal(72, (update (RenderSetRailsIndex 99) m).render.rails)

    [<Fact>]
    let ``RenderSetCircles clamps at the minimum`` () =
        Assert.Equal(1, (update (RenderSetCircles 0) (initMain ())).render.circles)

    [<Fact>]
    let ``RenderSetRadialsIndex clamps to the option ends`` () =
        let m = initMain ()
        Assert.Equal(4, (update (RenderSetRadialsIndex -1) m).render.radials)
        Assert.Equal(36, (update (RenderSetRadialsIndex 99) m).render.radials)

    [<Fact>]
    let ``RenderSetLineOpacity clamps into the unit interval`` () =
        let m = initMain ()
        Assert.Equal(0.0, (update (RenderSetLineOpacity -0.5) m).render.lineOpacity)
        Assert.Equal(1.0, (update (RenderSetLineOpacity 1.5) m).render.lineOpacity)

    [<Fact>]
    let ``in-range opacities are stored exactly`` () =
        let m = initMain ()
        Assert.Equal(0.5, (update (RenderSetRailOpacity 0.5) m).render.railOpacity)
        Assert.Equal(0.25, (update (RenderSetFaceOpacity 0.25) m).render.faceOpacity)

    [<Fact>]
    let ``render tuning leaves the scene itself untouched`` () =
        let before = withSample ()
        let after = before |> update RenderSwap |> update (RenderSetRailsIndex 1) |> update (RenderSetFaceOpacity 0.3)
        Assert.Equal<TestElement list>(before.elements, after.elements)
        Assert.Equal(before.selection, after.selection)
        Assert.Equal(before.view, after.view)

    // ==================== exact angles (RotSetAxis) and locks ====================

    [<Fact>]
    let ``RotSetAxis normalizes the angle mod 360`` () =
        let m = update (RotSetAxis (RotationControls.R1, 370.0)) (initMain ())   // table selected
        Assert.True(close m.view.r1.degrees 10.0, $"370° must store as 10°, got {m.view.r1.degrees}")

    [<Fact>]
    let ``RotSetAxis R3 respects the table lock`` () =
        let m = update (RotSetAxis (RotationControls.R3, 30.0)) (initMain ())   // table R3 unlocked by default
        Assert.True(close m.view.r3.degrees 30.0)
        let locked = update ToggleR3Lock m                                       // table selected → locks the table
        let after = update (RotSetAxis (RotationControls.R3, 60.0)) locked
        Assert.True(close after.view.r3.degrees 30.0, "a locked table R3 ignores the exact-angle set")

    [<Fact>]
    let ``RotSetAxis R3 on a default-locked element is inert until it is unlocked`` () =
        let m = withSample ()
        let still = update (RotSetAxis (RotationControls.R3, 20.0)) m
        Assert.True(close (elem 2 still).placement.r3.degrees 0.0, "elements start R3-locked")
        let unlocked = m |> update ToggleR3Lock |> update (RotSetAxis (RotationControls.R3, 20.0))
        Assert.True(close (elem 2 unlocked).placement.r3.degrees 20.0, "after the toggle the exact set applies")

    [<Fact>]
    let ``RotSetAxis touches only the named element axis`` () =
        let m = update (RotSetAxis (RotationControls.R2, 30.0)) (withSample ())
        Assert.True(close (elem 2 m).placement.r2.degrees 30.0)
        Assert.True(close (elem 2 m).placement.r1.degrees 0.0)
        Assert.True(close (elem 2 m).placement.r3.degrees 0.0)

    [<Fact>]
    let ``RotSetAxis with nothing selected is inert`` () =
        let m = { initMain () with selection = NothingSelected }
        Assert.Equal<Model>(m, update (RotSetAxis (RotationControls.R1, 45.0)) m)

    [<Theory>]
    [<InlineData(-15.0, 345.0)>]
    [<InlineData(360.0, 0.0)>]
    [<InlineData(720.0, 0.0)>]
    [<InlineData(10.0, 10.0)>]
    [<InlineData(-360.0, 0.0)>]
    let ``normalizeDegrees wraps every angle into 0 to 360`` (input : float) (expected : float) =
        Assert.True(close (normalizeDegrees input) expected, $"normalizeDegrees {input} = {normalizeDegrees input}, expected {expected}")

    // ==================== the confirm-gated rotation resets ====================

    [<Fact>]
    let ``RotRequestReset arms the confirm and RotCancel disarms it leaving the angles intact`` () =
        let rotated = update (RotateR1By 15.0) (initMain ())   // table selected
        let armed = update RotRequestReset rotated
        Assert.Equal(RotationControls.ConfirmReset, armed.rotationConfirm)
        let cancelled = update RotCancel armed
        Assert.Equal(RotationControls.NoConfirm, cancelled.rotationConfirm)
        Assert.True(close cancelled.view.r1.degrees 15.0, "cancel must not reset anything")

    [<Fact>]
    let ``RotRequestResetAll arms the reset-all confirm`` () =
        Assert.Equal(RotationControls.ConfirmResetAll, (update RotRequestResetAll (initMain ())).rotationConfirm)

    [<Fact>]
    let ``RotConfirm with no pending confirm is inert`` () =
        let m = update (RotateR1By 15.0) (initMain ())
        Assert.Equal<Model>(m, update RotConfirm m)

    [<Fact>]
    let ``a confirmed element reset leaves the table and the other elements alone`` () =
        let m =
            initMain ()
            |> update (RotateR1By 30.0)          // table selected → table R1 = 30°
            |> update (AddElement Sample)        // element 2 selected
            |> update (RotateR1By 20.0)          // element R1 = 20°
            |> update RotRequestReset
            |> update RotConfirm
        Assert.True(close (elem 2 m).placement.r1.degrees 0.0, "the selected element resets")
        Assert.True(close m.view.r1.degrees 30.0, "the table keeps its rotation")
        Assert.True(close (elem 0 m).placement.r1.degrees 0.0)

    // ============================ the R3-lock toggle ============================

    [<Fact>]
    let ``ToggleR3Lock flips the selected element's lock and a second toggle restores it`` () =
        let m = withSample ()
        Assert.True((elem 2 m).placement.r3Locked, "elements start R3-locked")
        let once = update ToggleR3Lock m
        Assert.False((elem 2 once).placement.r3Locked)
        Assert.True((elem 2 (update ToggleR3Lock once)).placement.r3Locked)

    [<Fact>]
    let ``ToggleR3Lock with nothing selected is inert`` () =
        let m = { initMain () with selection = NothingSelected }
        Assert.Equal<Model>(m, update ToggleR3Lock m)

    // ============================ wheel zoom clamps ============================

    [<Fact>]
    let ``the element draw zoom clamps at its maximum`` () =
        let m = update (Wheel (Set.ofList [ ModCtrl; ModAlt ], 100)) (withSample ())
        Assert.True(close (elem 2 m).zoom 50.0, $"expected the 50x cap, got {(elem 2 m).zoom}")

    [<Fact>]
    let ``the element draw zoom clamps at its minimum`` () =
        let m = update (Wheel (Set.ofList [ ModCtrl; ModAlt ], -100)) (withSample ())
        Assert.True(close (elem 2 m).zoom 1.0, $"expected the 1x floor, got {(elem 2 m).zoom}")

    [<Fact>]
    let ``the table zoom clamps at its maximum`` () =
        let m = update (Wheel (Set.empty, 100)) (initMain ())
        Assert.True(close m.view.zoom 5.0, $"expected the 5x cap, got {m.view.zoom}")

    [<Fact>]
    let ``the table zoom clamps at its minimum`` () =
        let m = update (Wheel (Set.empty, -100)) (initMain ())
        Assert.True(close m.view.zoom 0.2, $"expected the 0.2x floor, got {m.view.zoom}")

    [<Fact>]
    let ``zoom-all magnifies every element even while the table is selected`` () =
        let m = update (Wheel (Set.ofList [ ModCtrl; ModShift; ModAlt ], 2)) (initMain ())   // table selected
        Assert.Equal(TableSelected, m.selection)
        for e in m.elements do
            Assert.True(close e.zoom (defaultElementZoom * 1.1 * 1.1), $"every element zooms, got {e.zoom}")

    [<Fact>]
    let ``zooming the table leaves the element draw zoom alone`` () =
        let m = update (Wheel (Set.empty, 3)) (initMain ())
        Assert.True(m.view.zoom > 1.0, "the table zoomed")
        for e in m.elements do
            Assert.True(close e.zoom defaultElementZoom, "no element draw zoom changed")

    // ==================== the confirm-gated Library bind ====================

    [<Fact>]
    let ``RequestBindValueId sets the pending entry for a selected element without binding`` () =
        let m = update (RequestBindValueId "lens-a") (withSample ())
        Assert.Equal(Some "lens-a", m.pendingEntry)
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``RequestBindValueId is inert while the table is selected`` () =
        let m = initMain ()   // table selected
        Assert.Equal<Model>(m, update (RequestBindValueId "lens-a") m)

    [<Fact>]
    let ``ConfirmBindValueId commits the pending entry and clears it`` () =
        let m = withSample () |> update (RequestBindValueId "lens-a") |> update ConfirmBindValueId
        Assert.Equal(Some "lens-a", (elem 2 m).placement.valueId)
        Assert.Equal(None, m.pendingEntry)

    [<Fact>]
    let ``ConfirmBindValueId without a pending entry binds nothing`` () =
        let m = update ConfirmBindValueId (withSample ())
        Assert.Equal(None, (elem 2 m).placement.valueId)
        Assert.Equal(None, m.pendingEntry)

    [<Fact>]
    let ``CancelBindValueId clears the pending entry without binding`` () =
        let m = withSample () |> update (RequestBindValueId "lens-a") |> update CancelBindValueId
        Assert.Equal(None, m.pendingEntry)
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``a selection change clears the pending entry`` () =
        let m = withSample () |> update (RequestBindValueId "lens-a") |> clickAt offPlate
        Assert.Equal(NothingSelected, m.selection)
        Assert.Equal(None, m.pendingEntry)

    [<Fact>]
    let ``BindValueId binds a selected element directly and is inert for the table`` () =
        let bound = update (BindValueId "detector-b") (withSample ())
        Assert.Equal(Some "detector-b", (elem 2 bound).placement.valueId)
        let tableSel = initMain ()
        Assert.Equal<Model>(tableSel, update (BindValueId "detector-b") tableSel)

    // ========================= pointer drag threshold =========================

    [<Fact>]
    let ``PointerMove without a press is inert`` () =
        let m = initMain ()
        Assert.Equal<Model>(m, update (PointerMove { sx = 10.0; sy = 10.0 }) m)

    [<Fact>]
    let ``a sub-threshold jiggle still counts as a click-select`` () =
        // Element 0 of the static scene sits at x = -0.5 m → screen centre - 100 px.
        let start : ScreenPoint = { sx = center.sx - 100.0; sy = center.sy }
        let nearby : ScreenPoint = { sx = start.sx + 1.0; sy = start.sy }
        let m = init () |> update (PointerDown start) |> update (PointerMove nearby) |> update (PointerUp nearby)
        Assert.Equal(ElementSelected 0, m.selection)

    [<Fact>]
    let ``a pan-drag preserves the selection`` () =
        let selected = clickAt { sx = center.sx - 100.0; sy = center.sy } (init ())
        Assert.Equal(ElementSelected 0, selected.selection)
        let from : ScreenPoint = { sx = center.sx + 50.0; sy = center.sy + 50.0 }
        let dragged =
            selected
            |> update (PointerDown from)
            |> update (PointerMove { sx = from.sx + 120.0; sy = from.sy })
            |> update (PointerUp { sx = from.sx + 120.0; sy = from.sy })
        Assert.Equal(ElementSelected 0, dragged.selection)
        Assert.True(close dragged.view.panX 120.0, "the drag panned the view")

    // ===================== the Experiments-bay host guards =====================

    [<Fact>]
    let ``ExpChooseElement with an unknown element id is inert`` () =
        let m = withSample ()
        Assert.Equal<Model>(m, update (ExpChooseElement "not-an-element-id") m)

    [<Fact>]
    let ``ExpEdit with a non-numeric id is inert`` () =
        let m = initMain ()
        Assert.Equal<Model>(m, update (ExpEdit "not-a-number") m)

    [<Fact>]
    let ``ExpRemove with a non-numeric id is inert`` () =
        let m = initMain ()
        Assert.Equal<Model>(m, update (ExpRemove "not-a-number") m)

    // The workbench bays' disarm-on-query-change discipline moved WITH the bays into their
    // windows: the Materials bay's twins live in `MaterialsWindowTests` (spec 0038 step 013)
    // and the Library (samples) bay's in `LibraryWindowTests` (spec 0038 step 015).

    // ============== step 016 — the targeted Select bind + staleness rules (pure) ==============

    /// A recording Select-session handle: `cancelAndClose` tags each invocation (the real one
    /// closes the Select-state window, whose dismissal hook fires the session's onCancelled).
    let private recordingSession (target : ElementId) : ResizeArray<string> * SelectSession =
        let cancels = ResizeArray<string>()
        cancels, { target = target; cancelAndClose = fun () -> cancels.Add "cancel-and-close" }

    [<Fact>]
    let ``BindValueIdTo commits to the element WITH THAT ID — regardless of the current selection`` () =
        // Selection sits on the TABLE; the targeted bind still lands on the detector element
        // (never "the current selection" — the modeless Select window may outlive it).
        let m = initMain ()
        Assert.Equal(TableSelected, m.selection)
        let bound = update (BindValueIdTo (elementId "det", "det-intensity")) m
        Assert.Equal(Some "det-intensity", (elem 1 bound).placement.valueId)
        Assert.Equal<string option>(None, (elem 0 bound).placement.valueId)
        Assert.Equal<string option>(None, bound.selectStatus)
        Assert.Equal<string option>(None, bound.pendingEntry)
        Assert.Equal<SelectSession option>(None, bound.activeSelect)

    [<Fact>]
    let ``BindValueIdTo on a vanished element is a NO-OP plus the status line — never a throw`` () =
        let m = initMain ()
        let vanished = update (BindValueIdTo (elementId "not-on-the-table", "det-intensity")) m
        // Nothing changed but the status line.
        Assert.Equal<TestElement list>(m.elements, vanished.elements)
        match vanished.selectStatus with
        | Some text -> Assert.Contains("no longer on the table", text)
        | None -> Assert.Fail "the vanished target must surface the status line"
        // The next SUCCESSFUL targeted bind clears it; so does a selection change.
        let recovered = update (BindValueIdTo (elementId "det", "det-intensity")) vanished
        Assert.Equal<string option>(None, recovered.selectStatus)
        Assert.Equal<string option>(None, (clickAt offPlate vanished).selectStatus)

    [<Fact>]
    let ``a changed table selection cancels and closes the active Select session — an unchanged one does not`` () =
        let cancels, session = recordingSession (elementId "det")
        let m = { withSample () with activeSelect = Some session }
        // Element 2 is selected (withSample); an off-plate click CHANGES the selection → the
        // session cancels and closes exactly once (the pending-bind-clears precedent).
        let changed = clickAt offPlate m
        Assert.Equal(NothingSelected, changed.selection)
        Assert.Equal<string list>([ "cancel-and-close" ], List.ofSeq cancels)
        Assert.Equal<SelectSession option>(None, changed.activeSelect)
        // An UNCHANGED selection (a second off-plate click) leaves a live session alone.
        let cancels2, session2 = recordingSession (elementId "det")
        let unchanged = clickAt offPlate { changed with activeSelect = Some session2 }
        Assert.Equal(NothingSelected, unchanged.selection)
        Assert.Empty(cancels2)
        Assert.Equal<SelectSession option>(Some session2, unchanged.activeSelect)

    [<Fact>]
    let ``an element ADD and the selected element's REMOVAL both cancel the active Select session`` () =
        // AddElement selects the new element — a table-selection change (the staleness rule).
        let cancels, session = recordingSession (elementId "det")
        let added = update (AddElement Lens) { initMain () with activeSelect = Some session }
        Assert.Equal<string list>([ "cancel-and-close" ], List.ofSeq cancels)
        Assert.Equal<SelectSession option>(None, added.activeSelect)
        // RemoveSelected makes the session's target disappear — cancel and close.
        let cancels2, session2 = recordingSession (elementId "det")
        let armed = { initMain () with selection = ElementSelected 1; activeSelect = Some session2 }
        let removed = update RemoveSelected armed
        Assert.Equal<string list>([ "cancel-and-close" ], List.ofSeq cancels2)
        Assert.Equal<SelectSession option>(None, removed.activeSelect)
        Assert.Equal(1, List.length removed.elements)
        // An INERT remove (the table is selected) touches nothing — the session survives.
        let cancels3, session3 = recordingSession (elementId "det")
        let inert = update RemoveSelected { initMain () with activeSelect = Some session3 }
        Assert.Empty(cancels3)
        Assert.Equal<SelectSession option>(Some session3, inert.activeSelect)
