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

open OpticalConstructor.Tests

open Xunit
open Berreman.Constants
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Library
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.TableView
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

module MainSceneMsgTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9
    let private elem (i : int) (m : TableAndElementRotationModel) : TestElement = List.item i m.elements
    let private clickAt (sp : ScreenPoint) (m : TableAndElementRotationModel) : TableAndElementRotationModel =
        m |> updateTableAndElementRotation (PointerDown sp) |> updateTableAndElementRotation (PointerUp sp)

    /// A screen point clearly off the plate (the existing off-plate click precedent).
    let private offPlate : ScreenPoint = { sx = TableScene.center.sx; sy = TableScene.center.sy + 200.0 }

    /// Every catalogue kind, resolvable from its short palette code.
    let private allKinds : CatalogueKind list =
        [ LightSource; LinearPolarizer; CircularPolarizer; Sample; Lens; FlatMirror; CurvedMirror; Detector ]

    let private kindOfCode (code : string) : CatalogueKind =
        match allKinds |> List.filter (fun k -> k.kindCode = code) with
        | [ k ] -> k
        | matches -> failwith $"catalogue code '{code}' matched {List.length matches} kinds"

    /// The seeded entry a pre-binding row's kind code expects (a `PresetIds` member cannot ride
    /// `InlineData`).
    let private expectedSeedOf (code : string) : ElementValueId =
        match code with
        | "S" -> PresetIds.source600
        | "D" -> PresetIds.detectorIntensity
        | "LP" -> PresetIds.polarizerLp
        | "CP" -> PresetIds.polarizerCpLeft
        | other -> failwith $"catalogue code '{other}' has no seeded default entry"

    /// The common Lego fixture: the Main scene with one added Sample, selected (index 2).
    let private withSample () : TableAndElementRotationModel = updateTableAndElementRotation (AddElement Sample) (SceneTestFixture.mainScene ())

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
        let m = updateTableAndElementRotation (AddElement kind) (SceneTestFixture.mainScene ())
        Assert.Equal(3, List.length m.elements)
        Assert.Equal(ElementSelected (SceneElementIndex 2), m.selection)
        Assert.Equal(kind, (elem 2 m).placement.catalogueKind)
        Assert.True(close ((elem 2 m).placement.placementPoint.y / 1.0<meter>) 0.0, "the new element sits on the beam")
        Assert.True(close (elem 2 m).zoom.value defaultElementZoom.value)

    [<Fact>]
    let ``successive adds spread the middle elements along the beam`` () =
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample) |> updateTableAndElementRotation (AddElement Lens)
        Assert.True((elem 2 m).elementX < (elem 3 m).elementX, "later adds land further along the beam")
        Assert.False(close ((elem 2 m).elementX) ((elem 3 m).elementX), "no two adds land on the same spot")

    [<Fact>]
    let ``adding an element leaves the existing elements and the table view untouched`` () =
        let before = SceneTestFixture.mainScene ()
        let after = updateTableAndElementRotation (AddElement Lens) before
        Assert.Equal<TestElement list>(before.elements, after.elements |> List.take (List.length before.elements))
        Assert.Equal(before.view, after.view)

    [<Fact>]
    let ``adding an element preserves the palette and the snap chain`` () =
        let before = SceneTestFixture.mainScene ()
        let after = updateTableAndElementRotation (AddElement FlatMirror) before
        Assert.Equal<CatalogueKind list>(before.palette, after.palette)
        Assert.Equal(before.snapChain, after.snapChain)

    [<Fact>]
    let ``RemoveSelected with nothing selected is inert`` () =
        let m = { SceneTestFixture.mainScene () with selection = NothingSelected }
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation RemoveSelectedElement m)

    [<Fact>]
    let ``removing a middle element preserves the survivors' order`` () =
        let m = SceneTestFixture.mainScene () |> updateTableAndElementRotation (AddElement Sample) |> updateTableAndElementRotation (AddElement Lens)
        let removed = updateTableAndElementRotation RemoveSelectedElement { m with selection = ElementSelected (SceneElementIndex 2) }   // drop the Sample
        Assert.Equal<CatalogueKind list>(
            [ LightSource; Detector; Lens ],
            removed.elements |> List.map (fun e -> e.placement.catalogueKind))
        Assert.Equal(NothingSelected, removed.selection)

    [<Fact>]
    let ``an add after a remove appends at the end and selects the new element`` () =
        let m = withSample () |> updateTableAndElementRotation RemoveSelectedElement |> updateTableAndElementRotation (AddElement CircularPolarizer)
        Assert.Equal(3, List.length m.elements)
        Assert.Equal(ElementSelected (SceneElementIndex 2), m.selection)
        Assert.Equal(CircularPolarizer, (elem 2 m).placement.catalogueKind)

    // ============ step 018 — pre-binding (spec 0038 Part G, operator Q7/Q28) ============

    [<Theory>]
    [<InlineData("S")>]
    [<InlineData("D")>]
    [<InlineData("LP")>]
    [<InlineData("CP")>]
    let ``AddElement lands PRE-BOUND to the kind's seeded default entry`` (code : string) =
        let kind = kindOfCode code
        let seedId = expectedSeedOf code
        Assert.Equal(Some seedId, kind.defaultSeedEntry)
        let m = updateTableAndElementRotation (AddElement kind) (SceneTestFixture.mainScene ())
        Assert.Equal(Some seedId, (elem 2 m).placement.valueId)
        // The pre-bound id RESOLVES through the live Library seam to an entry valid for the kind.
        match m.library.tryGetEntry seedId with
        | Ok (Some entry) -> Assert.Contains(kind, entry.forKinds)
        | other -> Assert.Fail($"the seed id %s{seedId.value} did not resolve: %A{other}")

    [<Theory>]
    [<InlineData("Sa")>]
    [<InlineData("L")>]
    [<InlineData("FM")>]
    [<InlineData("CM")>]
    let ``a new sample stays UNBOUND (the inverse hook) and the not-bindable kinds bind nothing`` (code : string) =
        let kind = kindOfCode code
        Assert.Equal(None, kind.defaultSeedEntry)
        let m = updateTableAndElementRotation (AddElement kind) (SceneTestFixture.mainScene ())
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``AddElementBoundTo carries an explicit pre-bind — the CPR palette button's path`` () =
        let m = updateTableAndElementRotation (AddElementBoundTo (CircularPolarizer, Some PresetIds.polarizerCpRight)) (SceneTestFixture.mainScene ())
        Assert.Equal(3, List.length m.elements)
        Assert.Equal(ElementSelected (SceneElementIndex 2), m.selection)
        Assert.Equal(CircularPolarizer, (elem 2 m).placement.catalogueKind)
        Assert.Equal(Some PresetIds.polarizerCpRight, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``the palette offers THREE polarizer buttons over the UNCHANGED CatalogueKinds`` () =
        let buttons = (SceneTestFixture.mainScene ()).paletteButtons
        // The polarizer trio: LP / CPL / CPR, in palette order — LP adds a LinearPolarizer, the
        // CPL / CPR pair adds CircularPolarizers; only the label and the pre-bound entry differ.
        let polarizers =
            buttons
            |> List.filter (fun b -> match b.kind with LinearPolarizer | CircularPolarizer -> true | _ -> false)
        Assert.Equal<(ConstructorUiKey * CatalogueKind * ElementValueId option) list>(
            [ ConstructorUiKey "LP", LinearPolarizer, Some PresetIds.polarizerLp
              ConstructorUiKey "CPL", CircularPolarizer, Some PresetIds.polarizerCpLeft
              ConstructorUiKey "CPR", CircularPolarizer, Some PresetIds.polarizerCpRight ],
            polarizers |> List.map (fun b -> b.code, b.kind, b.prebind))
        // The codes are unique (they are the `PaletteAdd_<code>` automation-id suffixes)…
        Assert.Equal(List.length buttons, buttons |> List.map (fun b -> b.code) |> List.distinct |> List.length)
        // …the sample button is the UNBOUND one (the inverse hook)…
        Assert.Equal<ElementValueId option>(None, (buttons |> List.find (fun b -> b.kind = Sample)).prebind)
        // …and every other button carries its kind's seeded default.
        for b in buttons |> List.filter (fun b -> b.kind <> CircularPolarizer) do
            Assert.Equal<ElementValueId option>(b.kind.defaultSeedEntry, b.prebind)

    // ============================== the Move bay ==============================

    [<Fact>]
    let ``SlideSelectedBy (BarMeters accumulates) across dispatches`` () =
        let m = withSample () |> updateTableAndElementRotation (SlideSelectedBy (BarMeters 0.1)) |> updateTableAndElementRotation (SlideSelectedBy (BarMeters 0.1))
        Assert.True(close ((elem 2 m).elementX) -0.1, $"expected -0.1, got {(elem 2 m).elementX}")

    [<Fact>]
    let ``SlideSelectedBy (BarMeters clamps) at both ends of the ray and stays clamped`` () =
        // Spec 0046: the main beam runs from the source (x = -1) to the detector (x = +1).
        let m = withSample ()
        let atPlus = m |> updateTableAndElementRotation (SlideSelectedBy (BarMeters 10.0)) |> updateTableAndElementRotation (SlideSelectedBy (BarMeters 10.0))
        Assert.True(close ((elem 2 atPlus).elementX) 1.0, "clamped at the detector")
        let atMinus = atPlus |> updateTableAndElementRotation (SlideSelectedBy (BarMeters -10.0))
        Assert.True(close ((elem 2 atMinus).elementX) -1.0, "one big negative slide runs to the source")

    [<Fact>]
    let ``sliding moves the element only along the beam`` () =
        let m = updateTableAndElementRotation (SlideSelectedTo (BarMeters 0.4)) (withSample ())
        Assert.True(close ((elem 2 m).elementX) 0.4)
        Assert.True(close ((elem 2 m).placement.placementPoint.y / 1.0<meter>) 0.0, "y stays on the beam")

    [<Fact>]
    let ``slide messages are inert with nothing selected`` () =
        let m = { withSample () with selection = NothingSelected }
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation (SlideSelectedBy (BarMeters 0.5)) m)
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation (SlideSelectedTo (BarMeters 0.5)) m)
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation ResetSelectedPosition m)

    [<Fact>]
    let ``ResetSelectedPosition is inert when the table is selected`` () =
        let m = { withSample () with selection = TableSelected }
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation ResetSelectedPosition m)

    [<Fact>]
    let ``a ray with no detector ends 1 m along`` () =
        Assert.True(close ((SceneTestFixture.mainScene ()).openRayEndX) 1.0, "spec 0046: an open ray ends at the default detector point")

    // ============================= the Render bay =============================

    [<Fact>]
    let ``a double renderer swap round-trips the renderer kind`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal(m.render.kind, (m |> updateTableAndElementRotation RenderSwap |> updateTableAndElementRotation RenderSwap).render.kind)

    [<Fact>]
    let ``RenderSetRailsIndex (RendererControls.RendererPresetIndex clamps) to the option ends`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal(4, (updateTableAndElementRotation (RenderSetRailsIndex (RendererControls.RendererPresetIndex -5)) m).render.rails.value)
        Assert.Equal(72, (updateTableAndElementRotation (RenderSetRailsIndex (RendererControls.RendererPresetIndex 99)) m).render.rails.value)

    [<Fact>]
    let ``RenderSetCircles (RendererControls.CapCircles clamps) at the minimum`` () =
        Assert.Equal(1, (updateTableAndElementRotation (RenderSetCircles (RendererControls.CapCircles 0)) (SceneTestFixture.mainScene ())).render.circles.value)

    [<Fact>]
    let ``RenderSetRadialsIndex (RendererControls.RendererPresetIndex clamps) to the option ends`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal(4, (updateTableAndElementRotation (RenderSetRadialsIndex (RendererControls.RendererPresetIndex -1)) m).render.radials.value)
        Assert.Equal(36, (updateTableAndElementRotation (RenderSetRadialsIndex (RendererControls.RendererPresetIndex 99)) m).render.radials.value)

    [<Fact>]
    let ``RenderSetLineOpacity (RendererControls.RendererOpacity clamps) into the unit interval`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal(0.0, (updateTableAndElementRotation (RenderSetLineOpacity (RendererControls.RendererOpacity -0.5)) m).render.lineOpacity.value)
        Assert.Equal(1.0, (updateTableAndElementRotation (RenderSetLineOpacity (RendererControls.RendererOpacity 1.5)) m).render.lineOpacity.value)

    [<Fact>]
    let ``in-range opacities are stored exactly`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal(0.5, (updateTableAndElementRotation (RenderSetRailOpacity (RendererControls.RendererOpacity 0.5)) m).render.railOpacity.value)
        Assert.Equal(0.25, (updateTableAndElementRotation (RenderSetFaceOpacity (RendererControls.RendererOpacity 0.25)) m).render.faceOpacity.value)

    [<Fact>]
    let ``render tuning leaves the scene itself untouched`` () =
        let before = withSample ()
        let after = before |> updateTableAndElementRotation RenderSwap |> updateTableAndElementRotation (RenderSetRailsIndex (RendererControls.RendererPresetIndex 1)) |> updateTableAndElementRotation (RenderSetFaceOpacity (RendererControls.RendererOpacity 0.3))
        Assert.Equal<TestElement list>(before.elements, after.elements)
        Assert.Equal(before.selection, after.selection)
        Assert.Equal(before.view, after.view)

    // ==================== exact angles (RotSetAxis) and locks ====================

    [<Fact>]
    let ``RotSetAxis normalizes the angle mod 360`` () =
        let m = updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 370.0)) (SceneTestFixture.mainScene ())   // table selected
        Assert.True(close m.view.r1.degrees 10.0, $"370° must store as 10°, got {m.view.r1.degrees}")

    [<Fact>]
    let ``RotSetAxis R3 respects the table lock`` () =
        let m = updateTableAndElementRotation (RotSetAxis (RotationControls.R3, BarDegrees 30.0)) (SceneTestFixture.mainScene ())   // table R3 unlocked by default
        Assert.True(close m.view.r3.degrees 30.0)
        let locked = updateTableAndElementRotation ToggleR3Lock m                                       // table selected → locks the table
        let after = updateTableAndElementRotation (RotSetAxis (RotationControls.R3, BarDegrees 60.0)) locked
        Assert.True(close after.view.r3.degrees 30.0, "a locked table R3 ignores the exact-angle set")

    [<Fact>]
    let ``RotSetAxis R3 on a default-locked element is inert until it is unlocked`` () =
        let m = withSample ()
        let still = updateTableAndElementRotation (RotSetAxis (RotationControls.R3, BarDegrees 20.0)) m
        Assert.True(close (elem 2 still).placement.r3.degrees 0.0, "elements start R3-locked")
        let unlocked = m |> updateTableAndElementRotation ToggleR3Lock |> updateTableAndElementRotation (RotSetAxis (RotationControls.R3, BarDegrees 20.0))
        Assert.True(close (elem 2 unlocked).placement.r3.degrees 20.0, "after the toggle the exact set applies")

    [<Fact>]
    let ``RotSetAxis touches only the named element axis`` () =
        let m = updateTableAndElementRotation (RotSetAxis (RotationControls.R2, BarDegrees 30.0)) (withSample ())
        Assert.True(close (elem 2 m).placement.r2.degrees 30.0)
        Assert.True(close (elem 2 m).placement.r1.degrees 0.0)
        Assert.True(close (elem 2 m).placement.r3.degrees 0.0)

    [<Fact>]
    let ``RotSetAxis with nothing selected is inert`` () =
        let m = { SceneTestFixture.mainScene () with selection = NothingSelected }
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation (RotSetAxis (RotationControls.R1, BarDegrees 45.0)) m)

    [<Theory>]
    [<InlineData(-15.0, 345.0)>]
    [<InlineData(360.0, 0.0)>]
    [<InlineData(720.0, 0.0)>]
    [<InlineData(10.0, 10.0)>]
    [<InlineData(-360.0, 0.0)>]
    let ``(BarDegrees wraps).normalized.value every angle into 0 to 360`` (input : float) (expected : float) =
        Assert.True(close ((BarDegrees input).normalized.value) expected, $"normalizeDegrees {input} = {(BarDegrees input).normalized.value}, expected {expected}")

    // ==================== the confirm-gated rotation resets ====================

    [<Fact>]
    let ``RotRequestReset arms the confirm and RotCancel disarms it leaving the angles intact`` () =
        let rotated = updateTableAndElementRotation (RotateR1By (BarDegrees 15.0)) (SceneTestFixture.mainScene ())   // table selected
        let armed = updateTableAndElementRotation RotRequestReset rotated
        Assert.Equal(RotationControls.ConfirmReset, armed.rotationConfirm)
        let cancelled = updateTableAndElementRotation RotCancel armed
        Assert.Equal(RotationControls.NoConfirm, cancelled.rotationConfirm)
        Assert.True(close cancelled.view.r1.degrees 15.0, "cancel must not reset anything")

    [<Fact>]
    let ``RotRequestResetAll arms the reset-all confirm`` () =
        Assert.Equal(RotationControls.ConfirmResetAll, (updateTableAndElementRotation RotRequestResetAll (SceneTestFixture.mainScene ())).rotationConfirm)

    [<Fact>]
    let ``RotConfirm with no pending confirm is inert`` () =
        let m = updateTableAndElementRotation (RotateR1By (BarDegrees 15.0)) (SceneTestFixture.mainScene ())
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation RotConfirm m)

    [<Fact>]
    let ``a confirmed element reset leaves the table and the other elements alone`` () =
        let m =
            SceneTestFixture.mainScene ()
            |> updateTableAndElementRotation (RotateR1By (BarDegrees 30.0))          // table selected → table R1 = 30°
            |> updateTableAndElementRotation (AddElement Sample)        // element 2 selected
            |> updateTableAndElementRotation (RotateR1By (BarDegrees 20.0))          // element R1 = 20°
            |> updateTableAndElementRotation RotRequestReset
            |> updateTableAndElementRotation RotConfirm
        Assert.True(close (elem 2 m).placement.r1.degrees 0.0, "the selected element resets")
        Assert.True(close m.view.r1.degrees 30.0, "the table keeps its rotation")
        Assert.True(close (elem 0 m).placement.r1.degrees 0.0)

    // ============================ the R3-lock toggle ============================

    [<Fact>]
    let ``ToggleR3Lock flips the selected element's lock and a second toggle restores it`` () =
        let m = withSample ()
        Assert.True((elem 2 m).placement.r3Locked = RotationLocked, "elements start R3-locked")
        let once = updateTableAndElementRotation ToggleR3Lock m
        Assert.False((elem 2 once).placement.r3Locked = RotationLocked)
        Assert.True((elem 2 (updateTableAndElementRotation ToggleR3Lock once)).placement.r3Locked = RotationLocked)

    [<Fact>]
    let ``ToggleR3Lock with nothing selected is inert`` () =
        let m = { SceneTestFixture.mainScene () with selection = NothingSelected }
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation ToggleR3Lock m)

    // ============================ wheel zoom clamps ============================

    [<Fact>]
    let ``the element draw zoom clamps at its maximum`` () =
        let m = updateTableAndElementRotation (Wheel (Set.ofList [ ModCtrl; ModAlt ], WheelNotches 100)) (withSample ())
        Assert.True(close (elem 2 m).zoom.value 50.0, $"expected the 50x cap, got {(elem 2 m).zoom.value}")

    [<Fact>]
    let ``the element draw zoom clamps at its minimum`` () =
        let m = updateTableAndElementRotation (Wheel (Set.ofList [ ModCtrl; ModAlt ], WheelNotches -100)) (withSample ())
        Assert.True(close (elem 2 m).zoom.value 1.0, $"expected the 1x floor, got {(elem 2 m).zoom.value}")

    [<Fact>]
    let ``the table zoom clamps at its maximum`` () =
        let m = updateTableAndElementRotation (Wheel (Set.empty, WheelNotches 100)) (SceneTestFixture.mainScene ())
        Assert.True(close m.view.zoom.value 5.0, $"expected the 5x cap, got {m.view.zoom.value}")

    [<Fact>]
    let ``the table zoom clamps at its minimum`` () =
        let m = updateTableAndElementRotation (Wheel (Set.empty, WheelNotches -100)) (SceneTestFixture.mainScene ())
        Assert.True(close m.view.zoom.value 0.2, $"expected the 0.2x floor, got {m.view.zoom.value}")

    [<Fact>]
    let ``zoom-all magnifies every element even while the table is selected`` () =
        let m = updateTableAndElementRotation (Wheel (Set.ofList [ ModCtrl; ModShift; ModAlt ], WheelNotches 2)) (SceneTestFixture.mainScene ())   // table selected
        Assert.Equal(TableSelected, m.selection)
        for e in m.elements do
            Assert.True(close e.zoom.value (defaultElementZoom.value * 1.1 * 1.1), $"every element zooms, got {e.zoom.value}")

    [<Fact>]
    let ``zooming the table leaves the element draw zoom alone`` () =
        let m = updateTableAndElementRotation (Wheel (Set.empty, WheelNotches 3)) (SceneTestFixture.mainScene ())
        Assert.True(m.view.zoom.value > 1.0, "the table zoomed")
        for e in m.elements do
            Assert.True(close e.zoom.value defaultElementZoom.value, "no element draw zoom changed")

    // ==================== the confirm-gated Library bind ====================

    [<Fact>]
    let ``RequestBindValueId (ElementValueId sets) the pending entry for a selected element without binding`` () =
        let m = updateTableAndElementRotation (RequestBindValueId (ElementValueId "lens-a")) (withSample ())
        Assert.Equal(Some (ElementValueId "lens-a"), m.pendingEntry)
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``RequestBindValueId (ElementValueId is) inert while the table is selected`` () =
        let m = SceneTestFixture.mainScene ()   // table selected
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation (RequestBindValueId (ElementValueId "lens-a")) m)

    [<Fact>]
    let ``ConfirmBindValueId commits the pending entry and clears it`` () =
        let m = withSample () |> updateTableAndElementRotation (RequestBindValueId (ElementValueId "lens-a")) |> updateTableAndElementRotation ConfirmBindValueId
        Assert.Equal(Some (ElementValueId "lens-a"), (elem 2 m).placement.valueId)
        Assert.Equal(None, m.pendingEntry)

    [<Fact>]
    let ``ConfirmBindValueId without a pending entry binds nothing`` () =
        let m = updateTableAndElementRotation ConfirmBindValueId (withSample ())
        Assert.Equal(None, (elem 2 m).placement.valueId)
        Assert.Equal(None, m.pendingEntry)

    [<Fact>]
    let ``CancelBindValueId clears the pending entry without binding`` () =
        let m = withSample () |> updateTableAndElementRotation (RequestBindValueId (ElementValueId "lens-a")) |> updateTableAndElementRotation CancelBindValueId
        Assert.Equal(None, m.pendingEntry)
        Assert.Equal(None, (elem 2 m).placement.valueId)

    [<Fact>]
    let ``a selection change clears the pending entry`` () =
        let m = withSample () |> updateTableAndElementRotation (RequestBindValueId (ElementValueId "lens-a")) |> clickAt offPlate
        Assert.Equal(NothingSelected, m.selection)
        Assert.Equal(None, m.pendingEntry)

    [<Fact>]
    let ``BindValueId (ElementValueId binds) a selected element directly and is inert for the table`` () =
        let bound = updateTableAndElementRotation (BindValueId (ElementValueId "detector-b")) (withSample ())
        Assert.Equal(Some (ElementValueId "detector-b"), (elem 2 bound).placement.valueId)
        let tableSel = SceneTestFixture.mainScene ()
        Assert.Equal<TableAndElementRotationModel>(tableSel, updateTableAndElementRotation (BindValueId (ElementValueId "detector-b")) tableSel)

    // ========================= pointer drag threshold =========================

    [<Fact>]
    let ``PointerMove without a press is inert`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation (PointerMove { sx = 10.0; sy = 10.0 }) m)

    [<Fact>]
    let ``a sub-threshold jiggle still counts as a click-select`` () =
        // Element 0 of the static scene sits at x = -0.5 m → screen centre - 100 px.
        let start : ScreenPoint = { sx = TableScene.center.sx - 100.0; sy = TableScene.center.sy }
        let nearby : ScreenPoint = { sx = start.sx + 1.0; sy = start.sy }
        let m = SceneTestFixture.staticScene () |> updateTableAndElementRotation (PointerDown start) |> updateTableAndElementRotation (PointerMove nearby) |> updateTableAndElementRotation (PointerUp nearby)
        Assert.Equal(ElementSelected (SceneElementIndex 0), m.selection)

    [<Fact>]
    let ``a pan-drag preserves the selection`` () =
        let selected = clickAt { sx = TableScene.center.sx - 100.0; sy = TableScene.center.sy } (SceneTestFixture.staticScene ())
        Assert.Equal(ElementSelected (SceneElementIndex 0), selected.selection)
        let from : ScreenPoint = { sx = TableScene.center.sx + 50.0; sy = TableScene.center.sy + 50.0 }
        let dragged =
            selected
            |> updateTableAndElementRotation (PointerDown from)
            |> updateTableAndElementRotation (PointerMove { sx = from.sx + 120.0; sy = from.sy })
            |> updateTableAndElementRotation (PointerUp { sx = from.sx + 120.0; sy = from.sy })
        Assert.Equal(ElementSelected (SceneElementIndex 0), dragged.selection)
        Assert.True(close dragged.view.panX.value 120.0, "the drag panned the view")

    // ===================== the Experiments-bay host guards =====================

    [<Fact>]
    let ``ExpChooseElement with an unknown element id is inert`` () =
        let m = withSample ()
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation (ExpChooseElement (ElementId "not-an-element-id")) m)

    [<Fact>]
    let ``ExpEdit with an id not in the collection is inert`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation (ExpEdit (Experiments.ExperimentId 999)) m)

    [<Fact>]
    let ``ExpRemove with an id not in the collection is inert`` () =
        let m = SceneTestFixture.mainScene ()
        Assert.Equal<TableAndElementRotationModel>(m, updateTableAndElementRotation (ExpRemove (Experiments.ExperimentId 999)) m)

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
        let m = SceneTestFixture.mainScene ()
        Assert.Equal(TableSelected, m.selection)
        let bound = updateTableAndElementRotation (BindValueIdTo (ElementId "det", PresetIds.detectorIntensity)) m
        Assert.Equal(Some PresetIds.detectorIntensity, (elem 1 bound).placement.valueId)
        Assert.Equal<ElementValueId option>(None, (elem 0 bound).placement.valueId)
        Assert.Equal<ControlCaption option>(None, bound.selectStatus)
        Assert.Equal<ElementValueId option>(None, bound.pendingEntry)
        Assert.Equal<SelectSession option>(None, bound.activeSelect)

    [<Fact>]
    let ``BindValueIdTo on a vanished element is a NO-OP plus the status line — never a throw`` () =
        let m = SceneTestFixture.mainScene ()
        let vanished = updateTableAndElementRotation (BindValueIdTo (ElementId "not-on-the-table", PresetIds.detectorIntensity)) m
        // Nothing changed but the status line.
        Assert.Equal<TestElement list>(m.elements, vanished.elements)
        match vanished.selectStatus with
        | Some text -> Assert.Contains("no longer on the table", text.value)
        | None -> Assert.Fail "the vanished target must surface the status line"
        // The next SUCCESSFUL targeted bind clears it; so does a selection change.
        let recovered = updateTableAndElementRotation (BindValueIdTo (ElementId "det", PresetIds.detectorIntensity)) vanished
        Assert.Equal<ControlCaption option>(None, recovered.selectStatus)
        Assert.Equal<ControlCaption option>(None, (clickAt offPlate vanished).selectStatus)

    [<Fact>]
    let ``a changed table selection cancels and closes the active Select session — an unchanged one does not`` () =
        let cancels, session = recordingSession (ElementId "det")
        let m = { withSample () with activeSelect = Some session }
        // Element 2 is selected (withSample); an off-plate click CHANGES the selection → the
        // session cancels and closes exactly once (the pending-bind-clears precedent).
        let changed = clickAt offPlate m
        Assert.Equal(NothingSelected, changed.selection)
        Assert.Equal<string list>([ "cancel-and-close" ], List.ofSeq cancels)
        Assert.Equal<SelectSession option>(None, changed.activeSelect)
        // An UNCHANGED selection (a second off-plate click) leaves a live session alone.
        let cancels2, session2 = recordingSession (ElementId "det")
        let unchanged = clickAt offPlate { changed with activeSelect = Some session2 }
        Assert.Equal(NothingSelected, unchanged.selection)
        Assert.Empty(cancels2)
        Assert.Equal<SelectSession option>(Some session2, unchanged.activeSelect)

    [<Fact>]
    let ``an element ADD and the selected element's REMOVAL both cancel the active Select session`` () =
        // AddElement selects the new element — a table-selection change (the staleness rule).
        let cancels, session = recordingSession (ElementId "det")
        let added = updateTableAndElementRotation (AddElement Lens) { SceneTestFixture.mainScene () with activeSelect = Some session }
        Assert.Equal<string list>([ "cancel-and-close" ], List.ofSeq cancels)
        Assert.Equal<SelectSession option>(None, added.activeSelect)
        // RemoveSelected makes the session's target disappear — cancel and close.
        let cancels2, session2 = recordingSession (ElementId "det")
        let armed = { SceneTestFixture.mainScene () with selection = ElementSelected (SceneElementIndex 1); activeSelect = Some session2 }
        let removed = updateTableAndElementRotation RemoveSelectedElement armed
        Assert.Equal<string list>([ "cancel-and-close" ], List.ofSeq cancels2)
        Assert.Equal<SelectSession option>(None, removed.activeSelect)
        Assert.Equal(1, List.length removed.elements)
        // An INERT remove (the table is selected) touches nothing — the session survives.
        let cancels3, session3 = recordingSession (ElementId "det")
        let inert = updateTableAndElementRotation RemoveSelectedElement { SceneTestFixture.mainScene () with activeSelect = Some session3 }
        Assert.Empty(cancels3)
        Assert.Equal<SelectSession option>(Some session3, inert.activeSelect)

    // ========== step 017 — the Choose…/quick-pick Selector flow (pure) ==========

    [<Fact>]
    let ``the strip confirm and the direct bind CONVERGE on the targeted bind — one commit message`` () =
        // The quick-pick confirm (`ConfirmBindValueId`) and the legacy direct bind
        // (`BindValueId`) DELEGATE to the exact `BindValueIdTo` message a Select window's
        // onSelected dispatches — the resulting models are structurally identical, so binding
        // through the strip and through the Select window provably land the same commit arm.
        let pending = withSample () |> updateTableAndElementRotation (RequestBindValueId (ElementValueId "lens-a"))
        let viaTargeted = updateTableAndElementRotation (BindValueIdTo ((elem 2 pending).id, ElementValueId "lens-a")) pending
        Assert.Equal(Some (ElementValueId "lens-a"), (elem 2 viaTargeted).placement.valueId)
        Assert.Equal<TableAndElementRotationModel>(viaTargeted, updateTableAndElementRotation ConfirmBindValueId pending)
        Assert.Equal<TableAndElementRotationModel>(viaTargeted, updateTableAndElementRotation (BindValueId (ElementValueId "lens-a")) pending)

    [<Fact>]
    let ``the Select bay offers the quick-pick strip only BELOW the step-005 threshold`` () =
        // The seeded detector kind has 2 entries < the default threshold 5 → the inline strip.
        let detSelected = { SceneTestFixture.mainScene () with selection = ElementSelected (SceneElementIndex 1) }
        Assert.Equal<SelectorOffer>(QuickPickAndChoose, detSelected.selectorOffer)
        // The seeded Sample kind has 11 entries ≥ 5 → Choose… alone.
        Assert.Equal<SelectorOffer>(ChooseAlone, (withSample ()).selectorOffer)
        // AT the threshold is already Choose… alone (the gate is strictly-below).
        let atThreshold = { detSelected with quickPickThreshold = WorkbenchSettings.QuickPickThreshold 2 }
        Assert.Equal<SelectorOffer>(ChooseAlone, atThreshold.selectorOffer)
        // No element selected → nothing bindable, no offer.
        Assert.Equal<SelectorOffer>(NoSelectorOffer, (SceneTestFixture.mainScene ()).selectorOffer)
        Assert.Equal<SelectorOffer>(NoSelectorOffer, ({ SceneTestFixture.mainScene () with selection = NothingSelected }).selectorOffer)

    [<Fact>]
    let ``SelectSessionStarted stores the handle and SelectSessionEnded clears only that SAME session`` () =
        let cancels, session = recordingSession (ElementId "det")
        let started = updateTableAndElementRotation (SelectSessionStarted session) (SceneTestFixture.mainScene ())
        Assert.Equal<SelectSession option>(Some session, started.activeSelect)
        // A STALE session's end (superseded by a re-target) never clears the successor.
        let _, stale = recordingSession (ElementId "det")
        Assert.Equal<SelectSession option>(Some session, (updateTableAndElementRotation (SelectSessionEnded stale) started).activeSelect)
        // The live session's own end clears the handle — WITHOUT re-closing the window (the
        // window ended the session from its side; only the staleness rules close it).
        let ended = updateTableAndElementRotation (SelectSessionEnded session) started
        Assert.Equal<SelectSession option>(None, ended.activeSelect)
        Assert.Empty(cancels)
        // Ending an already-cleared session is a no-op.
        Assert.Equal<TableAndElementRotationModel>(ended, updateTableAndElementRotation (SelectSessionEnded session) ended)

    [<Fact>]
    let ``a committed targeted bind cancels and CLOSES a still-open Choose session`` () =
        // The quick-pick race: the strip commits while the Choose… Select window is still
        // open — the bind ends the session AND closes its window (on the window path the
        // window already closed itself; the extra Close is the safe no-op).
        let cancels, session = recordingSession (ElementId "det")
        let bound = updateTableAndElementRotation (BindValueIdTo (ElementId "det", PresetIds.detectorIntensity)) { SceneTestFixture.mainScene () with activeSelect = Some session }
        Assert.Equal<string list>([ "cancel-and-close" ], List.ofSeq cancels)
        Assert.Equal<SelectSession option>(None, bound.activeSelect)
        Assert.Equal(Some PresetIds.detectorIntensity, (elem 1 bound).placement.valueId)
        // The vanished-target no-op still ends the session (the choice was made either way).
        let cancels2, session2 = recordingSession (ElementId "gone")
        let vanished = updateTableAndElementRotation (BindValueIdTo (ElementId "gone", PresetIds.detectorIntensity)) { SceneTestFixture.mainScene () with activeSelect = Some session2 }
        Assert.Equal<string list>([ "cancel-and-close" ], List.ofSeq cancels2)
        Assert.Equal<SelectSession option>(None, vanished.activeSelect)
