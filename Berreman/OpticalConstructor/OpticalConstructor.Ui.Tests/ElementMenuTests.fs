namespace OpticalConstructor.Ui.Tests

open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish
open Xunit
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.TableView
open OpticalConstructor.Controls
open OpticalConstructor.Ui.TableAndElementRotationView

/// Spec 0046 — the Main workbench's reflected rays: the right-click element menu (switch an element's
/// reflected / transmitted rays, move it onto the reflected ray of a partially-reflecting element),
/// the beam tree it drives (elements snap onto reflected rays; removed / switched-off hosts fall back
/// to the main beam), the experiment captures that follow the detectors, the three-segment rendering
/// of a tilted element, and the page-level scroll viewer. Behaviour is proven through `update`; the
/// headless tests prove the right click and the menu rows reach the same messages.
module ElementMenuTests =

    let private tol = 1.0e-9
    let private elem (i : int) (m : Model) : TestElement = List.item i m.elements

    /// Bind element `i` through the ONE targeted bind arm.
    let private bind (i : int) (entryId : string) (m : Model) : Model = update (BindValueIdTo ((elem i m).id, entryId)) m

    let private plateId : string = (Library.SampleItem Library.SeedSamples.glassPlate1mm).entryId

    /// The canvas point an element is drawn at (its snapped centre through the live view).
    let private canvasPointOf (i : int) (m : Model) : ScreenPoint =
        TableView.project pixelsPerMeter center m.view ((snappedCentres m).[i])

    /// initMain = [ source (0); detector (1) ] + a sample (2) bound to a 1 mm glass PLATE (emits both),
    /// tilted to R2 = 30° so its reflected ray leaves the main beam.
    let private plateScene () : Model =
        initMain ()
        |> update (AddElement Sample)
        |> bind 2 plateId
        |> update (RotSetAxis (RotationControls.R2, 30.0))

    /// `plateScene` + a second detector (3) moved onto the sample's reflected ray.
    let private plateWithReflectedDetector () : Model =
        let m = plateScene () |> update (AddElement Detector)
        m |> update (PlaceOnBeam ((elem 3 m).id, BeamLayout.OnReflectedBeamOf (elem 2 m).id))

    // ------------------------------ the element menu (update) ------------------------------

    [<Fact>]
    let ``a right click on an element selects it and opens its menu; off every element or a left click closes it`` () =
        let m = plateScene ()
        let opened = m |> update (OpenElementMenuAt (canvasPointOf 1 m))
        Assert.Equal(ElementSelected 1, opened.selection)
        match opened.elementMenu with
        | Some menu -> Assert.Equal((elem 1 m).id, menu.target)
        | None -> Assert.Fail("the right click must open the element menu")
        let offElements = opened |> update (OpenElementMenuAt { sx = 5.0; sy = 5.0 })
        Assert.Equal(None, offElements.elementMenu)
        let p = canvasPointOf 0 opened
        let leftClicked = opened |> update (PointerDown p) |> update (PointerUp p)
        Assert.Equal(None, leftClicked.elementMenu)
        Assert.Equal(None, (opened |> update CloseElementMenu).elementMenu)

    [<Fact>]
    let ``the menu switches a sample's reflected ray off and on; a polarizer's and a full mirror's rays stay fixed`` () =
        let m = initMain () |> update (AddElement Sample) |> update (AddElement FlatMirror) |> update (AddElement LinearPolarizer)
        let sample = (elem 2 m).id
        let rOff = m |> update (ToggleElementRay (sample, BeamTree.Reflected))
        Assert.Equal(EmitTransmittedOnly, (elem 2 rOff).placement.emission)
        Assert.Equal(EmitBoth, (elem 2 (rOff |> update (ToggleElementRay (sample, BeamTree.Reflected)))).placement.emission)
        let mirror = (elem 3 m).id
        Assert.Equal((elem 3 m).placement, (elem 3 (m |> update (ToggleElementRay (mirror, BeamTree.Transmitted)))).placement)
        // Spec 0046: a polarizer transmits only — locked.
        let pol = (elem 4 m).id
        Assert.Equal(EmitTransmittedOnly, (elem 4 m).placement.emission)
        Assert.Equal((elem 4 m).placement, (elem 4 (m |> update (ToggleElementRay (pol, BeamTree.Transmitted)))).placement)

    [<Fact>]
    let ``an element moved onto a sample's reflected ray snaps onto it — off the straight beam`` () =
        let m = plateWithReflectedDetector ()
        Assert.Equal(BeamLayout.OnReflectedBeamOf (elem 2 m).id, (elem 3 m).beam)
        let centres = snappedCentres m
        Assert.True(abs centres.[3].y > 1.0e-3, $"the reflected detector leaves the straight beam (y = {centres.[3].y})")
        Assert.True(abs centres.[1].y < 2.0e-2, "the main detector stays on the (barely shifted) transmitted beam")
        // It lands a gap ALONG the reflected ray, so it never collapses onto its host.
        Assert.True(elementX (elem 3 m) > elementX (elem 2 m))

    [<Fact>]
    let ``removing the host moves the elements on its reflected ray back to the main beam`` () =
        let m = plateWithReflectedDetector ()
        let removed = { m with selection = ElementSelected 2 } |> update RemoveSelected
        Assert.Equal<BeamLayout.BeamHost list>(List.replicate 3 BeamLayout.OnMainBeam, removed.elements |> List.map (fun e -> e.beam))

    [<Fact>]
    let ``switching the host's reflected ray off parks its elements on the main beam; switching it on restores them`` () =
        let m = plateWithReflectedDetector ()
        let sampleId = (elem 2 m).id
        let off = m |> update (ToggleElementRay (sampleId, BeamTree.Reflected))
        Assert.Equal(BeamLayout.OnMainBeam, (List.item 3 (sceneLayout off).elements).beam)
        let on = off |> update (ToggleElementRay (sampleId, BeamTree.Reflected))
        Assert.Equal(BeamLayout.OnReflectedBeamOf sampleId, (List.item 3 (sceneLayout on).elements).beam)

    [<Fact>]
    let ``the source never moves and a loop-forming move is refused`` () =
        let m = plateWithReflectedDetector () |> update (AddElement LinearPolarizer)
        let polarizerId = (elem 4 m).id
        let sampleId = (elem 2 m).id
        let polarizerOnSample = m |> update (PlaceOnBeam (polarizerId, BeamLayout.OnReflectedBeamOf sampleId))
        // The sample may not move onto the ray of the polarizer sitting on its own ray (it would loop).
        Assert.Equal<Model>(polarizerOnSample, polarizerOnSample |> update (PlaceOnBeam (sampleId, BeamLayout.OnReflectedBeamOf polarizerId)))
        Assert.Equal<Model>(m, m |> update (PlaceOnBeam ((elem 0 m).id, BeamLayout.OnReflectedBeamOf sampleId)))

    [<Fact>]
    let ``the menu rows offer the rays and every beam the element may sit on`` () =
        let m = plateWithReflectedDetector ()
        let opened = m |> update (OpenElementMenuAt (canvasPointOf 3 m))
        match opened.elementMenu with
        | Some menu ->
            // A detector has no ray switches; it can sit on the main beam or the sample's reflected ray.
            let rows = elementMenuRows opened ignore menu
            Assert.True(List.length rows >= 6, $"rows = {List.length rows}")
        | None -> Assert.Fail("the menu must open on the reflected detector")

    // ------------------------------ adding a detector (spec 0046) ------------------------------

    [<Fact>]
    let ``an added detector goes to the END of the first free ray — the plate's reflected ray, not the main beam`` () =
        let m = plateScene () |> update (AddElement Detector)
        let added = elem 3 m
        Assert.Equal(BeamLayout.OnReflectedBeamOf (elem 2 m).id, added.beam)
        // Spec 0046: at the end of the open ray — 1 m along (the reflected ray carries nothing yet).
        Assert.True(abs (elementX added - openRayEndX m) < tol, $"it lands where the open ray ends (x = {elementX added})")
        // Both detectors are lit, so the plate's experiment captures T, R and Both.
        let sampleId = (elem 2 m).id
        Assert.Equal<Experiments.MeasurementMode list>(
            [ Experiments.CaptureTransmitted; Experiments.CaptureReflected; Experiments.CaptureBoth ], availableCaptures m sampleId)

    [<Fact>]
    let ``with no free ray an added detector goes onto the main beam and terminates it there`` () =
        // The plate's reflected ray switched off: the main beam (already ending on a detector) is the only ray.
        let rOff = plateScene () |> fun m -> m |> update (ToggleElementRay ((elem 2 m).id, BeamTree.Reflected))
        let m = rOff |> update (AddElement Detector)
        Assert.Equal(BeamLayout.OnMainBeam, (elem 3 m).beam)
        let laid = sceneLayout m
        // The new detector sits before the seeded one, so it terminates the main beam: the seeded detector
        // receives no light, and the beam is drawn no further than the new detector's face.
        Assert.Equal(BeamLayout.Lit, (List.item 3 laid.elements).light)
        Assert.Equal(BeamLayout.Unlit, (List.item 1 laid.elements).light)
        let newFace = (snappedCentres m).[3].x - (elem 3 m).zoom * float (elem 3 m).placement.box.b / 2.0
        for s in laid.segments do
            Assert.True(max s.start.x s.finish.x <= newFace + tol, $"a ray reaches x = {max s.start.x s.finish.x}, past the new detector")

    // ------------------------------ the rotation pivot (spec 0046) ------------------------------

    [<Fact>]
    let ``the pivot option moves the incident ray's entry to the middle of the incident face`` () =
        let m = plateScene ()
        Assert.Equal(BeamLayout.PivotAtCentre, m.rotationPivot)
        let faced = m |> update (SetRotationPivot BeamLayout.PivotAtIncidentFace)
        Assert.Equal(BeamLayout.PivotAtIncidentFace, faced.rotationPivot)
        // The sample's incident ray (the free-space run arriving at it) ends exactly at its beam point —
        // the middle of its incident face — and the sample's drawn centre moves behind it.
        let beamPoint = Vector3.create (elementX (elem 2 m)) 0.0 0.0
        let arriving = (sceneLayout faced).segments |> List.filter (fun s -> abs (s.finish.x - beamPoint.x) < tol && abs s.finish.y < tol)
        Assert.NotEmpty(arriving)
        Assert.True((snappedCentres faced).[2].x > beamPoint.x, "about its incident face, the sample's centre sits behind the beam point")
        Assert.Equal(BeamLayout.PivotAtCentre, (faced |> update (SetRotationPivot BeamLayout.PivotAtCentre)).rotationPivot)

    // ------------------------------ captures follow the detectors ------------------------------

    [<Fact>]
    let ``with only a transmitted detector the experiments offer only T and draw only T`` () =
        let m = plateScene () |> (fun m -> m |> update (ExpChooseElement (elem 2 m).id.value)) |> update (ExpChooseVariable ExperimentControls.VaryR2)
        Assert.Equal<Experiments.MeasurementMode list>([ Experiments.CaptureTransmitted ], availableCaptures m (elem 2 m).id)
        let st = experimentState m
        Assert.Equal<ExperimentControls.MeasurementChoice list>([ ExperimentControls.CaptureT ], st.measurementOffers |> List.map (fun o -> o.choice))
        // Spec 0046: the offer says what its detector reads.
        Assert.Equal("Transmitted (T) — intensity", (List.head st.measurementOffers).label)
        // Even an explicit Both request runs (and records) T alone.
        let both = m |> update (ExpChooseMeasurement ExperimentControls.CaptureBoth)
        Assert.Equal<string list>([ "Intensity" ], (experimentResult both).series |> List.map (fun s -> s.name))
        let committed = both |> update ExpCommit
        Assert.Equal(Experiments.CaptureTransmitted, (List.head committed.experimentCollection.experiments).measurement)

    [<Fact>]
    let ``a detector on the reflected ray adds R, and both detectors offer T, R and Both`` () =
        let rOnly =
            let m = plateWithReflectedDetector ()
            { m with elements = m.elements |> List.filter (fun e -> e.id <> (elem 1 m).id) }
        let sample (m : Model) : Library.ElementId = (m.elements |> List.find (fun e -> e.placement.catalogueKind = Sample)).id
        Assert.Equal<Experiments.MeasurementMode list>([ Experiments.CaptureReflected ], availableCaptures rOnly (sample rOnly))
        let both = plateWithReflectedDetector ()
        Assert.Equal<Experiments.MeasurementMode list>(
            [ Experiments.CaptureTransmitted; Experiments.CaptureReflected; Experiments.CaptureBoth ],
            availableCaptures both (sample both))

    [<Fact>]
    let ``with no detector nothing can be captured and Add is disabled`` () =
        let m = plateScene ()
        let noDetector = { m with elements = m.elements |> List.filter (fun e -> e.placement.catalogueKind <> Detector) }
        let chosen = noDetector |> update (ExpChooseElement (elem 1 noDetector).id.value)
        let st = experimentState chosen
        Assert.Empty(st.measurementOffers)
        Assert.False(st.canAdd, "nothing captures the element's light, so the experiment cannot be added")
        Assert.Empty((chosen |> update ExpCommit).experimentCollection.experiments)

    // ------------------------------ rendering ------------------------------

    [<Fact>]
    let ``a tilted sample is drawn in three segments, and R2 / R3 bend only the middle one`` () =
        let transmitOnly (m : Model) : Model = m |> update (ToggleElementRay ((elem 2 m).id, BeamTree.Reflected))
        let segmentsAt (r2 : float) (r3 : float) : BeamLayout.RaySegment list =
            plateScene ()
            |> transmitOnly
            |> update ToggleR3Lock
            |> update (RotSetAxis (RotationControls.R2, r2))
            |> update (RotSetAxis (RotationControls.R3, r3))
            |> mainSnap
            |> snd
        let dirOf (s : BeamLayout.RaySegment) : Vector3 =
            (Vector3.create (s.finish.x - s.start.x) (s.finish.y - s.start.y) (s.finish.z - s.start.z)).normalized
        for (r2, r3) in [ 20.0, 0.0; 50.0, 0.0; 20.0, 15.0 ] do
            match segmentsAt r2 r3 with
            | [ incoming; inside; outgoing ] ->
                Assert.Equal(BeamLayout.InsideElement, inside.kind)
                Assert.True(abs ((dirOf incoming).dot (dirOf outgoing) - 1.0) < tol, $"segments 1 and 3 parallel at R2 = %g{r2}, R3 = %g{r3}")
                Assert.True(abs ((dirOf outgoing).dot centralRayDirection - 1.0) < tol, "the exit beam never rotates with the sample")
                Assert.True((dirOf inside).dot centralRayDirection < 1.0 - 1.0e-6, "the inside segment is bent")
            | other -> Assert.Fail($"expected 3 segments, got %d{List.length other}")

    // ------------------------------ headless: right click → menu ------------------------------

    let private matchesId (id : string) (c : Control) : bool =
        c.Name = id || AutomationProperties.GetAutomationId c = id

    let private tryFind (window : Window) (id : string) : Control option =
        window.GetVisualDescendants() |> Seq.tryPick (function :? Control as c when matchesId id c && c.IsEffectivelyVisible -> Some c | _ -> None)

    /// Present AND effectively visible.
    let private isShown (window : Window) (id : string) : bool =
        match tryFind window id with
        | Some _ -> true
        | None -> false

    let private textUnder (c : Control) : string =
        c.GetVisualDescendants() |> Seq.tryPick (function :? TextBlock as t -> Some t.Text | _ -> None) |> Option.defaultValue ""

    let private mount (model0 : Model) : HostWindow =
        let window = HostWindow(Width = 980.0, Height = 1050.0)
        Program.mkSimple (fun () -> model0) update mainView |> Program.withHost window |> Program.run
        window.Show()
        Dispatcher.UIThread.RunJobs()
        window

    let private click (window : Window) (button : MouseButton) (p : Point) : unit =
        window.MouseDown(p, button, RawInputModifiers.None)
        Dispatcher.UIThread.RunJobs()
        window.MouseUp(p, button, RawInputModifiers.None)
        Dispatcher.UIThread.RunJobs()

    let private centreOf (window : Window) (c : Control) : Point =
        match c.TranslatePoint(Point(c.Bounds.Width / 2.0, c.Bounds.Height / 2.0), window) |> Option.ofNullable with
        | Some p -> p
        | None -> failwith "the control has no on-screen position"

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``right-clicking an element on the real Main view opens its menu, whose rows switch the ray`` () =
        HeadlessSession.run (fun () ->
            let model0 = initMain () |> update (AddElement Sample)
            let window = mount model0
            try
                match tryFind window UiIds.TableAndElementRotation.canvas with
                | Some canvas ->
                    let p = canvasPointOf 2 model0
                    match canvas.TranslatePoint(Point(p.sx, p.sy), window) |> Option.ofNullable with
                    | Some at -> click window MouseButton.Right at
                    | None -> Assert.Fail("the canvas has no on-screen position")
                | None -> Assert.Fail("the table canvas was not found")
                Assert.True(isShown window UiIds.ElementMenu.menu, "the right click must open the element menu")
                match tryFind window UiIds.ElementMenu.toggleReflected with
                | Some row ->
                    Assert.StartsWith("☑", textUnder row)
                    click window MouseButton.Left (centreOf window row)
                    match tryFind window UiIds.ElementMenu.toggleReflected with
                    | Some flipped -> Assert.StartsWith("☐", textUnder flipped)
                    | None -> Assert.Fail("the ray row must stay in the open menu")
                | None -> Assert.Fail("the menu must offer the reflected-ray switch")
                match tryFind window UiIds.ElementMenu.close with
                | Some close -> click window MouseButton.Left (centreOf window close)
                | None -> Assert.Fail("the menu must offer Close")
                Assert.False(isShown window UiIds.ElementMenu.menu, "Close must dismiss the menu")
            finally
                window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Rotation bay's checkbox switches every element to rotate about its incident face`` () =
        HeadlessSession.run (fun () ->
            let window = mount (plateScene ())
            try
                let checkBox () : CheckBox option =
                    window.GetVisualDescendants()
                    |> Seq.tryPick (function :? CheckBox as c when matchesId UiIds.TableAndElementRotation.pivotAtIncidentFace c -> Some c | _ -> None)
                match checkBox () with
                | Some box ->
                    Assert.False(box.IsChecked.GetValueOrDefault true, "unchecked by default: elements rotate about their centre")
                    click window MouseButton.Left (centreOf window box)
                    match checkBox () with
                    | Some ticked -> Assert.True(ticked.IsChecked.GetValueOrDefault false, "the click switches to the incident-face pivot")
                    | None -> Assert.Fail("the pivot checkbox must stay in the Rotation bay")
                | None -> Assert.Fail("the Rotation bay must offer the pivot checkbox")
            finally
                window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main view draws the orientation markers on the selected element only`` () =
        HeadlessSession.run (fun () ->
            let normalLines (window : Window) : int =
                window.GetVisualDescendants()
                |> Seq.filter (function
                    | :? Avalonia.Controls.Shapes.Line as l ->
                        match l.Stroke with
                        | :? Avalonia.Media.ISolidColorBrush as b -> b.Color = OpticalConstructor.Ui.ElementRenderer.normalAxisColor
                        | _ -> false
                    | _ -> false)
                |> Seq.length
            // Five elements on the beam, the sample selected: ONE N1 marker (the sample's) — the polarizers' and
            // the source's / detector's N1 markers, lying on the beam, are not drawn (operator .manual/004-005).
            let withSampleSelected = plateScene () |> update (AddElement LinearPolarizer) |> fun m -> { m with selection = ElementSelected 2 }
            for model, expected in [ withSampleSelected, 1; { withSampleSelected with selection = TableSelected }, 0 ] do
                let window = mount model
                try Assert.Equal(expected, normalLines window)
                finally window.Close())

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the Main view scrolls, and the Experiments bay shows only the captures the detectors allow`` () =
        HeadlessSession.run (fun () ->
            let model0 =
                plateScene ()
                |> (fun m -> m |> update (ExpChooseElement (elem 2 m).id.value))
                |> update (SelectBay BayNames.experiments)
            let window = mount model0
            try
                Assert.True(isShown window UiIds.TableAndElementRotation.scrollViewer, "the Main view must sit in a scroll viewer")
                Assert.True(isShown window (UiIds.Experiment.measurement "t"), "T is offered")
                Assert.False(isShown window (UiIds.Experiment.measurement "r"), "R is not offered without a reflected detector")
                Assert.False(isShown window (UiIds.Experiment.measurement "both"), "Both is not offered without a reflected detector")
            finally
                window.Close())
