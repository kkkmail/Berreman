namespace OpticalConstructor.Tests

open System
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.BeamLayout
open Xunit

/// Spec 0046 — the ONE crossing law (`RayModel.crossElement`) and the Main-scene beam tree
/// (`BeamLayout`): a tilted element is crossed in three segments (in, bent inside, out areParallel to
/// the incoming ray) for every element kind; partially-reflecting elements open reflected side beams
/// that other elements snap onto; hosts that vanish fall back to the main beam; the element-menu ray
/// switches keep both-off unrepresentable; and the experiment captures follow the detectors.
/// Pure — no Avalonia type is touched.
module BeamLayoutTests =

    /// Absolute float tolerance (the `RayModelTests` / `PlacementTests` precedent).
    let private tol = 1.0e-9
    let private close (a : float) (b : float) : bool = abs (a - b) <= tol

    let private sub (a : Vector3) (b : Vector3) : Vector3 = Vector3.create (a.x - b.x) (a.y - b.y) (a.z - b.z)
    let private areParallel (a : Vector3) (b : Vector3) : bool = close (a.normalized.dot b.normalized) 1.0
    let private dirOf (s : RaySegment) : Vector3 = (sub s.finish s.start).normalized

    let private el (id : string) (kind : CatalogueKind) (x : float) : LayoutElement =
        {
            id = Library.elementId id
            placement = ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> }
            host = OnMainBeam
            zoom = 5.0
        }

    let private tilted (r2 : float) (e : LayoutElement) : LayoutElement =
        { e with placement = withR2 (Angle.degree r2) e.placement }

    let private tiltedOut (r3 : float) (e : LayoutElement) : LayoutElement =
        { e with placement = withR3 (Angle.degree r3) { e.placement with r3Locked = false } }

    let private onReflectedOf (host : string) (e : LayoutElement) : LayoutElement =
        { e with host = OnReflectedBeamOf (Library.elementId host) }

    let private placedOf (id : string) (scene : SceneLayout) : PlacedElement =
        scene.elements |> List.find (fun p -> p.id = Library.elementId id)

    /// The element with its reflected ray switched off, so only its transmitted path is drawn.
    let private transmitting (e : LayoutElement) : LayoutElement =
        { e with placement = { e.placement with emission = EmitTransmittedOnly } }

    /// source (x = -1) → transmit-only sample (x = 0, tilted by r2) → detector (x = 1).
    let private sampleScene (r2 : float) : LayoutElement list =
        [ el "src" LightSource -1.0; transmitting (tilted r2 (el "s" Sample 0.0)); el "det" Detector 1.0 ]

    /// As `sampleScene`, but the sample emits BOTH branches, so it opens a reflected side beam.
    let private bothScene (r2 : float) : LayoutElement list =
        [ el "src" LightSource -1.0; tilted r2 (el "s" Sample 0.0); el "det" Detector 1.0 ]

    // --- the crossing law ----------------------------------------------------

    [<Fact>]
    let ``a tilted element is entered on its front face, crossed by Snell's law, and left areParallel and shifted`` () =
        let centre = Vector3.create 0.0 0.0 0.0
        let d = centralRayDirection
        let h = 0.025
        let theta1 = 30.0 * Math.PI / 180.0
        let n = Vector3.create (cos theta1) (sin theta1) 0.0
        let c = RayModel.crossElement centre d n (h * 1.0<meter>)
        // The entry is on the FRONT face plane and on the incoming line through the centre; the exit on
        // the BACK face plane.
        Assert.True(close ((sub c.entry centre).dot n) -h, "entry on the front face")
        Assert.True(close ((sub c.exit centre).dot n) h, "exit on the back face")
        Assert.True(close ((sub c.entry centre).cross d).norm 0.0, "entry on the incoming line through the centre")
        // Inside: Snell with n = 1.0 → 1.5; the run entry → exit follows the inside direction.
        let theta2 = acos (c.inside.dot n)
        Assert.True(close (sin theta1) (RayModel.refractiveIndexInside * sin theta2), "Snell's law inside")
        Assert.True(areParallel (sub c.exit c.entry) c.inside, "the inside run follows the refracted direction")
        // Out: areParallel to the incoming ray, laterally shifted by t·sin(θ1 − θ2) / cos θ2 (t = 2h).
        Assert.True(areParallel c.transmitted d, "the exit ray is areParallel to the incoming ray")
        let shift = ((sub c.exit centre).cross d).norm
        Assert.True(abs (shift - 2.0 * h * sin (theta1 - theta2) / cos theta2) < 1.0e-12, $"lateral shift {shift}")
        // The reflected ray leaves the entry at the mirror angle: equal angles to the normal.
        Assert.True(close (c.reflected.dot n) (-(d.dot n)), "angle of reflection = angle of incidence")

    [<Fact>]
    let ``a zero-depth element is crossed at its centre and normal incidence goes straight through`` () =
        let centre = Vector3.create 0.3 -0.2 0.0
        let thin = RayModel.crossElement centre centralRayDirection (Vector3.create 1.0 1.0 0.0) 0.0<meter>
        Assert.Equal(centre, thin.entry)
        Assert.Equal(centre, thin.exit)
        let square = RayModel.crossElement centre centralRayDirection centralRayDirection 0.025<meter>
        Assert.True(areParallel square.inside centralRayDirection && areParallel square.transmitted centralRayDirection)
        Assert.True(close square.exit.y centre.y, "no lateral shift at normal incidence")

    // --- the layout: three segments for every element -----------------------------------------

    [<Fact>]
    let ``a tilted sample is drawn in three segments, the first and the last parallel`` () =
        let scene = layout (sampleScene 30.0)
        match scene.segments with
        | [ toSample; inside; toDetector ] ->
            Assert.Equal(InFreeSpace, toSample.kind)
            Assert.Equal(InsideElement, inside.kind)
            Assert.Equal(InFreeSpace, toDetector.kind)
            Assert.True(areParallel (dirOf toSample) (dirOf toDetector), "segments 1 and 3 must be parallel")
            Assert.False(areParallel (dirOf inside) (dirOf toSample), "segment 2 (inside the sample) is bent")
            Assert.Equal(toSample.finish, inside.start)
            Assert.Equal(inside.finish, toDetector.start)
        | other -> Assert.Fail($"expected 3 segments, got %d{List.length other}")

    [<Fact>]
    let ``rotating the sample on R2 or R3 bends only the segment inside it; the exit stays areParallel to the beam`` () =
        let lastDir (elements : LayoutElement list) : Vector3 = (layout elements).segments |> List.last |> dirOf
        let insideDir (elements : LayoutElement list) : Vector3 =
            (layout elements).segments |> List.find (fun s -> s.kind = InsideElement) |> dirOf
        for r2 in [ 15.0; 30.0; 60.0 ] do
            Assert.True(areParallel (lastDir (sampleScene r2)) centralRayDirection, $"exit beam at R2 = %g{r2}")
        Assert.False(areParallel (insideDir (sampleScene 15.0)) (insideDir (sampleScene 60.0)), "the inside run follows R2")
        let outOfPlane = [ el "src" LightSource -1.0; transmitting (tiltedOut 25.0 (el "s" Sample 0.0)); el "det" Detector 1.0 ]
        Assert.True(areParallel (lastDir outOfPlane) centralRayDirection, "exit beam after an R3 tilt")
        Assert.True(abs (insideDir outOfPlane).z > 1.0e-3, "the inside run leaves the table plane under R3")

    [<Fact>]
    let ``the same law crosses every transmissive kind, and a detector at the end terminates the beam`` () =
        for kind in [ LinearPolarizer; CircularPolarizer; Lens; Sample ] do
            let scene = layout [ el "src" LightSource -1.0; transmitting (tilted 40.0 (el "x" kind 0.0)); el "det" Detector 1.0 ]
            Assert.Equal(3, List.length scene.segments)
            Assert.True(areParallel (dirOf (List.last scene.segments)) centralRayDirection, $"%A{kind} leaves parallel")
        // The last segment ends ON the detector's front face — nothing is drawn past it.
        let scene = layout (sampleScene 0.0)
        let det = placedOf "det" scene
        Assert.True(close (List.last scene.segments).finish.x (det.centre.x - 0.025), "the beam ends at the detector's face")

    [<Fact>]
    let ``a mirror reflects off its drawn front face and the downstream detector snaps onto the reflected ray`` () =
        let scene = layout [ el "src" LightSource -1.0; tilted 45.0 (el "m" FlatMirror 0.0); el "det" Detector 0.5 ]
        let det = placedOf "det" scene
        match det.incoming with
        | Some dir -> Assert.True(close dir.x 0.0 && close (abs dir.y) 1.0, $"the detector is on the reflected ray ({dir.x}, {dir.y})")
        | None -> Assert.Fail("the detector must receive a beam")
        Assert.Equal<PathStep list>(
            [ { element = Library.elementId "src"; branch = BeamTree.Transmitted }
              { element = Library.elementId "m"; branch = BeamTree.Reflected } ],
            det.path)

    // --- reflected side beams ------------------------------------------------------------------

    [<Fact>]
    let ``an element on a sample's reflected ray snaps onto it, off its front face, at its along-beam gap`` () =
        let elements = bothScene 30.0 @ [ onReflectedOf "s" (el "rdet" Detector 0.4) ]
        let scene = layout elements
        let rdet = placedOf "rdet" scene
        let crossing = RayModel.crossElement (placedOf "s" scene).centre centralRayDirection (RayModel.faceNormal centralRayDirection (tilted 30.0 (el "s" Sample 0.0)).placement) 0.025<meter>
        Assert.Equal(OnReflectedBeamOf (Library.elementId "s"), rdet.beam)
        match rdet.incoming with
        | Some dir -> Assert.True(areParallel dir crossing.reflected, "the detector receives the sample's reflected ray")
        | None -> Assert.Fail("the reflected detector must receive a beam")
        Assert.True(close ((sub rdet.centre crossing.entry).cross crossing.reflected).norm 0.0, "its centre is on the reflected ray")
        Assert.Contains({ element = Library.elementId "s"; branch = BeamTree.Reflected }, rdet.path)
        // The main detector still receives the TRANSMITTED beam.
        Assert.Contains({ element = Library.elementId "s"; branch = BeamTree.Transmitted }, (placedOf "det" scene).path)

    [<Fact>]
    let ``a reflected ray with nothing on it is still drawn, open-ended, so there is something to attach to`` () =
        let scene = layout (bothScene 30.0)
        // EmitBoth sample: the reflected stub is drawn too (free-space run of openBeamLength off the entry).
        let stubs = scene.segments |> List.filter (fun s -> s.kind = InFreeSpace && close (sub s.finish s.start).norm (openBeamLength / 1.0<meter>))
        Assert.Single(stubs) |> ignore
        Assert.False(areParallel (dirOf (List.head stubs)) centralRayDirection, "the stub is the reflected ray")

    /// The operator's 004 scene (.manual/004): source, LP (-0.3), a sample (-0.1) tilted by -50°, a second
    /// LP (0.1), detector (1.0) — both LPs at normal incidence emit BOTH branches, so each back-reflects.
    let private operatorScene () : LayoutElement list =
        [ el "src" LightSource -1.0; el "lp1" LinearPolarizer -0.3; tilted -50.0 (el "s" Sample -0.1); el "lp2" LinearPolarizer 0.1; el "det" Detector 1.0 ]

    /// The free-space segments starting exactly at `start`.
    let private openFrom (start : Vector3) (scene : SceneLayout) : RaySegment list =
        scene.segments |> List.filter (fun s -> s.kind = InFreeSpace && close (sub s.start start).norm 0.0)

    [<Fact>]
    let ``a back-reflection stops on the element it travels back to, never running through it`` () =
        let elements = operatorScene ()
        let scene = layout elements
        let entryOf (id : string) : Vector3 =
            let p = placedOf id scene
            let e = elements |> List.find (fun x -> x.id = Library.elementId id)
            match p.incoming with
            | Some d -> (RayModel.crossElement p.centre d (RayModel.faceNormal d e.placement) (halfDepthOf e)).entry
            | None -> p.centre
        let sampleExit =
            let p = placedOf "s" scene
            match p.incoming with
            | Some d -> (RayModel.crossElement p.centre d (RayModel.faceNormal d (tilted -50.0 (el "s" Sample -0.1)).placement) 0.025<meter>).exit
            | None -> p.centre
        // LP2 reflects straight back: its open ray ends ON the sample's back face — at the sample's exit point —
        // so it lies exactly on the outgoing ray and never shows past the sample (the operator's 004 stray).
        let backFromLp2 = openFrom (entryOf "lp2") scene |> List.filter (fun s -> (dirOf s).x < 0.0)
        match backFromLp2 with
        | [ s ] -> Assert.True(close (sub s.finish sampleExit).norm 0.0, $"LP2's back-reflection must end at the sample's exit, ended at ({s.finish.x}, {s.finish.y})")
        | other -> Assert.Fail($"expected one back-reflection from LP2, got %d{List.length other}")
        // LP1 reflects straight back to the SOURCE: its open ray ends on the source's front face.
        let sourceFace = -1.0 + 5.0 * 0.08 / 2.0
        match openFrom (entryOf "lp1") scene |> List.filter (fun s -> (dirOf s).x < 0.0) with
        | [ s ] -> Assert.True(close s.finish.x sourceFace, $"LP1's back-reflection must end on the source's face, ended at x = {s.finish.x}")
        | other -> Assert.Fail($"expected one back-reflection from LP1, got %d{List.length other}")

    [<Fact>]
    let ``no ray starts before the source's face or ends past the detector's face`` () =
        let scene = layout (operatorScene ())
        let sourceFace = -1.0 + 5.0 * 0.08 / 2.0
        let detectorFace = 1.0 - 5.0 * 0.01 / 2.0
        for s in scene.segments do
            for p in [ s.start; s.finish ] do
                Assert.True(p.x >= sourceFace - tol, $"a ray reaches x = {p.x}, behind the source's face")
                Assert.True(p.x <= detectorFace + tol, $"a ray reaches x = {p.x}, past the detector's face")

    [<Fact>]
    let ``an open ray with nothing in its way is drawn at its full open length`` () =
        // The sample's reflected ray (R2 = -50°) climbs away from every element.
        let scene = layout (operatorScene ())
        let up = scene.segments |> List.filter (fun s -> s.kind = InFreeSpace && (dirOf s).y > 0.5)
        match up with
        | [ s ] -> Assert.True(close (sub s.finish s.start).norm (openBeamLength / 1.0<meter>))
        | other -> Assert.Fail($"expected the sample's one reflected ray, got %d{List.length other}")

    [<Fact>]
    let ``a host whose reflected ray is gone, an unknown host, or a hosting loop falls back to the main beam`` () =
        // The sample's reflected ray switched off ⇒ the element sits on the main beam.
        let rOff =
            [ el "src" LightSource -1.0
              { (el "s" Sample 0.0) with placement = { (el "s" Sample 0.0).placement with emission = EmitTransmittedOnly } }
              onReflectedOf "s" (el "rdet" Detector 0.4) ]
        Assert.Equal(OnMainBeam, (placedOf "rdet" (layout rOff)).beam)
        Assert.Equal(OnMainBeam, (placedOf "x" (layout [ el "src" LightSource -1.0; onReflectedOf "gone" (el "x" Detector 0.5) ])).beam)
        // A loop (a on b's ray, b on a's ray) never strands both: every element is placed on a beam.
        let loop = [ el "src" LightSource -1.0; onReflectedOf "b" (el "a" Sample -0.2); onReflectedOf "a" (el "b" Sample 0.2) ]
        let scene = layout loop
        for p in scene.elements |> List.tail do
            match p.incoming with
            | Some _ -> ()
            | None -> Assert.Fail($"%s{p.id.value} must be placed on a beam")
        Assert.Contains(OnMainBeam, scene.elements |> List.tail |> List.map (fun p -> p.beam))

    [<Fact>]
    let ``only partially-reflecting elements open a reflected ray, and an element is never moved into its own subtree`` () =
        let elements =
            [ el "src" LightSource -1.0
              el "pol" LinearPolarizer -0.5
              el "m" FlatMirror -0.2
              el "s" Sample 0.0
              onReflectedOf "s" (el "lens" Lens 0.3)
              onReflectedOf "lens" (el "det" Detector 0.6) ]
        let ids (xs : Library.ElementId list) : string list = xs |> List.map (fun i -> i.value)
        // The source never moves; the mirror opens no side beam; the sample cannot host itself, and it
        // cannot move onto the reflected ray of the lens that sits on its own reflected ray.
        Assert.Empty(attachableHosts elements (Library.elementId "src"))
        Assert.Equal<string list>([ "pol"; "s"; "lens" ], ids (attachableHosts elements (Library.elementId "det")))
        Assert.Equal<string list>([ "pol" ], ids (attachableHosts elements (Library.elementId "s")))
        Assert.Equal<Set<Library.ElementId>>(set [ Library.elementId "lens"; Library.elementId "det" ], hostedUnder elements (Library.elementId "s"))
        Assert.Equal(OpensReflectedBeam, sideBeamOf (el "s" Sample 0.0).placement)
        Assert.Equal(NoSideBeam, sideBeamOf (el "m" FlatMirror 0.0).placement)
        Assert.Equal(NoSideBeam, sideBeamOf (el "src" LightSource 0.0).placement)

    // --- the element-menu ray switches -----------------------------------------------------------

    [<Fact>]
    let ``the ray switches keep both-off unrepresentable and leave mirrors, sources, detectors and reflect-only samples fixed`` () =
        let pol = (el "pol" LinearPolarizer 0.0).placement
        let rOff = toggleRay BeamTree.Reflected EmitBoth pol
        Assert.Equal(EmitTransmittedOnly, rOff.emission)
        // Switching off the LAST ray turns the other one back on.
        Assert.Equal(EmitReflectedOnly, (toggleRay BeamTree.Transmitted EmitBoth rOff).emission)
        Assert.Equal(EmitBoth, (toggleRay BeamTree.Reflected EmitBoth rOff).emission)
        for kind in [ FlatMirror; CurvedMirror; LightSource; Detector ] do
            let p = (el "x" kind 0.0).placement
            Assert.Equal(p, toggleRay BeamTree.Reflected EmitBoth p)
            match rayControlOf EmitBoth p with
            | RaysFixed _ -> ()
            | RaysSwitchable -> Assert.Fail($"%A{kind} rays must be fixed")
        // A sample bound to a ThinFilm is observable in reflection only.
        let film = { (el "s" Sample 0.0).placement with emission = EmitReflectedOnly }
        Assert.Equal(film, toggleRay BeamTree.Transmitted EmitReflectedOnly film)

    // --- what the experiments can capture ---------------------------------------------------------

    [<Fact>]
    let ``the captures follow the detectors: T behind the element, R on its reflected ray, both when both`` () =
        let detectors (scene : LayoutElement list) : Library.ElementId list =
            scene |> List.filter (fun e -> e.placement.catalogueKind = Detector) |> List.map (fun e -> e.id)
        let capturesOf (scene : LayoutElement list) (varied : string) : Experiments.MeasurementMode list =
            capturesFor (detectedBranches (layout scene) (detectors scene) (Library.elementId varied) (Library.elementId "s"))
        // Only the transmitted detector ⇒ only T.
        Assert.Equal<Experiments.MeasurementMode list>([ Experiments.CaptureTransmitted ], capturesOf (sampleScene 30.0) "s")
        // Only a reflected detector ⇒ only R.
        let rOnly = [ el "src" LightSource -1.0; tilted 30.0 (el "s" Sample 0.0); onReflectedOf "s" (el "rdet" Detector 0.4) ]
        Assert.Equal<Experiments.MeasurementMode list>([ Experiments.CaptureReflected ], capturesOf rOnly "s")
        // Both ⇒ T, R and Both — and varying the source (upstream of everything) sees both too.
        let both = bothScene 30.0 @ [ onReflectedOf "s" (el "rdet" Detector 0.4) ]
        let all = [ Experiments.CaptureTransmitted; Experiments.CaptureReflected; Experiments.CaptureBoth ]
        Assert.Equal<Experiments.MeasurementMode list>(all, capturesOf both "s")
        Assert.Equal<Experiments.MeasurementMode list>(all, capturesOf both "src")
        // A varied element the reflected detector cannot see (behind the sample on its transmitted
        // beam) only reaches the transmitted detector.
        let behind = both @ [ el "pol" LinearPolarizer 0.5 ]
        Assert.Equal<Experiments.MeasurementMode list>([ Experiments.CaptureTransmitted ], capturesOf behind "pol")
        // No detector ⇒ nothing.
        Assert.Empty(capturesOf [ el "src" LightSource -1.0; el "s" Sample 0.0 ] "s")

    [<Fact>]
    let ``the effective capture keeps an allowed request and otherwise takes the best the detectors allow`` () =
        let all = [ Experiments.CaptureTransmitted; Experiments.CaptureReflected; Experiments.CaptureBoth ]
        Assert.Equal(Some Experiments.CaptureReflected, effectiveCapture all Experiments.CaptureReflected)
        Assert.Equal(Some Experiments.CaptureTransmitted, effectiveCapture [ Experiments.CaptureTransmitted ] Experiments.CaptureBoth)
        Assert.Equal(Some Experiments.CaptureReflected, effectiveCapture [ Experiments.CaptureReflected ] Experiments.CaptureTransmitted)
        Assert.Equal(None, effectiveCapture [] Experiments.CaptureBoth)
