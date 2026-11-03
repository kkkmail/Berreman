/// The SHARED optical-element renderer (Spec 0027, task 018). The draw functions that used to live inside
/// the renderer test now live here so EVERY screen (the renderer test AND the Main screen) draws elements
/// through ONE source — a change here shows up everywhere. The renderer is selected by the serializable
/// `RendererControls.RendererKind` DU and tuned by its `State` (rail count, cap circles / radials, the
/// three transparency knobs); `rendererOf` recovers the actual draw function from the DU case. The yellow
/// (N2) and teal (N1) normals are never transparency-controlled. Spec 0046: they never share the RAY colour, and a
/// host may hide them per element (`NormalMarkers` — the Main scene shows them on the selected element only),
/// so a normal lying on the beam never reads as the ray running on past an element.
module OpticalConstructor.Ui.ElementRenderer

open Avalonia
open Avalonia.Collections
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.TableView
open OpticalConstructor.Controls

/// Whether an element's optical value is BOUND to a library entry, still UNBOUND, or belongs to a kind that
/// carries no bindable material value at all (a lens / mirror is pure geometry). `Drawable.bindingState`
/// derives it; the renderer turns UnboundElement into a colourblind-safe PATTERN cue (a dashed outline
/// and a ghosted fill), leaves BoundElement solid, and draws NotBindableElement normally — the readout and
/// the Details bay state the same binding in text besides.
type BindingState =
    | BoundElement
    | UnboundElement
    | NotBindableElement

/// A drawn distance along an element's oriented axes (N1, N2, N3), in table-frame metres: its box scaled
/// by its draw zoom.
type FrameLength =
    | FrameLength of float

    member this.value = let (FrameLength l) = this in l

    /// The same distance on the other side of the element's centre.
    member this.negated : FrameLength = FrameLength (-this.value)

/// Whether the renderer draws an element as the selected one.
type ElementHighlight =
    | ElementHighlighted
    | ElementNotHighlighted

/// A drawable element: its spec placement (used for ORIENTATION + box + kind + value binding), its full
/// 3-D table-frame `centre` (so an element snapped OUT of the table plane, e.g. after an R3-tilted mirror,
/// is drawn off the plane — not flattened to the 2-D placement point), a per-element visual draw zoom, and
/// a schematic optical sign for lens / curved-mirror caps.
type Drawable =
    {
        placement : ElementPlacement
        centre : Vector3
        zoom : BeamLayout.ElementZoom
        opticalSign : OpticalSign
        normals : NormalMarkers
    }

    /// The value-binding cue (Spec 0038 step 030), so an unbound element reads as dashed-and-ghosted
    /// without any hue change. A lens and a flat / curved mirror are pure geometry with no bindable
    /// material value, so they are NotBindableElement (drawn normally); every other kind is BoundElement
    /// once its `valueId` is set and UnboundElement while it is still `None`.
    member e.bindingState : BindingState =
        match e.placement.catalogueKind with
        | Lens | FlatMirror | CurvedMirror -> NotBindableElement
        | LightSource | LinearPolarizer | CircularPolarizer | Sample | Detector ->
            match e.placement.valueId with
            | Some _ -> BoundElement
            | None -> UnboundElement

    /// Half the box depth `b`, along N1.
    member e.halfDepth : FrameLength = FrameLength (e.zoom.value * (e.placement.box.b / 2.0 / 1.0<meter>))

    /// Half the box face extent `a1`, along N2.
    member e.halfWidth : FrameLength = FrameLength (e.zoom.value * (e.placement.box.a1 / 2.0 / 1.0<meter>))

    /// Half the box face extent `a2`, along N3: the radius of the drawn caps.
    member e.faceRadius : FrameLength = FrameLength (e.zoom.value * (e.placement.box.a2 / 2.0 / 1.0<meter>))

    /// The element's oriented bounding box corners (for the wireframe renderer), table-frame metres.
    member e.corners : Vector3 list =
        let { primaryNormal = n1; secondaryNormal = n2; thirdAxis = n3 } = orientedBasis e.placement
        let c = e.centre
        let hb = e.halfDepth.value
        let h1 = e.halfWidth.value
        let h2 = e.faceRadius.value
        let corner (sb : float) (s1 : float) (s2 : float) : Vector3 =
            Vector3.create
                (c.x + sb * hb * n1.x + s1 * h1 * n2.x + s2 * h2 * n3.x)
                (c.y + sb * hb * n1.y + s1 * h1 * n2.y + s2 * h2 * n3.y)
                (c.z + sb * hb * n1.z + s1 * h1 * n2.z + s2 * h2 * n3.z)
        [ corner 1.0 -1.0 -1.0; corner 1.0 -1.0 1.0; corner 1.0 1.0 1.0; corner 1.0 1.0 -1.0
          corner -1.0 -1.0 -1.0; corner -1.0 -1.0 1.0; corner -1.0 1.0 1.0; corner -1.0 1.0 -1.0 ]

/// Whether an element's N1 / N2 orientation markers are drawn (spec 0046). The test scenes show them on every
/// element; the Main scene only on the SELECTED one — on an element at normal incidence N1 lies on the beam and
/// runs 1.8 × its face half-size past its centre, across the next element's faces, where it read as the ray.
and NormalMarkers =
    | ShowNormals
    | HideNormals

/// Draws ONE element through the live `project`, highlighted or not. The whole point of the abstraction is
/// that the chosen renderer is the only one that draws.
type ElementRenderer =
    {
        draw : (Vector3 -> ScreenPoint) -> ElementHighlight -> Drawable -> IView list
    }

let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private brushA (a : RendererControls.RendererOpacity) (c : Color) : IBrush =
    SolidColorBrush(Color.FromArgb(byte (255.0 * a.clamped.value), c.R, c.G, c.B)) :> IBrush

let private plateColor : Color = Color.FromRgb(205uy, 205uy, 205uy)
let private edgeColor : Color = Color.FromRgb(50uy, 50uy, 50uy)
let private selectedStroke : Color = Color.FromRgb(0uy, 60uy, 160uy)
let private elementColor : Color = Color.FromRgb(60uy, 60uy, 60uy)
/// The N1 (face-normal) indicator. Spec 0046: deliberately NOT the ray colour — at normal incidence N1 lies
/// ON the beam, and drawn in the ray's blue it read as the ray running on past the element's faces (and on
/// past a detector).
let normalAxisColor : Color = Color.FromRgb(0uy, 150uy, 120uy)
let private n1Color = normalAxisColor
let private n2Color : Color = Color.FromRgb(210uy, 120uy, 0uy)
let private codeColor : Color = Color.FromRgb(20uy, 20uy, 30uy)
// The roll-SEAM rail: one rail painted a distinct red, always visible, so the spin about R1 is readable.
let private seamColor : Color = Color.FromRgb(215uy, 45uy, 45uy)
let private seamOpacity : RendererControls.RendererOpacity = RendererControls.RendererOpacity 0.95
let private noFill : RendererControls.RendererOpacity = RendererControls.RendererOpacity 0.0

type ElementHighlight with

    /// The shape renderer's outline colour.
    member this.strokeColor : Color =
        match this with
        | ElementHighlighted -> selectedStroke
        | ElementNotHighlighted -> edgeColor

    /// The shape renderer's outline width.
    member this.outlineWidth : StrokeWidth =
        match this with
        | ElementHighlighted -> StrokeWidth 2.5
        | ElementNotHighlighted -> StrokeWidth 1.5

let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : StrokeWidth) : IView =
    Line.create [ Line.startPoint a.toPoint; Line.endPoint b.toPoint; Line.stroke (brush c); Line.strokeThickness w.value ] :> IView

let private lineT (a : ScreenPoint) (b : ScreenPoint) (c : Color) (alpha : RendererControls.RendererOpacity) (w : StrokeWidth) : IView =
    Line.create [ Line.startPoint a.toPoint; Line.endPoint b.toPoint; Line.stroke (brushA alpha c); Line.strokeThickness w.value ] :> IView

/// The dash pattern an UNBOUND element's outline is drawn with — a colourblind-safe PATTERN cue (dash-then-
/// gap in stroke-thickness units), never a hue change; a bound element keeps a solid outline.
let private unboundDash : float list = [ 4.0; 3.0 ]

/// A cap polygon (a filled ring on a plane). When the element is UNBOUND its outline is DASHED and its fill
/// is GHOSTED — the opacity scaled to 0.3, so the unbound state reads as faint-and-dashed rather than merely
/// a different colour; otherwise it is drawn solid, exactly as before.
let private capPolygon (pts : Point list) (fillC : Color) (faceOpacity : RendererControls.RendererOpacity) (strokeC : Color) (w : StrokeWidth) (binding : BindingState) : IView =
    let unbound = (binding = UnboundElement)
    let fillOpacity = if unbound then RendererControls.RendererOpacity (faceOpacity.value * 0.3) else faceOpacity
    Polygon.create
        [ Polygon.points pts
          Polygon.fill (brushA fillOpacity fillC)
          Polygon.stroke (brush strokeC)
          Polygon.strokeThickness w.value
          if unbound then Polygon.strokeDashArray (AvaloniaList<float>(unboundDash)) ] :> IView

type CatalogueKind with

    /// The fill colour the SHAPE renderer gives this kind — a quick read of an element's "nature".
    member this.shapeColor : Color =
        match this with
        | LightSource -> Color.FromRgb(245uy, 200uy, 40uy)
        | LinearPolarizer -> Color.FromRgb(70uy, 130uy, 200uy)
        | CircularPolarizer -> Color.FromRgb(100uy, 170uy, 200uy)
        | Sample -> Color.FromRgb(90uy, 180uy, 110uy)
        | Lens -> Color.FromRgb(150uy, 200uy, 230uy)
        | FlatMirror -> Color.FromRgb(180uy, 180uy, 190uy)
        | CurvedMirror -> Color.FromRgb(150uy, 150uy, 165uy)
        | Detector -> Color.FromRgb(60uy, 60uy, 70uy)

/// A table-frame point: the element `centre` (full 3-D, so an out-of-plane snap is honoured) offset by the
/// element-frame coordinates `local` — local.x along N1, local.y along N2, local.z along N3.
let private offset (centre : Vector3) (basis : OrientedBasis) (local : Vector3) : Vector3 =
    let { primaryNormal = n1; secondaryNormal = n2; thirdAxis = n3 } = basis
    Vector3.create
        (centre.x + local.x * n1.x + local.y * n2.x + local.z * n3.x)
        (centre.y + local.x * n1.y + local.y * n2.y + local.z * n3.y)
        (centre.z + local.x * n1.z + local.y * n2.z + local.z * n3.z)

/// BOTH oriented normals — N1 (teal, beam axis) and N2 (yellow, roll) — always shown, never transparent.
let private normalsViews (project : Vector3 -> ScreenPoint) (highlight : ElementHighlight) (e : Drawable) : IView list =
    match e.normals with
    | HideNormals -> []
    | ShowNormals ->
        let { primaryNormal = n1; secondaryNormal = n2 } = orientedBasis e.placement
        let c = e.centre
        let half = e.faceRadius.value
        let centre = project c
        let along (n : Vector3) (len : float) = project (Vector3.create (c.x + len * n.x) (c.y + len * n.y) (c.z + len * n.z))
        let w =
            match highlight with
            | ElementHighlighted -> StrokeWidth 3.0
            | ElementNotHighlighted -> StrokeWidth 2.0
        [ line centre (along n1 (1.8 * half)) n1Color w
          line centre (along n2 (-(1.3 * half))) n2Color w ]

let private capPts (project : Vector3 -> ScreenPoint) (basis : OrientedBasis) (centre : Vector3) (d : FrameLength) (r : FrameLength) : Point list =
    [ for k in 0 .. 23 ->
        let t = 2.0 * System.Math.PI * float k / 24.0
        (project (offset centre basis (Vector3.create d.value (r.value * cos t) (r.value * sin t)))).toPoint ]

/// The side rails of a cylinder (rim on plane `dA` → rim on plane `dB`). Rail 0 sits at +N2 (the cap top,
/// opposite the yellow normal) and is the always-visible RED roll SEAM, so the R1 spin is readable where a
/// symmetric ring of identical rails has no phase. The rest are translucent at `railOpacity`.
let private railViews (project : Vector3 -> ScreenPoint) (basis : OrientedBasis) (centre : Vector3) (dA : FrameLength) (dB : FrameLength) (r : FrameLength) (rails : RendererControls.RailCount) (railOpacity : RendererControls.RendererOpacity) (w : StrokeWidth) : IView list =
    [ for i in 0 .. rails.value - 1 ->
        let t = 2.0 * System.Math.PI * float i / float rails.value
        let a = project (offset centre basis (Vector3.create dA.value (r.value * cos t) (r.value * sin t)))
        let b = project (offset centre basis (Vector3.create dB.value (r.value * cos t) (r.value * sin t)))
        if i = 0 then lineT a b seamColor seamOpacity (StrokeWidth (w.value * 1.9))
        else lineT a b edgeColor railOpacity w ]

/// A SPHERICAL cap (part of a sphere): the rim (radius `r` at `rimD`) curving to the apex (radius 0 at
/// `apexD`), with `circles` inner latitude rings and `radials` CURVED meridian polylines following the
/// sphere sagitta. Both `rimD`/`apexD` are within ±halfLen so nothing bulges out of the box. The rim FILL
/// uses `faceOpacity`; the inner circles + meridians use `lineOpacity`.
let private capSurface (project : Vector3 -> ScreenPoint) (basis : OrientedBasis) (centre : Vector3) (r : FrameLength) (rimD : FrameLength) (apexD : FrameLength) (circles : RendererControls.CapCircles) (radials : RendererControls.CapRadials) (fillC : Color) (strokeC : Color) (faceOpacity : RendererControls.RendererOpacity) (lineOpacity : RendererControls.RendererOpacity) (w : StrokeWidth) : IView list =
    let dAt (rho : float) : float = apexD.value + (rimD.value - apexD.value) * (1.0 - sqrt (max 0.0 (1.0 - (rho / r.value) ** 2.0)))
    let ringPts (rho : float) : Point list =
        [ for k in 0 .. 23 ->
            let t = 2.0 * System.Math.PI * float k / 24.0
            (project (offset centre basis (Vector3.create (dAt rho) (rho * cos t) (rho * sin t)))).toPoint ]
    let meridianSteps = 12
    let meridian (t : float) : IView =
        let pts =
            [ for j in 0 .. meridianSteps ->
                let rho = r.value * (1.0 - float j / float meridianSteps)
                (project (offset centre basis (Vector3.create (dAt rho) (rho * cos t) (rho * sin t)))).toPoint ]
        Polyline.create [ Polyline.points pts; Polyline.stroke (brushA lineOpacity strokeC); Polyline.strokeThickness (w.value * 0.6) ] :> IView
    let innerCircle (k : int) : IView =
        let rho = r.value * float k / float (circles.value + 1)
        Polygon.create [ Polygon.points (ringPts rho); Polygon.fill (brushA noFill fillC); Polygon.stroke (brushA lineOpacity strokeC); Polygon.strokeThickness (w.value * 0.5) ] :> IView
    [ Polygon.create [ Polygon.points (ringPts r.value); Polygon.fill (brushA faceOpacity fillC); Polygon.stroke (brush strokeC); Polygon.strokeThickness w.value ] :> IView ]
    @ [ for k in 1 .. circles.value -> innerCircle k ]
    @ [ for i in 0 .. radials.value - 1 -> meridian (2.0 * System.Math.PI * float i / float radials.value) ]

/// A flat element as a 3-D CYLINDER (axis = teal N1): two FLAT caps joined by the side rails.
let private cylinderViews (cfg : RendererControls.RendererControlsState) (project : Vector3 -> ScreenPoint) (highlight : ElementHighlight) (e : Drawable) : IView list =
    let basis = orientedBasis e.placement
    let c = e.centre
    let halfLen = e.halfDepth
    let r = e.faceRadius
    let sc = highlight.strokeColor
    let w = highlight.outlineWidth
    let back = StrokeWidth (w.value * 0.7)
    [ capPolygon (capPts project basis c halfLen.negated r) plateColor noFill sc back e.bindingState ]
    @ railViews project basis c halfLen halfLen.negated r cfg.rails cfg.railOpacity back
    @ [ capPolygon (capPts project basis c halfLen r) e.placement.catalogueKind.shapeColor cfg.faceOpacity sc w e.bindingState ]

/// A lens — the cylinder with two SPHERICAL caps that FIT THE BOUNDING BOX (biconvex apexes out to the box
/// faces for a converging sign, biconcave rims at the faces for diverging).
let private lensViews (cfg : RendererControls.RendererControlsState) (project : Vector3 -> ScreenPoint) (highlight : ElementHighlight) (e : Drawable) : IView list =
    let basis = orientedBasis e.placement
    let c = e.centre
    let halfLen = e.halfDepth
    let r = e.faceRadius
    let sag = 0.7 * halfLen.value
    let rimD, apexD =
        match e.opticalSign with
        | ConvergingSign | FlatSign -> FrameLength (halfLen.value - sag), halfLen
        | DivergingSign -> halfLen, FrameLength (halfLen.value - sag)
    let sc = highlight.strokeColor
    let w = highlight.outlineWidth
    let kc = (Lens).shapeColor
    capSurface project basis c r rimD apexD cfg.circles cfg.radials kc sc cfg.faceOpacity cfg.lineOpacity w
    @ capSurface project basis c r rimD.negated apexD.negated cfg.circles cfg.radials kc sc cfg.faceOpacity cfg.lineOpacity w
    @ railViews project basis c rimD rimD.negated r cfg.rails cfg.railOpacity (StrokeWidth (w.value * 0.7))

/// A curved mirror — a cylinder with ONE spherical reflective cap (concave for converging, convex for
/// diverging) plus a flat back, kept inside the bounding box.
let private mirrorViews (cfg : RendererControls.RendererControlsState) (project : Vector3 -> ScreenPoint) (highlight : ElementHighlight) (e : Drawable) : IView list =
    let basis = orientedBasis e.placement
    let c = e.centre
    let halfLen = e.halfDepth
    let r = e.faceRadius
    let sag = 0.7 * halfLen.value
    let rimD, apexD =
        match e.opticalSign with
        | ConvergingSign | FlatSign -> halfLen, FrameLength (halfLen.value - sag)
        | DivergingSign -> FrameLength (halfLen.value - sag), halfLen
    let sc = highlight.strokeColor
    let w = highlight.outlineWidth
    let back = StrokeWidth (w.value * 0.7)
    let kc = (CurvedMirror).shapeColor
    [ Polygon.create [ Polygon.points (capPts project basis c halfLen.negated r); Polygon.fill (brushA noFill kc); Polygon.stroke (brush sc); Polygon.strokeThickness back.value ] :> IView ]
    @ railViews project basis c rimD halfLen.negated r cfg.rails cfg.railOpacity back
    @ capSurface project basis c r rimD apexD cfg.circles cfg.radials kc sc cfg.faceOpacity cfg.lineOpacity w

let private codeLabel (project : Vector3 -> ScreenPoint) (highlight : ElementHighlight) (e : Drawable) : IView =
    let c = project e.centre
    TextBlock.create [
        TextBlock.left (c.sx + 8.0)
        TextBlock.top (c.sy - 26.0)
        TextBlock.text e.placement.catalogueKind.kindCode
        TextBlock.fontWeight (match highlight with ElementHighlighted -> FontWeight.Bold | ElementNotHighlighted -> FontWeight.SemiBold)
        TextBlock.foreground (brush codeColor)
    ] :> IView

/// Today's look — the projected bounding-box edges plus the N1 / N2 normals.
let wireframeRenderer : ElementRenderer =
    {
        draw =
            fun project highlight e ->
                let corners = e.corners |> List.map (fun p -> (project p).toPoint) |> List.toArray
                let boxColor, weight =
                    match highlight with
                    | ElementHighlighted -> selectedStroke, 2.5
                    | ElementNotHighlighted -> elementColor, 1.0
                // No fill in the wireframe look, so the unbound cue is the dashed box edges alone (a PATTERN
                // difference); the always-solid N1 / N2 normals stay solid so the orientation reads cleanly.
                let unbound = (e.bindingState = UnboundElement)
                let edges =
                    TableView.plateEdges
                    |> List.map (fun edge ->
                        Line.create
                            [ Line.startPoint (edge.fromCorner.pick corners)
                              Line.endPoint (edge.toCorner.pick corners)
                              Line.stroke (brush boxColor)
                              Line.strokeThickness weight
                              if unbound then Line.strokeDashArray (AvaloniaList<float>(unboundDash)) ] :> IView)
                edges @ normalsViews project highlight e
    }

/// Recover the draw function from the serializable renderer DU + config: the wireframe, or the nicer look —
/// cylinders / spherical caps coloured by nature, with the element code.
let rendererOf (cfg : RendererControls.RendererControlsState) : ElementRenderer =
    match cfg.kind with
    | RendererControls.Wireframe -> wireframeRenderer
    | RendererControls.Shapes ->
        {
            draw =
                fun project highlight e ->
                    let body =
                        match e.placement.catalogueKind with
                        | Lens -> lensViews cfg project highlight e
                        | CurvedMirror -> mirrorViews cfg project highlight e
                        | _ -> cylinderViews cfg project highlight e
                    body @ normalsViews project highlight e @ [ codeLabel project highlight e ]
        }
