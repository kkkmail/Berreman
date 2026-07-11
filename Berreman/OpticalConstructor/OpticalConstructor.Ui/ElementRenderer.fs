/// The SHARED optical-element renderer (Spec 0027, task 018). The draw functions that used to live inside
/// the renderer test now live here so EVERY screen (the renderer test AND the Main screen) draws elements
/// through ONE source — a change here shows up everywhere. The renderer is selected by the serializable
/// `RendererControls.RendererKind` DU and tuned by its `State` (rail count, cap circles / radials, the
/// three transparency knobs); `rendererOf` recovers the actual draw function from the DU case. The yellow
/// (N2) and blue (N1) normals are always drawn and never transparency-controlled.
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
/// carries no bindable material value at all (a lens / mirror is pure geometry). The host derives it with
/// `bindingStateOf`; the renderer turns UnboundElement into a colourblind-safe PATTERN cue (a dashed outline
/// and a ghosted fill), leaves BoundElement solid, and draws NotBindableElement normally — the readout and
/// the Details bay state the same binding in text besides.
type BindingState =
    | BoundElement
    | UnboundElement
    | NotBindableElement

/// A drawable element: its spec placement (used for ORIENTATION + box + kind), its full 3-D table-frame
/// `centre` (so an element snapped OUT of the table plane, e.g. after an R3-tilted mirror, is drawn off the
/// plane — not flattened to the 2-D placement point), a per-element visual draw zoom, a schematic optical
/// sign for lens / curved-mirror caps (+1 converging, −1 diverging, 0 flat), and its value-binding state (so
/// an unbound element reads as dashed-and-ghosted without any hue change).
type Drawable =
    {
        placement : ElementPlacement
        centre : Vector3
        zoom : float
        opticalSign : int
        bindingState : BindingState
    }

/// The 2-D placement point as a 3-D table-frame centre (z = 0): the centre for an element that sits ON the
/// table plane (the renderer test, and any non-snapped element).
let centreOfPlacement (p : ElementPlacement) : Vector3 =
    Vector3.create (p.placementPoint.x / 1.0<meter>) (p.placementPoint.y / 1.0<meter>) 0.0

/// The value-binding cue for a placement (Spec 0038 step 030). A lens and a flat / curved mirror are pure
/// geometry with no bindable material value, so they are NotBindableElement (drawn normally); every other
/// kind is BoundElement once its `valueId` is set and UnboundElement while it is still `None`.
let bindingStateOf (p : ElementPlacement) : BindingState =
    match p.catalogueKind with
    | Lens | FlatMirror | CurvedMirror -> NotBindableElement
    | LightSource | LinearPolarizer | CircularPolarizer | Sample | Detector ->
        match p.valueId with
        | Some _ -> BoundElement
        | None -> UnboundElement

/// Draws ONE element through the live `project`, given whether it is selected. The whole point of the
/// abstraction is that the chosen renderer is the only one that draws.
type ElementRenderer =
    {
        name : string
        draw : (Vector3 -> ScreenPoint) -> bool -> Drawable -> IView list
    }

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private brushA (a : float) (c : Color) : IBrush =
    SolidColorBrush(Color.FromArgb(byte (255.0 * max 0.0 (min 1.0 a)), c.R, c.G, c.B)) :> IBrush

let private plateColor = color 205 205 205
let private edgeColor = color 50 50 50
let private selectedStroke = color 0 60 160
let private elementColor = color 60 60 60
let private n1Color = color 30 90 200
let private n2Color = color 210 120 0
let private codeColor = color 20 20 30
// The roll-SEAM rail: one rail painted a distinct red, always visible, so the spin about R1 is readable.
let private seamColor = color 215 45 45

let private toPoint (sp : ScreenPoint) : Point = Point(sp.sx, sp.sy)
let private v3 (x : float) (y : float) (z : float) : Vector3 = Vector3.create x y z

let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : float) : IView =
    Line.create [ Line.startPoint (toPoint a); Line.endPoint (toPoint b); Line.stroke (brush c); Line.strokeThickness w ] :> IView

let private lineT (a : ScreenPoint) (b : ScreenPoint) (c : Color) (alpha : float) (w : float) : IView =
    Line.create [ Line.startPoint (toPoint a); Line.endPoint (toPoint b); Line.stroke (brushA alpha c); Line.strokeThickness w ] :> IView

let private strokeOf (selected : bool) : Color = if selected then selectedStroke else edgeColor

/// The dash pattern an UNBOUND element's outline is drawn with — a colourblind-safe PATTERN cue (dash-then-
/// gap in stroke-thickness units), never a hue change; a bound element keeps a solid outline.
let private unboundDash : float list = [ 4.0; 3.0 ]

/// How much an UNBOUND element's face fill is GHOSTED: its opacity is scaled to this fraction so the unbound
/// state reads as faint-and-dashed rather than merely a different colour.
let private ghostFill : float = 0.3

/// Whether a drawable is the UNBOUND case — the only binding state that changes the draw (dashed + ghosted).
let private isUnbound (e : Drawable) : bool =
    match e.bindingState with
    | UnboundElement -> true
    | BoundElement | NotBindableElement -> false

/// A cap polygon (a filled ring on a plane). When the element is UNBOUND its outline is DASHED and its fill
/// is GHOSTED (opacity scaled by `ghostFill`); otherwise it is drawn solid, exactly as before.
let private capPolygon (pts : Point list) (fillC : Color) (faceOpacity : float) (strokeC : Color) (w : float) (unbound : bool) : IView =
    let fillOp = if unbound then faceOpacity * ghostFill else faceOpacity
    Polygon.create
        [ Polygon.points pts
          Polygon.fill (brushA fillOp fillC)
          Polygon.stroke (brush strokeC)
          Polygon.strokeThickness w
          if unbound then Polygon.strokeDashArray (AvaloniaList<float>(unboundDash)) ] :> IView

/// The fill colour the SHAPE renderer gives each kind — a quick read of an element's "nature".
let private kindColor (k : CatalogueKind) : Color =
    match k with
    | LightSource -> color 245 200 40
    | LinearPolarizer -> color 70 130 200
    | CircularPolarizer -> color 100 170 200
    | Sample -> color 90 180 110
    | Lens -> color 150 200 230
    | FlatMirror -> color 180 180 190
    | CurvedMirror -> color 150 150 165
    | Detector -> color 60 60 70

/// A table-frame point: the element `centre` (full 3-D, so an out-of-plane snap is honoured) offset by
/// (a·N1, b·N2, cc·N3).
let private offset (centre : Vector3) (n1 : Vector3) (n2 : Vector3) (n3 : Vector3) (a : float) (b : float) (cc : float) : Vector3 =
    v3 (centre.x + a * n1.x + b * n2.x + cc * n3.x) (centre.y + a * n1.y + b * n2.y + cc * n3.y) (centre.z + a * n1.z + b * n2.z + cc * n3.z)

/// BOTH oriented normals — N1 (blue, beam axis) and N2 (yellow, roll) — always shown, never transparent.
let private normalsViews (project : Vector3 -> ScreenPoint) (selected : bool) (e : Drawable) : IView list =
    let (n1, n2, _) = orientedBasis e.placement
    let c = e.centre
    let half = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let centre = project c
    let along (n : Vector3) (len : float) = project (v3 (c.x + len * n.x) (c.y + len * n.y) (c.z + len * n.z))
    [ line centre (along n1 (1.8 * half)) n1Color (if selected then 3.0 else 2.0)
      line centre (along n2 (-(1.3 * half))) n2Color (if selected then 3.0 else 2.0) ]

let private capPts (project : Vector3 -> ScreenPoint) (n1 : Vector3) (n2 : Vector3) (n3 : Vector3) (centre : Vector3) (d : float) (r : float) : Point list =
    [ for k in 0 .. 23 ->
        let t = 2.0 * System.Math.PI * float k / 24.0
        toPoint (project (offset centre n1 n2 n3 d (r * cos t) (r * sin t))) ]

/// The side rails of a cylinder (rim on plane `dA` → rim on plane `dB`). Rail 0 sits at +N2 (the cap top,
/// opposite the yellow normal) and is the always-visible RED roll SEAM, so the R1 spin is readable where a
/// symmetric ring of identical rails has no phase. The rest are translucent at `railOpacity`.
let private railViews (project : Vector3 -> ScreenPoint) (n1 : Vector3) (n2 : Vector3) (n3 : Vector3) (centre : Vector3) (dA : float) (dB : float) (r : float) (rails : int) (railOpacity : float) (w : float) : IView list =
    [ for i in 0 .. rails - 1 ->
        let t = 2.0 * System.Math.PI * float i / float rails
        let a = project (offset centre n1 n2 n3 dA (r * cos t) (r * sin t))
        let b = project (offset centre n1 n2 n3 dB (r * cos t) (r * sin t))
        if i = 0 then lineT a b seamColor 0.95 (w * 1.9)
        else lineT a b edgeColor railOpacity w ]

/// A SPHERICAL cap (part of a sphere): the rim (radius `r` at `rimD`) curving to the apex (radius 0 at
/// `apexD`), with `circles` inner latitude rings and `radials` CURVED meridian polylines following the
/// sphere sagitta. Both `rimD`/`apexD` are within ±halfLen so nothing bulges out of the box. The rim FILL
/// uses `faceOpacity`; the inner circles + meridians use `lineOpacity`.
let private capSurface (project : Vector3 -> ScreenPoint) (n1 : Vector3) (n2 : Vector3) (n3 : Vector3) (centre : Vector3) (r : float) (rimD : float) (apexD : float) (circles : int) (radials : int) (fillC : Color) (strokeC : Color) (faceOpacity : float) (lineOpacity : float) (w : float) : IView list =
    let dAt (rho : float) : float = apexD + (rimD - apexD) * (1.0 - sqrt (max 0.0 (1.0 - (rho / r) ** 2.0)))
    let ringPts (rho : float) : Point list =
        [ for k in 0 .. 23 ->
            let t = 2.0 * System.Math.PI * float k / 24.0
            toPoint (project (offset centre n1 n2 n3 (dAt rho) (rho * cos t) (rho * sin t))) ]
    let meridianSteps = 12
    let meridian (t : float) : IView =
        let pts =
            [ for j in 0 .. meridianSteps ->
                let rho = r * (1.0 - float j / float meridianSteps)
                toPoint (project (offset centre n1 n2 n3 (dAt rho) (rho * cos t) (rho * sin t))) ]
        Polyline.create [ Polyline.points pts; Polyline.stroke (brushA lineOpacity strokeC); Polyline.strokeThickness (w * 0.6) ] :> IView
    let innerCircle (k : int) : IView =
        let rho = r * float k / float (circles + 1)
        Polygon.create [ Polygon.points (ringPts rho); Polygon.fill (brushA 0.0 fillC); Polygon.stroke (brushA lineOpacity strokeC); Polygon.strokeThickness (w * 0.5) ] :> IView
    [ Polygon.create [ Polygon.points (ringPts r); Polygon.fill (brushA faceOpacity fillC); Polygon.stroke (brush strokeC); Polygon.strokeThickness w ] :> IView ]
    @ [ for k in 1 .. circles -> innerCircle k ]
    @ [ for i in 0 .. radials - 1 -> meridian (2.0 * System.Math.PI * float i / float radials) ]

/// A flat element as a 3-D CYLINDER (axis = blue N1): two FLAT caps joined by the side rails.
let private cylinderViews (cfg : RendererControls.State) (project : Vector3 -> ScreenPoint) (selected : bool) (e : Drawable) : IView list =
    let (n1, n2, n3) = orientedBasis e.placement
    let c = e.centre
    let halfLen = e.zoom * (e.placement.box.b / 2.0 / 1.0<meter>)
    let r = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let sc = strokeOf selected
    let w = if selected then 2.5 else 1.5
    let unbound = isUnbound e
    [ capPolygon (capPts project n1 n2 n3 c (-halfLen) r) plateColor 0.0 sc (w * 0.7) unbound ]
    @ railViews project n1 n2 n3 c halfLen (-halfLen) r cfg.rails cfg.railOpacity (w * 0.7)
    @ [ capPolygon (capPts project n1 n2 n3 c halfLen r) (kindColor e.placement.catalogueKind) cfg.faceOpacity sc w unbound ]

/// A lens — the cylinder with two SPHERICAL caps that FIT THE BOUNDING BOX (biconvex apexes out to the box
/// faces for a converging sign, biconcave rims at the faces for diverging).
let private lensViews (cfg : RendererControls.State) (project : Vector3 -> ScreenPoint) (selected : bool) (e : Drawable) : IView list =
    let (n1, n2, n3) = orientedBasis e.placement
    let c = e.centre
    let halfLen = e.zoom * (e.placement.box.b / 2.0 / 1.0<meter>)
    let r = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let sag = 0.7 * halfLen
    let convex = e.opticalSign >= 0
    let rimD, apexD = if convex then (halfLen - sag, halfLen) else (halfLen, halfLen - sag)
    let sc = strokeOf selected
    let w = if selected then 2.5 else 1.5
    let kc = kindColor Lens
    capSurface project n1 n2 n3 c r rimD apexD cfg.circles cfg.radials kc sc cfg.faceOpacity cfg.lineOpacity w
    @ capSurface project n1 n2 n3 c r (-rimD) (-apexD) cfg.circles cfg.radials kc sc cfg.faceOpacity cfg.lineOpacity w
    @ railViews project n1 n2 n3 c rimD (-rimD) r cfg.rails cfg.railOpacity (w * 0.7)

/// A curved mirror — a cylinder with ONE spherical reflective cap (concave for converging, convex for
/// diverging) plus a flat back, kept inside the bounding box.
let private mirrorViews (cfg : RendererControls.State) (project : Vector3 -> ScreenPoint) (selected : bool) (e : Drawable) : IView list =
    let (n1, n2, n3) = orientedBasis e.placement
    let c = e.centre
    let halfLen = e.zoom * (e.placement.box.b / 2.0 / 1.0<meter>)
    let r = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let sag = 0.7 * halfLen
    let concave = e.opticalSign >= 0
    let rimD, apexD = if concave then (halfLen, halfLen - sag) else (halfLen - sag, halfLen)
    let sc = strokeOf selected
    let w = if selected then 2.5 else 1.5
    let kc = kindColor CurvedMirror
    [ Polygon.create [ Polygon.points (capPts project n1 n2 n3 c (-halfLen) r); Polygon.fill (brushA 0.0 kc); Polygon.stroke (brush sc); Polygon.strokeThickness (w * 0.7) ] :> IView ]
    @ railViews project n1 n2 n3 c rimD (-halfLen) r cfg.rails cfg.railOpacity (w * 0.7)
    @ capSurface project n1 n2 n3 c r rimD apexD cfg.circles cfg.radials kc sc cfg.faceOpacity cfg.lineOpacity w

let private codeLabel (project : Vector3 -> ScreenPoint) (selected : bool) (e : Drawable) : IView =
    let c = project e.centre
    TextBlock.create [
        TextBlock.left (c.sx + 8.0)
        TextBlock.top (c.sy - 26.0)
        TextBlock.text (Catalogue.kindCode e.placement.catalogueKind)
        TextBlock.fontWeight (if selected then FontWeight.Bold else FontWeight.SemiBold)
        TextBlock.foreground (brush codeColor)
    ] :> IView

/// The element's oriented bounding box corners (for the wireframe renderer), table-frame metres.
let elementCorners (e : Drawable) : Vector3 list =
    let (n1, n2, n3) = orientedBasis e.placement
    let c = e.centre
    let hb = e.zoom * (e.placement.box.b / 2.0 / 1.0<meter>)
    let h1 = e.zoom * (e.placement.box.a1 / 2.0 / 1.0<meter>)
    let h2 = e.zoom * (e.placement.box.a2 / 2.0 / 1.0<meter>)
    let corner (sb : float) (s1 : float) (s2 : float) : Vector3 =
        Vector3.create
            (c.x + sb * hb * n1.x + s1 * h1 * n2.x + s2 * h2 * n3.x)
            (c.y + sb * hb * n1.y + s1 * h1 * n2.y + s2 * h2 * n3.y)
            (c.z + sb * hb * n1.z + s1 * h1 * n2.z + s2 * h2 * n3.z)
    [ corner 1.0 -1.0 -1.0; corner 1.0 -1.0 1.0; corner 1.0 1.0 1.0; corner 1.0 1.0 -1.0
      corner -1.0 -1.0 -1.0; corner -1.0 -1.0 1.0; corner -1.0 1.0 1.0; corner -1.0 1.0 -1.0 ]

/// Today's look — the projected bounding-box edges plus the N1 / N2 normals.
let wireframeRenderer : ElementRenderer =
    {
        name = "Wireframe"
        draw =
            fun project selected e ->
                let corners = elementCorners e |> List.map (project >> toPoint) |> List.toArray
                let boxColor = if selected then selectedStroke else elementColor
                let weight = if selected then 2.5 else 1.0
                // No fill in the wireframe look, so the unbound cue is the dashed box edges alone (a PATTERN
                // difference); the always-solid N1 / N2 normals stay solid so the orientation reads cleanly.
                let unbound = isUnbound e
                let edges =
                    TableView.plateEdges
                    |> List.map (fun (a, b) ->
                        Line.create
                            [ Line.startPoint corners.[a]
                              Line.endPoint corners.[b]
                              Line.stroke (brush boxColor)
                              Line.strokeThickness weight
                              if unbound then Line.strokeDashArray (AvaloniaList<float>(unboundDash)) ] :> IView)
                edges @ normalsViews project selected e
    }

/// The nicer look — cylinders / spherical caps coloured by nature, with the element code.
let shapeRenderer (cfg : RendererControls.State) : ElementRenderer =
    {
        name = "Shapes + codes"
        draw =
            fun project selected e ->
                let body =
                    match e.placement.catalogueKind with
                    | Lens -> lensViews cfg project selected e
                    | CurvedMirror -> mirrorViews cfg project selected e
                    | _ -> cylinderViews cfg project selected e
                body @ normalsViews project selected e @ [ codeLabel project selected e ]
    }

/// Recover the draw function from the serializable renderer DU + config.
let rendererOf (cfg : RendererControls.State) : ElementRenderer =
    match cfg.kind with
    | RendererControls.Wireframe -> wireframeRenderer
    | RendererControls.Shapes -> shapeRenderer cfg
