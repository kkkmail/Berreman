/// The snap-to-beam test window (Spec 0027, task 010; standardized in task 014): a light source plus a
/// few downstream elements on the central ray. Rotating the SOURCE re-aims the beam, and every downstream
/// element SNAPS to the new beam direction, preserving its along-ray distance — exactly the
/// `RayModel.snapChain` law (B.3): each element sits a fixed gap along the CURRENT direction from the
/// previous node, so tilting the source drags the whole straight chain around. The downstream elements
/// are untilted pass-through stops (`BeamTree.Transmitted`), so the beam stays a straight line whose
/// angle is set by the source. The table is the shared `TableScene` and the rotation bar is the shared
/// `RotationControls` (acting on the source). `Model`/`Msg`/`update` are pure and Avalonia-free.
module OpticalConstructor.TestWindows.SnapToBeamView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.Table
open OpticalConstructor.Domain.TableView
open OpticalConstructor.Controls

[<RequireQualifiedAccess>]
module UiIds =
    let canvas = "SnapToBeamCanvas"
    let readout = "SnapToBeamReadout"

// The canvas geometry / projection are the ONE shared optical-table scene (`TableScene`).
let canvasWidth : float = TableScene.canvasWidth
let canvasHeight : float = TableScene.canvasHeight
let stepDegrees : float = 5.0
let bigStepDegrees : float = 15.0
let private drawZoom : float = 5.0

type Model =
    {
        table : OpticalTable
        view : TableViewState        // fixed top-down (snap test, not table rotation)
        /// The light source — rotating its R2 (in-plane steer) re-aims the beam; R3 tips it out of plane.
        source : ElementPlacement
        /// The downstream elements as (kind, along-ray gap from the previous node). Untilted pass-through
        /// stops, so the beam stays a straight line at whatever angle the source aims it.
        downstream : (CatalogueKind * float<meter>) list
        /// The source's R3 lock state (the shared bar drives it; the source is unlocked by default).
        sourceR3Locked : bool
        rotationConfirm : RotationControls.ResetConfirm
    }

/// A light source on the left of the table plus a polarizer, a sample and a detector spaced along the
/// beam (the gaps sum to ~1.8 m, well inside the 2.0 m table).
let init () : Model =
    {
        table = Table.defaultTable
        view = Table.defaultView
        source = { ElementPlacement.create LightSource RayModel.defaultSourcePoint with r3Locked = false }
        downstream =
            [ LinearPolarizer, 0.6<meter>
              Sample, 0.6<meter>
              Detector, 0.6<meter> ]
        sourceR3Locked = false
        rotationConfirm = RotationControls.NoConfirm
    }

type Msg =
    | RotateSourceBy of float                     // degrees about the source's R2 (the steer; kept for the wheel)
    | RotateSourceAxisBy of int * float           // degrees about the source's axis 1/2/3
    | SetSourceAxis of int * float                // set the source's axis 1/2/3 to exactly N degrees
    | ToggleSourceR3Lock
    | RotRequestReset
    | RotRequestResetAll
    | RotConfirm
    | RotCancel
    | ResetSource                                 // reset ALL source rotations to zero

let sourceR2Degrees (m : Model) : float = m.source.r2.degrees

let private bumpAxis (axis : int) (deg : float) (p : ElementPlacement) : ElementPlacement =
    match axis with
    | 1 -> withR1 (p.r1 + Angle.degree deg) p
    | 2 -> withR2 (p.r2 + Angle.degree deg) p
    | _ -> withR3 (p.r3 + Angle.degree deg) p

let private setAxis (axis : int) (deg : float) (p : ElementPlacement) : ElementPlacement =
    match axis with
    | 1 -> withR1 (Angle.degree deg) p
    | 2 -> withR2 (Angle.degree deg) p
    | _ -> withR3 (Angle.degree deg) p

let private resetSourceRotations (m : Model) : Model =
    { m with source = { m.source with r1 = Angle.zero; r2 = Angle.zero; r3 = Angle.zero } }

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | RotateSourceBy d -> { model with source = withR2 (model.source.r2 + Angle.degree d) model.source }
    | RotateSourceAxisBy (axis, d) ->
        if axis = 3 && model.sourceR3Locked then model
        else { model with source = bumpAxis axis d model.source }
    | SetSourceAxis (axis, v) ->
        if axis = 3 && model.sourceR3Locked then model
        else { model with source = setAxis axis v model.source }
    | ToggleSourceR3Lock -> { model with sourceR3Locked = not model.sourceR3Locked }
    | RotRequestReset -> { model with rotationConfirm = RotationControls.ConfirmReset }
    | RotRequestResetAll -> { model with rotationConfirm = RotationControls.ConfirmResetAll }
    | RotConfirm ->
        let m =
            match model.rotationConfirm with
            | RotationControls.ConfirmReset | RotationControls.ConfirmResetAll -> resetSourceRotations model
            | RotationControls.NoConfirm -> model
        { m with rotationConfirm = RotationControls.NoConfirm }
    | RotCancel -> { model with rotationConfirm = RotationControls.NoConfirm }
    | ResetSource -> resetSourceRotations model

// ---------------------------------------------------------------------------
// Snapping the chain onto the beam (the spec law this screen demonstrates).
// ---------------------------------------------------------------------------

let private stops (m : Model) : RayModel.RaySegmentSpec list =
    m.downstream
    |> List.map (fun (kind, gap) ->
        ({ placement = ElementPlacement.create kind { x = 0.0<meter>; y = 0.0<meter> }
           gap = gap
           branch = BeamTree.Transmitted } : RayModel.RaySegmentSpec))

/// The downstream elements snapped onto the beam the source currently aims (its oriented N1 = `r1Axis`).
let snapped (m : Model) : RayModel.SnappedElement list =
    RayModel.snapChain (RayModel.pointToVector3 m.source.placementPoint) (r1Axis m.source) (stops m)

/// The cumulative along-ray distances of the downstream elements (for the readout / tests).
let cumulativeGaps (m : Model) : float list =
    m.downstream |> List.map (fun (_, g) -> g / 1.0<meter>) |> List.scan (+) 0.0 |> List.tail

// ---------------------------------------------------------------------------
// Colours + the FuncUI view.
// ---------------------------------------------------------------------------

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

let private edgeColor = color 50 50 50
let private rayColor = color 30 90 200
let private sourceColor = color 230 170 30
let private elementColor = color 70 130 200
let private codeColor = color 20 20 30

let private v3 (x : float) (y : float) (z : float) : Vector3 = Vector3.create x y z
let private projectPt (m : Model) (p : Vector3) : ScreenPoint = TableScene.project m.view p

let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : float) : IView =
    Line.create [
        Line.startPoint (TableScene.toPoint a); Line.endPoint (TableScene.toPoint b); Line.stroke (brush c); Line.strokeThickness w
    ] :> IView

/// The source marker plus a short arrow along the direction it currently emits (its oriented N1).
let private sourceViews (m : Model) : IView list =
    let srcV = RayModel.pointToVector3 m.source.placementPoint
    let dir = (r1Axis m.source).normalized
    let tip = v3 (srcV.x + 0.35 * dir.x) (srcV.y + 0.35 * dir.y) (srcV.z + 0.35 * dir.z)
    let sScr = projectPt m srcV
    [ line sScr (projectPt m tip) sourceColor 3.0
      Ellipse.create [
          Ellipse.left (sScr.sx - 7.0); Ellipse.top (sScr.sy - 7.0); Ellipse.width 14.0; Ellipse.height 14.0
          Ellipse.fill (brush sourceColor); Ellipse.stroke (brush edgeColor); Ellipse.strokeThickness 1.0
      ] :> IView ]

/// The beam polyline (source → each snapped element) and, at each element, a tick perpendicular to the
/// beam (the flat element face) plus its short code — all of which swing round as the source is rotated.
let private beamViews (m : Model) : IView list =
    let srcV = RayModel.pointToVector3 m.source.placementPoint
    let snaps = snapped m
    let nodes = srcV :: (snaps |> List.map (fun s -> s.position))
    let beam =
        nodes
        |> List.pairwise
        |> List.map (fun (a, b) -> line (projectPt m a) (projectPt m b) rayColor 2.0)
    let elementMarks =
        snaps
        |> List.collect (fun s ->
            let p = s.position
            let inc = s.incoming.normalized
            let perp = (v3 (-inc.y) inc.x 0.0).normalized   // in-plane, perpendicular to the beam
            let r = drawZoom * (s.placement.box.a2 / 2.0 / 1.0<meter>)
            let a = projectPt m (v3 (p.x + r * perp.x) (p.y + r * perp.y) (p.z + r * perp.z))
            let b = projectPt m (v3 (p.x - r * perp.x) (p.y - r * perp.y) (p.z - r * perp.z))
            let centreScr = projectPt m p
            [ line a b elementColor 4.0
              TextBlock.create [
                  TextBlock.left (centreScr.sx + 8.0)
                  TextBlock.top (centreScr.sy - 22.0)
                  TextBlock.text (TableAndElementRotationView.kindCode s.placement.catalogueKind)
                  TextBlock.fontWeight FontWeight.SemiBold
                  TextBlock.foreground (brush codeColor)
              ] :> IView ])
    beam @ elementMarks

let private tableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (TableScene.plateViews model.view model.table false @ beamViews model @ sourceViews model)
    ] :> IView

let private readoutText (m : Model) : string =
    let dists = cumulativeGaps m |> List.map (sprintf "%.1f") |> String.concat ", "
    sprintf "Source R2 = %+.0f°   ·   %d elements snapped to the beam at %s m" (sourceR2Degrees m) (List.length m.downstream) dists

/// The shared rotation bar, acting on the SOURCE. Its R2 is the steer that swings the beam; R1 spins the
/// source (no beam change) and R3 tips the beam out of the table plane.
let private rotationState (model : Model) : RotationControls.State =
    {
        r1 = model.source.r1.degrees; r2 = model.source.r2.degrees; r3 = model.source.r3.degrees
        r3Locked = model.sourceR3Locked; enabled = true; confirm = model.rotationConfirm
    }

let private rotationHandlers (dispatch : Msg -> unit) : RotationControls.Handlers =
    {
        rotate = fun axis d -> dispatch (RotateSourceAxisBy ((match axis with RotationControls.R1 -> 1 | RotationControls.R2 -> 2 | RotationControls.R3 -> 3), d))
        setAngle = fun axis v -> dispatch (SetSourceAxis ((match axis with RotationControls.R1 -> 1 | RotationControls.R2 -> 2 | RotationControls.R3 -> 3), v))
        toggleR3Lock = fun () -> dispatch ToggleSourceR3Lock
        requestReset = fun () -> dispatch RotRequestReset
        requestResetAll = fun () -> dispatch RotRequestResetAll
        confirm = fun () -> dispatch RotConfirm
        cancel = fun () -> dispatch RotCancel
    }

let private controlBar (model : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RotationControls.view (rotationState model) (rotationHandlers dispatch)
            TextBlock.create [ TextBlock.name UiIds.readout; TextBlock.text (readoutText model); TextBlock.verticalAlignment VerticalAlignment.Center ]
            TextBlock.create [
                TextBlock.foreground (brush (color 100 100 100))
                TextBlock.text "rotate the source with the bar (or the wheel = R2, Shift = larger step) — the downstream elements snap to the re-aimed beam, keeping their along-ray spacing"
            ]
        ]
    ] :> IView

let view (model : Model) (dispatch : Msg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush (color 250 250 250))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    let step = if e.KeyModifiers.HasFlag KeyModifiers.Shift then bigStepDegrees else stepDegrees
                    dispatch (RotateSourceBy ((if e.Delta.Y >= 0.0 then 1.0 else -1.0) * step)))
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
