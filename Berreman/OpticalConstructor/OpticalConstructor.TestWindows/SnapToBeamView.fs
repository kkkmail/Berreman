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
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

type SnapToBeamModel =
    {
        table : OpticalTable
        view : TableViewState        // fixed top-down (snap test, not table rotation)
        /// The light source — rotating its R2 (in-plane steer) re-aims the beam; R3 tips it out of plane.
        /// Its own R3 lock is the one the shared bar toggles (unlocked by default).
        source : ElementPlacement
        /// The downstream elements as (kind, along-ray gap from the previous node). Untilted pass-through
        /// stops, so the beam stays a straight line at whatever angle the source aims it.
        downstream : (CatalogueKind * float<meter>) list
        rotationConfirm : RotationControls.ResetConfirm
    }

/// A light source on the left of the table plus a polarizer, a sample and a detector spaced along the
/// beam (the gaps sum to ~1.8 m, well inside the 2.0 m table).
let initSnapToBeam () : SnapToBeamModel =
    {
        table = Table.defaultTable
        view = Table.defaultView
        source = { ElementPlacement.create LightSource RayModel.defaultSourcePoint with r1Locked = RotationFree; r2Locked = RotationFree; r3Locked = RotationFree }
        downstream =
            [ LinearPolarizer, 0.6<meter>
              Sample, 0.6<meter>
              Detector, 0.6<meter> ]
        rotationConfirm = RotationControls.NoConfirm
    }

type SnapToBeamMsg =
    | RotateSourceBy of BarDegrees                                        // about the source's R2 (the steer; kept for the wheel)
    | RotateSourceAxisBy of RotationControls.RotationAxis * BarDegrees    // about one of the source's axes
    | SetSourceAxis of RotationControls.RotationAxis * BarDegrees         // set one of the source's axes to exactly N degrees
    | ToggleSourceR3Lock
    | SnapToBeamRotRequestReset
    | SnapToBeamRotRequestResetAll
    | SnapToBeamRotConfirm
    | SnapToBeamRotCancel
    | ResetSource                                 // reset ALL source rotations to zero

type SnapToBeamModel with

    /// The source's R2, the steer that aims the beam.
    member model.sourceR2 : BarDegrees = BarDegrees model.source.r2.degrees

    /// The source turned about `axis` by `d`, unwrapped; a locked axis stays put.
    member internal model.sourceTurned (axis : RotationControls.RotationAxis) (d : BarDegrees) : SnapToBeamModel =
        let s = model.source
        let step = Angle.degree (d.value * 1.0<deg>)
        let turned =
            match axis with
            | RotationControls.R1 -> withR1 (s.r1 + step) s
            | RotationControls.R2 -> withR2 (s.r2 + step) s
            | RotationControls.R3 -> withR3 (s.r3 + step) s
        { model with source = turned }

    /// The source's `axis` set to exactly `d`, unwrapped; a locked axis stays put.
    member internal model.sourceSetTo (axis : RotationControls.RotationAxis) (d : BarDegrees) : SnapToBeamModel =
        let s = model.source
        let a = Angle.degree (d.value * 1.0<deg>)
        let set =
            match axis with
            | RotationControls.R1 -> withR1 a s
            | RotationControls.R2 -> withR2 a s
            | RotationControls.R3 -> withR3 a s
        { model with source = set }

    member internal model.withSourceRotationsReset : SnapToBeamModel = { model with source = model.source.withRotationsReset }

let updateSnapToBeam (msg : SnapToBeamMsg) (model : SnapToBeamModel) : SnapToBeamModel =
    match msg with
    | RotateSourceBy d -> model.sourceTurned RotationControls.R2 d
    | RotateSourceAxisBy (axis, d) -> model.sourceTurned axis d
    | SetSourceAxis (axis, d) -> model.sourceSetTo axis d
    | ToggleSourceR3Lock -> { model with source = setR3Locked model.source.r3Locked.toggled model.source }
    | SnapToBeamRotRequestReset -> { model with rotationConfirm = RotationControls.ConfirmReset }
    | SnapToBeamRotRequestResetAll -> { model with rotationConfirm = RotationControls.ConfirmResetAll }
    | SnapToBeamRotConfirm ->
        let m =
            match model.rotationConfirm with
            | RotationControls.ConfirmReset | RotationControls.ConfirmResetAll -> model.withSourceRotationsReset
            | RotationControls.NoConfirm -> model
        { m with rotationConfirm = RotationControls.NoConfirm }
    | SnapToBeamRotCancel -> { model with rotationConfirm = RotationControls.NoConfirm }
    | ResetSource -> model.withSourceRotationsReset

// ---------------------------------------------------------------------------
// Snapping the chain onto the beam (the spec law this screen demonstrates).
// ---------------------------------------------------------------------------

type SnapToBeamModel with

    member internal model.stops : RayModel.RaySegmentSpec list =
        model.downstream
        |> List.map (fun (kind, gap) ->
            ({ placement = ElementPlacement.create kind { x = 0.0<meter>; y = 0.0<meter> }
               gap = gap
               branch = BeamTree.TransmittedBeam } : RayModel.RaySegmentSpec))

    /// The downstream elements snapped onto the beam the source currently aims (its oriented N1 = `r1Axis`).
    member model.snapped : RayModel.SnappedElement list =
        RayModel.snapChain model.source.placementPoint.toVector3 (r1Axis model.source) model.stops

    /// The cumulative along-ray distances of the downstream elements (for the readout / tests).
    member model.cumulativeGaps : float<meter> list =
        model.downstream |> List.map snd |> List.scan (+) 0.0<meter> |> List.tail

    member internal model.readoutText : string =
        let dists = model.cumulativeGaps |> List.map (fun x -> $"%.1f{x / 1.0<meter>}") |> String.concat ", "
        $"Source R2 = %+.0f{model.sourceR2.value}°   ·   %d{(List.length model.downstream)} elements snapped to the beam at %s{dists} m"

    /// The shared rotation bar, acting on the SOURCE. Its R2 is the steer that swings the beam; R1 spins the
    /// source (no beam change) and R3 tips the beam out of the table plane.
    member internal model.rotationState : RotationControls.State =
        {
            r1 = BarDegrees model.source.r1.degrees
            r2 = model.sourceR2
            r3 = BarDegrees model.source.r3.degrees
            r1Lock = RotationControls.NotLockable
            r2Lock = RotationControls.NotLockable
            r3Lock = (match model.source.r3Locked with RotationLocked -> RotationControls.AxisLocked | RotationFree -> RotationControls.AxisFree)
            availability = ControlOffered
            confirm = model.rotationConfirm
        }

// ---------------------------------------------------------------------------
// Colours + the FuncUI view.
// ---------------------------------------------------------------------------

let private edgeColor : Color = Color.FromRgb(50uy, 50uy, 50uy)
let private rayColor : Color = Color.FromRgb(30uy, 90uy, 200uy)
let private sourceColor : Color = Color.FromRgb(230uy, 170uy, 30uy)
let private elementColor : Color = Color.FromRgb(70uy, 130uy, 200uy)
let private codeColor : Color = Color.FromRgb(20uy, 20uy, 30uy)
let private hintColor : Color = Color.FromRgb(100uy, 100uy, 100uy)
let private canvasBackgroundColor : Color = Color.FromRgb(250uy, 250uy, 250uy)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

let private projectPt (m : SnapToBeamModel) (p : Vector3) : ScreenPoint = TableScene.projectOnScene m.view p

let private line (a : ScreenPoint) (b : ScreenPoint) (c : Color) (w : StrokeWidth) : IView =
    Line.create [
        Line.startPoint a.toPoint; Line.endPoint b.toPoint; Line.stroke (brush c); Line.strokeThickness w.value
    ] :> IView

type SnapToBeamModel with

    /// The source marker plus a short arrow along the direction it currently emits (its oriented N1).
    member internal m.sourceViews : IView list =
        let srcV = m.source.placementPoint.toVector3
        let dir = (r1Axis m.source).normalized
        let tip = Vector3.create (srcV.x + 0.35 * dir.x) (srcV.y + 0.35 * dir.y) (srcV.z + 0.35 * dir.z)
        let sScr = projectPt m srcV
        [ line sScr (projectPt m tip) sourceColor (StrokeWidth 3.0)
          Ellipse.create [
              Ellipse.left (sScr.sx - 7.0); Ellipse.top (sScr.sy - 7.0); Ellipse.width 14.0; Ellipse.height 14.0
              Ellipse.fill (brush sourceColor); Ellipse.stroke (brush edgeColor); Ellipse.strokeThickness 1.0
          ] :> IView ]

    /// The beam polyline (source → each snapped element) and, at each element, a tick perpendicular to the
    /// beam (the flat element face) plus its short code — all of which swing round as the source is rotated.
    member internal m.beamViews : IView list =
        let srcV = m.source.placementPoint.toVector3
        let snaps = m.snapped
        let nodes = srcV :: (snaps |> List.map (fun s -> s.position))
        let beam =
            nodes
            |> List.pairwise
            |> List.map (fun (a, b) -> line (projectPt m a) (projectPt m b) rayColor (StrokeWidth 2.0))
        let elementMarks =
            snaps
            |> List.collect (fun s ->
                let p = s.position
                let inc = s.incoming.normalized
                let perp = (Vector3.create (-inc.y) inc.x 0.0).normalized   // in-plane, perpendicular to the beam
                let r = defaultElementZoom.value * (s.placement.box.a2 / 2.0 / 1.0<meter>)
                let a = projectPt m (Vector3.create (p.x + r * perp.x) (p.y + r * perp.y) (p.z + r * perp.z))
                let b = projectPt m (Vector3.create (p.x - r * perp.x) (p.y - r * perp.y) (p.z - r * perp.z))
                let centreScr = projectPt m p
                [ line a b elementColor (StrokeWidth 4.0)
                  TextBlock.create [
                      TextBlock.left (centreScr.sx + 8.0)
                      TextBlock.top (centreScr.sy - 22.0)
                      TextBlock.text s.placement.catalogueKind.kindCode
                      TextBlock.fontWeight FontWeight.SemiBold
                      TextBlock.foreground (brush codeColor)
                  ] :> IView ])
        beam @ elementMarks

    member internal model.tableCanvas : IView =
        Canvas.create [
            Canvas.name ConstructorUiId.snapToBeamCanvas.value
            Canvas.width TableScene.canvasWidth.value
            Canvas.height TableScene.canvasHeight.value
            Canvas.horizontalAlignment HorizontalAlignment.Left
            Canvas.verticalAlignment VerticalAlignment.Top
            Canvas.children (TableScene.plateViews model.view model.table PlateNotSelected @ model.beamViews @ model.sourceViews)
        ] :> IView

let private rotationHandlers (dispatch : SnapToBeamMsg -> unit) : RotationControls.Handlers =
    {
        rotate = fun axis d -> dispatch (RotateSourceAxisBy (axis, d))
        setAngle = fun axis d -> dispatch (SetSourceAxis (axis, d))
        toggleLock = fun axis -> match axis with RotationControls.R3 -> dispatch ToggleSourceR3Lock | RotationControls.R1 | RotationControls.R2 -> ()
        requestReset = fun () -> dispatch SnapToBeamRotRequestReset
        requestResetAll = fun () -> dispatch SnapToBeamRotRequestResetAll
        confirm = fun () -> dispatch SnapToBeamRotConfirm
        cancel = fun () -> dispatch SnapToBeamRotCancel
    }

let private controlBar (model : SnapToBeamModel) (dispatch : SnapToBeamMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RotationControls.view model.rotationState (rotationHandlers dispatch)
            TextBlock.create [ TextBlock.name ConstructorUiId.snapToBeamReadout.value; TextBlock.text model.readoutText; TextBlock.verticalAlignment VerticalAlignment.Center ]
            TextBlock.create [
                TextBlock.foreground (brush hintColor)
                TextBlock.text "rotate the source with the bar (or the wheel = R2, Shift = larger step) — the downstream elements snap to the re-aimed beam, keeping their along-ray spacing"
            ]
        ]
    ] :> IView

/// The wheel's steer per notch: the Main screen's 5°, or 15° with Shift held.
let private wheelSteerDegrees (modifiers : KeyModifiers) : BarDegrees =
    if modifiers.HasFlag KeyModifiers.Shift then BarDegrees 15.0 else wheelStepDegrees

let snapToBeamView (model : SnapToBeamModel) (dispatch : SnapToBeamMsg -> unit) : IView =
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush canvasBackgroundColor)
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    let notches = WheelNotches (if e.Delta.Y >= 0.0 then 1 else -1)
                    dispatch (RotateSourceBy (notches.turnedBy (wheelSteerDegrees e.KeyModifiers))))
                Border.child model.tableCanvas
            ]
        ]
    ] :> IView
