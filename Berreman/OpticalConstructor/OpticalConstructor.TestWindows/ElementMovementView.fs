/// The element-movement test window (Spec 0027, task 010): a SINGLE element in the middle of the
/// table that the user slides along the central ray, to work out the movement quirks before the main
/// screen supports it. Movement follows the spec's "slide along the ray" gesture (§E.4): a left-drag
/// on the element slides it along the beam, and ←/→ nudge it (Shift = a larger step). The view is a
/// fixed top-down projection (movement, not rotation, is the point here), so the screen→table inverse
/// is the straight `(sx - centre)/pixelsPerMeter`. The `Model`/`Msg`/`update` are pure and Avalonia-free.
module OpticalConstructor.TestWindows.ElementMovementView

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

/// Whether a left-drag that began on the element is sliding it.
type ElementMovementDragState =
    | ElementSliding
    | ElementNotSliding

type ElementMovementModel =
    {
        table : OpticalTable
        view : TableViewState        // fixed top-down (movement test, not rotation)
        element : ElementPlacement   // the single element the test slides
        dragging : ElementMovementDragState
    }

/// A single sample element in the MIDDLE of the table — the position the test slides.
let initElementMovement () : ElementMovementModel =
    {
        table = Table.defaultTable
        view = Table.defaultView
        element = ElementPlacement.create Sample { x = 0.0<meter>; y = 0.0<meter> }
        dragging = ElementNotSliding
    }

type ElementMovementMsg =
    | SlideTo of BarMeters        // set the element's along-beam position x (table metres)
    | SlideBy of BarMeters        // nudge x by a delta (the arrow keys)
    | BeginDrag of ScreenPoint
    | DragTo of ScreenPoint
    | EndDrag
    | ResetPosition

/// A press this close to the element's projected centre grabs it.
let private selectRadiusPx : CanvasLength = CanvasLength 70.0

type ElementMovementModel with

    /// The element's along-beam position (its table x).
    member model.elementX : BarMeters = BarMeters (model.element.placementPoint.x / 1.0<meter>)

    /// The element slides within ± half the plate.
    member internal model.slideRange : RayPositionControls.PositionRange =
        let half = model.table.length / 2.0 / 1.0<meter>
        RayPositionControls.BoundedRange { lowest = BarMeters -half; highest = BarMeters half }

    /// The element moved to along-beam `x`, clamped to the plate; y stays 0 (it rides the central ray).
    member internal model.slidTo (x : BarMeters) : ElementMovementModel =
        let clamped = model.slideRange.clampedPosition x
        { model with element = { model.element with placementPoint = { model.element.placementPoint with x = clamped.value * 1.0<meter> } } }

    member internal model.elementCentreScreen : ScreenPoint = TableScene.projectOnScene model.view model.element.placementPoint.toVector3

let updateElementMovement (msg : ElementMovementMsg) (model : ElementMovementModel) : ElementMovementModel =
    match msg with
    | SlideTo x -> model.slidTo x
    | SlideBy dx -> model.slidTo (BarMeters (model.elementX.value + dx.value))
    // A press starts a drag ONLY when it lands on the element; then the element follows the pointer's x.
    | BeginDrag sp -> if sp.distanceTo model.elementCentreScreen <= selectRadiusPx then { model with dragging = ElementSliding } else model
    | DragTo sp ->
        match model.dragging with
        // Screen x → table x for the fixed top-down view (no pan / zoom): the inverse of `TableScene.projectOnScene`.
        | ElementSliding -> model.slidTo (BarMeters ((sp.sx - TableScene.center.sx) / TableScene.pixelsPerMeter.value))
        | ElementNotSliding -> model
    | EndDrag -> { model with dragging = ElementNotSliding }
    | ResetPosition -> model.slidTo (BarMeters 0.0)

// ---------------------------------------------------------------------------
// Colours + the FuncUI view.
// ---------------------------------------------------------------------------

/// The element's fill: (90, 140, 200) at 70 % opacity.
let private elementFillColor : Color = Color.FromArgb(178uy, 90uy, 140uy, 200uy)
let private elementStrokeColor : Color = Color.FromRgb(0uy, 60uy, 160uy)
let private hintColor : Color = Color.FromRgb(100uy, 100uy, 100uy)
let private canvasBackgroundColor : Color = Color.FromRgb(250uy, 250uy, 250uy)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

type ElementMovementModel with

    /// The element's top-down footprint (depth `b` along the beam × face `a2` across it, magnified by the
    /// default draw zoom), as a filled rectangle that highlights while dragging.
    member internal model.elementView : IView =
        let cx = model.elementX.value
        let hb = defaultElementZoom.value * (model.element.box.b / 2.0 / 1.0<meter>)
        let ha = defaultElementZoom.value * (model.element.box.a2 / 2.0 / 1.0<meter>)
        let corners =
            [ (cx - hb, -ha); (cx + hb, -ha); (cx + hb, ha); (cx - hb, ha) ]
            |> List.map (fun (x, y) -> (TableScene.projectOnScene model.view (Vector3.create x y 0.0)).toPoint)
        Polygon.create [
            Polygon.points corners
            Polygon.fill (brush elementFillColor)
            Polygon.stroke (brush elementStrokeColor)
            Polygon.strokeThickness (match model.dragging with ElementSliding -> 3.0 | ElementNotSliding -> 2.0)
        ] :> IView

    member internal model.tableCanvas : IView =
        Canvas.create [
            Canvas.name ConstructorUiId.elementMovementCanvas.value
            Canvas.width TableScene.canvasWidth.value
            Canvas.height TableScene.canvasHeight.value
            Canvas.horizontalAlignment HorizontalAlignment.Left
            Canvas.verticalAlignment VerticalAlignment.Top
            Canvas.children (TableScene.plateViews model.view model.table PlateNotSelected @ TableScene.sourceDetectorRayViews model.view @ [ model.elementView ])
        ] :> IView

/// The control bar IS the shared `RayPositionControls` (the standardized position-on-the-ray control),
/// plus the element's name and a usage hint. The bar's − / + / field / Reset map straight onto the
/// pure `SlideBy` / `SlideTo` / `ResetPosition` messages; the host owns the clamp (±half the plate).
let private controlBar (model : ElementMovementModel) (dispatch : ElementMovementMsg -> unit) : IView =
    let state : RayPositionControls.RayPositionControlsState =
        { position = model.elementX; range = model.slideRange; availability = ControlOffered }
    let handlers : RayPositionControls.RayPositionControlsHandlers =
        {
            moveBy = fun dx -> dispatch (SlideBy dx)
            setPosition = fun x -> dispatch (SlideTo x)
            reset = fun () -> dispatch ResetPosition
        }
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.spacing 10.0
                StackPanel.children [
                    RayPositionControls.rayPositionControlsView state handlers
                    TextBlock.create [ TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.text model.element.catalogueKind.kindName ]
                ]
            ]
            TextBlock.create [
                TextBlock.foreground (brush hintColor)
                TextBlock.text "drag the element to slide it along the beam · ←/→ nudge it (Shift = larger step) · it stays on the central ray"
            ]
        ]
    ] :> IView

let elementMovementView (model : ElementMovementModel) (dispatch : ElementMovementMsg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint ConstructorUiId.elementMovementCanvas e
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush canvasBackgroundColor)
                Border.focusable true
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (BeginDrag (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (DragTo (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch EndDrag)
                Border.onKeyDown (fun e ->
                    // The ray-position bar's own step: 0.05 m, Shift = 0.20 m.
                    let step = RayPositionControls.stepMeters e.KeyModifiers
                    match e.Key with
                    | Key.Left -> e.Handled <- true; dispatch (SlideBy (step.inDirection StepDecrease))
                    | Key.Right -> e.Handled <- true; dispatch (SlideBy (step.inDirection StepIncrease))
                    | _ -> ())
                Border.child model.tableCanvas
            ]
        ]
    ] :> IView
