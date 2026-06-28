/// A diagnostic test window (Spec 0027): draw the optical elements through a SWAPPABLE renderer, to iron
/// out renderer quirks. After task 018 this screen is THIN — the renderer itself (the config DU + the
/// "how it looks" knobs + the draw functions) lives in the SHARED `RendererControls` (the control) and
/// `ElementRenderer` (the drawing), so the renderer test and the Main screen use ONE renderer and a change
/// here shows up on both. This module is just the diagnostic SCENE: a rotatable / pannable / zoomable
/// table, a seed of elements, and the shared renderer control. `Model`/`Msg`/`update` are pure.
module OpticalConstructor.TestWindows.RendererTestView

open Avalonia
open Avalonia.Controls
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
    let canvas = "RendererTestCanvas"

// ---------------------------------------------------------------------------
// Wheel gesture map (the rotations act on the selection; a plain / Ctrl wheel zooms the table).
// ---------------------------------------------------------------------------

type WheelModifier =
    | ModCtrl
    | ModShift
    | ModAlt

type WheelAction =
    | RotateSel1
    | RotateSel2
    | RotateSel3
    | ZoomTable
    | NoWheelAction

let wheelAction (mods : Set<WheelModifier>) : WheelAction =
    let has m = Set.contains m mods
    match has ModCtrl, has ModShift, has ModAlt with
    | false, true, false -> RotateSel1
    | true, true, false -> RotateSel2
    | false, false, true -> RotateSel3
    | false, false, false -> ZoomTable
    | true, false, false -> ZoomTable
    | _ -> NoWheelAction

// ---------------------------------------------------------------------------
// Pure model.
// ---------------------------------------------------------------------------

type DragState =
    | NotPressed
    | Pressed of ScreenPoint
    | Panning of ScreenPoint

type Model =
    {
        table : OpticalTable
        view : TableViewState
        /// The drawn elements (the SHARED `ElementRenderer.Drawable`: placement + draw zoom + optical sign).
        elements : ElementRenderer.Drawable list
        /// The selected element (drawn highlighted), or none. Clicking selects the nearest element.
        selected : int option
        drag : DragState
        /// The shared renderer "large control" config (wireframe ⇄ shapes, cap detail, transparency).
        render : RendererControls.State
    }

// The canvas geometry / projection are the ONE shared optical-table scene (`TableScene`).
let canvasWidth : float = TableScene.canvasWidth
let canvasHeight : float = TableScene.canvasHeight

let private elementSelectRadiusPx : float = 50.0
let private dragThresholdPx : float = 3.0
let private wheelStepDegrees : float = 5.0

/// One light source, a couple of optical elements, and a detector along the central ray, so the codes and
/// shapes for several kinds are visible at once. R3 is UNLOCKED here so you can tip elements out of the
/// plane and watch the shape renderer open up. The linear polarizer starts selected.
let init () : Model =
    let mk (x : float) (kind : CatalogueKind) (sign : int) : ElementRenderer.Drawable =
        { placement = { ElementPlacement.create kind { x = x * 1.0<meter>; y = 0.0<meter> } with r3Locked = false }
          zoom = 3.5
          opticalSign = sign }
    {
        table = Table.defaultTable
        view = Table.defaultView
        // A source and a flat polarizer / detector (cylinders), a convex + a concave lens, and a concave
        // (focusing) + a convex curved mirror — spread along the beam, so both cap signs are visible.
        elements =
            [ mk -0.9 LightSource 0
              mk -0.6 LinearPolarizer 0
              mk -0.3 Lens 1
              mk 0.0 Lens -1
              mk 0.3 CurvedMirror 1
              mk 0.6 CurvedMirror -1
              mk 0.9 Detector 0 ]
        selected = Some 1
        drag = NotPressed
        render = RendererControls.defaultState
    }

type Msg =
    /// Swap the active renderer (Wireframe ⇄ Shapes).
    | SwapRenderer
    /// The shared renderer control's other knobs.
    | RenderSetRailsIndex of int
    | RenderSetCircles of int
    | RenderSetRadialsIndex of int
    | RenderSetRailOpacity of float
    | RenderSetFaceOpacity of float
    | RenderSetLineOpacity of float
    | PointerDown of ScreenPoint
    | PointerMove of ScreenPoint
    | PointerUp of ScreenPoint
    | Wheel of Set<WheelModifier> * int

// ---------------------------------------------------------------------------
// Projection + selection + gestures.
// ---------------------------------------------------------------------------

let private projectPt (view : TableViewState) (p : Vector3) : ScreenPoint = TableScene.project view p

let private dist (a : ScreenPoint) (b : ScreenPoint) : float = sqrt ((a.sx - b.sx) ** 2.0 + (a.sy - b.sy) ** 2.0)

let private elementCentre (view : TableViewState) (e : ElementRenderer.Drawable) : ScreenPoint =
    projectPt view (Vector3.create (e.placement.placementPoint.x / 1.0<meter>) (e.placement.placementPoint.y / 1.0<meter>) 0.0)

let private selectNearest (sp : ScreenPoint) (m : Model) : int option =
    m.elements
    |> List.mapi (fun i e -> i, dist sp (elementCentre m.view e))
    |> List.sortBy snd
    |> List.tryHead
    |> Option.bind (fun (i, d) -> if d <= elementSelectRadiusPx then Some i else None)

let private mapElement (i : int) (f : ElementRenderer.Drawable -> ElementRenderer.Drawable) (m : Model) : Model =
    { m with elements = m.elements |> List.mapi (fun j e -> if j = i then f e else e) }

let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (m : Model) : Model =
    { m with view = TableScene.panViewBy (pt.sx - refPt.sx) (pt.sy - refPt.sy) m.view; drag = Panning pt }

let private zoomBy (notches : int) (m : Model) : Model = { m with view = TableScene.zoomViewBy notches m.view }

/// Rotate the SELECTION about axis (1/2/3) by `notches` steps: the selected element, or the table view.
let private rotateSelected (axis : int) (notches : int) (m : Model) : Model =
    let a = Angle.degree (wheelStepDegrees * float notches)
    let rot (r : Angle) = r + a
    match m.selected with
    | Some i ->
        mapElement i (fun e ->
            { e with placement = (match axis with 1 -> withR1 (rot e.placement.r1) | 2 -> withR2 (rot e.placement.r2) | _ -> withR3 (rot e.placement.r3)) e.placement }) m
    | None ->
        { m with view = TableScene.rotateViewBy axis (wheelStepDegrees * float notches) m.view }

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | SwapRenderer -> { model with render = RendererControls.swap model.render }
    | RenderSetRailsIndex i -> { model with render = RendererControls.withRailsIndex i model.render }
    | RenderSetCircles n -> { model with render = RendererControls.withCircles n model.render }
    | RenderSetRadialsIndex i -> { model with render = RendererControls.withRadialsIndex i model.render }
    | RenderSetRailOpacity v -> { model with render = RendererControls.withRailOpacity v model.render }
    | RenderSetFaceOpacity v -> { model with render = RendererControls.withFaceOpacity v model.render }
    | RenderSetLineOpacity v -> { model with render = RendererControls.withLineOpacity v model.render }
    | PointerDown pt -> { model with drag = Pressed pt }
    | PointerMove pt ->
        match model.drag with
        | NotPressed -> model
        | Pressed start -> if dist start pt < dragThresholdPx then model else panFrom start pt model
        | Panning last -> panFrom last pt model
    | PointerUp _ ->
        // A clean click (no drag) selects the nearest element (or deselects); a pan does not.
        let model' =
            match model.drag with
            | Pressed start -> { model with selected = selectNearest start model }
            | _ -> model
        { model' with drag = NotPressed }
    | Wheel (mods, notches) ->
        match wheelAction mods with
        | RotateSel1 -> rotateSelected 1 notches model
        | RotateSel2 -> rotateSelected 2 notches model
        | RotateSel3 -> rotateSelected 3 notches model
        | ZoomTable -> zoomBy notches model
        | NoWheelAction -> model

// ---------------------------------------------------------------------------
// The FuncUI view.
// ---------------------------------------------------------------------------

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

/// The table plate (highlighted when the table is the rotation target — nothing selected) + the ray.
let private plateViews (model : Model) : IView list =
    TableScene.plateViews model.view model.table (model.selected = None)
    @ TableScene.sourceDetectorRayViews model.view

/// The elements, drawn through the SHARED renderer (the only one that draws is the chosen one).
let private elementsViews (model : Model) : IView list =
    let renderer = ElementRenderer.rendererOf model.render
    let project = projectPt model.view
    model.elements
    |> List.mapi (fun i e -> renderer.draw project (model.selected = Some i) e)
    |> List.concat

let private tableCanvas (model : Model) : IView =
    Canvas.create [
        Canvas.name UiIds.canvas
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        Canvas.horizontalAlignment HorizontalAlignment.Left
        Canvas.verticalAlignment VerticalAlignment.Top
        Canvas.children (plateViews model @ elementsViews model)
    ] :> IView

let private renderHandlers (dispatch : Msg -> unit) : RendererControls.Handlers =
    {
        swap = fun () -> dispatch SwapRenderer
        setRailsIndex = fun i -> dispatch (RenderSetRailsIndex i)
        setCircles = fun n -> dispatch (RenderSetCircles n)
        setRadialsIndex = fun i -> dispatch (RenderSetRadialsIndex i)
        setRailOpacity = fun v -> dispatch (RenderSetRailOpacity v)
        setFaceOpacity = fun v -> dispatch (RenderSetFaceOpacity v)
        setLineOpacity = fun v -> dispatch (RenderSetLineOpacity v)
    }

let private controlBar (model : Model) (dispatch : Msg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RendererControls.view model.render (renderHandlers dispatch)
            TextBlock.create [
                TextBlock.foreground (brush (color 100 100 100))
                TextBlock.text "click an element to select it · drag = pan · wheel = zoom · Shift/Ctrl+Shift/Alt+wheel = R1/R2/R3 of the selection · the RED seam rail tracks the roll about R1 · L/CM caps are spherical (+/- sign)"
            ]
        ]
    ] :> IView

let view (model : Model) (dispatch : Msg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint UiIds.canvas e
    let wheelMods (km : KeyModifiers) : Set<WheelModifier> =
        [ if km.HasFlag KeyModifiers.Control then ModCtrl
          if km.HasFlag KeyModifiers.Shift then ModShift
          if km.HasFlag KeyModifiers.Alt then ModAlt ]
        |> Set.ofList
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush (color 250 250 250))
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (PointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (PointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (PointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    dispatch (Wheel (wheelMods e.KeyModifiers, (if e.Delta.Y >= 0.0 then 1 else -1))))
                Border.child (tableCanvas model)
            ]
        ]
    ] :> IView
