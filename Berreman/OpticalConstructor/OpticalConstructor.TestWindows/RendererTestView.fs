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
open OpticalConstructor.Ui
open OpticalConstructor.Ui.TableAndElementRotationView

// ---------------------------------------------------------------------------
// Wheel gesture map (the rotations act on the selection; a plain / Ctrl wheel zooms the table).
// ---------------------------------------------------------------------------

type RendererTestWheelModifier =
    | RendererTestModCtrl
    | RendererTestModShift
    | RendererTestModAlt

type RendererTestWheelAction =
    | RendererTestRotateSel1
    | RendererTestRotateSel2
    | RendererTestRotateSel3
    | RendererTestZoomTable
    | RendererTestNoWheelAction

let rendererTestWheelAction (mods : Set<RendererTestWheelModifier>) : RendererTestWheelAction =
    let has m = Set.contains m mods
    match has RendererTestModCtrl, has RendererTestModShift, has RendererTestModAlt with
    | false, true, false -> RendererTestRotateSel1
    | true, true, false -> RendererTestRotateSel2
    | false, false, true -> RendererTestRotateSel3
    | false, false, false -> RendererTestZoomTable
    | true, false, false -> RendererTestZoomTable
    | _ -> RendererTestNoWheelAction

// ---------------------------------------------------------------------------
// Pure model.
// ---------------------------------------------------------------------------

type RendererTestDragState =
    | RendererTestNotPressed
    | RendererTestPressed of ScreenPoint
    | RendererTestPanning of ScreenPoint

type RendererTestModel =
    {
        table : OpticalTable
        view : TableViewState
        /// The drawn elements (the SHARED `ElementRenderer.Drawable`: placement + draw zoom + optical sign).
        elements : ElementRenderer.Drawable list
        /// The selected element (drawn highlighted), or none. Clicking selects the nearest element.
        selected : SceneElementIndex option
        drag : RendererTestDragState
        /// The shared renderer "large control" config (wireframe ⇄ shapes, cap detail, transparency).
        render : RendererControls.RendererControlsState
    }

let private elementSelectRadiusPx : CanvasLength = CanvasLength 50.0
let private dragThresholdPx : CanvasLength = CanvasLength 3.0

/// One light source, a couple of optical elements, and a detector along the central ray, so the codes and
/// shapes for several kinds are visible at once. R3 is UNLOCKED here so you can tip elements out of the
/// plane and watch the shape renderer open up. The linear polarizer starts selected.
let initRendererTest () : RendererTestModel =
    let mk (x : float<meter>) (kind : CatalogueKind) (sign : OpticalSign) : ElementRenderer.Drawable =
        let placement = { ElementPlacement.create kind { x = x; y = 0.0<meter> } with r3Locked = RotationFree }
        { placement = placement
          centre = placement.placementPoint.toVector3
          zoom = BeamLayout.ElementZoom 3.5
          opticalSign = sign
          normals = ElementRenderer.ShowNormals }
    {
        table = Table.defaultTable
        view = Table.defaultView
        // A source and a flat polarizer / detector (cylinders), a convex + a concave lens, and a concave
        // (focusing) + a convex curved mirror — spread along the beam, so both cap signs are visible.
        elements =
            [ mk -0.9<meter> LightSource FlatSign
              mk -0.6<meter> LinearPolarizer FlatSign
              mk -0.3<meter> Lens ConvergingSign
              mk 0.0<meter> Lens DivergingSign
              mk 0.3<meter> CurvedMirror ConvergingSign
              mk 0.6<meter> CurvedMirror DivergingSign
              mk 0.9<meter> Detector FlatSign ]
        selected = Some (SceneElementIndex 1)
        drag = RendererTestNotPressed
        render = RendererControls.defaultState
    }

type RendererTestMsg =
    /// Swap the active renderer (Wireframe ⇄ Shapes).
    | SwapRenderer
    /// The shared renderer control's other knobs.
    | RendererTestSetRailsIndex of RendererControls.RendererPresetIndex
    | RendererTestSetCircles of RendererControls.CapCircles
    | RendererTestSetRadialsIndex of RendererControls.RendererPresetIndex
    | RendererTestSetRailOpacity of RendererControls.RendererOpacity
    | RendererTestSetFaceOpacity of RendererControls.RendererOpacity
    | RendererTestSetLineOpacity of RendererControls.RendererOpacity
    | RendererTestPointerDown of ScreenPoint
    | RendererTestPointerMove of ScreenPoint
    | RendererTestPointerUp of ScreenPoint
    | RendererTestWheel of Set<RendererTestWheelModifier> * WheelNotches

// ---------------------------------------------------------------------------
// Selection + gestures.
// ---------------------------------------------------------------------------

type RendererTestModel with

    /// The elements, each with its index.
    member model.indexedElements : (SceneElementIndex * ElementRenderer.Drawable) list =
        model.elements |> List.mapi (fun i e -> SceneElementIndex i, e)

    /// The element whose projected centre is nearest `pt`, if it lies within the select radius.
    member internal model.nearestElement (pt : ScreenPoint) : SceneElementIndex option =
        model.indexedElements
        |> List.map (fun (i, e) -> i, pt.distanceTo (TableScene.projectOnScene model.view e.placement.placementPoint.toVector3))
        |> List.sortBy snd
        |> List.tryHead
        |> Option.bind (fun (i, d) -> if d <= elementSelectRadiusPx then Some i else None)

let private mapElement (i : SceneElementIndex) (f : ElementRenderer.Drawable -> ElementRenderer.Drawable) (m : RendererTestModel) : RendererTestModel =
    { m with elements = m.indexedElements |> List.map (fun (j, e) -> if j = i then f e else e) }

let private panFrom (refPt : ScreenPoint) (pt : ScreenPoint) (m : RendererTestModel) : RendererTestModel =
    { m with view = TableScene.panViewBy refPt pt m.view; drag = RendererTestPanning pt }

/// Rotate the SELECTION about `axis` by `a`: the selected element, or the table view.
let private rotateSelected (axis : RotationControls.RotationAxis) (a : Angle) (m : RendererTestModel) : RendererTestModel =
    match m.selected with
    | Some i ->
        mapElement i (fun e ->
            let p = e.placement
            let turned =
                match axis with
                | RotationControls.R1 -> withR1 (p.r1 + a) p
                | RotationControls.R2 -> withR2 (p.r2 + a) p
                | RotationControls.R3 -> withR3 (p.r3 + a) p
            { e with placement = turned }) m
    | None -> { m with view = TableScene.rotateViewBy axis a m.view }

let updateRendererTest (msg : RendererTestMsg) (model : RendererTestModel) : RendererTestModel =
    match msg with
    | SwapRenderer -> { model with render = model.render.swapped }
    | RendererTestSetRailsIndex i -> { model with render = model.render.withRailsIndex i }
    | RendererTestSetCircles n -> { model with render = model.render.withCircles n }
    | RendererTestSetRadialsIndex i -> { model with render = model.render.withRadialsIndex i }
    | RendererTestSetRailOpacity v -> { model with render = model.render.withRailOpacity v }
    | RendererTestSetFaceOpacity v -> { model with render = model.render.withFaceOpacity v }
    | RendererTestSetLineOpacity v -> { model with render = model.render.withLineOpacity v }
    | RendererTestPointerDown pt -> { model with drag = RendererTestPressed pt }
    | RendererTestPointerMove pt ->
        match model.drag with
        | RendererTestNotPressed -> model
        | RendererTestPressed start -> if start.distanceTo pt < dragThresholdPx then model else panFrom start pt model
        | RendererTestPanning last -> panFrom last pt model
    | RendererTestPointerUp _ ->
        // A clean click (no drag) selects the nearest element (or deselects); a pan does not.
        let model' =
            match model.drag with
            | RendererTestPressed start -> { model with selected = model.nearestElement start }
            | _ -> model
        { model' with drag = RendererTestNotPressed }
    | RendererTestWheel (mods, notches) ->
        // The Main screen's 5° per notch, added unwrapped (as the table view's own rotation adds it).
        let a = Angle.degree ((notches.turnedBy wheelStepDegrees).value * 1.0<deg>)
        match rendererTestWheelAction mods with
        | RendererTestRotateSel1 -> rotateSelected RotationControls.R1 a model
        | RendererTestRotateSel2 -> rotateSelected RotationControls.R2 a model
        | RendererTestRotateSel3 -> rotateSelected RotationControls.R3 a model
        | RendererTestZoomTable -> { model with view = TableScene.zoomViewBy notches model.view }
        | RendererTestNoWheelAction -> model

// ---------------------------------------------------------------------------
// The FuncUI view.
// ---------------------------------------------------------------------------

let private hintColor : Color = Color.FromRgb(100uy, 100uy, 100uy)
let private canvasBackgroundColor : Color = Color.FromRgb(250uy, 250uy, 250uy)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush

type RendererTestModel with

    /// The table plate (highlighted when the table is the rotation target — nothing selected), the ray,
    /// and the elements drawn through the SHARED renderer (the only one that draws is the chosen one).
    member internal model.tableCanvas : IView =
        let plate = match model.selected with None -> PlateSelected | Some _ -> PlateNotSelected
        let renderer = ElementRenderer.rendererOf model.render
        let elementViews =
            model.indexedElements
            |> List.collect (fun (i, e) ->
                let highlight = if Some i = model.selected then ElementRenderer.ElementHighlighted else ElementRenderer.ElementNotHighlighted
                renderer.draw (TableScene.projectOnScene model.view) highlight e)
        Canvas.create [
            Canvas.name ConstructorUiId.rendererTestCanvas.value
            Canvas.width TableScene.canvasWidth.value
            Canvas.height TableScene.canvasHeight.value
            Canvas.horizontalAlignment HorizontalAlignment.Left
            Canvas.verticalAlignment VerticalAlignment.Top
            Canvas.children (TableScene.plateViews model.view model.table plate @ TableScene.sourceDetectorRayViews model.view @ elementViews)
        ] :> IView

let private renderHandlers (dispatch : RendererTestMsg -> unit) : RendererControls.RendererControlsHandlers =
    {
        swap = fun () -> dispatch SwapRenderer
        setRailsIndex = fun i -> dispatch (RendererTestSetRailsIndex i)
        setCircles = fun n -> dispatch (RendererTestSetCircles n)
        setRadialsIndex = fun i -> dispatch (RendererTestSetRadialsIndex i)
        setRailOpacity = fun v -> dispatch (RendererTestSetRailOpacity v)
        setFaceOpacity = fun v -> dispatch (RendererTestSetFaceOpacity v)
        setLineOpacity = fun v -> dispatch (RendererTestSetLineOpacity v)
    }

let private controlBar (model : RendererTestModel) (dispatch : RendererTestMsg -> unit) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 6.0
        StackPanel.margin (Thickness 8.0)
        StackPanel.children [
            RendererControls.rendererControlsView model.render (renderHandlers dispatch)
            TextBlock.create [
                TextBlock.foreground (brush hintColor)
                TextBlock.text "click an element to select it · drag = pan · wheel = zoom · Shift/Ctrl+Shift/Alt+wheel = R1/R2/R3 of the selection · the RED seam rail tracks the roll about R1 · L/CM caps are spherical (+/- sign)"
            ]
        ]
    ] :> IView

let private wheelModifiers (km : KeyModifiers) : Set<RendererTestWheelModifier> =
    [ if km.HasFlag KeyModifiers.Control then RendererTestModCtrl
      if km.HasFlag KeyModifiers.Shift then RendererTestModShift
      if km.HasFlag KeyModifiers.Alt then RendererTestModAlt ]
    |> Set.ofList

let rendererTestView (model : RendererTestModel) (dispatch : RendererTestMsg -> unit) : IView =
    let toScreen (e : PointerEventArgs) : ScreenPoint = SceneInput.canvasPoint ConstructorUiId.rendererTestCanvas e
    DockPanel.create [
        DockPanel.children [
            Border.create [ Border.dock Dock.Top; Border.child (controlBar model dispatch) ]
            Border.create [
                Border.background (brush canvasBackgroundColor)
                Border.onPointerPressed (fun e -> e.Handled <- true; dispatch (RendererTestPointerDown (toScreen e)))
                Border.onPointerMoved (fun e -> e.Handled <- true; dispatch (RendererTestPointerMove (toScreen e)))
                Border.onPointerReleased (fun e -> e.Handled <- true; dispatch (RendererTestPointerUp (toScreen e)))
                Border.onPointerWheelChanged (fun e ->
                    e.Handled <- true
                    dispatch (RendererTestWheel (wheelModifiers e.KeyModifiers, WheelNotches (if e.Delta.Y >= 0.0 then 1 else -1))))
                Border.child model.tableCanvas
            ]
        ]
    ] :> IView
