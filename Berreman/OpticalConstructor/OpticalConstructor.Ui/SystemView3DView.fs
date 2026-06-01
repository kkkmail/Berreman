/// 2-D orthographic system view (spec 0024 Part U8 / R-4, AC-U8.4). Renders the
/// frozen `SystemView3D` geometry — `placeElements` (the to-scale element boxes along
/// the beam path) and `beamSegments` (the reflected/transmitted rays read from the
/// already-solved per-node `EmFieldSystem`) — as a 2-D orthographic *top view* on the
/// slice-002 `ChartHosts.canvasHost`. A real OpenTK GL viewport is an OPTIONAL
/// follow-up that MUST NOT block U8 (a GL surface cannot render headlessly), so the
/// Canvas projection keeps the `ui-smoke` gate headless-green.
///
/// This view recomputes NO geometry and NEVER re-solves: `placeElements`/`beamSegments`
/// are the frozen seams, and the beam directions are read from the construction page's
/// already-solved `results` map (`construction.results`, keyed by node path) — the
/// `EmFieldSystem`s Part U2 produced. The orthographic projection maps the viewport
/// `Vec3` onto the canvas at the render boundary only (§A.3): horizontal = beam-path
/// (`z`), vertical = transverse (`x`); no projected value is written back into a model.
///
/// New sibling `*View.fs` module per §0.1; authored against the public MIT
/// `Avalonia.FuncUI` 1.6.0 DSL surface (§0.2) — no clone reference; the Canvas host is
/// the public-Avalonia primitive (§0.3), no GL.
module OpticalConstructor.Ui.SystemView3DView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open OpticalConstructor.Domain.BeamTree

// ---------------------------------------------------------------------------
// Display-layout constants (pure, render-boundary only — §A.3). `pixelsPerMeter`
// keeps the inter-element gaps a few dozen pixels apart; a finite-film extent is
// floored to a visible minimum box width (the films are sub-pixel at this scale).
// ---------------------------------------------------------------------------

[<Literal>]
let private pixelsPerMeter = 16000.0
[<Literal>]
let private elementHeight = 50.0
[<Literal>]
let private marginX = 30.0
[<Literal>]
let private baseY = 70.0
[<Literal>]
let private rayDrawLen = 36.0
[<Literal>]
let private minBoxWidth = 5.0

/// Node paths in `placeElements`' traversal order — pre-order, children in the
/// `Map.toList` order `placeElements` folds over — so the i-th placed element pairs
/// with the i-th path and its solved `EmFieldSystem` is looked up by that path.
let rec private preorderPaths (path : ConstructionPage.NodePath) (node : BeamNode) : ConstructionPage.NodePath list =
    path :: (node.children |> Map.toList |> List.collect (fun (b, c) -> preorderPaths (path @ [ b ]) c))

/// Orthographic top-view projection at the render boundary: horizontal = beam-path
/// `z`, vertical = transverse `x` (centred on the baseline). Pure — returns fresh
/// canvas coordinates, never written back into the model.
let private projectX (v : SystemView3D.Vec3) : float = marginX + v.z
let private projectY (v : SystemView3D.Vec3) : float = baseY - v.x

let private elementBox (pe : SystemView3D.PlacedElement) : IView =
    let w = max pe.extent minBoxWidth
    let cx = projectX pe.position
    Rectangle.create [
        Rectangle.left (cx - w / 2.0)
        Rectangle.top (baseY - elementHeight / 2.0)
        Rectangle.width w
        Rectangle.height elementHeight
        Rectangle.fill (Brushes.SteelBlue :> IBrush)
        Rectangle.stroke (Brushes.Black :> IBrush)
        Rectangle.strokeThickness 0.5
    ] :> IView

let private beamLine (seg : SystemView3D.BeamSegment) : IView =
    let x0 = projectX seg.origin
    let y0 = projectY seg.origin
    // The direction is a unit vector (no length to scale); project its (z, x) delta
    // and draw a fixed-length ray segment in canvas space.
    let x1 = x0 + seg.direction.z * rayDrawLen
    let y1 = y0 - seg.direction.x * rayDrawLen
    let brush =
        match seg.branch with
        | Reflected -> Brushes.Goldenrod :> IBrush
        | _ -> Brushes.OrangeRed :> IBrush
    Line.create [
        Line.startPoint (Point(x0, y0))
        Line.endPoint (Point(x1, y1))
        Line.stroke brush
        Line.strokeThickness 1.5
    ] :> IView

/// The 2-D orthographic system view (R-4 / AC-U8.4): the placed element boxes plus the
/// per-node beam segments, hosted on the slice-002 Canvas host. Consumes the construction
/// page's already-solved `results` — no re-solve, no GL.
let systemPanel (construction : ConstructionPage.Model) : IView =
    let tree = construction.project.beamTree
    let placed = SystemView3D.placeElements pixelsPerMeter tree
    let paths = preorderPaths [] tree.root
    let pairs = List.zip placed paths
    let boxes = placed |> List.map elementBox
    let beams =
        pairs
        |> List.collect (fun (pe, path) ->
            match Map.tryFind path construction.results with
            | Some ems -> SystemView3D.beamSegments pe.position ems |> List.map beamLine
            | None -> [])
    let maxX =
        match placed |> List.map (fun pe -> projectX pe.position) with
        | [] -> marginX
        | xs -> List.max xs
    let canvasWidth = maxX + rayDrawLen + marginX
    let section =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 2.0
            StackPanel.margin 4.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "3-D system view"; TextBlock.fontWeight FontWeight.Bold ]
                Border.create [
                    Border.width canvasWidth
                    Border.height (baseY + elementHeight)
                    Border.child (ChartHosts.canvasHost (boxes @ beams))
                ]
            ]
        ]
    ScrollViewer.create [ ScrollViewer.content section ] :> IView
