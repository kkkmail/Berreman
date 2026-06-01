/// Results panel view (spec 0024 Part U6 / R-1..R-2, AC-U6.1 / AC-U6.2). Shows the
/// schematic cross-section and supports multi-system comparison. The schematic band
/// layout, the ray geometry, and the comparison overlay all already exist and are
/// frozen (§0.1); this slice renders the bands + ray on the slice-002 Canvas host
/// (`ChartHosts.canvasHost`) and the overlay in the slice-002 AvaPlot host
/// (`ChartHosts.scottPlotHost`), and routes the workspace selection through the frozen
/// `Workspace.update`.
///
/// Like the slice-002 `ChartView.chartPanel`, the slice-003 `ConstructionView.stackPanel`,
/// the slice-004 `MaterialsView.materialsPanel`, and the slice-005 `SourceView.sourcePanel`,
/// the panel takes its sub-state (the `Workspace.Model` and the edited `SourceSpec`) plus a
/// dispatch — NOT `RootModel`, which lives in `Shell.fs` — so it composes under the root
/// without a module cycle. `Shell` passes `RootMsg.Workspace >> dispatch`.
///
/// This view recomputes NO geometry and re-implements NO workspace reducer: the schematic
/// bands come from `Schematic.layout`, the ray from `Schematic.rayGeometry`, the overlay
/// from `Workspace.renderOverlay`, and visibility/active edits route the frozen
/// `Workspace.update` (R-1 / R-2 / Non-requirements). It embeds no Canvas / `AvaPlot`
/// hosting of its own — it reuses `ChartHosts.fs` (slice 002).
///
/// New sibling `*View.fs` module per §0.1; authored against the public MIT
/// `Avalonia.FuncUI` 1.6.0 DSL surface (§0.2) — no clone reference.
module OpticalConstructor.Ui.ResultsView

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Media
open Berreman.Fields
open Berreman.FieldFunctions
open Berreman.Dispersion
open Analytics.Variables
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Ui.Charts

/// `Workspace` is both a module and the `Shell` `RootMsg` case name; the alias keeps the
/// module reachable unambiguously, mirroring `Shell.fs`.
module WS = OpticalConstructor.Ui.Workspace

// ---------------------------------------------------------------------------
// The system whose cross-section the schematic draws (R-1). The workspace owns the
// active/visible selection (§J.5), so the active index is the natural source; it falls
// back to the first system (then `None`) so the view is total over any project.
// ---------------------------------------------------------------------------

let private activeSystem (ws : WS.Model) : OpticalSystem option =
    let systems = ws.project.systems
    match ws.active with
    | Some i when i >= 0 && i < List.length systems -> Some systems.[i]
    | _ -> match systems with | s :: _ -> Some s | [] -> None

// ---------------------------------------------------------------------------
// Schematic on a Canvas (R-1 / AC-U6.1). The band layout and the ray geometry are read
// from the frozen `Schematic.layout` / `Schematic.rayGeometry` builders — NO geometry is
// recomputed here; this only projects the pure band/ray values onto Avalonia shapes and
// hosts them via the slice-002 `ChartHosts.canvasHost`.
// ---------------------------------------------------------------------------

/// A representative per-layer material key for `Schematic.layout` colouring. The
/// `OpticalSystem.films` `Layer` carries no material id, and the §A.7 per-layer
/// material-id assignment lift is a later part (same representative-inputs precedent as
/// `ChartView`'s sample sweep / slice-004's `referenceWavelength`). `colorForMaterial`
/// is total for any string, so the bands still colour deterministically.
let private materialKey (i : int) : string = sprintf "layer-%d" i

/// The drawn stack column geometry (pure layout constants — display only, §A.3).
[<Literal>]
let private bandWidth = 200.0
[<Literal>]
let private leftMargin = 80.0
/// Top padding so the upward-mirrored reflected ray stays on the canvas.
[<Literal>]
let private topPad = 60.0
/// The drawn length of each ray segment.
[<Literal>]
let private rayLength = 50.0

let private brushOf (c : Schematic.SchematicColor) : IBrush =
    SolidColorBrush(Color.FromRgb(c.red, c.green, c.blue)) :> IBrush

/// Project a `Schematic.Band` list and the ray geometry onto Canvas children (R-1).
/// One `Rectangle` per band, stacked top-to-bottom by cumulative band heights, plus the
/// incident / reflected / transmitted ray `Line`s from the top film interface.
let private schematicChildren (sys : OpticalSystem) (incidence : IncidenceAngle) : IView list =
    let bands = Schematic.layout Nanometer materialKey sys
    // Prefix sums of band heights: `tops.[i]` is the (unpadded) top of band i.
    let tops = bands |> List.scan (fun acc (b : Schematic.Band) -> acc + b.height) 0.0 |> List.toArray

    let bandViews =
        bands
        |> List.mapi (fun i (b : Schematic.Band) ->
            Rectangle.create [
                Rectangle.left leftMargin
                Rectangle.top (topPad + tops.[i])
                Rectangle.width bandWidth
                Rectangle.height b.height
                Rectangle.fill (brushOf b.color)
                Rectangle.stroke (Brushes.Black :> IBrush)
                Rectangle.strokeThickness 0.5
            ] :> IView)

    // The ray meets the stack at the top film interface (top of band 1 — the first film,
    // below the incident half-space band 0). `Schematic.rayGeometry` supplies the unit
    // direction vectors; the view only scales and positions them.
    let ray = Schematic.rayGeometry incidence
    let px = leftMargin + bandWidth / 2.0
    let py = topPad + (if tops.Length >= 2 then tops.[1] else Schematic.halfSpaceBandHeight)
    let line (x0, y0) (x1, y1) (brush : IBrush) : IView =
        Line.create [
            Line.startPoint (Point(x0, y0))
            Line.endPoint (Point(x1, y1))
            Line.stroke brush
            Line.strokeThickness 1.5
        ] :> IView
    let incidentBrush = Brushes.OrangeRed :> IBrush
    let rayViews =
        [
            // Incident ray descending toward the surface point.
            line (px - rayLength * ray.incidentDx, py - rayLength * ray.incidentDy) (px, py) incidentBrush
            // Reflected ray mirrored about the surface normal (upward).
            line (px, py) (px + rayLength * ray.reflectedDx, py + rayLength * ray.reflectedDy) (Brushes.Goldenrod :> IBrush)
            // Transmitted ray indicated continuing into the stack.
            line (px, py) (px + rayLength * ray.transmittedDx, py + rayLength * ray.transmittedDy) incidentBrush
        ]
    bandViews @ rayViews

/// The schematic section (R-1 / AC-U6.1): the band layout + ray overlay on the Canvas
/// host. A fixed canvas size that fits the whole stack so nothing is clipped; the panel
/// is scrollable so it lays out independently of the panel height.
let private schematicSection (sys : OpticalSystem) (incidence : IncidenceAngle) : IView =
    let bands = Schematic.layout Nanometer materialKey sys
    let totalHeight = bands |> List.sumBy (fun (b : Schematic.Band) -> b.height)
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.margin 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text "Schematic"; TextBlock.fontWeight FontWeight.Bold ]
            Border.create [
                Border.width (leftMargin + bandWidth + rayLength + 40.0)
                Border.height (topPad * 2.0 + totalHeight)
                Border.child (ChartHosts.canvasHost (schematicChildren sys incidence))
            ]
        ]
    ] :> IView

// ---------------------------------------------------------------------------
// Workspace multi-system comparison (R-2 / AC-U6.2). The per-system visibility/active
// controls dispatch `Workspace.Msg` (routed through the frozen `Workspace.update` in
// `Shell.update`); the overlay is built by the frozen `Workspace.renderOverlay` and
// hosted in the slice-002 AvaPlot adapter — NEVER embedding its own hosting.
// ---------------------------------------------------------------------------

/// The `OpticalSystem -> FixedInfo` lift the overlay plots each visible system under: the
/// edited source's incident light over the system's dispersive form. Same shape as
/// `SynthesisFitPage.comparisonOverlay`'s `mkInfo` — no new source store (§J.5).
let private lift (source : SourceSpec.SourceSpec) (system : OpticalSystem) : FixedInfo =
    { incidentLightInfo = source.light; opticalSystem = system.dispersive }

/// The multi-system comparison overlay hosted in the AvaPlot adapter (R-2 / AC-U6.2). The
/// `ScottPlot.Plot` is built by the frozen `Workspace.renderOverlay` (which delegates to
/// the Part H §H.4 `Plot1DView.renderComparison`) over the visible systems, rebuilt each
/// render so a visibility toggle reaches the plot; `scottPlotHost` calls `Refresh()` and
/// degrades to the §U1.8 placeholder if no native graphics surface is available. NEVER a
/// WebView2 host (Non-requirements).
let private overlayHost (source : SourceSpec.SourceSpec) (ws : WS.Model) : IView =
    let x = Analytics.StandardLightVariables.wavelength200to800Range 12
    let plot = WS.renderOverlay ChartSettings.ChartSettings.defaultValue (lift source) OpticalFunction.R x ws
    ChartHosts.scottPlotHost plot

/// One system row (R-2): a visibility toggle (`ToggleVisible i`) and a set-active button
/// (`SetActive i`), each routed through the frozen `Workspace.update`.
let private systemRow (dispatch : WS.Msg -> unit) (ws : WS.Model) (i : int) (sys : OpticalSystem) : IView =
    let visible = Set.contains i ws.visible
    let isActive = ws.active = Some i
    let name = sys.description |> Option.defaultValue (sprintf "System %d" i)
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text name; TextBlock.width 200.0; TextBlock.verticalAlignment VerticalAlignment.Center ]
            Button.create [
                Button.content (if visible then "Visible" else "Hidden")
                Button.background (if visible then Brushes.SteelBlue :> IBrush else Brushes.Transparent :> IBrush)
                Button.onClick (fun _ -> dispatch (WS.ToggleVisible i))
            ]
            Button.create [
                Button.content (if isActive then "Active" else "Set active")
                Button.background (if isActive then Brushes.SteelBlue :> IBrush else Brushes.Transparent :> IBrush)
                Button.onClick (fun _ -> dispatch (WS.SetActive i))
            ]
        ]
    ] :> IView

/// The workspace comparison section (R-2): the per-system visibility/active controls over
/// the AvaPlot-hosted overlay.
let private workspaceSection (source : SourceSpec.SourceSpec) (ws : WS.Model) (dispatch : WS.Msg -> unit) : IView =
    let rows =
        match ws.project.systems with
        | [] -> [ TextBlock.create [ TextBlock.text "(no systems to compare)" ] :> IView ]
        | systems -> systems |> List.mapi (systemRow dispatch ws)
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 4.0
        StackPanel.margin 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text "Comparison"; TextBlock.fontWeight FontWeight.Bold ]
            StackPanel.create [
                StackPanel.orientation Orientation.Vertical
                StackPanel.spacing 2.0
                StackPanel.children rows
            ]
            Border.create [ Border.height 220.0; Border.child (overlayHost source ws) ]
        ]
    ] :> IView

// ---------------------------------------------------------------------------
// The results panel (R-1 / R-2): the schematic cross-section of the active system over
// the multi-system comparison controls + overlay. Scrollable so each section lays out
// independently of the panel height.
// ---------------------------------------------------------------------------

let resultsPanel (workspace : WS.Model) (source : SourceSpec.SourceSpec) (dispatch : WS.Msg -> unit) : IView =
    let schematic =
        match activeSystem workspace with
        | Some sys -> schematicSection sys source.light.incidenceAngle
        | None -> TextBlock.create [ TextBlock.text "No system to display."; TextBlock.margin 4.0 ] :> IView
    let body =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.spacing 6.0
            StackPanel.children [
                TextBlock.create [ TextBlock.text "Results"; TextBlock.fontWeight FontWeight.Bold; TextBlock.margin 4.0 ]
                schematic
                workspaceSection source workspace dispatch
            ]
        ]
    ScrollViewer.create [ ScrollViewer.content body ] :> IView
