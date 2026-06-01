/// Chart panel view (spec 0024 Part U5 / R-2..R-4). The primary results surface: the
/// 1-D charts (`Plot1DView.renderSweep`/`renderComparison`/`renderFieldDepth`/
/// `renderDispersion`, all `ScottPlot.Plot`) hosted in the U5.1 `AvaPlot` host, the
/// 3-D surface (`Plot3DView.render`, `Plotly.NET.GenericChart`) in the U5.1 WebView2
/// host, plus chart-settings and on-plot cursor controls.
///
/// This module owns the chart panel's local MVU surface — `ChartMsg` + a pure
/// `update` over `(ChartSettings * Readout.Markers)` — which `Shell.update` delegates
/// to. The view takes the chart sub-state and a `ChartMsg` dispatch (NOT `RootModel`,
/// which lives in `Shell.fs`), so it composes under the root without a cycle, exactly
/// as `ConstructionView.stackPanel` takes a `ConstructionPage.Model`.
///
/// Applied settings reach a plot ONLY through the existing `ChartSettings`
/// projections: the 1-D charts are built via the `Plot1DView.*` seams (which call
/// `applyToScottPlot`) and the 3-D chart through `applyToPlotly` (R-3). This view
/// re-applies no axis/legend styling itself. New sibling `*View.fs` module (§0.1);
/// public MIT `Avalonia.FuncUI` 1.6.0 DSL only (§0.2); the `Charts/` builders are
/// frozen and only called (§0.1 / Non-requirements).
module OpticalConstructor.Ui.ChartView

open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Fields
open Berreman.Media
open Berreman.FieldFunctions
open Analytics
open Analytics.Variables
open OpticalConstructor.Domain
open OpticalConstructor.Ui.Charts

// ---------------------------------------------------------------------------
// The chart panel's local MVU surface (R-3). The `Chart` `RootMsg` case wraps a
// `ChartMsg`; `Shell.update` runs `update` over `(model.chart, model.markers)`.
// ---------------------------------------------------------------------------

/// Chart-panel edits: chart-settings changes (which edit `ChartSettings.ChartSettings`)
/// and on-plot cursor placements (which edit `Readout.Markers`). Both reach the model
/// via the `Chart` `RootMsg` case (R-3).
type ChartMsg =
    | SetTitle of string
    | SetShowLegend of bool
    | SetShowGrid of bool
    | SetXScale of ChartSettings.AxisScale
    | PlaceCursor1 of Readout.ReadoutPoint
    | PlaceCursor2 of Readout.ReadoutPoint

/// Pure dispatcher over the chart sub-state. Settings edits update the
/// `ChartSettings.ChartSettings`; cursor placements update `Readout.Markers` — the
/// markers are a plain value the model holds, never renderer-held state (R-3 / §H.7).
let update (msg : ChartMsg) (settings : ChartSettings.ChartSettings, markers : Readout.Markers)
    : ChartSettings.ChartSettings * Readout.Markers =
    match msg with
    | SetTitle t -> { settings with title = t }, markers
    | SetShowLegend b -> { settings with showLegend = b }, markers
    | SetShowGrid b -> { settings with showGrid = b }, markers
    | SetXScale s -> { settings with xScale = s }, markers
    | PlaceCursor1 p -> settings, { markers with cursor1 = Some p }
    | PlaceCursor2 p -> settings, { markers with cursor2 = Some p }

// ---------------------------------------------------------------------------
// Representative sweep inputs (built from the reused Analytics seams). The real
// model-driven sweep — the `OpticalSystem -> FixedInfo` source lift (Part U4) and the
// effectful sweep `Cmd` (Part U7 / JobRunner) — is out of this slice's scope and stays
// deferred; these samples drive the rendering path end-to-end now so the AvaPlot host
// shows real curves through the `Plot1DView.*` seams. Held at module scope so the
// sweep solve runs once, not on every render.
// ---------------------------------------------------------------------------

let private sampleLight : IncidentLightInfo = IncidentLightInfo.create (WaveLength.nm 550.0<nm>)
let private sampleInfo : FixedInfo = StandardSystems.transparentGlassFilm (Thickness.nm 500.0<nm>) sampleLight
let private sampleInfo2 : FixedInfo = StandardSystems.transparentGlassFilm (Thickness.nm 900.0<nm>) sampleLight
let private sampleX : RangedVariable = StandardLightVariables.wavelength200to800Range 12
let private sampleY : RangedVariable = StandardLightVariables.incidenceAngleRange60 12
let private sampleFns : OpticalFunction list = [ OpticalFunction.R; OpticalFunction.T ]

/// A small field-depth profile (canonical-SI depth, |E|² value) — pure render data
/// for `renderFieldDepth`; Part F §F.6 `fieldDepthProfile` supplies the real profile
/// once the field-depth panel is wired.
let private sampleProfile : (float<meter> * float) list =
    [ (0.0<meter>, 1.0); (1.0e-7<meter>, 0.6); (2.0e-7<meter>, 0.3); (3.0e-7<meter>, 0.1) ]

/// A small dispersion series — pure render data for `renderDispersion`; the engine
/// dispersion builders (`SeriesData.plotN11Series` …) supply the real series once the
/// dispersion panel is wired.
let private sampleDispersion : SeriesData.Series1D list =
    [ { name = "n (sample)"; points = [| (200.0, 1.52); (400.0, 1.50); (800.0, 1.47) |] } ]

/// The Sweep series, precomputed once, so the cursor "mark peak" control reads a peak
/// without triggering another solve.
let private sweepSeries : SeriesData.Series1D list =
    SeriesData.series1DMany sampleInfo sampleFns sampleX

/// The peak of the first Sweep trace — the point the "mark cursor" control places
/// (uses `Readout.peak`, R-3 / §H.7). Falls back to the origin for an empty series.
let private peakPoint : Readout.ReadoutPoint =
    match sweepSeries with
    | s :: _ when s.points.Length > 0 -> Readout.peak s.points
    | _ -> (0.0, 0.0)

// ---------------------------------------------------------------------------
// Plot builders. Each 1-D chart is rebuilt from the CURRENT settings on every render
// so a settings edit reaches the plot through `applyToScottPlot` (R-3 / AC-U5.2); the
// `AvaPlot` host calls `Refresh()` each time (AC-U5.1).
// ---------------------------------------------------------------------------

let private sweepPlot s = Plot1DView.renderSweep s sampleInfo sampleFns sampleX
let private comparisonPlot s = Plot1DView.renderComparison s [ sampleInfo; sampleInfo2 ] OpticalFunction.R sampleX
let private fieldDepthPlot s = Plot1DView.renderFieldDepth s Units.Nanometer "depth (nm)" sampleProfile
let private dispersionPlot s = Plot1DView.renderDispersion s (Some (200.0, 800.0)) sampleDispersion

/// The 3-D surface, built lazily and routed through `applyToPlotly` (R-3) so the host
/// forces (and the upstream `calculate3D` sweep runs) ONLY if a WebView2 host is
/// actually available — never on the placeholder path (R-4).
let private surface3D (s : ChartSettings.ChartSettings) : Lazy<Plotly.NET.GenericChart> =
    lazy (Plot3DView.render sampleInfo OpticalFunction.R sampleX sampleY |> ChartSettings.applyToPlotly s)

// ---------------------------------------------------------------------------
// View.
// ---------------------------------------------------------------------------

let private toolbar (settings : ChartSettings.ChartSettings) (markers : Readout.Markers) (dispatch : ChartMsg -> unit) : IView =
    let cursorText =
        match markers.cursor1 with
        | Some (x, y) -> sprintf "cursor: (%.1f, %.3f)" x y
        | None -> "cursor: —"
    let nextScale = match settings.xScale with | ChartSettings.Linear -> ChartSettings.Log10 | ChartSettings.Log10 -> ChartSettings.Linear
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.spacing 6.0
        StackPanel.margin 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text "Title:"; TextBlock.verticalAlignment VerticalAlignment.Center ]
            TextBox.create [
                TextBox.width 140.0
                TextBox.text settings.title
                TextBox.onTextChanged (fun t -> dispatch (SetTitle t))
            ]
            Button.create [
                Button.content (if settings.showLegend then "Legend: on" else "Legend: off")
                Button.onClick (fun _ -> dispatch (SetShowLegend (not settings.showLegend)))
            ]
            Button.create [
                Button.content (if settings.showGrid then "Grid: on" else "Grid: off")
                Button.onClick (fun _ -> dispatch (SetShowGrid (not settings.showGrid)))
            ]
            Button.create [
                Button.content (match settings.xScale with | ChartSettings.Linear -> "X: linear" | ChartSettings.Log10 -> "X: log")
                Button.onClick (fun _ -> dispatch (SetXScale nextScale))
            ]
            Button.create [
                Button.content "Mark cursor at peak"
                Button.onClick (fun _ -> dispatch (PlaceCursor1 peakPoint))
            ]
            TextBlock.create [ TextBlock.text cursorText; TextBlock.verticalAlignment VerticalAlignment.Center ]
        ]
    ] :> IView

/// One titled, fixed-height chart row so each hosted plot lays out a frame.
let private chartRow (title : string) (host : IView) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Vertical
        StackPanel.spacing 2.0
        StackPanel.margin 4.0
        StackPanel.children [
            TextBlock.create [ TextBlock.text title; TextBlock.fontWeight FontWeight.Bold ]
            Border.create [ Border.height 220.0; Border.child host ]
        ]
    ] :> IView

/// The chart panel (R-2/R-3/R-4): the four 1-D charts in the `AvaPlot` host and the
/// 3-D surface in the WebView2 host, under a settings/cursor toolbar. Scrollable so
/// every row lays out independently of the panel height.
let chartPanel (settings : ChartSettings.ChartSettings) (markers : Readout.Markers) (dispatch : ChartMsg -> unit) : IView =
    let body =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            StackPanel.children [
                toolbar settings markers dispatch
                chartRow "Sweep (R, T vs λ)" (ChartHosts.scottPlotHost (sweepPlot settings))
                chartRow "Comparison (R, two systems)" (ChartHosts.scottPlotHost (comparisonPlot settings))
                chartRow "Field depth (|E|²)" (ChartHosts.scottPlotHost (fieldDepthPlot settings))
                chartRow "Dispersion (n vs λ)" (ChartHosts.scottPlotHost (dispersionPlot settings))
                chartRow "3-D surface (R vs λ × angle)" (ChartHosts.webView2Host (surface3D settings))
            ]
        ]
    ScrollViewer.create [ ScrollViewer.content body ] :> IView
