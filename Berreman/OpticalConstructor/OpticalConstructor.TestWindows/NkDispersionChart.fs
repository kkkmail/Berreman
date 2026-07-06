/// Spec 0033 (019) — the pure n/k dispersion chart on the shared dual-axis chart spine (018).
/// Routes an entry's `OpticalPropertiesWithDisp` through the engine's OWN eps dispersion path
/// (`epsWithDisp.getEps`, n = Re[√ε₁₁] / k = Im[√ε₁₁] — no n/k formula re-derived) into ONE shared
/// `ExperimentChart`: the n series on the LEFT axis and the k series on the RIGHT (via the paired
/// `ChartStyle` seed below — the side is STYLE, not data), the x-axis relabelled to the display
/// unit by the Domain `SpectralAxis` helpers. Dispersive entries yield curves; non-dispersive
/// entries yield flat lines through the SAME builder. Pure — no window, no ScottPlot. The one
/// inline FuncUI canvas rendering of the chart (`inlineCanvas`, spec 0033 step 024) lives at the
/// bottom, shared by the Material editor's live preview and the Materials workbench's View panel.
///
/// NOTE (§7 skepticism rule): the slice spec routed the series through the engine data builders
/// `Analytics.Variables.calculateN11Re` / `calculateXi11Im`, but those sample wavelengths through
/// `Analytics.getWaveLengthValue`, which re-wraps the meter magnitude as a nm scalar and so
/// evaluates the dispersion at λ×10⁻⁹ — a dispersive entry draws a FLAT line at the wrong value,
/// violating this step's own acceptance. Following the two in-repo precedents that document the
/// same defect (`SourceSpec.SpectralProfile.sample`, `MaterialImport.exportCsv`), the builder walks
/// the canonical meter grid itself and reads n,k through the engine `getEps` seam — exactly
/// `exportCsv`'s n,k extraction, reshaped into chart series.
module OpticalConstructor.TestWindows.NkDispersionChart

open System.Numerics
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Media
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Berreman.Constants
open Berreman.Fields
open Berreman.Dispersion
open Analytics.Variables
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Units
open OpticalConstructor.Controls
open OpticalConstructor.Controls.ExperimentChart

/// The chart's fixed series order: n first (left axis), k second (right axis).
let nSeriesIndex : int = 0
let kSeriesIndex : int = 1

/// The complex refraction index √ε₁₁ sampled over the range's canonical-meter grid (the
/// `MaterialImport.exportCsv` extraction). The grid is `SpectralAxis.axisTicks` in nm — the SAME
/// `s + (e − s)·i/n` meter grid the display ticks come from — so the x relabel below is a pure
/// rescale of these samples' positions, never a re-sample (AC-D7).
let private complexN11Samples (o : OpticalPropertiesWithDisp) (range : Range<WaveLength>) : Complex list =
    SpectralAxis.axisTicks Nanometer range
    |> List.map (fun nm -> Complex.Sqrt (o.epsWithDisp.getEps (WaveLength.Nm (nm * 1.0<nm>))).[0, 0])

/// The n/k dispersion chart: `OpticalPropertiesWithDisp` + display unit + canonical-meter range →
/// the shared `ExperimentChart`. n = Re[√ε₁₁] and k = Im[√ε₁₁] via the engine `getEps` path; the
/// display unit affects ONLY the x-axis label/scale, never the sampled spectral range (AC-D7).
/// Pair with `nkDispersionStyle` for the left/right axis assignment.
let nkDispersionChart (o : OpticalPropertiesWithDisp) (u : UnitOfMeasure) (range : Range<WaveLength>) : ExperimentChart =
    let samples = complexN11Samples o range
    let xs = SpectralAxis.axisTicks u range
    let points (part : Complex -> float) : (float * float) list =
        List.zip xs (samples |> List.map part)
    {
        series =
            [
                { name = "n"; points = points (fun c -> c.Real) }
                { name = "k"; points = points (fun c -> c.Imaginary) }
            ]
        xLabel = SpectralAxis.axisLabel u
        yLabel = "n"
        title = "n/k dispersion"
        description = "n = Re[√ε₁₁] on the left axis, k = Im[√ε₁₁] on the right; a non-dispersive material draws flat lines."
        angular = false
    }

/// The chart's paired style seed: `ChartStyle.defaultState` births every series on the LEFT axis
/// (the 018 spine), so the k series is flipped to the RIGHT via the `setSeriesAxisSide` seam —
/// n keeps the left. The per-side auto bounds then fit n and k independently (018 `dataBounds`).
let nkDispersionStyle (chart : ExperimentChart) : ChartStyle.ChartStyleState =
    ChartStyle.defaultState chart |> ChartStyle.setSeriesAxisSide kSeriesIndex ChartStyle.RightAxis

// ---------------------------------------------------------------------------------------------
// The shared INLINE dual-axis canvas rendering of the chart above (spec 0033 step 024 — REAL-MOVED
// from the MaterialEditorView preview so the Materials workbench's View panel and the editor's
// live preview draw through ONE renderer). Still windowless and ScottPlot-free: it builds a plain
// FuncUI `Canvas` (axis lines + the two polylines + captions), per-side bounds from the 018
// `ChartStyle.dataBounds` so one series never flattens the other — the inline-chart-canvas
// approach of `ExperimentControls`.
// ---------------------------------------------------------------------------------------------

let private canvasWidth = 560.0
let private canvasHeight = 190.0
let private marginLeft = 46.0
let private marginRight = 46.0
let private marginTop = 12.0
let private marginBottom = 26.0

let private color (r : int) (g : int) (b : int) : Color = Color.FromRgb(byte r, byte g, byte b)
let private brush (c : Color) : IBrush = SolidColorBrush(c) :> IBrush
let private axisColor = color 120 120 120
let private captionColor = color 110 110 110

/// Set `AutomationProperties.AutomationId` (a freely-mutable attached property — unlike
/// `Control.Name`) through FuncUI's attr builder, so two hosts can draw the same canvas under
/// their own stable ids.
let private automationId (autoId : string) : IAttr<Canvas> =
    AttrBuilder<Canvas>.CreateProperty<string>(AutomationProperties.AutomationIdProperty, autoId, ValueNone)

/// The inline dual-axis rendering of the n/k chart: n on the LEFT axis, k on the RIGHT (the
/// paired `nkDispersionStyle` seed above), carrying the host's `autoId` as its automation id.
let inlineCanvas (autoId : string) (chart : ExperimentChart) : IView =
    let style = nkDispersionStyle chart
    let sided = chart.series |> List.mapi (fun i s -> s, (ChartStyle.seriesStyleOf i style).axisSide)
    let bounds = ChartStyle.dataBounds sided
    let plotWidth = canvasWidth - marginLeft - marginRight
    let plotHeight = canvasHeight - marginTop - marginBottom
    let toPlot ((xlo, xhi) : float * float) ((ylo, yhi) : float * float) (x : float) (y : float) : Point =
        Point(
            marginLeft + plotWidth * (x - xlo) / (xhi - xlo),
            marginTop + plotHeight * (1.0 - (y - ylo) / (yhi - ylo)))
    let axisLine (x1 : float, y1 : float) (x2 : float, y2 : float) : IView =
        Line.create [
            Line.startPoint (Point(x1, y1))
            Line.endPoint (Point(x2, y2))
            Line.stroke (brush axisColor)
            Line.strokeThickness 1.0
        ] :> IView
    let axes =
        [
            axisLine (marginLeft, marginTop) (marginLeft, marginTop + plotHeight)
            axisLine (marginLeft + plotWidth, marginTop) (marginLeft + plotWidth, marginTop + plotHeight)
            axisLine (marginLeft, marginTop + plotHeight) (marginLeft + plotWidth, marginTop + plotHeight)
        ]
    let seriesViews =
        sided
        |> List.mapi (fun i (s, side) ->
            let yRange =
                match side with
                | ChartStyle.LeftAxis -> bounds.yLeft
                | ChartStyle.RightAxis -> bounds.yRight
            Polyline.create [
                Polyline.points (s.points |> List.map (fun (x, y) -> toPlot bounds.x yRange x y))
                Polyline.stroke (brush (Color.Parse (ChartStyle.seriesStyleOf i style).colorHex))
                Polyline.strokeThickness 1.5
            ] :> IView)
    let caption (text : string) (colorHex : string option) (left : float) (top : float) : IView =
        TextBlock.create [
            TextBlock.text text
            TextBlock.fontSize 10.0
            TextBlock.foreground (match colorHex with Some hex -> brush (Color.Parse hex) | None -> brush captionColor)
            TextBlock.left left
            TextBlock.top top
        ] :> IView
    let colorOf (i : int) : string option = Some (ChartStyle.seriesStyleOf i style).colorHex
    Canvas.create [
        automationId autoId
        Canvas.width canvasWidth
        Canvas.height canvasHeight
        Canvas.children (
            axes
            @ seriesViews
            @ [
                caption "n (left)" (colorOf nSeriesIndex) 2.0 0.0
                caption "k (right)" (colorOf kSeriesIndex) (canvasWidth - 44.0) 0.0
                caption chart.xLabel None (marginLeft + plotWidth / 2.0 - 20.0) (canvasHeight - 16.0)
            ])
    ] :> IView
