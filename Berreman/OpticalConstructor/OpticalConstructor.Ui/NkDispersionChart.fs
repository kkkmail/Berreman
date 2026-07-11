/// Spec 0033 (019) — the pure n/k dispersion chart on the shared dual-axis chart spine (018).
/// Routes an entry's `OpticalPropertiesWithDisp` through the engine's OWN eps dispersion path
/// (`epsWithDisp.getEps`, n = Re[√ε₁₁] / k = Im[√ε₁₁] — no n/k formula re-derived) into ONE shared
/// `ExperimentChart`: the n series on the LEFT axis and the k series on the RIGHT (via the paired
/// `ChartStyle` seed below — the side is STYLE, not data), the x-axis relabelled to the display
/// unit by the Domain `SpectralAxis` helpers. Dispersive entries yield curves; non-dispersive
/// entries yield flat lines through the SAME builder. Pure — no window, no ScottPlot. Spec 0035 (016)
/// deleted the primitive inline FuncUI canvas that once lived at the bottom; the Material editor's live
/// preview and the Materials workbench's View panel now embed the shared dual-axis ScottPlot chart
/// (`OpticalConstructor.Controls.EmbeddedChart.create`) over this builder's chart + `nkDispersionStyle`.
///
/// NOTE (§7 skepticism rule): the slice spec routed the series through the engine data builders
/// `Analytics.Variables.calculateN11Re` / `calculateXi11Im`, but those sample wavelengths through
/// `Analytics.getWaveLengthValue`, which re-wraps the meter magnitude as a nm scalar and so
/// evaluates the dispersion at λ×10⁻⁹ — a dispersive entry draws a FLAT line at the wrong value,
/// violating this step's own acceptance. Following the two in-repo precedents that document the
/// same defect (`SourceSpec.SpectralProfile.sample`, `MaterialImport.exportCsv`), the builder walks
/// the canonical meter grid itself and reads n,k through the engine `getEps` seam — exactly
/// `exportCsv`'s n,k extraction, reshaped into chart series.
module OpticalConstructor.Ui.NkDispersionChart

open System.Numerics
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
