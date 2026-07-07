/// Material spectral-preview routing (§D.10). Routes a `MaterialEntry`'s dispersion
/// into the engine's EXISTING `Analytics/Charting.fs` plots (`plotN11`/`plotXi11`/…)
/// — this module does NOT re-implement dispersion plotting. The spectral axis exposes
/// a nm/µm/Å/eV/cm⁻¹ unit toggle implemented by the `Units` seam (D.2) at the chart
/// boundary ONLY: the underlying `Range<WaveLength>` and the engine data stay in
/// canonical meters (the toggle relabels/rescales the axis, it never re-stores data,
/// §D.11 / AC-D7). The pure spectral-axis helpers (`axisLabel` / `spectralRange` /
/// `axisTicks`) were REAL-MOVED to `OpticalConstructor.Domain.SpectralAxis`
/// (spec 0033 step 019) so non-Ui chart hosts share them; callers point there.
module OpticalConstructor.Ui.MaterialPreview

open Berreman.Fields
open Berreman.Dispersion
open Analytics.Variables
open Analytics.Charting
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary

/// The `(properties, range)` inputs handed to the engine dispersion plots. The
/// spectral-axis display unit is passed THROUGH unchanged: it MUST NOT alter the
/// canonical `Range<WaveLength>` (AC-D7 — the eV and nm toggles plot the SAME range,
/// only the axis labels/scale differ).
let plotInput (entry : MaterialEntry) (range : Range<WaveLength>) (_axisUnit : UnitOfMeasure) : OpticalPropertiesWithDisp * Range<WaveLength> =
    (entry.properties, range)

/// Route the entry into the engine's existing dispersion plots (R-6). Each `show*`
/// passes the entry's `OpticalPropertiesWithDisp` and the canonical `Range<WaveLength>`
/// straight to the matching `Analytics.Charting` function; the axis unit only affects
/// the display, never the plotted data.
let showN11 (entry : MaterialEntry) (range : Range<WaveLength>) (axisUnit : UnitOfMeasure) =
    let (o, r) = plotInput entry range axisUnit in plotN11 o r

let showXi11 (entry : MaterialEntry) (range : Range<WaveLength>) (axisUnit : UnitOfMeasure) =
    let (o, r) = plotInput entry range axisUnit in plotXi11 o r

let showN22 (entry : MaterialEntry) (range : Range<WaveLength>) (axisUnit : UnitOfMeasure) =
    let (o, r) = plotInput entry range axisUnit in plotN22 o r

let showXi22 (entry : MaterialEntry) (range : Range<WaveLength>) (axisUnit : UnitOfMeasure) =
    let (o, r) = plotInput entry range axisUnit in plotXi22 o r

let showN33 (entry : MaterialEntry) (range : Range<WaveLength>) (axisUnit : UnitOfMeasure) =
    let (o, r) = plotInput entry range axisUnit in plotN33 o r

let showXi33 (entry : MaterialEntry) (range : Range<WaveLength>) (axisUnit : UnitOfMeasure) =
    let (o, r) = plotInput entry range axisUnit in plotXi33 o r

let showRho11 (entry : MaterialEntry) (range : Range<WaveLength>) (axisUnit : UnitOfMeasure) =
    let (o, r) = plotInput entry range axisUnit in plotRho11 o r
