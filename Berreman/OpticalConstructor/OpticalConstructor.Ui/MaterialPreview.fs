/// Material spectral-preview routing (§D.10). Routes a `MaterialEntry`'s dispersion
/// into the engine's EXISTING `Analytics/Charting.fs` plots (`plotN11`/`plotXi11`/…)
/// — this module does NOT re-implement dispersion plotting. The spectral axis exposes
/// a nm/µm/Å/eV/cm⁻¹ unit toggle implemented by the `Units` seam (D.2) at the chart
/// boundary ONLY: the underlying `Range<WaveLength>` and the engine data stay in
/// canonical meters (the toggle relabels/rescales the axis, it never re-stores data,
/// §D.11 / AC-D7).
module OpticalConstructor.Ui.MaterialPreview

open Berreman.Fields
open Berreman.Dispersion
open Analytics.Variables
open Analytics.Charting
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary

/// Human-facing axis label for a spectral display unit. Display intent only.
let axisLabel (u : UnitOfMeasure) : string =
    match u with
    | Meter -> "λ (m)"
    | Millimeter -> "λ (mm)"
    | Micrometer -> "λ (µm)"
    | Nanometer -> "λ (nm)"
    | Angstrom -> "λ (Å)"
    | ElectronVolt -> "E (eV)"
    | Wavenumber -> "ν̃ (cm⁻¹)"

/// Build the canonical-meter `Range<WaveLength>` from user endpoints entered in unit
/// `u` (§D.2 `toWaveLength`). The stored range is always canonical meters, so an EUV
/// user may enter eV and a green-laser user nm without manual conversion (R-6).
let spectralRange (u : UnitOfMeasure) (startValue : float) (endValue : float) (points : int) : Range<WaveLength> =
    {
        startValue = toWaveLength u startValue
        endValue = toWaveLength u endValue
        numberOfPoints = points
    }

/// Axis tick positions in the display unit (§D.2 `wavelengthToUnit`). RELABEL/RESCALE
/// only — the underlying `Range<WaveLength>` and the engine data stay in meters (§D.11).
let axisTicks (u : UnitOfMeasure) (range : Range<WaveLength>) : float list =
    let s = range.startValue.value
    let e = range.endValue.value
    let n = max 1 range.numberOfPoints
    [ for i in 0 .. range.numberOfPoints -> fromMeters u (s + (e - s) * (float i) / (float n)) ]

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
