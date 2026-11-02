namespace OpticalConstructor.Domain

open Berreman.Fields
open Analytics.Variables
open OpticalConstructor.Domain.Units

/// The pure spectral-axis helpers (§D.10/§D.11), REAL-MOVED here from
/// `OpticalConstructor.Ui.MaterialPreview` (spec 0033 step 019) so chart hosts outside
/// the Ui project (the TestWindows n/k dispersion chart) reach them without a Ui
/// reference. The display unit is a chart-boundary relabel/rescale ONLY: the underlying
/// `Range<WaveLength>` and the engine data stay in canonical meters (AC-D7).
module SpectralAxis =

    /// Build the canonical-meter `Range<WaveLength>` from user endpoints entered in unit
    /// `u` (§D.2 `toWaveLength`). The stored range is always canonical meters, so an EUV
    /// user may enter eV and a green-laser user nm without manual conversion (R-6).
    let spectralRange (u : UnitOfMeasure) (startValue : UnitMagnitude) (endValue : UnitMagnitude) (points : NumberOfPoints) : Range<WaveLength> =
        {
            startValue = toWaveLength u startValue
            endValue = toWaveLength u endValue
            numberOfPoints = points
        }

    /// Axis tick positions in the display unit (§D.2 `wavelengthToUnit`). RELABEL/RESCALE
    /// only — the underlying `Range<WaveLength>` and the engine data stay in meters (§D.11).
    /// The grid is the engine's own: `s + (e − s)·i/n` linearly in meters, the same points
    /// `RangedVariable.value` samples (`Variables.fs`), so a chart may zip these ticks with
    /// an engine-calculated series to relabel its x-axis without touching the data.
    let axisTicks (u : UnitOfMeasure) (range : Range<WaveLength>) : UnitMagnitude list =
        let s = range.startValue.value
        let e = range.endValue.value
        let n = max 1 range.numberOfPoints.value
        [ for i in 0 .. range.numberOfPoints.value -> fromMeters u (s + (e - s) * (float i) / (float n)) ]
