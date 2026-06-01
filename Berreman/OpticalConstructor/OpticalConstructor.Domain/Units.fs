namespace OpticalConstructor.Domain

open Berreman.Constants
open Berreman.Fields

/// Units spine (§A.10 / D.1–D.3). The single canonical SI base is the engine's
/// own: meters for length/thickness (`Constants.fs:6`) and `WaveLength` reduced
/// to meters via `WaveLength.value` (`Fields.fs:284`). This module is the SOLE
/// unit-conversion seam in the solution; conversions happen ONLY here, at the
/// UI/IO boundary, and never store a non-SI value in the model, JSON, or `.binz`.
module Units =

    /// Boundary unit-of-measure vocabulary (D.1). Enumerates exactly the
    /// human-facing units 010 §2 names. This DU is the ONLY place a non-SI unit
    /// name appears; it is a UI/IO label, never a storage type. It is the F#
    /// realisation of the `unitOfMeasure` schema `$def` (§A.7). No new F#
    /// `[<Measure>]` types are introduced — length units reduce through the
    /// existing `meter`/`mm`/`mkm`/`nm` measures (`Constants.fs:6,10,13,17`);
    /// Å, eV and cm⁻¹ are handled by the scalar conversions below.
    type UnitOfMeasure =
        | Meter
        | Millimeter
        | Micrometer
        | Nanometer
        | Angstrom
        | ElectronVolt
        | Wavenumber

    /// E[eV] = 1239.84 / λ[nm]. The eV↔wavelength relation; this literal MUST
    /// appear exactly once in the solution (D.2).
    let evNmProduct = 1239.84

    /// 1 Å = 1.0e-10 m (D.2). The Ångström factor, the one length factor with no
    /// `Constants.fs` constant of its own.
    let private angstromToMeter = 1.0e-10<meter>

    /// Convert a human-entered scalar in unit `u` into the engine's canonical SI
    /// base, `float<meter>` (D.2). Length units multiply by the existing
    /// `mmToMeter`/`mkmToMeter`/`nmToMeter` constants (`Constants.fs:27,30,33`)
    /// — no factor is re-derived. `ElectronVolt` maps energy→wavelength by
    /// `λ[nm] = evNmProduct / E[eV]`; `Wavenumber` maps `λ[nm] = 1.0e7 / ν̃[cm⁻¹]`;
    /// both then go through `nmToMeter`.
    let toMeters (u : UnitOfMeasure) (x : float) : float<meter> =
        match u with
        | Meter -> x * 1.0<meter>
        | Millimeter -> (x * 1.0<mm>) * mmToMeter
        | Micrometer -> (x * 1.0<mkm>) * mkmToMeter
        | Nanometer -> (x * 1.0<nm>) * nmToMeter
        | Angstrom -> x * angstromToMeter
        | ElectronVolt -> ((evNmProduct / x) * 1.0<nm>) * nmToMeter
        | Wavenumber -> ((1.0e7 / x) * 1.0<nm>) * nmToMeter

    /// Convert a human-entered scalar in unit `u` into the engine's `WaveLength`
    /// DU (D.3 / R-8), so the engine's own `.value` reduction performs the final
    /// SI step. `Nanometer`/`Micrometer` return `Nm`/`Mkm` natively
    /// (`Fields.fs:294,295`); `Angstrom`/`ElectronVolt`/`Wavenumber` — which
    /// `WaveLength` cannot represent natively — first reduce to nm (D.2) and
    /// return `Nm`. No Å/eV/cm⁻¹ case is added to the engine `WaveLength`.
    let toWaveLength (u : UnitOfMeasure) (x : float) : WaveLength =
        match u with
        | Nanometer -> WaveLength.Nm (x * 1.0<nm>)
        | Micrometer -> WaveLength.Mkm (x * 1.0<mkm>)
        | Meter | Millimeter | Angstrom | ElectronVolt | Wavenumber ->
            // Reduce to nm through the canonical SI base, then hand the engine a `Nm`.
            let nm = (toMeters u x) / nmToMeter
            WaveLength.Nm nm

    /// Inverse of `toMeters` for display (D.2): a canonical `float<meter>` back
    /// to a human-facing scalar in unit `u`, dividing by the same constants and
    /// inverting the eV/cm⁻¹ relations.
    let fromMeters (u : UnitOfMeasure) (m : float<meter>) : float =
        match u with
        | Meter -> m / 1.0<meter>
        | Millimeter -> (m / mmToMeter) / 1.0<mm>
        | Micrometer -> (m / mkmToMeter) / 1.0<mkm>
        | Nanometer -> (m / nmToMeter) / 1.0<nm>
        | Angstrom -> m / angstromToMeter
        | ElectronVolt -> evNmProduct / ((m / nmToMeter) / 1.0<nm>)
        | Wavenumber -> 1.0e7 / ((m / nmToMeter) / 1.0<nm>)

    /// Inverse of `toWaveLength` for display (D.2): read the canonical value via
    /// `WaveLength.value` (`Fields.fs:284`) and project it onto unit `u`. The
    /// SOLE conversion seam — no other module hand-rolls a unit conversion.
    let wavelengthToUnit (u : UnitOfMeasure) (w : WaveLength) : float =
        fromMeters u w.value
