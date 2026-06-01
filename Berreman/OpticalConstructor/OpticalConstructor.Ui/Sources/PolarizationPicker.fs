/// [Standard] polarization presets + bidirectional Poincaré/ellipse picker (§E.4).
///
/// Per the §0/P3 testability mandate and the established precedent
/// (`StackEditor.fs`, `ConstructionPage.fs`) this is an Avalonia-free seam: the
/// presets, the bidirectional azimuth/ellipticity write-back, the live Stokes
/// readout, and the Poincaré-marker / ellipse coordinates are pure functions of a
/// `SourceSpec`, carrying NO Avalonia type. The FuncUI controls and the 3D
/// Poincaré sphere binding are deferred — and the 3D rendering MUST reuse the
/// OpenTK viewport reserved by Part A §A.6 (delivered by slice 016); this module
/// adds NO second 3D renderer.
///
/// The live `StokesVector` readout is computed from the produced incident
/// `EmField` via the engine seam: `EmField.create` (`BerremanMatrix.fs:194`) then
/// `EmField.stokesVector` (`FieldFunctions.fs:44`). The picker NEVER re-derives
/// Stokes parameters by hand. `unpolarized` is the §E.7 `Coherence.Unpolarized`
/// flag, never a sentinel `Polarization`/`Ellipticity` value.
module OpticalConstructor.Ui.Sources.PolarizationPicker

open System
open Berreman.Geometry
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.BerremanMatrix
open Berreman.FieldFunctions
open OpticalConstructor.Domain.SourceSpec

// ---------------------------------------------------------------------------
// Presets (§E.4 / AC-E4). s, p, 45°, RCP, LCP, unpolarized. s/p/45 reuse the
// engine Polarization constructors; RCP/LCP set Ellipticity.create ±1.0 (which
// clamps to ±1); unpolarized sets the E.7 Coherence flag.
// ---------------------------------------------------------------------------

type PolarizationPreset =
    | PresetS
    | PresetP
    | Preset45
    | PresetRCP
    | PresetLCP
    | PresetUnpolarized

/// Apply a polarization preset to a source (§E.4). s/p/45° set the polarization
/// azimuth (reusing `Polarization.s`/`Polarization.p`/`Polarization.create`) and
/// clear ellipticity/coherence to the linear default; RCP/LCP set the ellipticity
/// to ±1 via `Ellipticity.create` (which clamps), keeping the azimuth; unpolarized
/// sets `Coherence.Unpolarized` (never a sentinel polarization/ellipticity).
let applyPreset (preset : PolarizationPreset) (s : SourceSpec) : SourceSpec =
    let linear (pol : Polarization) =
        { s with
            light = { s.light with polarization = pol; ellipticity = Ellipticity.defaultValue }
            coherence = Coherent }
    let circular (e : float) =
        { s with light = { s.light with ellipticity = Ellipticity.create e } }
    match preset with
    | PresetS -> linear Polarization.s
    | PresetP -> linear Polarization.p
    | Preset45 -> linear (Polarization.create (Angle.degree 45.0))
    | PresetRCP -> circular 1.0
    | PresetLCP -> circular -1.0
    | PresetUnpolarized -> { s with coherence = Unpolarized }

// ---------------------------------------------------------------------------
// Bidirectional write-back (§E.4). Dragging azimuth/ellipticity on the ellipse or
// the Poincaré marker writes Polarization/Ellipticity straight back onto the
// source; ellipticity clamps via Ellipticity.create.
// ---------------------------------------------------------------------------

/// Write azimuth (polarization) and ellipticity back onto the source from a drag
/// on the ellipse / Poincaré marker (§E.4). `azimuth` becomes the `Polarization`
/// (via `Polarization.create`); `ell` becomes the `Ellipticity` (clamped to ±1).
let setAzimuthEllipticity (azimuth : Angle) (ell : float) (s : SourceSpec) : SourceSpec =
    { s with light = { s.light with polarization = Polarization.create azimuth; ellipticity = Ellipticity.create ell } }

// ---------------------------------------------------------------------------
// Live Stokes readout (§E.4 / AC-E4). Computed from the produced incident EmField
// via EmField.stokesVector — never hand-derived.
// ---------------------------------------------------------------------------

/// The live `StokesVector` of the source's incident field (§E.4): build the
/// incident `EmField` in vacuum via the engine `EmField.create`
/// (`BerremanMatrix.fs:194`) over `SourceSpec.toIncidentLight`, then read
/// `EmField.stokesVector` (`FieldFunctions.fs:44`).
let liveStokes (s : SourceSpec) : StokesVector =
    let info = SourceSpec.toIncidentLight s
    // `EmField.stokesVector` is computed on the complex basis (an option); a
    // well-formed incident field always has one, so default to Zero defensively.
    (EmField.create(info, OpticalProperties.vacuum)).stokesVector
    |> Option.defaultValue StokesVector.Zero

// ---------------------------------------------------------------------------
// Poincaré-marker + ellipse coordinates (§E.4). Pure geometry the reserved OpenTK
// viewport (slice 016) and the 2D ellipse rendering consume; NO renderer here.
// ---------------------------------------------------------------------------

/// The Poincaré-sphere marker (S1, S2, S3) for the source's polarization state
/// (§E.4): with azimuth ψ and ellipticity angle χ = atan(e), the unit-sphere
/// point is (cos2χ cos2ψ, cos2χ sin2ψ, sin2χ). Consumed by the reserved OpenTK
/// viewport (slice 016); this module adds NO 3D renderer.
let poincareMarker (s : SourceSpec) : float * float * float =
    let psi = s.light.polarization.value
    let chi = atan (s.light.ellipticity.value)
    let s1 = cos (2.0 * chi) * cos (2.0 * psi)
    let s2 = cos (2.0 * chi) * sin (2.0 * psi)
    let s3 = sin (2.0 * chi)
    (s1, s2, s3)

/// The polarization-ellipse parameters (orientation azimuth ψ in radians and the
/// signed axial ratio = the ellipticity value) for the 2D ellipse rendering
/// (§E.4). Pure; the view binds these to the ellipse control.
let ellipseParameters (s : SourceSpec) : {| azimuth : float; axialRatio : float |} =
    {| azimuth = s.light.polarization.value; axialRatio = s.light.ellipticity.value |}
