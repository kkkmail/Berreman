/// [Core] source editor (§E.2, §E.3, §E.10).
///
/// Per the §0/P3 testability mandate and the established precedent
/// (`StackEditor.fs`, `ConstructionPage.fs`) the EDIT SEAM is Avalonia-free: the
/// `SourceMsg`/`update` projecting a `SourceSpec` is a pure, immutable
/// transformation carrying NO Avalonia type, so every §E.2/§E.3/§E.10 acceptance
/// is unit-testable at the UI/model seam without an Avalonia host. The FuncUI
/// `view` body that binds these to controls (the public MIT `Avalonia.FuncUI`
/// NuGet from slice 001, never the audit-gated clone) is deferred to a later
/// UI-wiring slice, exactly as `Ui.fs` records.
///
/// The editor rebuilds the underlying `IncidentLightInfo` through the single
/// `SourceSpec.toIncidentLight` constructor (`SourceSpec.fs`) and converts the
/// wavelength entry through the SOLE `Units` seam (`toWaveLength`, §D.2/§A.10) —
/// nm/µm map natively, Å/mm/eV/cm⁻¹ reduce to `WaveLength.Nm`. The view does NOT
/// call the 4×4 solver (producing fields is Part F).
module OpticalConstructor.Ui.Sources.SourceEditorView

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Analytics.Variables
open Analytics.StandardLightVariables
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.SourceSpec

// ---------------------------------------------------------------------------
// Wavelength entry at the boundary (§E.2 / AC-E2). The chosen display unit is
// converted to a WaveLength through the sole Units seam; the domain model stores
// no non-SI unit (the WaveLength's .value is meters).
// ---------------------------------------------------------------------------

/// Build a `WaveLength` from an operator entry in display unit `u` (§E.2). nm/µm
/// land on `WaveLength.Nm`/`WaveLength.Mkm` natively; Å, mm, eV, cm⁻¹ — which the
/// engine `WaveLength` cannot represent — reduce to nm via the §A.10 conversions.
let waveLengthFromInput (u : UnitOfMeasure) (x : float) : WaveLength = toWaveLength u x

// ---------------------------------------------------------------------------
// The editor model messages & pure update (Avalonia-free).
// One control per IncidentLightInfo field (§E.2): wavelength, incidence angle,
// polarization, ellipticity, plus the per-source intensity weight and display unit.
// ---------------------------------------------------------------------------

type SourceMsg =
    /// Wavelength entered in a display unit; converted at the boundary and the
    /// display unit is recorded on the source.
    | SetWaveLength of UnitOfMeasure * float
    | SetIncidenceAngleDegrees of float
    | SetPolarization of Polarization
    | SetEllipticity of float
    | SetIntensity of float
    | SetDisplayUnit of UnitOfMeasure

let private setLight (f : IncidentLightInfo -> IncidentLightInfo) (s : SourceSpec) : SourceSpec =
    { s with light = f s.light }

/// Pure editor update: apply a `SourceMsg` to a source, yielding a NEW source.
let update (msg : SourceMsg) (s : SourceSpec) : SourceSpec =
    match msg with
    | SetWaveLength (u, x) ->
        { setLight (fun l -> { l with waveLength = waveLengthFromInput u x }) s with displayUnit = u }
    | SetIncidenceAngleDegrees d ->
        setLight (fun l -> { l with incidenceAngle = IncidenceAngle.create (Angle.degree d) }) s
    | SetPolarization p -> setLight (fun l -> { l with polarization = p }) s
    | SetEllipticity e -> setLight (fun l -> { l with ellipticity = Ellipticity.create e }) s
    | SetIntensity i -> { s with intensity = i }
    | SetDisplayUnit u -> { s with displayUnit = u }

// ---------------------------------------------------------------------------
// Single-value-or-range axis (§E.3 / AC-E3). The Fixed/Ranged toggle the editor
// surfaces; the Ranged case reuses the StandardLightVariables presets and the
// engine RangedVariable rather than constructing fresh Range<_> values. The
// produced RangedVariable is handed to the sweep layer (Part F) UNCHANGED — this
// module does NOT implement sweep execution / charting / calculate wiring.
// ---------------------------------------------------------------------------

/// A ranged wavelength axis over the §E.3 preset span (200–800 nm,
/// `StandardLightVariables.fs:64`) — a `WaveLengthRange`, no fresh `Range<_>`.
let wavelengthAxisRanged (numberOfPoints : int) : SourceAxis<WaveLength> =
    Ranged (wavelength200to800Range numberOfPoints)

/// A ranged incidence-angle axis over the §E.3 preset span (0–89°,
/// `StandardLightVariables.fs:40`) — an `IncidenceAngleRange`, no fresh `Range<_>`.
let incidenceAngleAxisRanged (numberOfPoints : int) : SourceAxis<IncidenceAngle> =
    Ranged (incidenceAngleRange numberOfPoints)

/// A fixed (single-value) wavelength axis carrying the committed value.
let wavelengthAxisFixed (w : WaveLength) : SourceAxis<WaveLength> = Fixed w

/// The `RangedVariable` a `Ranged` axis hands to the sweep layer unchanged
/// (`None` for a `Fixed` axis). The matching case (`WaveLengthRange`/
/// `IncidenceAngleRange`) is preserved verbatim — no new range type (§E.3).
let axisRangedVariable (axis : SourceAxis<'T>) : RangedVariable option =
    match axis with
    | Ranged r -> Some r
    | Fixed _ -> None

// ---------------------------------------------------------------------------
// Source palette discoverability (§E.10 / AC-E10). Adding a Source is a one-click
// palette action with no confirmation gate (a source is non-destructive and
// freely removable from the sources list of §E.8).
// ---------------------------------------------------------------------------

/// A reasonable default new source: a vacuum, normal-incidence, monochromatic
/// 600 nm s-polarised collimated coherent source at unit intensity, nm display.
let defaultSource (sourceId : string) : SourceSpec =
    {
        sourceId = sourceId
        name = None
        light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)
        displayUnit = Nanometer
        intensity = 1.0
        spectralProfile = None
        cone = None
        gaussianBeam = None
        coherence = Coherent
    }

/// A named element-palette entry. `requiresConfirmation = false` makes adding the
/// element a one-click action with no gate (§E.10).
type PaletteEntry =
    {
        title : string
        element : ConstructorElement
        requiresConfirmation : bool
    }

/// The Source palette entry (§E.10): one-click add of a `ConstructorElement.Source`
/// with no confirmation gate, reachable from the palette navigation.
let sourcePaletteEntry : PaletteEntry =
    {
        title = "Source"
        element = ConstructorElement.Source
        requiresConfirmation = false
    }
