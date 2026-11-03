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
open OpticalConstructor.Controls

// ---------------------------------------------------------------------------
// The editor model messages & pure update (Avalonia-free).
// One control per IncidentLightInfo field (§E.2): wavelength, incidence angle,
// polarization, ellipticity, plus the per-source intensity weight and display unit.
// ---------------------------------------------------------------------------

type SourceMsg =
    /// Wavelength entered in a display unit; converted at the boundary through the sole
    /// `Units.toWaveLength` seam (§E.2 / AC-E2): nm/µm land on `WaveLength.Nm`/`WaveLength.Mkm`,
    /// Å, mm, eV, cm⁻¹ reduce to nm. The display unit is recorded on the source.
    | SetWaveLength of UnitOfMeasure * UnitMagnitude
    | SetIncidenceAngle of IncidenceAngle
    | SetPolarization of Polarization
    | SetEllipticity of Ellipticity
    | SetIntensity of IlluminationWeight
    | SetDisplayUnit of UnitOfMeasure

let private setLight (f : IncidentLightInfo -> IncidentLightInfo) (s : SourceSpec) : SourceSpec =
    { s with light = f s.light }

/// Pure editor update: apply a `SourceMsg` to a source, yielding a NEW source.
let update (msg : SourceMsg) (s : SourceSpec) : SourceSpec =
    match msg with
    | SetWaveLength (u, x) ->
        { setLight (fun l -> { l with waveLength = toWaveLength u x }) s with displayUnit = u }
    | SetIncidenceAngle a -> setLight (fun l -> { l with incidenceAngle = a }) s
    | SetPolarization p -> setLight (fun l -> { l with polarization = p }) s
    | SetEllipticity e -> setLight (fun l -> { l with ellipticity = e }) s
    | SetIntensity i -> { s with intensity = i }
    | SetDisplayUnit u -> { s with displayUnit = u }

// ---------------------------------------------------------------------------
// Single-value-or-range axis (§E.3 / AC-E3). The Fixed/Ranged toggle the editor
// surfaces; the Ranged case reuses the StandardLightVariables presets and the
// engine RangedVariable rather than constructing fresh Range<_> values. The
// produced RangedVariable is handed to the sweep layer (Part F) UNCHANGED — this
// module does NOT implement sweep execution / charting / calculate wiring.
// ---------------------------------------------------------------------------

type NumberOfPoints with

    /// A ranged wavelength axis of this many points over the §E.3 preset span (200–800 nm,
    /// `StandardLightVariables.fs:64`) — a `WaveLengthRange`, no fresh `Range<_>`.
    member this.wavelengthSourceAxis : SourceAxis<WaveLength> = Ranged this.wavelength200to800Range

    /// A ranged incidence-angle axis of this many points over the §E.3 preset span (0–89°,
    /// `StandardLightVariables.fs:40`) — an `IncidenceAngleRange`, no fresh `Range<_>`.
    member this.incidenceAngleSourceAxis : SourceAxis<IncidenceAngle> = Ranged this.incidenceAngleRange

type WaveLength with

    /// A fixed (single-value) wavelength axis carrying this committed value.
    member this.fixedSourceAxis : SourceAxis<WaveLength> = Fixed this

type SourceAxis<'T> with

    /// The `RangedVariable` a `Ranged` axis hands to the sweep layer unchanged
    /// (`None` for a `Fixed` axis). The matching case (`WaveLengthRange`/
    /// `IncidenceAngleRange`) is preserved verbatim — no new range type (§E.3).
    member this.rangedVariableOpt : RangedVariable option =
        match this with
        | Ranged r -> Some r
        | Fixed _ -> None

// ---------------------------------------------------------------------------
// Source palette discoverability (§E.10 / AC-E10). Adding a Source is a one-click
// palette action with no confirmation gate (a source is non-destructive and
// freely removable from the sources list of §E.8).
// ---------------------------------------------------------------------------

type SourceSpecId with

    /// A reasonable default new source under this id: a vacuum, normal-incidence, monochromatic
    /// 600 nm s-polarised collimated coherent source at unit intensity, nm display.
    member this.defaultSource : SourceSpec =
        {
            sourceId = this
            name = None
            light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)
            displayUnit = Nanometer
            intensity = IlluminationWeight 1.0
            spectralProfile = None
            cone = None
            gaussianBeam = None
            coherence = Coherent
        }

/// Whether adding a palette element asks for confirmation first (§E.10).
type AddConfirmation =
    | AddWithoutConfirmation
    | AddAfterConfirmation

/// A named element-palette entry. `AddWithoutConfirmation` makes adding the
/// element a one-click action with no gate (§E.10).
type PaletteEntry =
    {
        title : ControlCaption
        element : ConstructorElement
        addConfirmation : AddConfirmation
    }

/// The Source palette entry (§E.10): one-click add of a `ConstructorElement.SourceElement`
/// with no confirmation gate, reachable from the palette navigation.
let sourcePaletteEntry : PaletteEntry =
    {
        title = ControlCaption "Source"
        element = ConstructorElement.SourceElement
        addConfirmation = AddWithoutConfirmation
    }
