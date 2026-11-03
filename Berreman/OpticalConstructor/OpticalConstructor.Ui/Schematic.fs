/// §J.1 — to-scale schematic cross-section view [Core]. A vertical, drawn-to-scale
/// cross-section of a single `OpticalSystem` (`Media.fs:94`): film bands scaled by
/// `Thickness` (`Media.fs:12`), the half-space incident/exit media, the substrate
/// (a `Plate` as a parallel band, a `Wedge` with a sloped lower edge), per-layer
/// thickness labels in the element's default unit, and the incident-light ray at
/// its angle. The renderer reads geometry ONLY from canonical domain values and
/// introduces NO parallel geometry model.
///
/// Per the §0/P3 testability mandate and the established precedent
/// (`StackEditor.fs`, `ConstructionPage.fs`, `SourceEditorView.fs`) the geometry
/// is a pure projection carrying NO Avalonia type, so every AC-J1 case is provable
/// headless (band-height monotonicity, the fixed semi-infinite band, the sloped
/// wedge edge, the deterministic material colour). The FuncUI/Avalonia `Canvas`
/// `view` that binds these geometry values to controls is deferred to a later
/// UI-wiring slice (the public MIT `Avalonia.FuncUI` NuGet from slice 001, never
/// the audit-gated clone), exactly as the sibling modules record; no caching,
/// retained-mode scene graph, or drawing-backend abstraction (out of scope).
module OpticalConstructor.Ui.Schematic

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.MaterialLibrary
open OpticalConstructor.Optimization.DesignParameters
open OpticalConstructor.Controls
open OpticalConstructor.Controls.LayerBandsControls

// ---------------------------------------------------------------------------
// Material colour (R-1 item 6). A deterministic, pure function of a material's
// stable identity (its `MaterialId`, §A.7): the same id is the same colour across
// every redraw. The table is a static map in this module; it is NOT a config knob.
// Unknown ids fall back to a stable hash into the palette, so the mapping is still
// deterministic for materials with no curated colour.
// ---------------------------------------------------------------------------

/// The fallback palette unknown material ids hash into (a fixed, deterministic set).
let private palette : BandColorHex [] =
    [|
        BandColorHex "#1F77B4"; BandColorHex "#FF7F0E"; BandColorHex "#2CA02C"; BandColorHex "#D62728"
        BandColorHex "#9467BD"; BandColorHex "#8C564B"; BandColorHex "#E377C2"; BandColorHex "#BCBD22"
    |]

/// Curated colours for the built-in material ids (`MaterialLibrary.builtInEntries`), keyed by the
/// elevated `MaterialId` (spec 0033 step 002).
let private curated : Map<MaterialId, BandColorHex> =
    Map
        [
            MaterialIds.silicon,         BandColorHex "#5A5A6E"
            MaterialIds.langasite,       BandColorHex "#78C8DC"
            MaterialIds.glass152,        BandColorHex "#C8E1F5"
            MaterialIds.glass150,        BandColorHex "#CDE6FA"
            MaterialIds.glass175,        BandColorHex "#AACDEB"
            MaterialIds.glass200,        BandColorHex "#8CB9E1"
            MaterialIds.uniaxialCrystal, BandColorHex "#AFE1AF"
            MaterialIds.biaxialCrystal,  BandColorHex "#96D296"
        ]

/// The colour for a material (R-1 item 6): a curated colour when one exists, otherwise a
/// deterministic palette slot from a stable hash of the id's Guid string form. Pure and total —
/// the same id always yields the same colour.
let colorForMaterial (materialId : MaterialId) : BandColorHex =
    match Map.tryFind materialId curated with
    | Some c -> c
    | None ->
        // An FNV-1a fold, NOT `String.GetHashCode`: its seed is randomised per process, which would
        // change an unknown material's colour between runs.
        let hash = (string materialId.value |> Seq.fold (fun acc ch -> (acc ^^^ int ch) * 16777619) (int 2166136261u)) &&& 0x7fffffff
        palette.[hash % palette.Length]

// ---------------------------------------------------------------------------
// Band heights (R-1 item 1). Each film band's drawn height is a monotonic
// function of its Thickness in canonical meters; the half-space case
// (`Thickness.Infinity`) draws as a FIXED-height "semi-infinite" band, not scaled.
// ---------------------------------------------------------------------------

/// The fixed drawn height of a semi-infinite (`Thickness.Infinity`) band — the
/// half-space case is NOT scaled by thickness (R-1 item 1).
let semiInfiniteBandHeight : BandWeight = BandWeight 48.0

/// The fixed drawn height of an incident/exit half-space medium band (R-1 item 3).
let halfSpaceBandHeight : BandWeight = BandWeight 32.0

/// The drawn band height for a layer thickness (R-1 item 1): `Thickness.Infinity`
/// → the fixed semi-infinite height; a finite `Thickness d` → a strictly
/// increasing function of `d` (`double<meter>`). No converted value is ever
/// written back into the model (§A.3).
let bandHeight (t : Thickness) : BandWeight =
    match t with
    | Thickness.Infinity -> semiInfiniteBandHeight
    | Thickness.Thickness d ->
        // A floor keeps a very thin film visible; the scaled term on top keeps the height strictly
        // increasing in thickness, and the view normalises the band column to the canvas height.
        let minFiniteBandHeight = 6.0
        let pixelsPerMeter = 2.0e8
        BandWeight (minFiniteBandHeight + pixelsPerMeter * (d / 1.0<meter>))

// ---------------------------------------------------------------------------
// Substrate band (R-1 item 4). A Plate draws as a parallel-edged band; a Wedge
// draws with a non-parallel lower edge whose slope comes from the wedge angle,
// making the wedge visually distinct from a plate.
// ---------------------------------------------------------------------------

/// The lower-edge slope (dy/dx) of a wedge substrate band.
type WedgeEdgeSlope =
    | WedgeEdgeSlope of float

    member this.value = let (WedgeEdgeSlope s) = this in s

type WedgeAngle with

    /// The lower-edge slope of this wedge's substrate band (`Fields.fs:328`, reached via `w.angle`):
    /// `tan(angle)`, so a larger wedge angle tilts the edge more and a zero angle is flat.
    member this.edgeSlope : WedgeEdgeSlope = WedgeEdgeSlope (tan this.value)

/// How a substrate band is drawn (R-1 item 4): a `Plate` has a flat (parallel)
/// lower edge; a `Wedge` carries the non-zero lower-edge slope from its angle.
type SubstrateBand =
    | PlateBand
    | WedgeBand of slope : WedgeEdgeSlope

type Substrate with

    /// This substrate's drawn band shape. `Plate` → `PlateBand`; `Wedge` → `WedgeBand`
    /// carrying the angle-derived `edgeSlope` (`Media.fs:40`).
    member this.substrateBand : SubstrateBand =
        match this with
        | Plate _ -> PlateBand
        | Wedge w -> WedgeBand w.angle.edgeSlope

// ---------------------------------------------------------------------------
// Incident-light ray (R-1 item 5). A line entering the upper medium at the active
// source's incidence angle, with reflected and transmitted continuations
// indicated AT THE TOP FILM INTERFACE ONLY (full per-interface beam tracing is
// Part B's beam tree and is NOT re-derived here).
// ---------------------------------------------------------------------------

/// The schematic ray geometry as unit direction vectors (+x to the right, +y down
/// into the stack). The incident ray descends at the incidence angle; the
/// reflected ray mirrors it about the surface normal (y flips); the transmitted
/// ray is merely indicated continuing into the stack (Snell refraction is Part B).
type RayGeometry =
    {
        incidenceAngle : IncidenceAngle
        incidentDirection : RealVector2
        reflectedDirection : RealVector2
        transmittedDirection : RealVector2
    }

type IncidenceAngle with

    /// The incident-ray geometry at this angle — the active source's
    /// `IncidentLightInfo.incidenceAngle` (`Fields.fs:342`).
    member this.rayGeometry : RayGeometry =
        let theta = this.value
        let sx = sin theta
        let cy = cos theta
        {
            incidenceAngle = this
            incidentDirection = RealVector2.create [ sx; cy ]      // descending into the stack
            reflectedDirection = RealVector2.create [ sx; -cy ]    // mirrored about the normal (upward)
            transmittedDirection = RealVector2.create [ sx; cy ]   // indicated continuation only
        }

// ---------------------------------------------------------------------------
// The whole-system band layout (R-1 items 1–4). A pure OpticalSystem -> band list
// projection: upper half-space, each film top-to-bottom in list order, the
// substrate (if any), then the lower half-space. Thickness labels reuse the
// StackEditor display seam (the SOLE Units boundary conversion, §D.2 / §A.10) —
// no second thickness formatter.
// ---------------------------------------------------------------------------

/// A single drawn band of the cross-section.
type SchematicBand =
    {
        label : ControlCaption
        height : BandWeight
        color : BandColorHex
        substrate : SubstrateBand option   // Some only for the substrate band
    }

/// The ordered band layout for a system (R-1 items 1–4). `materialKey` maps a film
/// to its material for colouring (R-1 item 6) — the `OpticalSystem.films` `Layer`
/// carries no id of its own, so the caller (which holds the project's per-layer
/// material assignments, §A.7) supplies it; `unit` is the element's per-element
/// default unit for the thickness labels.
let layoutSchematic (unit : UnitOfMeasure) (materialKey : FilmIndex -> MaterialId) (sys : OpticalSystem) : SchematicBand list =
    let upper : SchematicBand = { label = ControlCaption "Incident medium"; height = halfSpaceBandHeight; color = BandColorHex "#F5F5F5"; substrate = None }
    let lower : SchematicBand = { label = ControlCaption "Exit medium";     height = halfSpaceBandHeight; color = BandColorHex "#EBEBEB"; substrate = None }

    let filmBands =
        sys.films
        |> List.mapi (fun i l ->
            let film = FilmIndex i
            let band : SchematicBand =
                {
                    label = ControlCaption $"Layer %d{i} — %s{(StackEditor.displayThickness unit l.thickness).value}"
                    height = bandHeight l.thickness
                    color = colorForMaterial (materialKey film)
                    substrate = None
                }
            band)

    let substrateBands =
        match sys.substrate with
        | None -> []
        | Some s ->
            let label = match s with | Plate _ -> ControlCaption "Substrate (plate)" | Wedge _ -> ControlCaption "Substrate (wedge)"
            let band : SchematicBand = { label = label; height = semiInfiniteBandHeight; color = BandColorHex "#DCDCE1"; substrate = Some s.substrateBand }
            [ band ]

    [ upper ] @ filmBands @ substrateBands @ [ lower ]
