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
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain.Units

// ---------------------------------------------------------------------------
// Material colour (R-1 item 6). A deterministic, pure function of a material's
// stable identity key (the `materialEntry` id, §A.7): the same id is the same
// colour across every redraw. The table is a static map in this module; it is
// NOT a config knob. Unknown ids fall back to a stable hash into the palette, so
// the mapping is still deterministic for materials with no curated colour.
// ---------------------------------------------------------------------------

/// An sRGB colour as plain bytes — a pure value the deferred Avalonia view maps
/// to `Avalonia.Media.Color`; this module takes no Avalonia drawing dependency.
type SchematicColor =
    {
        red : byte
        green : byte
        blue : byte
    }

    /// The `#RRGGBB` hex form (handy for tests and the deferred view binding).
    member c.toHex = sprintf "#%02X%02X%02X" c.red c.green c.blue

let private rgb (r : int) (g : int) (b : int) : SchematicColor =
    { red = byte r; green = byte g; blue = byte b }

/// The fallback palette unknown material ids hash into (a fixed, deterministic set).
let private palette : SchematicColor [] =
    [|
        rgb 31 119 180; rgb 255 127 14; rgb 44 160 44; rgb 214 39 40
        rgb 148 103 189; rgb 140 86 75; rgb 227 119 194; rgb 188 189 34
    |]

/// Curated colours for the built-in material ids (`MaterialLibrary.builtInEntries`).
let private curated : Map<string, SchematicColor> =
    Map
        [
            "silicon",          rgb 90 90 110
            "langasite",        rgb 120 200 220
            "glass-1.52",       rgb 200 225 245
            "glass-1.50",       rgb 205 230 250
            "glass-1.75",       rgb 170 205 235
            "glass-2.00",       rgb 140 185 225
            "uniaxial-crystal", rgb 175 225 175
            "biaxial-crystal",  rgb 150 210 150
        ]

/// Deterministic, process-independent hash of a string (FNV-1a-style fold). NOT
/// `String.GetHashCode`, whose seed is randomised per process — that would change
/// an unknown material's colour between runs.
let private stableHash (s : string) : int =
    (s |> Seq.fold (fun acc ch -> (acc ^^^ int ch) * 16777619) (int 2166136261u)) &&& 0x7fffffff

/// The colour for a material's stable identity key (R-1 item 6): a curated colour
/// when one exists, otherwise a deterministic palette slot from the id's stable
/// hash. Pure and total — the same id always yields the same colour.
let colorForMaterial (materialId : string) : SchematicColor =
    match Map.tryFind materialId curated with
    | Some c -> c
    | None -> palette.[stableHash materialId % palette.Length]

// ---------------------------------------------------------------------------
// Band heights (R-1 item 1). Each film band's drawn height is a monotonic
// function of its Thickness in canonical meters; the half-space case
// (`Thickness.Infinity`) draws as a FIXED-height "semi-infinite" band, not scaled.
// ---------------------------------------------------------------------------

/// The fixed drawn height of a semi-infinite (`Thickness.Infinity`) band — the
/// half-space case is NOT scaled by thickness (R-1 item 1).
[<Literal>]
let semiInfiniteBandHeight = 48.0

/// The fixed drawn height of an incident/exit half-space medium band (R-1 item 3).
[<Literal>]
let halfSpaceBandHeight = 32.0

/// The minimum drawn height of a finite film band, so a very thin film is still
/// visible; the scaled term is added on top, keeping the height strictly
/// increasing in thickness.
let private minFiniteBandHeight = 6.0

/// Pixels per canonical meter for the scaled term. The drawn height is monotonic
/// in thickness (the only property R-1 item 1 requires); a display-layout pass
/// (the deferred view) normalises the band column to the available canvas height.
let private pixelsPerMeter = 2.0e8

/// The drawn band height for a layer thickness (R-1 item 1): `Thickness.Infinity`
/// → the fixed semi-infinite height; a finite `Thickness d` → a strictly
/// increasing function of `d` (`double<meter>`). No converted value is ever
/// written back into the model (§A.3).
let bandHeight (t : Thickness) : float =
    match t with
    | Thickness.Infinity -> semiInfiniteBandHeight
    | Thickness.Thickness d -> minFiniteBandHeight + pixelsPerMeter * (d / 1.0<meter>)

// ---------------------------------------------------------------------------
// Substrate band (R-1 item 4). A Plate draws as a parallel-edged band; a Wedge
// draws with a non-parallel lower edge whose slope comes from the wedge angle,
// making the wedge visually distinct from a plate.
// ---------------------------------------------------------------------------

/// The lower-edge slope (dy/dx) of a wedge substrate band, taken from the wedge's
/// `WedgeAngle` (`Fields.fs:328`, reached via `w.angle`): `tan(angle)`, so a
/// larger wedge angle tilts the edge more and a zero angle is flat.
let wedgeSlope (a : WedgeAngle) : float = tan a.value

/// How a substrate band is drawn (R-1 item 4): a `Plate` has a flat (parallel)
/// lower edge; a `Wedge` carries the non-zero lower-edge slope from its angle.
type SubstrateBand =
    | PlateBand
    | WedgeBand of slope : float

/// Classify a substrate into its drawn band shape. `Plate` → `PlateBand`; `Wedge`
/// → `WedgeBand` carrying the angle-derived `wedgeSlope` (`Media.fs:40`).
let substrateBand (s : Substrate) : SubstrateBand =
    match s with
    | Plate _ -> PlateBand
    | Wedge w -> WedgeBand (wedgeSlope w.angle)

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
        angleRadians : float
        incidentDx : float
        incidentDy : float
        reflectedDx : float
        reflectedDy : float
        transmittedDx : float
        transmittedDy : float
    }

/// Build the incident-ray geometry from the active source's
/// `IncidentLightInfo.incidenceAngle` (`Fields.fs:342`, reached via `.value`).
let rayGeometry (a : IncidenceAngle) : RayGeometry =
    let theta = a.value
    let sx = sin theta
    let cy = cos theta
    {
        angleRadians = theta
        incidentDx = sx;  incidentDy = cy        // descending into the stack
        reflectedDx = sx; reflectedDy = -cy      // mirrored about the normal (upward)
        transmittedDx = sx; transmittedDy = cy   // indicated continuation only
    }

// ---------------------------------------------------------------------------
// The whole-system band layout (R-1 items 1–4). A pure OpticalSystem -> band list
// projection: upper half-space, each film top-to-bottom in list order, the
// substrate (if any), then the lower half-space. Thickness labels reuse the
// StackEditor display seam (the SOLE Units boundary conversion, §D.2 / §A.10) —
// no second thickness formatter.
// ---------------------------------------------------------------------------

/// A single drawn band of the cross-section.
type Band =
    {
        label : string
        height : float
        color : SchematicColor
        substrate : SubstrateBand option   // Some only for the substrate band
    }

/// The ordered band layout for a system (R-1 items 1–4). `materialKey` maps a film
/// index to its stable material id for colouring (R-1 item 6) — the
/// `OpticalSystem.films` `Layer` carries no id of its own, so the caller (which
/// holds the project's per-layer material assignments, §A.7) supplies the key;
/// `unit` is the element's per-element default unit for the thickness labels.
let layout (unit : UnitOfMeasure) (materialKey : int -> string) (sys : OpticalSystem) : Band list =
    let upper = { label = "Incident medium"; height = halfSpaceBandHeight; color = rgb 245 245 245; substrate = None }
    let lower = { label = "Exit medium";     height = halfSpaceBandHeight; color = rgb 235 235 235; substrate = None }

    let filmBands =
        sys.films
        |> List.mapi (fun i l ->
            {
                label = sprintf "Layer %d — %s" i (StackEditor.displayThickness unit l.thickness)
                height = bandHeight l.thickness
                color = colorForMaterial (materialKey i)
                substrate = None
            })

    let substrateBands =
        match sys.substrate with
        | None -> []
        | Some s ->
            let label = match s with | Plate _ -> "Substrate (plate)" | Wedge _ -> "Substrate (wedge)"
            [ { label = label; height = semiInfiniteBandHeight; color = rgb 220 220 225; substrate = Some (substrateBand s) } ]

    [ upper ] @ filmBands @ substrateBands @ [ lower ]
