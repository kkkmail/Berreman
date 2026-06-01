/// §J.12 — 2D/3D rendered view of the element/system with the beam path [Standard]
/// (010 §1 line 127). `SystemView3D` delivers the custom OpenTK 3D system/beam
/// viewport that Part A RESERVES for exactly this surface (§A.1, §A.6) — this is
/// the part that CONSUMES that reservation. It draws, to scale, the placed
/// `ConstructorElement`s of the open project's `BeamTree`/`BeamNode` topology
/// (§A.4) positioned along the beam path, and draws the beam path itself as the
/// reflected/transmitted ray segments, reading each segment's direction from the
/// ALREADY-SOLVED `EmFieldSystem.reflected`/`.transmitted` (`Fields.fs:553-554`,
/// the Poynting unit normal `EmField.normal`) that Part B produced.
///
/// This part MUST NOT re-solve the system or re-derive beam routing: every geometry
/// function takes the already-solved `EmFieldSystem` (or the solved beam tree) as
/// INPUT and renders it. Geometry reads canonical SI (meters) from the domain
/// values and converts to viewport coordinates ONLY at the render boundary (§A.3);
/// no converted value is written back into the model.
///
/// Per §0/P3 the geometry is a pure projection carrying NO OpenTK type, so AC-J12
/// is provable headless (segment direction = the solved branch normal; no re-solve;
/// no non-SI write-back). The OpenTK surface that binds these values is a later
/// UI-wiring slice. This viewport is DISTINCT from — and does not duplicate — the
/// §J.1 Avalonia-`Canvas` 2D schematic (slice 014) and the §E.4/§H.8 Poincaré-sphere
/// OpenTK use; it adds no full 3D vector wave optics, diffraction, or second physics
/// engine (Part A §A.5), and no retained-mode scene graph / LOD / camera persistence
/// (out of scope §J.12).
module OpticalConstructor.Ui.SystemView3D

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open OpticalConstructor.Domain.BeamTree

// ---------------------------------------------------------------------------
// Viewport coordinates (pure; the deferred OpenTK view maps `Vec3` to a
// `Vector3`). NO OpenTK type appears in this module.
// ---------------------------------------------------------------------------

/// A viewport 3-vector — plain floats the deferred OpenTK renderer maps onto its
/// own vector type; this module takes no OpenTK drawing dependency.
type Vec3 =
    {
        x : float
        y : float
        z : float
    }

/// The default viewport scale: pixels (viewport units) per canonical meter. A
/// display-layout constant, NOT a config knob; the deferred view normalises the
/// scene to the available surface.
[<Literal>]
let defaultPixelsPerMeter = 2.0e5

/// Convert a canonical-SI length to a viewport coordinate at the render boundary
/// (§A.3). Pure — it returns a new float and never writes the converted value back
/// into the model.
let metersToViewport (pixelsPerMeter : float) (m : float<meter>) : float =
    pixelsPerMeter * (m / 1.0<meter>)

// ---------------------------------------------------------------------------
// Element placement to scale (R-3). Each ConstructorElement is laid out along the
// beam path (+z), sized by its system's finite-film thickness in canonical meters
// scaled at the render boundary. Half-spaces (Thickness.Infinity) contribute no
// finite extent.
// ---------------------------------------------------------------------------

/// The total finite-film thickness of a system in canonical meters (the half-space
/// `Thickness.Infinity` films contribute nothing), read straight off the engine
/// `Thickness` (`Media.fs:12`) — not re-typed.
let systemThicknessMeters (sys : OpticalSystem) : float<meter> =
    sys.films
    |> List.sumBy (fun l ->
        match l.thickness with
        | Thickness.Thickness d -> d
        | Thickness.Infinity -> 0.0<meter>)

/// A placed element of the system view: its `ConstructorElement`, its viewport
/// position along the beam path, and its drawn extent along that path (both scaled
/// from canonical meters at the render boundary).
type PlacedElement =
    {
        element : ConstructorElement
        position : Vec3
        extent : float
    }

/// The nominal viewport gap between consecutive elements, in canonical meters (a
/// layout constant so a zero-thickness element — Source/Detector — still spaces
/// out). Converted to viewport units like every other length.
let private elementGapMeters = 5.0e-3<meter>

/// Lay the beam tree's `ConstructorElement`s out to scale along the beam path
/// (R-3). A depth-first walk in deterministic branch order (`Reflected` before
/// `Transmitted`, the `BeamBranch` declaration order `Map.toList` yields) places
/// each node at the accumulated path position and sizes it by its system thickness;
/// nothing is written back into the model (the positions are fresh `Vec3` values).
let placeElements (pixelsPerMeter : float) (tree : BeamTree) : PlacedElement list =
    let rec go (zStart : float<meter>) (node : BeamNode) : float<meter> * PlacedElement list =
        let t = systemThicknessMeters node.system
        let placed =
            {
                element = node.element
                position = { x = 0.0; y = 0.0; z = metersToViewport pixelsPerMeter zStart }
                extent = metersToViewport pixelsPerMeter t
            }
        let zAfter = zStart + t + elementGapMeters
        let childNodes = node.children |> Map.toList |> List.map snd
        let (zEnd, childPlaced) =
            childNodes
            |> List.fold
                (fun (z, acc) child ->
                    let (z', cp) = go z child
                    (z', acc @ cp))
                (zAfter, [])
        (zEnd, placed :: childPlaced)

    go 0.0<meter> tree.root |> snd

// ---------------------------------------------------------------------------
// Beam-path segments (R-3). Each segment's direction is READ from the already-
// solved EmFieldSystem.reflected/.transmitted (Fields.fs:553-554) via the engine's
// Poynting unit normal EmField.normal — CONSUME the solved beam tree, never
// re-solve or re-route.
// ---------------------------------------------------------------------------

/// A drawn beam segment: which branch it is, its viewport origin (the parent
/// element's position), and its unit direction in viewport axes — taken from the
/// solved branch field, NOT re-derived.
type BeamSegment =
    {
        branch : BeamBranch
        origin : Vec3
        direction : Vec3
    }

/// The viewport direction of the `branch` beam leaving a SOLVED node: read from the
/// already-solved `EmFieldSystem` (`ems` is the input — this function NEVER calls
/// the solver) via `branchEmField` (`Fields.fs:552-554`) and the engine's Poynting
/// unit normal `EmField.normal`. `None` when the solved field is degenerate
/// (norm ≈ 0). The viewport axes mirror the canonical axes 1:1 (a unit vector
/// carries no length to scale), so no non-SI value is written back.
let beamDirection (branch : BeamBranch) (ems : EmFieldSystem) : Vec3 option =
    (branchEmField branch ems).normal
    |> Option.map (fun n -> { x = n.x; y = n.y; z = n.z })

/// The reflected and transmitted beam segments leaving a SOLVED node placed at
/// `origin` (R-3): each direction is read from `ems` via `beamDirection`, so the
/// rendered beam path is the engine's already-solved routing. A degenerate branch
/// (no `normal`) is dropped. `ems` is the solved input — no re-solve happens here.
let beamSegments (origin : Vec3) (ems : EmFieldSystem) : BeamSegment list =
    [ Reflected; Transmitted ]
    |> List.choose (fun branch ->
        beamDirection branch ems
        |> Option.map (fun dir -> { branch = branch; origin = origin; direction = dir }))
