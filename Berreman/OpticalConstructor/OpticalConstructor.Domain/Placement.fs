namespace OpticalConstructor.Domain

open Berreman.Constants
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media

/// Element geometry / placement / rotation domain (Spec 0026 Part A). This is a
/// NET-NEW, pure, headless-testable layer that sits ABOVE the beam-tree topology
/// (`BeamTree.fs`) and the units spine (`Units.fs`). It introduces NO drawing
/// code (Part C / slice 004) and NO ray routing (Part B / slice 002): only the
/// placement record, its bounding box and two normals, the three rotations with
/// per-axis locks, the catalogue-kind tag, the value-id slot, the emission
/// metadata, and the catalogue -> domain mapping. Every magnitude is held in
/// canonical SI meters (constraint 0.2); angles reuse the engine `Angle` type
/// (`Geometry.fs:34`) rather than a parallel rotation representation (R-9).
module Placement =

    /// A pure, serializable 3-vector for box normals / oriented axes. This is
    /// deliberately NOT the Math.NET-backed engine `RealVector3` (`Geometry.fs:72`):
    /// placement must JSON-round-trip and pass schema validation (AC-A6), and a
    /// plain `{x;y;z}` record serializes through System.Text.Json with no custom
    /// converter while keeping every geometry rule provable headless (constraint
    /// 0.3). It carries a direction only — no unit of measure.
    type Vector3 =
        {
            x : float
            y : float
            z : float
        }

        static member create (x : float) (y : float) (z : float) : Vector3 = { x = x; y = y; z = z }

        member v.dot (w : Vector3) : float = v.x * w.x + v.y * w.y + v.z * w.z

        member v.cross (w : Vector3) : Vector3 =
            {
                x = v.y * w.z - v.z * w.y
                y = v.z * w.x - v.x * w.z
                z = v.x * w.y - v.y * w.x
            }

        member v.norm : float = sqrt (v.dot v)

        member v.normalized : Vector3 =
            let n = v.norm
            if n = 0.0 then v else { x = v.x / n; y = v.y / n; z = v.z / n }

    /// The central-ray direction in the table frame at rest (the +X axis). An
    /// unrotated element's primary normal N1 points along this (A.3.3).
    let centralRayDirection : Vector3 = Vector3.create 1.0 0.0 0.0

    /// The table-normal direction (the +Z axis, "up" out of the table plane). An
    /// unrotated element's secondary normal N2 points along this (A.3.3).
    let tableNormal : Vector3 = Vector3.create 0.0 0.0 1.0

    /// Default face extent of an optical element (polarizer, sample, lens, mirror,
    /// detector): A_O (A.3.4). A single edit here restyles every optical element.
    let opticalFaceExtent : float<meter> = 0.05<meter>

    /// Default depth of an optical element along its primary normal: B_O (A.3.4).
    let opticalDepth : float<meter> = 0.01<meter>

    /// Default face extent of a light source: A_L (A.3.4).
    let sourceFaceExtent : float<meter> = 0.025<meter>

    /// Default depth of a light source along its primary normal: B_L (A.3.4);
    /// `B_L > A_L` makes the source long and narrow.
    let sourceDepth : float<meter> = 0.08<meter>

    /// The element's rectangular bounding box (A.3): an `A1 x A2 x B` box — a
    /// square `a1 x a2` face (`a1 = a2` for the standard box) extruded by depth
    /// `b` along the primary normal N1. The box carries TWO normals (A.3.2):
    ///   * `n1` — primary normal, perpendicular to the `a1 x a2` face; R1 spins
    ///     about N1 (R1 and N1 coincide by definition);
    ///   * `n2` — secondary normal, perpendicular to N1 in the rest pose.
    /// At rest pose the stored normals are `centralRayDirection` / `tableNormal`
    /// (A.3.3); `Placement.orientedBasis` applies the rotations to them.
    type BoundingBox =
        {
            a1 : float<meter>
            a2 : float<meter>
            b : float<meter>
            n1 : Vector3
            n2 : Vector3
        }

    /// The UI-facing catalogue roles of Part F (A.5.1). This is a presentation
    /// distinction (e.g. linear vs circular polarizer) layered over the engine
    /// `ConstructorElement` domain DU (`BeamTree.fs:35`); `toConstructorElement`
    /// collapses it onto the existing domain cases. The engine `Analyzer` case is
    /// deliberately NOT a catalogue role (A.5.2).
    type CatalogueKind =
        | LightSource
        | LinearPolarizer
        | CircularPolarizer
        | Sample
        | Lens
        | FlatMirror
        | CurvedMirror
        | Detector

    /// Per-element ray-group emission metadata (A.7 / R-7). The three-state DU
    /// makes the "both groups suppressed" state UNREPRESENTABLE by construction
    /// (AC-A5): an element emits its reflected group, its transmitted group, or
    /// both — never neither. The smart setters below enforce the invariant
    /// "turning the second one off automatically turns the other back on".
    type Emission =
        | EmitReflectedOnly
        | EmitTransmittedOnly
        | EmitBoth

        member e.emitsReflected : bool =
            match e with
            | EmitReflectedOnly | EmitBoth -> true
            | EmitTransmittedOnly -> false

        member e.emitsTransmitted : bool =
            match e with
            | EmitTransmittedOnly | EmitBoth -> true
            | EmitReflectedOnly -> false

        /// Turn the reflected group on/off (A.7.1). Turning it OFF can never
        /// produce the both-off state: it forces the transmitted group back on.
        /// Turning it ON keeps the transmitted group as-is.
        static member withReflected (on : bool) (e : Emission) : Emission =
            match on, e.emitsTransmitted with
            | true, true -> EmitBoth
            | true, false -> EmitReflectedOnly
            | false, _ -> EmitTransmittedOnly

        /// Turn the transmitted group on/off (A.7.1), symmetric to `withReflected`:
        /// turning it OFF forces the reflected group back on.
        static member withTransmitted (on : bool) (e : Emission) : Emission =
            match on, e.emitsReflected with
            | true, true -> EmitBoth
            | true, false -> EmitTransmittedOnly
            | false, _ -> EmitReflectedOnly

    /// A point on the 2-D table plane in canonical meters (R-2). The placement
    /// point is the CENTRE of the element's bounding box — drawing, snapping
    /// (Part B), and slide bounds (Part E) all treat it as the centre, never an
    /// edge or corner.
    type TablePoint =
        {
            x : float<meter>
            y : float<meter>
        }

        static member origin : TablePoint = { x = 0.0<meter>; y = 0.0<meter> }

    /// The standard `A_O x A_O x B_O` box shared by every optical element and the
    /// detector (A.3.4), with rest-pose normals.
    let private restBox (a1 : float<meter>) (a2 : float<meter>) (b : float<meter>) : BoundingBox =
        { a1 = a1; a2 = a2; b = b; n1 = centralRayDirection; n2 = tableNormal }

    /// The default bounding box for a catalogue role (A.3.4). Optical elements and
    /// the detector are thin-and-wide `A_O x A_O x B_O`; the light source is
    /// long-and-narrow `A_L x A_L x B_L`.
    let defaultBox (kind : CatalogueKind) : BoundingBox =
        match kind with
        | LightSource -> restBox sourceFaceExtent sourceFaceExtent sourceDepth
        | _ -> restBox opticalFaceExtent opticalFaceExtent opticalDepth

    /// The default emission for a catalogue role (A.7.2). A mirror suppresses
    /// transmission (consistent with `BeamNode.attach`, `BeamTree.fs:74` — a
    /// mirror carries the reflected branch only); every other element emits both.
    let defaultEmission (kind : CatalogueKind) : Emission =
        match kind with
        | FlatMirror | CurvedMirror -> EmitReflectedOnly
        | _ -> EmitBoth

    /// The immutable per-element placement record (A.1 / R-1). Holds the box
    /// centre, the three rotation angles (engine `Angle`), the three independent
    /// lock flags, the catalogue-kind tag, the value-id slot, the bounding box,
    /// the emission metadata, and the preferred display unit (display metadata
    /// only, mirroring `BeamNode.defaultUnit`).
    type ElementPlacement =
        {
            placementPoint : TablePoint
            r1 : Angle
            r2 : Angle
            r3 : Angle
            r1Locked : bool
            r2Locked : bool
            r3Locked : bool
            catalogueKind : CatalogueKind
            valueId : string option
            box : BoundingBox
            emission : Emission
            displayUnit : Units.UnitOfMeasure
        }

        /// A freshly-placed element (A.1.2 / R-6 / R-7.2): R3 is locked by default
        /// and R1/R2 are unlocked (R3 must be deliberately unlocked before it can
        /// change); `valueId` is unbound (`None`); the box and emission default by
        /// role; the display unit defaults to millimeters (a bench-scale length).
        static member create (kind : CatalogueKind) (point : TablePoint) : ElementPlacement =
            {
                placementPoint = point
                r1 = Angle.zero
                r2 = Angle.zero
                r3 = Angle.zero
                r1Locked = false
                r2Locked = false
                r3Locked = true
                catalogueKind = kind
                valueId = None
                box = defaultBox kind
                emission = defaultEmission kind
                displayUnit = Units.Millimeter
            }

    /// Lock-respecting R1 setter (A.4.5): a locked axis ignores the change request.
    let withR1 (a : Angle) (p : ElementPlacement) : ElementPlacement =
        if p.r1Locked then p else { p with r1 = a }

    /// Lock-respecting R2 setter (A.4.5).
    let withR2 (a : Angle) (p : ElementPlacement) : ElementPlacement =
        if p.r2Locked then p else { p with r2 = a }

    /// Lock-respecting R3 setter (A.4.5). R3 starts locked (A.1.2), so this is
    /// inert until `setR3Locked false` has been called.
    let withR3 (a : Angle) (p : ElementPlacement) : ElementPlacement =
        if p.r3Locked then p else { p with r3 = a }

    /// Lock / unlock R1 (A.4.5), freezing it at its current value.
    let setR1Locked (locked : bool) (p : ElementPlacement) : ElementPlacement = { p with r1Locked = locked }

    /// Lock / unlock R2 (A.4.5).
    let setR2Locked (locked : bool) (p : ElementPlacement) : ElementPlacement = { p with r2Locked = locked }

    /// Lock / unlock R3 (A.4.5).
    let setR3Locked (locked : bool) (p : ElementPlacement) : ElementPlacement = { p with r3Locked = locked }

    /// Rotate `v` about the unit-normalized `axis` by `angle` (Rodrigues' rotation
    /// formula). Reuses the engine `Angle` for the radian value (R-9). Used to
    /// build the oriented basis from the box's rest normals.
    let private rotateAbout (axis : Vector3) (angle : Angle) (v : Vector3) : Vector3 =
        let k = axis.normalized
        let c = cos angle.value
        let s = sin angle.value
        let kv = k.dot v
        let kxv = k.cross v
        {
            x = v.x * c + kxv.x * s + k.x * kv * (1.0 - c)
            y = v.y * c + kxv.y * s + k.y * kv * (1.0 - c)
            z = v.z * c + kxv.z * s + k.z * kv * (1.0 - c)
        }

    /// The element's oriented `(N1, N2, N3)` basis in the table frame (A.3.3 /
    /// A.4). The rotations are applied to the box's rest normals in mount order:
    ///   * R3 about the third box axis (`N1 x N2`) tips the face OUT of the table
    ///     plane (A.4.4);
    ///   * R2 about the table normal steers the element within the plane (A.4.2);
    ///   * R1 about the (now oriented) face normal spins the element (A.4.1).
    /// R1 leaves N1 unchanged (it IS the spin axis). At rest pose (R1=R2=R3=0) the
    /// result is `(centralRayDirection, tableNormal, N1 x N2)` (AC-A1).
    let orientedBasis (p : ElementPlacement) : Vector3 * Vector3 * Vector3 =
        let n1Rest = p.box.n1
        let n2Rest = p.box.n2
        let n3Rest = n1Rest.cross n2Rest
        // R3 about the third axis: tips the face normal out of the table plane.
        let n1a = rotateAbout n3Rest p.r3 n1Rest
        let n2a = rotateAbout n3Rest p.r3 n2Rest
        // R2 about the table normal: in-plane steer.
        let n1b = rotateAbout tableNormal p.r2 n1a
        let n2b = rotateAbout tableNormal p.r2 n2a
        let n3b = rotateAbout tableNormal p.r2 n3Rest
        // R1 about the (now oriented) face normal: spins the element about N1.
        let n2c = rotateAbout n1b p.r1 n2b
        let n3c = rotateAbout n1b p.r1 n3b
        (n1b, n2c, n3c)

    /// The element's oriented primary/secondary normals (A.3.2-A.3.3). At rest
    /// pose N1 points along the central ray and N2 along the table normal (AC-A1).
    let orientedNormals (p : ElementPlacement) : Vector3 * Vector3 =
        let (n1, n2, _) = orientedBasis p
        (n1, n2)

    /// The R1 axis (the oriented primary normal N1). At rest it coincides with the
    /// central ray; once R2 != 0 it decouples from the central ray (A.4.1); once
    /// R3 != 0 it tips out of the table plane so it is no longer perpendicular to
    /// the R2 axis (A.4.4 / AC-A4).
    let r1Axis (p : ElementPlacement) : Vector3 =
        let (n1, _, _) = orientedBasis p
        n1

    /// The R2 axis is the table normal (A.4.2): R2 turns the element about it,
    /// independent of the element's own orientation.
    let r2Axis : Vector3 = tableNormal

    /// The element's table-frame orientation (A.4.3 / AC-A3). R2 is measured FROM
    /// the incident ray, not from a fixed table axis, so an element whose incident
    /// ray already makes angle `incidentRayAngle` with the horizontal and whose
    /// own dialled tilt is `r2` has table-frame orientation `incidentRayAngle + r2`.
    let tableFrameOrientation (incidentRayAngle : Angle) (r2 : Angle) : Angle =
        incidentRayAngle + r2

    /// A placeholder vacuum stack for a freshly-mapped Sample (A.5.2). The
    /// concrete material behind a Sample is bound later through `valueId` (R-6 /
    /// Part F); until then the engine domain case carries a vacuum system so the
    /// catalogue -> domain mapping stays a total, pure function.
    let private placeholderSampleSystem : OpticalSystem =
        {
            description = Some "Placeholder vacuum sample (Spec 0026 A.5.2; material bound via valueId in Part F)"
            upper = OpticalProperties.vacuum
            films = []
            substrate = None
            lower = OpticalProperties.vacuum
        }

    /// Map a catalogue kind to its existing engine domain case (A.5.2 / AC-A7):
    /// Light Source -> `Source`; BOTH Linear and Circular Polarizer -> `Polarizer`
    /// (a polarizer is a polarizer regardless of placement; linear vs circular is
    /// a catalogue-kind distinction, not a new domain case); Sample -> `Sample`;
    /// Lens -> `Lens`; Flat Mirror -> `FlatMirror`; Curved Mirror -> `CurvedMirror`;
    /// Detector -> `Detector`. The engine `Analyzer` case (`BeamTree.fs:42`) is
    /// never produced and is left untouched.
    let toConstructorElement (kind : CatalogueKind) : BeamTree.ConstructorElement =
        match kind with
        | LightSource -> BeamTree.Source
        | LinearPolarizer -> BeamTree.Polarizer
        | CircularPolarizer -> BeamTree.Polarizer
        | Sample -> BeamTree.Sample placeholderSampleSystem
        | Lens -> BeamTree.Lens
        | FlatMirror -> BeamTree.FlatMirror
        | CurvedMirror -> BeamTree.CurvedMirror
        | Detector -> BeamTree.Detector
