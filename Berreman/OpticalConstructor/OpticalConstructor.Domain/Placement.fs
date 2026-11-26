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

    /// A real number of the table's vector space: a scale factor, a length along a
    /// direction or an inner product — one type for all three, as STL's
    /// `Softellect.Math.Scalar` (STL spec 0219 plan 4.5).
    type VectorScalar =
        | VectorScalar of float

        member this.value = let (VectorScalar v) = this in v

        static member zero : VectorScalar = VectorScalar 0.0
        static member (+) (VectorScalar a, VectorScalar b) : VectorScalar = VectorScalar (a + b)
        static member (-) (VectorScalar a, VectorScalar b) : VectorScalar = VectorScalar (a - b)
        static member (*) (VectorScalar a, VectorScalar b) : VectorScalar = VectorScalar (a * b)
        static member (/) (VectorScalar a, VectorScalar b) : VectorScalar = VectorScalar (a / b)
        static member (~-) (VectorScalar a) : VectorScalar = VectorScalar (-a)

        /// Resolved by the core `abs` / `sqrt` operators.
        static member Abs (VectorScalar a) : VectorScalar = VectorScalar (abs a)
        static member Sqrt (VectorScalar a) : VectorScalar = VectorScalar (sqrt a)

    /// A pure, serializable 3-vector for box normals / oriented axes. This is
    /// deliberately NOT the Math.NET-backed engine `RealVector3` (`Geometry.fs:72`):
    /// placement must JSON-round-trip and pass schema validation (AC-A6), and a
    /// plain `{x;y;z}` record serializes through System.Text.Json with no custom
    /// converter while keeping every geometry rule provable headless (constraint
    /// 0.3). It carries a direction only — no unit of measure.
    [<Softellect.Sys.NumericCarrier("the components of a table-frame 3-vector, the numbers every rotation, reflection, refraction and crossing of the ray geometry computes with (R3; the serializable twin of the engine's MathNet-backed RealVector3)")>]
    type Vector3 =
        {
            x : float
            y : float
            z : float
        }

        static member create (x : float) (y : float) (z : float) : Vector3 = { x = x; y = y; z = z }

        member v.dot (w : Vector3) : VectorScalar = VectorScalar (v.x * w.x + v.y * w.y + v.z * w.z)

        member v.cross (w : Vector3) : Vector3 =
            {
                x = v.y * w.z - v.z * w.y
                y = v.z * w.x - v.x * w.z
                z = v.x * w.y - v.y * w.x
            }

        member v.norm : float = sqrt (v.dot v).value

        member v.normalized : Vector3 =
            let n = v.norm
            if n = 0.0 then v else { x = v.x / n; y = v.y / n; z = v.z / n }

    /// The two orthonormal vectors spanning the cross-section perpendicular to a
    /// direction (B.1.1). `inPlane` is perpendicular to the direction within the
    /// table plane; `outOfPlane` completes the right-handed frame (the table normal
    /// for an in-plane ray). The eight side rays of a bundle lie on the unit circle
    /// these two span — that is the "rifle-target" cross-section.
    type CrossSection =
        {
            inPlane : Vector3
            outOfPlane : Vector3
        }

    /// The absolute R2 (`steer`, in the table plane) and R3 (`tip`, out of it) that
    /// orient a rest element's primary normal N1 along a direction (A.4.3).
    type BeamOrientation =
        {
            steer : Angle
            tip : Angle
        }

    type Vector3 with

        /// The cross-section perpendicular to this direction (B.1.1).
        member direction.crossSection : CrossSection =
            let d = direction.normalized
            let inPlane =
                let candidate = Vector3.create (-d.y) d.x 0.0
                if candidate.norm < 1.0e-12 then Vector3.create 0.0 1.0 0.0
                else candidate.normalized
            { inPlane = inPlane; outOfPlane = (d.cross inPlane).normalized }

        /// The absolute `(R2, R3)` that orient a rest element's primary normal N1 (the +X central-ray
        /// direction) along this direction — so an element with these angles FACES the beam travelling
        /// along it. With the rest basis (N1 = +X, table normal = +Z) the oriented normal is
        /// `(cosR3·cosR2, cosR3·sinR2, sinR3)`, hence `R2 = atan2(dir.y, dir.x)`, `R3 = asin(dir.z)`. Used to
        /// auto-orient an element to the (possibly reflected) beam it sits on: add the element's own dialled
        /// R2/R3 on top, so at dialled 0 it faces the beam and a dialled value tilts it relative to the beam.
        member direction.beamOrientation : BeamOrientation =
            let d = direction.normalized
            { steer = Angle (atan2 d.y d.x); tip = Angle (asin (max -1.0 (min 1.0 d.z))) }

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
    /// collapses it onto the existing domain cases. The engine `AnalyzerElement` case is
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

    /// Whether a ray group is switched on or off (A.7.1).
    type RayGroupSwitch =
        | RayGroupOn
        | RayGroupOff

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
        static member withReflected (switch : RayGroupSwitch) (e : Emission) : Emission =
            match switch, e.emitsTransmitted with
            | RayGroupOn, true -> EmitBoth
            | RayGroupOn, false -> EmitReflectedOnly
            | RayGroupOff, _ -> EmitTransmittedOnly

        /// Turn the transmitted group on/off (A.7.1), symmetric to `withReflected`:
        /// turning it OFF forces the reflected group back on.
        static member withTransmitted (switch : RayGroupSwitch) (e : Emission) : Emission =
            match switch, e.emitsReflected with
            | RayGroupOn, true -> EmitBoth
            | RayGroupOn, false -> EmitTransmittedOnly
            | RayGroupOff, _ -> EmitReflectedOnly

    /// A drawn ray group (B.2.1). The INCIDENT group (IRG) falls on an element and
    /// is the frame against which the element's rotations are measured (A.4.3). The
    /// REFLECTED (RRG) and TRANSMITTED (TRG) groups leave the element and correspond
    /// to the engine `ReflectedBeam` / `TransmittedBeam` branches (BeamTree.fs, B.7.1).
    type RayGroupKind =
        | Incident
        | ReflectedRayGroup
        | TransmittedRayGroup

        /// The engine beam-tree branch a drawn outgoing group corresponds to (B.7.1).
        /// The incident group is not an engine branch (`None`). The drawing MUST NOT
        /// invent a branch the engine does not compute.
        member kind.toBeamBranch : BeamTree.BeamBranch option =
            match kind with
            | ReflectedRayGroup -> Some BeamTree.ReflectedBeam
            | TransmittedRayGroup -> Some BeamTree.TransmittedBeam
            | Incident -> None

    type Emission with

        /// The OUTGOING groups an element with this emission produces (B.2.1). Every
        /// element except a mirror produces both an RRG and a TRG; a mirror produces an
        /// RRG only. This follows the A.7 emission metadata exactly (R-7).
        member emission.emittedGroups : RayGroupKind list =
            match emission with
            | EmitReflectedOnly -> [ ReflectedRayGroup ]
            | EmitTransmittedOnly -> [ TransmittedRayGroup ]
            | EmitBoth -> [ ReflectedRayGroup; TransmittedRayGroup ]

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

        /// The point lifted into the table plane (z = 0), in canonical meters. The
        /// placement point is the box centre (A.2).
        member p.toVector3 : Vector3 =
            Vector3.create (p.x / 1.0<meter>) (p.y / 1.0<meter>) 0.0

    /// A table-frame direction an element can be moved along (spec 0046): x along the
    /// table's length (the central ray at rest), y across it, z up out of the table plane.
    type TableAxis =
        | TableX
        | TableY
        | TableZ

    /// Whether an element may be moved along one table axis (spec 0046).
    type MoveLock =
        | MoveLocked
        | MoveFree

    /// The per-axis move locks of a placement (spec 0046), one per `TableAxis`.
    type MoveLocks =
        {
            xLock : MoveLock
            yLock : MoveLock
            zLock : MoveLock
        }

    /// How an element moves on the table (spec 0046): the light source — the ray root —
    /// moves freely in all three table directions; every other element is snapped to its
    /// beam and moves only ALONG it.
    type Mobility =
        | MovesFreely
        | MovesAlongBeam

    /// Whether a rotation axis may be turned (A.4.5). The project file keeps it a
    /// boolean, `true` for locked (`CanonicalJson`).
    type RotationLock =
        | RotationLocked
        | RotationFree

        /// The other state: a locked axis frees, a free one locks.
        member lock.toggled : RotationLock =
            match lock with
            | RotationLocked -> RotationFree
            | RotationFree -> RotationLocked

    /// The Library entry an element is bound to (R-6 / Part F): the entry id written
    /// onto the bound table element.
    type ElementValueId =
        | ElementValueId of string

        member this.value = let (ElementValueId s) = this in s

    type CatalogueKind with

        member kind.mobility : Mobility =
            match kind with
            | LightSource -> MovesFreely
            | LinearPolarizer | CircularPolarizer | Sample | Lens | FlatMirror | CurvedMirror | Detector -> MovesAlongBeam

        /// The default emission for a catalogue role (A.7.2). A mirror suppresses
        /// transmission (consistent with `BeamNode.attach`, `BeamTree.fs:74` — a
        /// mirror carries the reflected branch only); spec 0046 (operator): a lens and
        /// every polarizer carry the transmitted branch only (a splitting polarizer is a
        /// future kind); every other element emits both.
        member kind.defaultEmission : Emission =
            match kind with
            | FlatMirror | CurvedMirror -> EmitReflectedOnly
            | Lens | LinearPolarizer | CircularPolarizer -> EmitTransmittedOnly
            | LightSource | Sample | Detector -> EmitBoth

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

    /// The immutable per-element placement record (A.1 / R-1). Holds the box
    /// centre, the three rotation angles (engine `Angle`), the three independent
    /// lock flags, the catalogue-kind tag, the value-id slot, the bounding box,
    /// the emission metadata, and the preferred display unit (display metadata
    /// only, mirroring `BeamNode.defaultUnit`). Spec 0046: plus the centre's height
    /// above the table plane and the per-axis move locks — only an element that
    /// `MovesFreely` (the light source) is placed by them; every other element's
    /// position comes from the beam it is snapped to.
    type ElementPlacement =
        {
            placementPoint : TablePoint
            /// The box centre's height above the table plane (table-frame z).
            height : float<meter>
            moveLocks : MoveLocks
            r1 : Angle
            r2 : Angle
            r3 : Angle
            r1Locked : RotationLock
            r2Locked : RotationLock
            r3Locked : RotationLock
            catalogueKind : CatalogueKind
            valueId : ElementValueId option
            box : BoundingBox
            emission : Emission
            displayUnit : Units.UnitOfMeasure
        }

        /// A freshly-placed element (A.1.2 / R-6 / R-7.2): R3 is locked by default
        /// and R1/R2 are unlocked (R3 must be deliberately unlocked before it can
        /// change); `valueId` is unbound (`None`); the box and emission default by
        /// role; the display unit defaults to millimeters (a bench-scale length).
        /// Spec 0046: the LIGHT SOURCE starts with every move AND every rotation
        /// locked — it is the one element placed freely, so nothing shifts the whole
        /// beam by accident; it sits in the table plane (height 0).
        static member create (kind : CatalogueKind) (point : TablePoint) : ElementPlacement =
            let moveLock, rotationLock =
                match kind.mobility with
                | MovesFreely -> MoveLocked, RotationLocked
                | MovesAlongBeam -> MoveFree, RotationFree
            {
                placementPoint = point
                height = 0.0<meter>
                moveLocks = { xLock = moveLock; yLock = moveLock; zLock = moveLock }
                r1 = Angle.zero
                r2 = Angle.zero
                r3 = Angle.zero
                r1Locked = rotationLock
                r2Locked = rotationLock
                r3Locked = RotationLocked
                catalogueKind = kind
                valueId = None
                box = defaultBox kind
                emission = kind.defaultEmission
                displayUnit = Units.Millimeter
            }

        /// The outgoing groups DRAWN leaving the element (B.2.1 / B.2.2 / B.7.1). A
        /// detector TERMINATES the group it receives — no group is drawn leaving it
        /// (B.2.2). A mirror PRODUCES an RRG and ONLY an RRG, UNCONDITIONALLY: the draw
        /// layer enforces that kind-invariant STRUCTURALLY — the mirror arm returns
        /// `[ ReflectedRayGroup ]` independent of the per-element `emission` toggle entirely. So a
        /// mirror never draws a TRG even when toggled to `EmitBoth`
        /// (`Emission.withTransmitted RayGroupOn`), AND never draws the empty set even when
        /// toggled to `EmitTransmittedOnly` (`Emission.withReflected RayGroupOff`) — both states
        /// representable on the plain `ElementPlacement` record. This mirrors the engine,
        /// where `BeamNode.attach` takes a reflected attach on a mirror unconditionally
        /// and rejects a transmitted one with `Error MirrorBranchMustBeReflectedErr`
        /// (`isMirror`): the draw layer can thus neither invent
        /// the branch the engine refuses to route nor SUPPRESS the mandatory reflected
        /// branch B.2.1 requires (B.7.1). Every other element draws exactly the groups its
        /// A.7 emission metadata emits.
        member p.outgoingGroups : RayGroupKind list =
            match p.catalogueKind with
            | Detector -> []
            | FlatMirror | CurvedMirror -> [ ReflectedRayGroup ]
            // Spec 0046 (operator): a lens and every polarizer carry the transmitted branch only —
            // structurally, as a mirror carries the reflected one, whatever the per-element emission says.
            | Lens | LinearPolarizer | CircularPolarizer -> [ TransmittedRayGroup ]
            | LightSource
            | Sample -> p.emission.emittedGroups

        /// The SINGLE outgoing branch the downstream chain follows past the element on one beam (B.3) —
        /// DERIVED from what the element emits (`outgoingGroups`, i.e. its kind + A.7 emission), never
        /// hardcoded: a mirror tracks the REFLECTED beam (its only outgoing group); every transmissive element
        /// — and the terminal detector — tracks the TRANSMITTED beam. So a snap chain "knows" to reflect off a
        /// flat mirror and pass through a polarizer without the caller telling it which. (A beam splitter that
        /// emits BOTH would fork the tree; the linear chain follows the transmitted beam where it exists.)
        member p.primaryBranch : BeamTree.BeamBranch =
            let groups = p.outgoingGroups
            if List.contains TransmittedRayGroup groups then BeamTree.TransmittedBeam
            elif List.contains ReflectedRayGroup groups then BeamTree.ReflectedBeam
            else BeamTree.TransmittedBeam     // a detector emits nothing; its (unused) branch is transmitted

    /// Lock-respecting R1 setter (A.4.5): a locked axis ignores the change request.
    let withR1 (a : Angle) (p : ElementPlacement) : ElementPlacement =
        match p.r1Locked with
        | RotationLocked -> p
        | RotationFree -> { p with r1 = a }

    /// Lock-respecting R2 setter (A.4.5).
    let withR2 (a : Angle) (p : ElementPlacement) : ElementPlacement =
        match p.r2Locked with
        | RotationLocked -> p
        | RotationFree -> { p with r2 = a }

    /// Lock-respecting R3 setter (A.4.5). R3 starts locked (A.1.2), so this is
    /// inert until `setR3Locked RotationFree` has been called.
    let withR3 (a : Angle) (p : ElementPlacement) : ElementPlacement =
        match p.r3Locked with
        | RotationLocked -> p
        | RotationFree -> { p with r3 = a }

    /// Lock / unlock R1 (A.4.5), freezing it at its current value.
    let setR1Locked (lock : RotationLock) (p : ElementPlacement) : ElementPlacement = { p with r1Locked = lock }

    /// Lock / unlock R2 (A.4.5).
    let setR2Locked (lock : RotationLock) (p : ElementPlacement) : ElementPlacement = { p with r2Locked = lock }

    /// Lock / unlock R3 (A.4.5).
    let setR3Locked (lock : RotationLock) (p : ElementPlacement) : ElementPlacement = { p with r3Locked = lock }

    /// The move lock of one table axis (spec 0046).
    let moveLockOf (axis : TableAxis) (p : ElementPlacement) : MoveLock =
        match axis with
        | TableX -> p.moveLocks.xLock
        | TableY -> p.moveLocks.yLock
        | TableZ -> p.moveLocks.zLock

    /// Lock / unlock moving along one table axis (spec 0046).
    let withMoveLock (axis : TableAxis) (lock : MoveLock) (p : ElementPlacement) : ElementPlacement =
        match axis with
        | TableX -> { p with moveLocks = { p.moveLocks with xLock = lock } }
        | TableY -> { p with moveLocks = { p.moveLocks with yLock = lock } }
        | TableZ -> { p with moveLocks = { p.moveLocks with zLock = lock } }

    /// The placement's position along one table axis, in metres (spec 0046).
    let positionOn (axis : TableAxis) (p : ElementPlacement) : float<meter> =
        match axis with
        | TableX -> p.placementPoint.x
        | TableY -> p.placementPoint.y
        | TableZ -> p.height

    /// Lock-respecting position setter (spec 0046, the A.4.5 rule for moves): a locked
    /// axis ignores the change request.
    let withPositionOn (axis : TableAxis) (v : float<meter>) (p : ElementPlacement) : ElementPlacement =
        match moveLockOf axis p with
        | MoveLocked -> p
        | MoveFree ->
            match axis with
            | TableX -> { p with placementPoint = { p.placementPoint with x = v } }
            | TableY -> { p with placementPoint = { p.placementPoint with y = v } }
            | TableZ -> { p with height = v }

    /// The box centre as a table-frame 3-vector in metres: the placement point lifted to
    /// its height (spec 0046).
    let centreOf (p : ElementPlacement) : Vector3 =
        Vector3.create (p.placementPoint.x / 1.0<meter>) (p.placementPoint.y / 1.0<meter>) (p.height / 1.0<meter>)

    /// Rotate `v` about the unit-normalized `axis` by `angle` (Rodrigues' rotation
    /// formula). Reuses the engine `Angle` for the radian value (R-9). Used to
    /// build the oriented basis from the box's rest normals.
    let private rotateAbout (axis : Vector3) (angle : Angle) (v : Vector3) : Vector3 =
        let k = axis.normalized
        let c = cos angle.value
        let s = sin angle.value
        let kv = (k.dot v).value
        let kxv = k.cross v
        {
            x = v.x * c + kxv.x * s + k.x * kv * (1.0 - c)
            y = v.y * c + kxv.y * s + k.y * kv * (1.0 - c)
            z = v.z * c + kxv.z * s + k.z * kv * (1.0 - c)
        }

    /// The element's oriented basis in the table frame (A.3.3 / A.4): N1, N2 and N3.
    type OrientedBasis =
        {
            primaryNormal : Vector3
            secondaryNormal : Vector3
            thirdAxis : Vector3
        }

    /// The element's oriented `(N1, N2, N3)` basis in the table frame (A.3.3 /
    /// A.4). The rotations are applied to the box's rest normals in mount order:
    ///   * R3 about the third box axis (`N1 x N2`) tips the face OUT of the table
    ///     plane (A.4.4);
    ///   * R2 about the table normal steers the element within the plane (A.4.2);
    ///   * R1 about the (now oriented) face normal spins the element (A.4.1).
    /// R1 leaves N1 unchanged (it IS the spin axis). At rest pose (R1=R2=R3=0) the
    /// result is `(centralRayDirection, tableNormal, N1 x N2)` (AC-A1).
    let orientedBasis (p : ElementPlacement) : OrientedBasis =
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
        { primaryNormal = n1b; secondaryNormal = n2c; thirdAxis = n3c }

    /// The R1 axis (the oriented primary normal N1). At rest it coincides with the
    /// central ray; once R2 != 0 it decouples from the central ray (A.4.1); once
    /// R3 != 0 it tips out of the table plane so it is no longer perpendicular to
    /// the R2 axis (A.4.4 / AC-A4).
    let r1Axis (p : ElementPlacement) : Vector3 = (orientedBasis p).primaryNormal

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
            description = Some (OpticalSystemDescription "Placeholder vacuum sample (Spec 0026 A.5.2; material bound via valueId in Part F)")
            upper = OpticalProperties.vacuum
            films = []
            substrate = None
            lower = OpticalProperties.vacuum
        }

    /// Map a catalogue kind to its existing engine domain case (A.5.2 / AC-A7):
    /// Light Source -> `SourceElement`; BOTH Linear and Circular Polarizer -> `PolarizerElement`
    /// (a polarizer is a polarizer regardless of placement; linear vs circular is
    /// a catalogue-kind distinction, not a new domain case); every other kind -> its
    /// `…Element` case. The engine `AnalyzerElement` case is never produced.
    let toConstructorElement (kind : CatalogueKind) : BeamTree.ConstructorElement =
        match kind with
        | LightSource -> BeamTree.SourceElement
        | LinearPolarizer -> BeamTree.PolarizerElement
        | CircularPolarizer -> BeamTree.PolarizerElement
        | Sample -> BeamTree.SampleElement placeholderSampleSystem
        | Lens -> BeamTree.LensElement
        | FlatMirror -> BeamTree.FlatMirrorElement
        | CurvedMirror -> BeamTree.CurvedMirrorElement
        | Detector -> BeamTree.DetectorElement
