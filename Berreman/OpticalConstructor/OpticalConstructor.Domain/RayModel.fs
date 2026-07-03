namespace OpticalConstructor.Domain

open System
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain.Placement

/// Ray model, snapping & schematic tracing (Spec 0026 Part B). This is a NET-NEW,
/// PURE geometry layer that draws the light path: one central ray (CR) plus eight
/// side rays, the incident / reflected / transmitted ray groups, snapping of
/// elements onto the central ray, distance-preserving downstream travel when an
/// upstream element is tilted, default light-source / detector placement, the
/// orphaning state machine, and schematic Snell tracing through tilted elements.
///
/// It sits ABOVE `Placement.fs` (slice 001) and `BeamTree.fs`, reusing both rather
/// than re-typing geometry (constraint 0.1 / 0.7): `Placement.Vector3` carries ray
/// directions and ray-bundle positions, `Placement.r1Axis` supplies the element
/// face normal so the R2/R3 rotation law is not duplicated, and the engine
/// `BeamTree.BeamBranch` (`Reflected` / `Transmitted`, BeamTree.fs:21) is the branch
/// the drawn RRG / TRG correspond to (B.7.1). The module carries NO Avalonia type
/// (constraint 0.3): every rule below is provable headless. The quantitative result
/// at each element still comes from the existing beam-tree solver (BeamTree.fs:91);
/// this is the visual layer only (B.0).
///
/// Positions are held as `Placement.Vector3` whose components are canonical SI
/// meters (constraint 0.2); along-ray distances are `float<meter>`. Unit conversion
/// happens only at the `Units.fs` boundary — no conversion is hand-rolled here.
module RayModel =

    // --- Named constants -----------------------------------------------------

    /// The on-screen radius of the eight-ray bundle is a fixed fraction of the
    /// element's clear face: `bundle_radius = 0.4 × (face_extent / 2)` (B.1.2). The
    /// 0.4 is a schematic-legibility choice (a clear aperture is ≈2–3× the beam, so
    /// 0.4 keeps all eight side rays clearly inside the face).
    let bundleRadiusFraction : float = 0.4

    /// A ray group is one central ray plus EIGHT side rays (B.1.1). At normal
    /// incidence the eight land as equidistant points 45° apart on a circle (the
    /// "rifle-target" pattern), with the CR striking the centre.
    let sideRayCount : int = 8

    /// Schematic refractive index OUTSIDE an element (B.6.1 / constraint 0.5).
    let refractiveIndexOutside : float = 1.0

    /// Schematic refractive index INSIDE an element (B.6.1 / constraint 0.5). The
    /// schematic bends rays with these two indices only — it is NOT physical-grade.
    let refractiveIndexInside : float = 1.5

    /// The default along-CR distance between the light source and the detector in a
    /// fresh project (B.4.1): exactly 2.0 m, measured along the central ray.
    let defaultSourceDetectorDistance : float<meter> = 2.0<meter>

    /// The re-snap distance (B.5.3): a restored ray re-captures an orphaned element
    /// when it passes within this perpendicular distance of the element's placement
    /// point. A named constant; `restoreRayWithin` takes it as a parameter so the
    /// distance is also configurable (e.g. from Part E settings).
    let resnapDistance : float<meter> = 0.02<meter>

    // --- Vector helpers (canonical-meter positions) --------------------------

    let private add (a : Vector3) (b : Vector3) : Vector3 =
        Vector3.create (a.x + b.x) (a.y + b.y) (a.z + b.z)

    let private sub (a : Vector3) (b : Vector3) : Vector3 =
        Vector3.create (a.x - b.x) (a.y - b.y) (a.z - b.z)

    let private scale (s : float) (a : Vector3) : Vector3 =
        Vector3.create (s * a.x) (s * a.y) (s * a.z)

    /// Lift a 2-D table placement point (canonical meters) into the table plane
    /// (z = 0). The placement point is the box centre (A.2 / Placement.fs).
    let pointToVector3 (p : TablePoint) : Vector3 =
        Vector3.create (p.x / 1.0<meter>) (p.y / 1.0<meter>) 0.0

    // --- The central ray and its 3-D geometry --------------------------------

    /// A drawn ray: an origin point (canonical meters) and a unit direction. The
    /// central ray originates at the light source and terminates at the detector
    /// (B.4.1); the side rays share its direction at normal incidence (B.1.1).
    type Ray =
        {
            origin : Vector3
            direction : Vector3
        }

        /// The point reached by travelling `distance` along the ray (canonical
        /// meters). Distances along rays are SI meters (constraint 0.2).
        member r.pointAt (distance : float<meter>) : Vector3 =
            add r.origin (scale (distance / 1.0<meter>) r.direction.normalized)

    /// The in-plane angle of a direction within the table plane (about the table
    /// normal). Used to re-base the rest-frame face normal onto the actual incident
    /// ray (A.4.3 — R2 is measured FROM the incident ray, not a fixed table axis).
    let private inPlaneAngle (v : Vector3) : Angle = Angle.radian (atan2 v.y v.x)

    /// Rotate `v` about the table normal (+Z) by `a` — an in-plane steer (A.4.2).
    let private rotateAboutTableNormal (a : Angle) (v : Vector3) : Vector3 =
        let c = cos a.value
        let s = sin a.value
        Vector3.create (v.x * c - v.y * s) (v.x * s + v.y * c) v.z

    /// The two orthonormal vectors spanning the cross-section perpendicular to
    /// `direction` (B.1.1). `inPlane` is perpendicular to the direction within the
    /// table plane; `outOfPlane` completes the right-handed frame (the table normal
    /// for an in-plane ray). The eight side rays of a bundle lie on the unit circle
    /// these two span — that is the "rifle-target" cross-section.
    let crossSectionBasis (direction : Vector3) : Vector3 * Vector3 =
        let d = direction.normalized
        let inPlane =
            let candidate = Vector3.create (-d.y) d.x 0.0
            if candidate.norm < 1.0e-12 then Vector3.create 0.0 1.0 0.0
            else candidate.normalized
        let outOfPlane = (d.cross inPlane).normalized
        (inPlane, outOfPlane)

    // --- The eight-ray bundle (B.1) ------------------------------------------

    /// The on-screen radius of the eight-ray bundle for an element whose clear face
    /// is `faceExtent` (B.1.2): `0.4 × (face_extent / 2)`.
    let bundleRadius (faceExtent : float<meter>) : float<meter> =
        bundleRadiusFraction * (faceExtent / 2.0)

    /// A drawn ray bundle: the central-ray position plus the eight side-ray
    /// positions (B.1.1). At normal incidence the eight sides are equidistant points
    /// 45° apart on a circle of radius `bundleRadius`, with `center` at the middle.
    type RayBundle =
        {
            center : Vector3
            sideRays : Vector3 list
        }

    /// Build the nine-ray bundle whose central ray is at `center` travelling along
    /// `direction`, sized to an element face of `faceExtent` (B.1, AC-B1). The eight
    /// side rays land 45° apart on the circle of radius `bundleRadius faceExtent` in
    /// the cross-section perpendicular to `direction`. The side rays share the
    /// central-ray direction here — they "visibly spread" only once a focusing
    /// element (lens, curved mirror, prism, wedge) bends them, which is a drawing /
    /// later-slice concern (B.1.1).
    let bundleAt (center : Vector3) (direction : Vector3) (faceExtent : float<meter>) : RayBundle =
        let r = (bundleRadius faceExtent) / 1.0<meter>
        let (u, v) = crossSectionBasis direction
        let sides =
            [ for k in 0 .. sideRayCount - 1 ->
                let theta = float k * (2.0 * Math.PI / float sideRayCount)
                add center (add (scale (r * cos theta) u) (scale (r * sin theta) v)) ]
        { center = center; sideRays = sides }

    // --- Incident / reflected / transmitted groups (B.2) ---------------------

    /// A drawn ray group (B.2.1). The INCIDENT group (IRG) falls on an element and
    /// is the frame against which the element's rotations are measured (A.4.3). The
    /// REFLECTED (RRG) and TRANSMITTED (TRG) groups leave the element and correspond
    /// to the engine `Reflected` / `Transmitted` branches (BeamTree.fs:21, B.7.1).
    type RayGroupKind =
        | Incident
        | Reflected
        | Transmitted

    /// The OUTGOING groups an element with this `emission` produces (B.2.1). Every
    /// element except a mirror produces both an RRG and a TRG; a mirror produces an
    /// RRG only. This follows the A.7 emission metadata exactly (R-7).
    let emittedGroups (emission : Emission) : RayGroupKind list =
        match emission with
        | EmitReflectedOnly -> [ Reflected ]
        | EmitTransmittedOnly -> [ Transmitted ]
        | EmitBoth -> [ Reflected; Transmitted ]

    /// The outgoing groups DRAWN leaving an element (B.2.1 / B.2.2 / B.7.1). A
    /// detector TERMINATES the group it receives — no group is drawn leaving it
    /// (B.2.2). A mirror PRODUCES an RRG and ONLY an RRG, UNCONDITIONALLY: the draw
    /// layer enforces that kind-invariant STRUCTURALLY — the mirror arm returns
    /// `[ Reflected ]` independent of the per-element `emission` toggle entirely. So a
    /// mirror never draws a TRG even when toggled to `EmitBoth`
    /// (`Emission.withTransmitted true`), AND never draws the empty set even when
    /// toggled to `EmitTransmittedOnly` (`Emission.withReflected false`) — both states
    /// representable on the plain `ElementPlacement` record. This mirrors the engine,
    /// where `BeamNode.attach` takes a `Reflected` attach on a mirror unconditionally
    /// and rejects a `Transmitted` one with `Error MirrorBranchMustBeReflected`
    /// (BeamTree.fs:74-77, `isMirror` :64-67): the draw layer can thus neither invent
    /// the branch the engine refuses to route nor SUPPRESS the mandatory reflected
    /// branch B.2.1 requires (B.7.1). Every other element draws exactly the groups its
    /// A.7 emission metadata emits.
    let outgoingGroups (p : ElementPlacement) : RayGroupKind list =
        match p.catalogueKind with
        | Detector -> []
        | FlatMirror | CurvedMirror -> [ Reflected ]
        | LightSource | LinearPolarizer | CircularPolarizer
        | Sample | Lens -> emittedGroups p.emission

    /// The engine beam-tree branch a drawn outgoing group corresponds to (B.7.1):
    /// `Reflected` → `BeamTree.Reflected`, `Transmitted` → `BeamTree.Transmitted`.
    /// The incident group is not an engine branch (`None`). The drawing MUST NOT
    /// invent a branch the engine does not compute.
    let toBeamBranch (kind : RayGroupKind) : BeamTree.BeamBranch option =
        match kind with
        | Reflected -> Some BeamTree.Reflected
        | Transmitted -> Some BeamTree.Transmitted
        | Incident -> None

    /// The SINGLE outgoing branch the downstream chain follows past an element on one beam (B.3) —
    /// DERIVED from what the element emits (`outgoingGroups`, i.e. its kind + A.7 emission), never
    /// hardcoded: a mirror tracks the REFLECTED beam (its only outgoing group); every transmissive element
    /// — and the terminal detector — tracks the TRANSMITTED beam. So a snap chain "knows" to reflect off a
    /// flat mirror and pass through a polarizer without the caller telling it which. (A beam splitter that
    /// emits BOTH would fork the tree; the linear chain follows the transmitted beam where it exists.)
    let primaryBranch (p : ElementPlacement) : BeamTree.BeamBranch =
        let groups = outgoingGroups p
        if List.contains Transmitted groups then BeamTree.Transmitted
        elif List.contains Reflected groups then BeamTree.Reflected
        else BeamTree.Transmitted     // a detector emits nothing; its (unused) branch is transmitted

    /// The absolute `(R2, R3)` that orient a rest element's primary normal N1 (the +X central-ray
    /// direction) along `dir` — so an element with these angles FACES the beam travelling in `dir`. With
    /// the rest basis (N1 = +X, table normal = +Z) the oriented normal is
    /// `(cosR3·cosR2, cosR3·sinR2, sinR3)`, hence `R2 = atan2(dir.y, dir.x)`, `R3 = asin(dir.z)`. Used to
    /// auto-orient an element to the (possibly reflected) beam it sits on: add the element's own dialled
    /// R2/R3 on top, so at dialled 0 it faces the beam and a dialled value tilts it relative to the beam.
    let beamOrientation (dir : Vector3) : Angle * Angle =
        let d = dir.normalized
        Angle.radian (atan2 d.y d.x), Angle.radian (asin (max -1.0 (min 1.0 d.z)))

    // --- Schematic Snell tracing (B.6) ---------------------------------------

    /// The element's face normal (N1) in the world frame, given the actual incident
    /// ray direction (A.4.3 / B.6.1). Reuses `Placement.r1Axis` (which folds in R2
    /// and R3 about the fixed +X rest central ray) and re-bases it onto the incident
    /// ray by rotating about the table normal — so an element whose incident ray
    /// makes angle φ and whose own tilt is R2 has a face normal at φ + R2, exactly
    /// the additive `tableFrameOrientation` law (A.4.3). R3 tips it out of plane.
    let faceNormal (incoming : Vector3) (p : ElementPlacement) : Vector3 =
        rotateAboutTableNormal (inPlaneAngle incoming) (r1Axis p)

    /// Reflect a unit direction off a surface with unit `surfaceNormal` (B.3.2 — a
    /// mirror's reflected ray R): `d_out = d − 2 (d·n) n`. Tilting the surface by R2
    /// rotates the reflected ray by 2·R2, steering the beam within the table plane.
    let reflect (incoming : Vector3) (surfaceNormal : Vector3) : Vector3 =
        let d = incoming.normalized
        let n = surfaceNormal.normalized
        let dn = d.dot n
        (sub d (scale (2.0 * dn) n)).normalized

    /// Refract a unit direction crossing a surface, going from index `n1` to `n2`
    /// (Snell's law, vector form — B.6.1). Returns `None` on total internal
    /// reflection (never reached for the schematic's n1 = 1.0 < n2 = 1.5). The
    /// surface normal is oriented against the incoming ray internally, so the caller
    /// may pass either face orientation.
    let refract (n1 : float) (n2 : float) (incoming : Vector3) (surfaceNormal : Vector3) : Vector3 option =
        let d = incoming.normalized
        let nn = surfaceNormal.normalized
        let cosiRaw = -(d.dot nn)
        let (n, cosi) = if cosiRaw < 0.0 then (scale (-1.0) nn, -cosiRaw) else (nn, cosiRaw)
        let eta = n1 / n2
        let k = 1.0 - eta * eta * (1.0 - cosi * cosi)
        if k < 0.0 then None
        else Some ((add (scale eta d) (scale (eta * cosi - sqrt k) n)).normalized)

    /// The OUTGOING tracked-ray direction past an element on `branch`, given the
    /// `incoming` ray direction (B.3.2 / B.6.1). The reflected branch reflects off
    /// the element's face; the transmitted branch refracts through it with the
    /// schematic n = 1.0 / 1.5. At normal incidence both return the incoming
    /// direction (a square element does not steer a normal beam); a tilt (R2 ≠ 0 /
    /// R3 ≠ 0) genuinely changes the reflected direction and may change the
    /// transmitted one.
    let outgoingDirection (branch : BeamTree.BeamBranch) (incoming : Vector3) (p : ElementPlacement) : Vector3 =
        let n = faceNormal incoming p
        match branch with
        | BeamTree.Reflected -> reflect incoming n
        | BeamTree.Transmitted ->
            match refract refractiveIndexOutside refractiveIndexInside incoming n with
            | Some d -> d
            | None -> incoming.normalized

    // --- Default light-source / detector placement (B.4) ---------------------

    /// The default light-source placement point (B.4.1): near the left side of the
    /// table, vertically centred (y = 0). The table-frame origin is taken at the
    /// table centre with +X to the right (finalised by Part C / slice 004); the
    /// source therefore sits one metre left so the default CR spans the table.
    let defaultSourcePoint : TablePoint = { x = -1.0<meter>; y = 0.0<meter> }

    /// The default detector placement point (B.4.1): near the right side, vertically
    /// centred, exactly `defaultSourceDetectorDistance` (2.0 m) along the +X central
    /// ray from the light source.
    let defaultDetectorPoint : TablePoint =
        { x = defaultSourcePoint.x + defaultSourceDetectorDistance; y = 0.0<meter> }

    // --- Snapping and distance-preserving downstream travel (B.3) ------------

    /// One element to snap onto the ray chain (B.3.1): its placement (rotation,
    /// kind, …), the along-ray `gap` from the previous node, and which outgoing
    /// `branch` the chain follows past it (the tracked ray).
    type RaySegmentSpec =
        {
            placement : ElementPlacement
            gap : float<meter>
            branch : BeamTree.BeamBranch
        }

    /// An element snapped onto a ray (B.3.1): its placement, its absolute snapped
    /// position (canonical meters), and the unit incoming / outgoing ray directions
    /// at it. Drawing layers (slice 004) consume these.
    type SnappedElement =
        {
            placement : ElementPlacement
            position : Vector3
            incoming : Vector3
            outgoing : Vector3
        }

    /// Snap a chain of elements onto the ray starting at `source` heading
    /// `initialDirection` (B.3.1 / B.3.2 / AC-B3). Walks the ray polyline: each
    /// element sits `gap` meters along the CURRENT direction from the previous node,
    /// and its tracked branch sets the direction onward. Because the walk advances
    /// by the fixed gaps, every along-ray distance is preserved exactly; tilting an
    /// upstream element re-aims its outgoing segment and so drags everything
    /// downstream along the new beam direction with unchanged inter-element spacing.
    let snapChain (source : Vector3) (initialDirection : Vector3) (stops : RaySegmentSpec list) : SnappedElement list =
        let rec walk (pos : Vector3) (dir : Vector3) (remaining : RaySegmentSpec list) (acc : SnappedElement list) =
            match remaining with
            | [] -> List.rev acc
            | stop :: rest ->
                let d = dir.normalized
                let elementPos = add pos (scale (stop.gap / 1.0<meter>) d)
                let outDir = outgoingDirection stop.branch d stop.placement
                let snapped =
                    { placement = stop.placement; position = elementPos; incoming = d; outgoing = outDir }
                walk elementPos outDir rest (snapped :: acc)
        walk source initialDirection stops []

    /// The drawn central-ray chain (B.3 / B.4): the light-source `source` point, the
    /// initial CR direction, and the ordered elements snapped onto it. Moving or
    /// resetting the source drags the whole chain because every element is snapped
    /// to the ray by along-ray gap, not by absolute position (B.4.2).
    type RayChain =
        {
            source : TablePoint
            initialDirection : Vector3
            stops : RaySegmentSpec list
        }

        /// A fresh chain rooted at the default light-source placement, heading along
        /// the +X central ray (B.4.1).
        static member create (stops : RaySegmentSpec list) : RayChain =
            { source = defaultSourcePoint; initialDirection = centralRayDirection; stops = stops }

    /// Snap a `RayChain` into absolute geometry (B.3.1).
    let snap (chain : RayChain) : SnappedElement list =
        snapChain (pointToVector3 chain.source) chain.initialDirection chain.stops

    /// Move the light source to `p` (B.4.2). The downstream elements re-snap to the
    /// ray on the next `snap`, preserving their along-ray distances.
    let moveSource (p : TablePoint) (chain : RayChain) : RayChain = { chain with source = p }

    /// Reset the light source to its default placement (B.4.2 — the position reset
    /// reachable from the source's local element menu, Part E). The remaining
    /// elements re-snap, preserving along-ray distances.
    let resetSource (chain : RayChain) : RayChain = { chain with source = defaultSourcePoint }

    // --- Orphaned elements (B.5) ---------------------------------------------

    /// An identifier for a tracked ray an element can sit on (B.5). The central ray
    /// is always present; the reflected / transmitted branch rays carry an id so a
    /// specific branch can be removed or restored.
    type RayId =
        | CentralRay
        | ReflectedBranch of int
        | TransmittedBranch of int

    /// An element together with the ray it is attached to (B.5). `ray = None` means
    /// the element is ORPHANED — left without a ray.
    type RayAttachment =
        {
            placement : ElementPlacement
            ray : RayId option
        }

    /// Perpendicular distance from `point` to the line of `ray` (canonical meters) —
    /// used to decide whether a restored ray passes close enough to re-capture an
    /// orphan (B.5.3).
    let distancePointToRay (point : Vector3) (ray : Ray) : float<meter> =
        let d = ray.direction.normalized
        let w = sub point ray.origin
        let proj = scale (w.dot d) d
        (sub w proj).norm * 1.0<meter>

    /// Remove a ray: every element sitting on `rayId` becomes orphaned (B.5.1). This
    /// is the SOLE orphaning operation — it covers both spec triggers: switching the
    /// tracked branch (T → R) untracks the old branch ray, and removing one ray while
    /// both R and T are in use drops that ray. Elements on other rays are untouched.
    let removeRay (rayId : RayId) (elements : RayAttachment list) : RayAttachment list =
        elements |> List.map (fun e -> if e.ray = Some rayId then { e with ray = None } else e)

    /// Restore a previously-removed ray, re-snapping orphans within `maxDistance`
    /// (B.5.3). An ALREADY-attached element is untouched; an orphan re-snaps to
    /// `rayId` iff the restored `ray` passes within `maxDistance` of its placement
    /// point, otherwise it stays orphaned until the user reattaches it.
    let restoreRayWithin
        (maxDistance : float<meter>)
        (rayId : RayId)
        (ray : Ray)
        (elements : RayAttachment list)
        : RayAttachment list =
        elements
        |> List.map (fun e ->
            match e.ray with
            | Some _ -> e
            | None ->
                if distancePointToRay (pointToVector3 e.placement.placementPoint) ray <= maxDistance
                then { e with ray = Some rayId }
                else e)

    /// Restore a ray at the configured `resnapDistance` (B.5.3).
    let restoreRay (rayId : RayId) (ray : Ray) (elements : RayAttachment list) : RayAttachment list =
        restoreRayWithin resnapDistance rayId ray elements

    /// Apply an ordinary edit (rotation / drag) to an attached element (B.5.2). It
    /// updates the placement but PRESERVES the ray attachment — no ordinary
    /// operation orphans an element; only `removeRay` does.
    let withPlacement (p : ElementPlacement) (e : RayAttachment) : RayAttachment = { e with placement = p }
