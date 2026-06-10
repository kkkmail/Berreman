namespace OpticalConstructor.Tests

open System
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.Domain.RayModel
open Xunit

/// Part B headless ray-model tests (Spec 0026, slice 002): the eight-ray
/// "rifle-target" circle (AC-B1), mirror RRG-only / detector termination (AC-B2),
/// distance-preserving downstream travel when an upstream element tilts (AC-B3),
/// the default 2.0 m light-source/detector placement and position reset (AC-B4),
/// the orphaning state machine (AC-B5), and schematic Snell bending with
/// n = 1.0 / 1.5 (AC-B6). Everything here is provable headless — no Avalonia type
/// is touched (constraint 0.3).
module RayModelTests =

    /// Absolute float tolerance, matching the project's `abs (a - b) <= eps`
    /// comparison precedent (e.g. `PlacementTests.fs`, `UnitsTests.fs`).
    let private tol = 1.0e-9
    let private close (a : float) (b : float) : bool = abs (a - b) <= tol

    /// Euclidean distance between two canonical-meter positions.
    let private distV (a : Vector3) (b : Vector3) : float =
        let dx = a.x - b.x
        let dy = a.y - b.y
        let dz = a.z - b.z
        sqrt (dx * dx + dy * dy + dz * dz)

    // --- AC-B1: the eight-ray "rifle-target" circle ------------------------

    [<Fact>]
    let ``AC-B1 at normal incidence the eight side rays sit 45 deg apart on a circle of radius 0.4 x face/2 with the CR at the centre`` () =
        let center = Vector3.create 0.0 0.0 0.0
        let bundle = bundleAt center centralRayDirection opticalFaceExtent
        // Exactly eight side rays plus the central ray at the centre.
        Assert.Equal(8, List.length bundle.sideRays)
        Assert.True(close (distV bundle.center center) 0.0, "CR must strike the centre of the circle")
        // Radius = 0.4 x (face_extent / 2): for the 0.05 m optical face that is 0.01 m.
        let expectedR = (bundleRadius opticalFaceExtent) / 1.0<meter>
        Assert.True(close expectedR 0.01, $"bundle radius {expectedR}")
        let offsets =
            bundle.sideRays
            |> List.map (fun s -> Vector3.create (s.x - center.x) (s.y - center.y) (s.z - center.z))
            |> List.toArray
        // Every side ray is equidistant from the centre (it is a CIRCLE).
        for o in offsets do
            Assert.True(close o.norm expectedR, $"side-ray radius {o.norm} vs {expectedR}")
        // Consecutive side rays (with wrap-around) are exactly 45 deg apart.
        let n = offsets.Length
        for k in 0 .. n - 1 do
            let a = offsets.[k]
            let b = offsets.[(k + 1) % n]
            let cosAngle = (a.dot b) / (a.norm * b.norm)
            Assert.True(close cosAngle (cos (Math.PI / 4.0)), $"cos(angle) between adjacent side rays = {cosAngle}")

    // --- AC-B2: mirror RRG-only; both for non-mirror; detector terminates ---

    [<Fact>]
    let ``AC-B2 a mirror draws an RRG and no TRG, a non-mirror with both emissions draws both, and a detector terminates`` () =
        let mirror = ElementPlacement.create FlatMirror TablePoint.origin
        let mirrorGroups = outgoingGroups mirror
        Assert.Contains(Reflected, mirrorGroups)
        Assert.DoesNotContain(Transmitted, mirrorGroups)
        // The mirror kind-invariant is STRUCTURAL, not a function of the per-element
        // emission toggle: a mirror whose emission is forced to EmitBoth
        // (Emission.withTransmitted true on its default EmitReflectedOnly) STILL draws
        // an RRG and no TRG. This pins the B.7.1 rule the default-emission mirror above
        // cannot exercise — outgoingGroups must never invent the Transmitted branch the
        // engine's BeamNode.attach rejects (Error MirrorBranchMustBeReflected,
        // BeamTree.fs:74-77).
        let mirrorBoth = { mirror with emission = Emission.withTransmitted true mirror.emission }
        Assert.Equal(EmitBoth, mirrorBoth.emission)   // precondition: the phantom-TRG emission state IS representable
        let mirrorBothGroups = outgoingGroups mirrorBoth
        Assert.Contains(Reflected, mirrorBothGroups)
        Assert.DoesNotContain(Transmitted, mirrorBothGroups)
        // The MIRROR-IMAGE corner of the same structural rule: a mirror whose emission
        // is forced to EmitTransmittedOnly (Emission.withReflected false on its default
        // EmitReflectedOnly) must STILL draw exactly [ Reflected ] -- NON-EMPTY, no TRG.
        // The mirror arm is unconditional in p.emission, so it can neither invent the
        // forbidden Transmitted branch nor SUPPRESS the mandatory reflected branch
        // B.2.1 requires ("a mirror produces only an RRG"). The intersect/filter form
        // would have returned [] here -- a mirror drawing nothing at all.
        let mirrorTransmitted = { mirror with emission = Emission.withReflected false mirror.emission }
        Assert.Equal(EmitTransmittedOnly, mirrorTransmitted.emission)   // precondition: the RRG-suppressing state IS representable
        let mirrorTransmittedGroups = outgoingGroups mirrorTransmitted
        Assert.NotEmpty(mirrorTransmittedGroups)
        Assert.Equal(1, List.length mirrorTransmittedGroups)            // exactly one group ...
        Assert.Equal(Reflected, List.head mirrorTransmittedGroups)      // ... and it is the RRG
        Assert.DoesNotContain(Transmitted, mirrorTransmittedGroups)
        // A non-mirror element emitting both draws both groups.
        let lens = ElementPlacement.create Lens TablePoint.origin
        let lensGroups = outgoingGroups lens
        Assert.Contains(Reflected, lensGroups)
        Assert.Contains(Transmitted, lensGroups)
        // Every drawn outgoing group corresponds to an engine branch (B.7.1) ...
        for g in mirrorGroups @ mirrorBothGroups @ mirrorTransmittedGroups @ lensGroups do
            match toBeamBranch g with
            | Some _ -> ()
            | None -> failwith $"outgoing group {g} has no engine branch"
        // ... and a mirror never produces the engine Transmitted branch
        // (consistent with BeamNode.attach rejecting Transmitted on a mirror).
        for g in mirrorGroups @ mirrorBothGroups @ mirrorTransmittedGroups do
            Assert.True(toBeamBranch g <> Some BeamTree.Transmitted, "a mirror must not draw a transmitted group")
        // A detector terminates the group it receives: nothing leaves it (B.2.2).
        let detector = ElementPlacement.create Detector TablePoint.origin
        Assert.Empty(outgoingGroups detector)

    // --- AC-B3: distance-preserving downstream travel ----------------------

    [<Fact>]
    let ``AC-B3 changing an element R2 keeps every downstream along-ray distance unchanged while positions follow the new ray`` () =
        let lens = ElementPlacement.create Lens TablePoint.origin
        let detector = ElementPlacement.create Detector TablePoint.origin
        // Source at the origin heading +X; a tilted mirror reflects, two elements ride
        // its reflected branch. The two downstream elements sit square to their beam
        // (R2 = 0) so only the mirror's tilt steers the chain.
        let stopsFor (mirror : ElementPlacement) : RaySegmentSpec list =
            [ { placement = mirror;   gap = 0.5<meter>; branch = BeamTree.Reflected }
              { placement = lens;     gap = 0.3<meter>; branch = BeamTree.Transmitted }
              { placement = detector; gap = 0.4<meter>; branch = BeamTree.Transmitted } ]
        let source = Vector3.create 0.0 0.0 0.0
        let mirror0 = ElementPlacement.create FlatMirror TablePoint.origin |> withR2 (Angle.degree 30.0)
        let mirror1 = mirror0 |> withR2 (Angle.degree 45.0)
        let before = snapChain source centralRayDirection (stopsFor mirror0) |> List.toArray
        let after  = snapChain source centralRayDirection (stopsFor mirror1) |> List.toArray
        // Along-ray distances (the gaps) are preserved exactly in BOTH configurations.
        Assert.True(close (distV before.[1].position before.[0].position) 0.3)
        Assert.True(close (distV before.[2].position before.[1].position) 0.4)
        Assert.True(close (distV after.[1].position after.[0].position) 0.3)
        Assert.True(close (distV after.[2].position after.[1].position) 0.4)
        // The mirror itself does not move (it is at a fixed gap from the source).
        Assert.True(close (distV after.[0].position before.[0].position) 0.0)
        // But the downstream absolute positions FOLLOW the new reflected direction.
        Assert.True(distV after.[1].position before.[1].position > 1.0e-6, "lens did not travel with the new ray")
        Assert.True(distV after.[2].position before.[2].position > 1.0e-6, "detector did not travel with the new ray")

    // --- AC-B4: default 2.0 m placement and light-source reset --------------

    [<Fact>]
    let ``AC-B4 a fresh project places LS left and D right exactly 2.0 m apart along the CR and the LS reset re-snaps preserving distances`` () =
        // Both vertically centred; source to the left of the detector.
        Assert.True(close (defaultSourcePoint.y / 1.0<meter>) 0.0, "light source must be vertically centred")
        Assert.True(close (defaultDetectorPoint.y / 1.0<meter>) 0.0, "detector must be vertically centred")
        Assert.True(defaultSourcePoint.x < defaultDetectorPoint.x, "light source must be left of the detector")
        let separation = distV (pointToVector3 defaultSourcePoint) (pointToVector3 defaultDetectorPoint)
        Assert.True(close separation 2.0, $"LS-D separation along the CR = {separation} m")
        // A chain with two downstream elements: move the source away, then reset it.
        let e1 = ElementPlacement.create Lens TablePoint.origin
        let e2 = ElementPlacement.create Sample TablePoint.origin
        let stops =
            [ { placement = e1; gap = 0.4<meter>; branch = BeamTree.Transmitted }
              { placement = e2; gap = 0.7<meter>; branch = BeamTree.Transmitted } ]
        let chain = RayChain.create stops
        let moved = moveSource { x = 0.3<meter>; y = 0.5<meter> } chain
        let resetChain = resetSource moved
        let snapped0 = snap chain |> List.toArray
        let snappedMoved = snap moved |> List.toArray
        let snappedReset = snap resetChain |> List.toArray
        // The reset restores the default source placement exactly.
        Assert.True(close (resetChain.source.x / 1.0<meter>) (defaultSourcePoint.x / 1.0<meter>))
        Assert.True(close (resetChain.source.y / 1.0<meter>) (defaultSourcePoint.y / 1.0<meter>))
        // The source->e1 (0.4 m) and e1->e2 (0.7 m) along-ray distances are preserved
        // through the move AND the reset (snap re-walks the ray by along-ray gaps).
        Assert.True(close (distV (pointToVector3 chain.source) snapped0.[0].position) 0.4)
        Assert.True(close (distV (pointToVector3 moved.source) snappedMoved.[0].position) 0.4)
        Assert.True(close (distV (pointToVector3 resetChain.source) snappedReset.[0].position) 0.4)
        Assert.True(close (distV snapped0.[1].position snapped0.[0].position) 0.7)
        Assert.True(close (distV snappedMoved.[1].position snappedMoved.[0].position) 0.7)
        Assert.True(close (distV snappedReset.[1].position snappedReset.[0].position) 0.7)
        // After the reset the whole snapped chain is back to its default geometry.
        Assert.True(close (distV snappedReset.[0].position snapped0.[0].position) 0.0)
        Assert.True(close (distV snappedReset.[1].position snapped0.[1].position) 0.0)

    // --- AC-B5: orphaning rules --------------------------------------------

    [<Fact>]
    let ``AC-B5 removing a ray orphans exactly its elements, ordinary edits never orphan, and a restored ray re-snaps only orphans within the configured distance`` () =
        let pA = ElementPlacement.create Lens   { x = 0.0<meter>; y = 0.0<meter> }
        let pB = ElementPlacement.create Sample { x = 0.5<meter>; y = 0.0<meter> }   // on the y = 0 ray
        let pC = ElementPlacement.create Lens   { x = 0.5<meter>; y = 0.5<meter> }   // 0.5 m off the y = 0 ray
        let elements =
            [ { placement = pA; ray = Some (ReflectedBranch 1) }
              { placement = pB; ray = Some (TransmittedBranch 1) }
              { placement = pC; ray = Some (TransmittedBranch 1) } ]
        // Remove the transmitted ray: exactly the T elements orphan; the R element stays.
        let afterRemove = removeRay (TransmittedBranch 1) elements |> List.toArray
        Assert.Equal(Some (ReflectedBranch 1), afterRemove.[0].ray)
        Assert.True(Option.isNone afterRemove.[1].ray, "element on the removed ray must orphan")
        Assert.True(Option.isNone afterRemove.[2].ray, "element on the removed ray must orphan")
        // No OTHER operation orphans: an ordinary rotation/drag keeps the attachment.
        let rotated = withPlacement (withR2 (Angle.degree 10.0) pA) afterRemove.[0]
        Assert.Equal(Some (ReflectedBranch 1), rotated.ray)
        // Restore the transmitted ray along y = 0: it passes through pB (distance 0) but
        // is 0.5 m from pC (well beyond the 0.02 m resnap distance).
        let restoredRay = { origin = Vector3.create 0.0 0.0 0.0; direction = centralRayDirection }
        let afterRestore = restoreRay (TransmittedBranch 1) restoredRay (List.ofArray afterRemove) |> List.toArray
        Assert.Equal(Some (TransmittedBranch 1), afterRestore.[1].ray)   // re-snapped
        Assert.True(Option.isNone afterRestore.[2].ray, "far orphan must stay orphaned")
        // Restore only touches orphans — the reflected-branch element is unchanged.
        Assert.Equal(Some (ReflectedBranch 1), afterRestore.[0].ray)

    // --- AC-B6: schematic Snell bending ------------------------------------

    [<Fact>]
    let ``AC-B6 a ray crossing a tilted element bends per Snell's law with n = 1.0 outside and n = 1.5 inside`` () =
        let incoming = centralRayDirection
        let tilted = ElementPlacement.create Sample TablePoint.origin |> withR2 (Angle.degree 30.0)
        let n = (faceNormal incoming tilted).normalized
        // Angle of incidence is the element's 30 deg tilt; the schematic transmitted
        // ray refracts toward the normal entering the n = 1.5 medium.
        let theta1 = acos (incoming.normalized.dot n)
        let outDir = (outgoingDirection BeamTree.Transmitted incoming tilted).normalized
        let theta2 = acos (outDir.dot n)
        Assert.True(close (theta1 * 180.0 / Math.PI) 30.0, $"incidence {theta1 * 180.0 / Math.PI} deg")
        Assert.True(theta2 < theta1, "the refracted ray must bend toward the normal")
        // Snell's law: n_out sin(theta1) = n_in sin(theta2).
        let lhs = refractiveIndexOutside * sin theta1
        let rhs = refractiveIndexInside * sin theta2
        Assert.True(close lhs rhs, $"Snell mismatch: {lhs} vs {rhs}")
        // A square (normal-incidence) element does not bend the ray.
        let square = ElementPlacement.create Sample TablePoint.origin
        let outSquare = (outgoingDirection BeamTree.Transmitted incoming square).normalized
        Assert.True(close (outSquare.dot incoming.normalized) 1.0, "normal incidence must pass straight through")
