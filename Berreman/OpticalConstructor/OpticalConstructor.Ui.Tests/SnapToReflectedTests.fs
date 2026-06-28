namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Threading
open Xunit
open Berreman.Geometry
open OpticalConstructor.Domain
open OpticalConstructor.Domain.Placement
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.SnapToReflectedView

/// Tests for the snap-to-reflected-light test window (Spec 0027, task 012): a source, a flat mirror at
/// R2 = 45°, and a detector. The light is REFLECTED off the mirror (it does NOT pass through), and the
/// detector snaps onto the reflected beam. The spec law: rotating the mirror over R2 or R3 re-aims the
/// reflected ray (the detector follows); rotating it over R1 — the spin about the face normal — leaves
/// the beam (and so the detector) unchanged. This is all pure `RayModel.snapChain` geometry.
module SnapToReflectedTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9

    let private mirrorSnap (m : Model) : RayModel.SnappedElement = snapped m |> List.head
    let private detectorSnap (m : Model) : RayModel.SnappedElement = snapped m |> List.item 1
    let private mirrorR1Degrees (m : Model) : float = (List.item 1 m.elements).r1.degrees

    let private separation (a : RayModel.SnappedElement) (b : RayModel.SnappedElement) : float =
        let dx = a.position.x - b.position.x
        let dy = a.position.y - b.position.y
        let dz = a.position.z - b.position.z
        sqrt (dx * dx + dy * dy + dz * dz)

    [<Fact>]
    let ``the scene is source, flat mirror at R2 = 45, detector`` () =
        let m = init ()
        let kinds = m.elements |> List.map (fun e -> e.catalogueKind)
        Assert.True((kinds = [ LightSource; FlatMirror; Detector ]), "scene is source, flat mirror, detector")
        Assert.True(close (List.item 1 m.elements).r2.degrees 45.0, "mirror seeded at R2 = 45")

    [<Fact>]
    let ``at R2 = 45 the light is reflected (not transmitted) and the detector snaps onto the reflected ray`` () =
        let m = init ()
        let mir = mirrorSnap m
        // The beam arrives travelling +X and leaves travelling -Y: it is REFLECTED 90°, not passed through.
        Assert.True(close mir.incoming.x 1.0 && close mir.incoming.y 0.0, "incident travels +X")
        Assert.True(close mir.outgoing.x 0.0 && close mir.outgoing.y -1.0, $"reflected travels -Y (got {mir.outgoing.x}, {mir.outgoing.y})")
        let dot = mir.incoming.x * mir.outgoing.x + mir.incoming.y * mir.outgoing.y + mir.incoming.z * mir.outgoing.z
        Assert.True(abs dot < 1.0e-9, "the reflected beam is perpendicular to the incident beam at R2 = 45 (it did not go straight through)")
        // The detector sits ON the reflected ray: (detector − mirror) points along the mirror's outgoing dir.
        let det = detectorSnap m
        let dx = det.position.x - mir.position.x
        let dy = det.position.y - mir.position.y
        let dz = det.position.z - mir.position.z
        let len = sqrt (dx * dx + dy * dy + dz * dz)
        Assert.True(len > 1.0e-9, "the detector is downstream of the mirror")
        Assert.True(close (dx / len) mir.outgoing.x && close (dy / len) mir.outgoing.y && close (dz / len) mir.outgoing.z,
                    "the detector lies on the reflected ray")

    [<Fact>]
    let ``rotating the mirror over R2 re-aims the reflected beam and moves the snapped detector`` () =
        let baseM = init ()
        let turned = update (RotateR2By 20.0) baseM
        Assert.True(close (List.item 1 turned.elements).r2.degrees 65.0, "mirror R2 = 45 + 20")
        let moved = separation (detectorSnap baseM) (detectorSnap turned)
        Assert.True(moved > 1.0e-3, $"R2 moved the snapped detector (Δ = {moved})")

    [<Fact>]
    let ``rotating the mirror over R3 re-aims the reflected beam and moves the snapped detector`` () =
        let baseM = init ()
        let turned = update (RotateR3By 20.0) baseM
        Assert.True(close (List.item 1 turned.elements).r3.degrees 20.0, "mirror R3 = 20 (R3 is unlocked in this scene)")
        let moved = separation (detectorSnap baseM) (detectorSnap turned)
        Assert.True(moved > 1.0e-3, $"R3 moved the snapped detector (Δ = {moved})")

    [<Fact>]
    let ``rotating the mirror over R1 does NOT affect the beam — the detector stays put`` () =
        let baseM = init ()
        let spun = update (RotateR1By 35.0) baseM
        Assert.True(close (mirrorR1Degrees spun) 35.0, "the mirror's R1 really did change")
        // R1 is the spin about the face normal (`Placement.r1Axis` depends only on R2/R3), so the reflected
        // direction — and therefore the snapped detector — is unchanged.
        let a = (detectorSnap baseM).position
        let b = (detectorSnap spun).position
        Assert.True(close a.x b.x && close a.y b.y && close a.z b.z,
                    $"R1 left the reflected detector unchanged (got {b.x}, {b.y}, {b.z} vs {a.x}, {a.y}, {a.z})")

    // ----- Part C: elements on the reflected beam are oriented RELATIVE to that beam -----

    let private closeVec (a : Vector3) (b : Vector3) : bool =
        abs (a.x - b.x) <= 1.0e-6 && abs (a.y - b.y) <= 1.0e-6 && abs (a.z - b.z) <= 1.0e-6

    let private dot (a : Vector3) (b : Vector3) : float = a.x * b.x + a.y * b.y + a.z * b.z

    /// The detector's DRAWN primary normal N1 (it is element 2, the one on the reflected beam).
    let private detectorDrawN1 (m : Model) : Vector3 =
        let p = List.item 2 (drawPlacements m)
        let (n1, _, _) = orientedBasis p
        n1

    /// The reflected beam direction (the mirror's outgoing ray).
    let private reflectedDir (m : Model) : Vector3 = (snapped m |> List.head).outgoing

    [<Fact>]
    let ``the detector on the reflected beam faces it at rest (R2/R3 relative to the beam)`` () =
        let m = init ()
        Assert.True(closeVec (detectorDrawN1 m) (reflectedDir m), "the detector (dialled 0) faces the reflected beam")
        // At mirror R2 = 45 the reflected beam is −Y, so the detector faces −Y.
        Assert.True(close (detectorDrawN1 m).x 0.0 && close (detectorDrawN1 m).y -1.0, "the detector faces −Y")

    [<Fact>]
    let ``rotating the mirror R2 keeps the detector facing the new reflected beam`` () =
        let m = update (RotateR2By 25.0) (init ())     // the mirror is selected by default
        Assert.True(closeVec (detectorDrawN1 m) (reflectedDir m), "the detector re-aims to the new reflected beam")

    [<Fact>]
    let ``rotating the mirror R1 leaves the detector orientation unchanged`` () =
        let baseM = init ()
        let spun = update (RotateR1By 35.0) baseM
        Assert.True(closeVec (detectorDrawN1 baseM) (detectorDrawN1 spun),
                    "R1 of the mirror does not move the beam, so the detector orientation is unchanged")

    [<Fact>]
    let ``the detector's own dialled R2 tilts it relative to the beam, not the table`` () =
        let m = { (init ()) with selection = ElementSelected 2 } |> update (RotateR2By 30.0)   // dial the DETECTOR
        let n1 = detectorDrawN1 m
        let beam = reflectedDir m
        Assert.False(closeVec n1 beam, "the detector is now tilted off the beam")
        let ang = acos (max -1.0 (min 1.0 (dot n1 beam))) * 180.0 / System.Math.PI
        Assert.True(abs (ang - 30.0) < 1.0e-3, $"the detector is tilted 30° RELATIVE to the reflected beam (got {ang})")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the snap-to-reflected window host opens and renders headlessly`` () =
        HeadlessSession.run (fun () ->
            let window = SnapToReflectedWindow()
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
