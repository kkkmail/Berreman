namespace OpticalConstructor.Ui.Tests

open Avalonia.Controls
open Avalonia.Threading
open Xunit
open Berreman.Constants
open Berreman.Geometry
open OpticalConstructor.Controls
open OpticalConstructor.Domain
open OpticalConstructor.TestWindows
open OpticalConstructor.TestWindows.SnapToBeamView

/// Tests for the snap-to-beam test window (Spec 0027, task 010): rotating the light source re-aims the
/// beam and the downstream elements snap onto it (`RayModel.snapChain`), preserving along-ray spacing.
module SnapToBeamTests =

    let private close (a : float) (b : float) : bool = abs (a - b) <= 1.0e-9
    let private sourceX (m : SnapToBeamModel) : float = (m.source.placementPoint.toVector3).x

    [<Fact>]
    let ``at rest the elements snap onto the +X central ray at their cumulative gaps`` () =
        let m = initSnapToBeam ()
        let snaps = m.snapped
        let cum = m.cumulativeGaps
        Assert.Equal(List.length cum, List.length snaps)
        List.zip snaps cum
        |> List.iter (fun (s, d) ->
            Assert.True(close s.position.y 0.0, "on the central ray (y = 0)")
            Assert.True(close s.position.x (sourceX m + d / 1.0<meter>), $"at the cumulative gap along +X (got x = {s.position.x})"))

    [<Fact>]
    let ``rotating the source re-aims the beam and the elements snap to it (same distance, new direction)`` () =
        let m = updateSnapToBeam (RotateSourceBy (BarDegrees 30.0)) (initSnapToBeam ())
        Assert.True(close m.sourceR2.value 30.0)
        let srcV = m.source.placementPoint.toVector3
        let snaps = m.snapped
        let cum = m.cumulativeGaps
        List.zip snaps cum
        |> List.iter (fun (s, d) ->
            let dx = s.position.x - srcV.x
            let dy = s.position.y - srcV.y
            let dz = s.position.z - srcV.z
            Assert.True(close (sqrt (dx * dx + dy * dy + dz * dz)) (d / 1.0<meter>), "same along-ray distance from the source")
            Assert.True(abs s.position.y > 1.0e-6, "swung off the central ray (y <> 0)"))

    [<Fact>]
    let ``Reset returns the source to R2 = 0 and the elements back onto the central ray`` () =
        let turned = updateSnapToBeam (RotateSourceBy (BarDegrees 40.0)) (initSnapToBeam ())
        let reset = updateSnapToBeam ResetSource turned
        Assert.True(close reset.sourceR2.value 0.0)
        Assert.True(reset.snapped |> List.forall (fun s -> close s.position.y 0.0), "all back on the central ray")

    [<Fact>]
    [<Trait("Category", "ui-smoke")>]
    let ``the snap-to-beam window host opens and renders headlessly`` () =
        HeadlessSession.run (fun () ->
            let window = SnapToBeamWindow()
            window.Show()
            Dispatcher.UIThread.RunJobs()
            Assert.True(window.IsVisible)
            window.Close())
