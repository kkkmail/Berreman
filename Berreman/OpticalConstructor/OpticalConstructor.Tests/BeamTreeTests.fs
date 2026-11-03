namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open OpticalConstructor.Domain.CurvedElements
open Xunit

/// Beam-tree topology & mirror/branch validation — AC-A4, AC-B2, AC-B3.
/// Mirror nodes admit the reflected branch only (validated via `Result`, not
/// exceptions); non-mirror nodes hold both branches; a linear chain is the
/// degenerate single-child-per-node case.
module BeamTreeTests =

    let private vacuumSystem : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = []
            substrate = None
            lower = OpticalProperties.vacuum
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)

    let private node (element : ConstructorElement) : BeamNode =
        {
            element = element
            system = vacuumSystem
            incident = light
            children = Map.empty
            defaultUnit = Nanometer
        }

    let private okOrFail (r : Result<BeamNode, BeamTreeError>) : BeamNode =
        match r with
        | Ok n -> n
        | Error e -> failwith $"unexpected BeamTreeError: {e}"

    [<Fact>]
    let ``AC-B3 / AC-A4 mirror rejects Transmitted attach and accepts Reflected only`` () =
        let mirror = node FlatMirrorElement
        let child = node DetectorElement

        match BeamNode.attach BeamBranch.TransmittedBeam child mirror with
        | Error MirrorBranchMustBeReflectedErr -> ()
        | Ok _ -> Assert.Fail("a mirror MUST reject a Transmitted attachment")

        match BeamNode.attach BeamBranch.ReflectedBeam child mirror with
        | Ok parent ->
            Assert.True(parent.children.ContainsKey BeamBranch.ReflectedBeam)
            Assert.False(parent.children.ContainsKey BeamBranch.TransmittedBeam)
            Assert.Equal(1, parent.children.Count)
        | Error _ -> Assert.Fail("a mirror MUST accept a Reflected attachment")

    [<Fact>]
    let ``AC-B2 non-mirror node holds both Reflected and Transmitted children`` () =
        let sample = node (SampleElement vacuumSystem)
        let withR = sample |> BeamNode.attach BeamBranch.ReflectedBeam (node DetectorElement) |> okOrFail
        let withRT = withR |> BeamNode.attach BeamBranch.TransmittedBeam (node AnalyzerElement) |> okOrFail

        Assert.True(withRT.children.ContainsKey BeamBranch.ReflectedBeam)
        Assert.True(withRT.children.ContainsKey BeamBranch.TransmittedBeam)
        Assert.Equal(2, withRT.children.Count)

    [<Fact>]
    let ``AC-B2 linear chain is single-child-per-node on Transmitted`` () =
        // source -> polarizer -> sample -> analyzer -> detector
        let detector = node DetectorElement
        let analyzer = node AnalyzerElement |> BeamNode.attach BeamBranch.TransmittedBeam detector |> okOrFail
        let sample = node (SampleElement vacuumSystem) |> BeamNode.attach BeamBranch.TransmittedBeam analyzer |> okOrFail
        let polarizer = node PolarizerElement |> BeamNode.attach BeamBranch.TransmittedBeam sample |> okOrFail
        let source = node SourceElement |> BeamNode.attach BeamBranch.TransmittedBeam polarizer |> okOrFail

        // Every node in the chain carries exactly one child, on the Transmitted branch.
        let rec walk (n : BeamNode) (depth : int) =
            Assert.True(n.children.Count <= 1)
            match Map.tryFind BeamBranch.TransmittedBeam n.children with
            | Some next ->
                Assert.False(n.children.ContainsKey BeamBranch.ReflectedBeam)
                walk next (depth + 1)
            | None -> depth
        let depth = walk source 0
        Assert.Equal(4, depth)

    [<Fact>]
    let ``AC-C4 curved fan: CurvedMirror is Reflected-only, Lens has both branches`` () =
        // The mirror special case (§C.6): a CurvedMirror zone node exposes the
        // Reflected branch ONLY and drops the transmitted field.
        let mirrorZone = attachCurvedElement CurvedMirrorElement (node CurvedMirrorElement)
        Assert.True(mirrorZone.children.ContainsKey BeamBranch.ReflectedBeam)
        Assert.False(mirrorZone.children.ContainsKey BeamBranch.TransmittedBeam)
        Assert.Equal(1, mirrorZone.children.Count)

        // A Lens zone node exposes BOTH branches.
        let lensZone = attachCurvedElement LensElement (node LensElement)
        Assert.True(lensZone.children.ContainsKey BeamBranch.ReflectedBeam)
        Assert.True(lensZone.children.ContainsKey BeamBranch.TransmittedBeam)
        Assert.Equal(2, lensZone.children.Count)

    [<Fact>]
    let ``AC-B8 changing defaultUnit leaves the stored SI system untouched`` () =
        let n0 = node (SampleElement vacuumSystem)
        let n1 = { n0 with defaultUnit = Millimeter }
        // The display-only hook changed; the stored stack is byte-identical.
        Assert.Equal(Millimeter, n1.defaultUnit)
        Assert.True(System.Object.ReferenceEquals(n0.system, n1.system))
