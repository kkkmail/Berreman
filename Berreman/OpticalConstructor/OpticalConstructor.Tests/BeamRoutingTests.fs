namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.BerremanMatrix
open Berreman.Solvers
open OpticalConstructor.Domain.Units
open OpticalConstructor.Domain.BeamTree
open Xunit

/// Beam routing through the reused engine seams — AC-A3, AC-B4. The root solve
/// goes through `OpticalSystemSolver` and reads `Solution.emSys`; the child
/// incident field is the parent branch field advanced by `EmField.propagate`;
/// the child is solved by `BaseOpticalSystemSolver(emf, ShortOpticalSystem)`;
/// the two branches carry independent field state.
module BeamRoutingTests =

    let private glass = OpticalProperties.fromRefractionIndex (RefractionIndex 1.5)

    let private sampleSystem : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = []
            substrate = None
            lower = glass
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)

    let private rootNode : BeamNode =
        {
            element = Sample sampleSystem
            system = sampleSystem
            incident = light
            children = Map.empty
            defaultUnit = Nanometer
        }

    let private eNorm (f : EmField) : float = f.e.value.norm

    [<Fact>]
    let ``AC-A3 solve goes through OpticalSystemSolver and exposes both outgoing beams`` () =
        let ems = solve rootNode
        // Same engine path as a direct OpticalSystemSolver(info, system).solution.emSys.
        let direct = OpticalSystemSolver(light, sampleSystem).solution.emSys
        Assert.Equal(eNorm direct.reflected, eNorm ems.reflected, 12)
        Assert.Equal(eNorm direct.transmitted, eNorm ems.transmitted, 12)
        // Two outgoing beams exist (reflected & transmitted), each with field components.
        Assert.False(List.isEmpty ems.reflected.emComponents)
        Assert.False(List.isEmpty ems.transmitted.emComponents)

    [<Fact>]
    let ``AC-A3 detectors on different branches report independent fields`` () =
        let ems = solve rootNode
        let r = branchEmField BeamBranch.Reflected ems
        let t = branchEmField BeamBranch.Transmitted ems
        // EmFieldSystem isolates reflected vs transmitted -> the branch states differ.
        Assert.True(abs (eNorm r - eNorm t) > 1e-9, $"reflected {eNorm r} vs transmitted {eNorm t}")

    [<Fact>]
    let ``AC-B4 child incident equals parent branch field advanced by EmField.propagate`` () =
        let ems = solve rootNode
        let gap : Layer = { properties = OpticalProperties.vacuum; thickness = Thickness.nm 250.0<nm> }

        let viaModule = childIncidentField BeamBranch.Transmitted gap ems
        let manual = (branchEmField BeamBranch.Transmitted ems).propagate gap
        Assert.Equal(eNorm manual, eNorm viaModule, 12)

    [<Fact>]
    let ``AC-B4 child is solved via BaseOpticalSystemSolver on the propagated EmField`` () =
        let ems = solve rootNode
        let gap : Layer = { properties = OpticalProperties.vacuum; thickness = Thickness.nm 250.0<nm> }
        let childSys : ShortOpticalSystem = { films = []; lower = glass }

        let childEms = routeAndSolve BeamBranch.Transmitted gap ems childSys
        let directChild =
            BaseOpticalSystemSolver(childIncidentField BeamBranch.Transmitted gap ems, childSys).emSys
        Assert.Equal(eNorm directChild.transmitted, eNorm childEms.transmitted, 12)
