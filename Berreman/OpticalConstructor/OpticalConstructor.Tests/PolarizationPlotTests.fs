namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.Geometry
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Fields
open Berreman.Solvers
open Berreman.FieldFunctions
open OpticalConstructor.Ui.Charts
open Xunit

/// §H.8 polarization-plot tests (slice 012). AC-H7 — for a known reflected
/// `EmFieldSystem`, the polarization-ellipse curve parameters match the engine
/// `ellipticityR`/`azimuthR`, and the Poincaré point equals the normalized
/// `Solution.stokesR`. No Stokes parameter is recomputed by Part H.
module PolarizationPlotTests =

    let private glass = OpticalProperties.fromRefractionIndex (RefractionIndex.create 1.52)

    /// Vacuum over a glass substrate.
    let private system : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = []
            substrate = None
            lower = glass
        }

    /// Off-normal, 45°-polarized incidence so the reflected field carries both s and
    /// p amplitudes — a non-degenerate ellipse with a non-trivial azimuth.
    let private light =
        { IncidentLightInfo.createInclined (WaveLength.nm 600.0<nm>) (Angle.degree 50.0 |> IncidenceAngle) with
            polarization = Polarization.create (Angle.degree 45.0) }

    let private solution = OpticalSystemSolver(light, system).solution

    let private normPi (a : float) = ((a % System.Math.PI) + System.Math.PI) % System.Math.PI

    // ----------------------------------------------------------------- ellipse

    [<Fact>]
    let ``AC-H7 ellipse semi-axes and orientation match ellipticityR and azimuthR`` () =
        let em = solution.emSys
        let n = 360
        let curve = PolarizationPlots.ellipseCurve n em.ellipticityR em.azimuthR
        let radii = curve |> Array.map (fun (x, y) -> sqrt (x * x + y * y))

        // Semi-major axis is 1 along the principal direction; semi-minor is |ellipticityR|.
        Assert.Equal(1.0, Array.max radii, 6)
        Assert.Equal(abs em.ellipticityR, Array.min radii, 6)

        // The major-axis point lies along the azimuth (mod π).
        let i = radii |> Array.findIndex (fun r -> r = Array.max radii)
        let (mx, my) = curve.[i]
        Assert.Equal(normPi em.azimuthR, normPi (atan2 my mx), 4)

    // ----------------------------------------------------------------- Poincaré

    [<Fact>]
    let ``AC-H7 Poincare point equals the normalized reflected Stokes vector`` () =
        let (StokesVector v) = solution.stokesR
        let s0 = v.[0]
        Assert.True(abs s0 > 0.0, "reflected S0 must be non-zero")

        let (px, py, pz) = PolarizationPlots.poincarePoint solution.stokesR
        Assert.Equal(v.[1] / s0, px, 9)
        Assert.Equal(v.[2] / s0, py, 9)
        Assert.Equal(v.[3] / s0, pz, 9)
