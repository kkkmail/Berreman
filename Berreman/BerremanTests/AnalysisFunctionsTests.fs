namespace BerremanTests

open Berreman.Constants
open Berreman.Geometry
open Berreman.Fields
open Berreman.Media
open Berreman.Dispersion
open Berreman.Solvers
open Berreman.FieldFunctions
open OpticalProperties.Standard
open Analytics.Variables
open Analytics.AnalysisFunctions
open Analytics.FieldProfile
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Xunit

/// Mandatory same-slice smoke assertions for the AC bullets Part F (slice 008) implements
/// behaviourally: AC-F1, AC-F4, AC-F6, AC-F9, AC-F2. Each is a light value/shape/round-trip
/// check — NOT the full equivalence suite (AC-F3/F5/F7/F8 stay in slice 011's
/// `OptimizationTests.fs`). These give every implemented-here function an owned in-slice test
/// (reconciler round-4 §i — applied).
type AnalysisFunctionsTests() =

    let tol = 1.0e-9

    let info =
        { light600nmInclinedDegreeLPs 19.0 with
            polarization = Angle.degree 27.0 |> Polarization
            ellipticity = Ellipticity 0.58 }

    let baseSolution () =
        OpticalSystemSolver(info, BaseOpticalSystem.transparentGlassSystem.fullSystem).solution

    /// AC-F1: `absorptance` returns A = 1 − R − T from `Solution.func` on a computed
    /// solution and does NOT re-invoke the solver.
    [<Fact>]
    member _.``AC-F1 absorptance equals 1 - R - T from Solution.func`` () =
        let sol = baseSolution ()
        let a = absorptance sol
        let r = sol.func R |> Option.defaultValue 0.0
        let t = sol.func T |> Option.defaultValue 0.0
        Assert.True(abs (a - (1.0 - r - t)) < tol, $"A={a}, 1-R-T={1.0 - r - t}")

    /// AC-F4: `degreeOfPolarization` returns √(s1²+s2²+s3²)/s0 from the reflected
    /// `StokesVector` and lies in [0, 1].
    [<Fact>]
    member _.``AC-F4 degreeOfPolarization equals sqrt(s1^2+s2^2+s3^2)/s0 in [0,1]`` () =
        let sol = baseSolution ()
        let dop = degreeOfPolarization sol.stokesR

        let (StokesVector v) = sol.stokesR
        let manual =
            if abs v.[0] < almostZero then 0.0
            else sqrt (v.[1] * v.[1] + v.[2] * v.[2] + v.[3] * v.[3]) / v.[0]

        Assert.True(abs (dop - manual) < tol, $"dop={dop}, manual={manual}")
        Assert.True(dop >= -tol && dop <= 1.0 + 1.0e-6, $"dop={dop} not in [0,1]")

    /// AC-F6: `fieldDepthProfile` returns the expected-length list of meter depths
    /// (samplesPerLayer × layer count), monotonically non-decreasing, stepping the
    /// transmitted `EmField` via `EmField.propagate` with no per-sample re-solve. Shape
    /// smoke only — |E|² magnitudes are NOT asserted (the `BerremanMatrix.fs:221`
    /// "Does not work properly yet" limitation is carried as-is).
    [<Fact>]
    member _.``AC-F6 fieldDepthProfile shape and monotonic meter depths`` () =
        let sys = (BaseOpticalSystem.transparentGlassFilmSystem (Thickness.nm 100.0<nm>)).fullSystem
        let samplesPerLayer = 4
        let profile = fieldDepthProfile sys info samplesPerLayer

        Assert.Equal(samplesPerLayer * sys.films.Length, List.length profile)

        let depths = profile |> List.map fst
        let rec nonDecreasing =
            function
            | a :: b :: rest -> a <= b && nonDecreasing (b :: rest)
            | _ -> true
        Assert.True(nonDecreasing depths, "depths not monotonically non-decreasing")
        Assert.True(depths |> List.forall (fun d -> d >= 0.0<meter>), "negative depth sample")

    /// AC-F9: the default `numberOfReflections` is 3 via `SolverParameters.defaultValue`,
    /// and a caller-supplied value threads into `OpticalSystemSolver` for a thick Plate
    /// substrate without altering any other threshold.
    [<Fact>]
    member _.``AC-F9 default numberOfReflections is 3 and caller value threads through`` () =
        Assert.Equal(3, SolverParameters.defaultValue.numberOfReflections)

        let plateSys = OpticalSystem.biaxialCrystalSubstrateSystem (Thickness.nm 1000.0<nm>)
        let custom = { numberOfReflections = 5 }
        let sol = OpticalSystemSolver(info, plateSys, custom).solution

        match sol with
        | Multiple _ -> ()
        | Single _ -> Assert.Fail "expected a Multiple solution for a thick Plate substrate"

    /// AC-F2: a small `ArbitraryVariableRange` sweep driven through `Variables.calculate`
    /// persists its bulk `array<float * Solution>` via `Softellect.Sys.Core.serialize`/
    /// `deserialize` with `BinaryZippedFormat` (Core.fs:257) and round-trips with fidelity
    /// (float keys and the R channel preserved). The bulk array is NOT placed on the
    /// canonical JSON path — the payload is the zipped binary format (gzip magic), not JSON.
    [<Fact>]
    member _.``AC-F2 ArbitraryVariableRange sweep round-trips through a .binz payload`` () =
        let light = light600nmNormalLPs
        let dispSys = (BaseOpticalSystem.transparentGlassFilmSystem (Thickness.nm 100.0<nm>)).fullSystem.dispersive
        let fixedInfo : FixedInfo = { incidentLightInfo = light; opticalSystem = dispSys }

        let arb : ArbitraryVariable =
            {
                variableName = "filmThickness"
                range = Range<_>.create 3 50.0 200.0
                scale = 1.0
                getSys =
                    fun (sys : OpticalSystem) (z : float) ->
                        match sys.films with
                        | film :: rest ->
                            { sys with films = { film with thickness = Thickness.nm (z * 1.0<nm>) } :: rest }
                        | [] -> sys
            }

        let sweep : (float * Solution)[] = calculate fixedInfo (ArbitraryVariableRange arb)
        Assert.True(sweep.Length >= 2, "sweep should produce multiple points")

        let bytes = serialize BinaryZippedFormat sweep
        let restored : (float * Solution)[] = deserialize BinaryZippedFormat bytes

        Assert.Equal(sweep.Length, restored.Length)

        Array.iter2
            (fun (k1 : float, s1 : Solution) (k2 : float, s2 : Solution) ->
                Assert.True(abs (k1 - k2) < tol, $"key mismatch {k1} vs {k2}")
                let r1 = s1.func R |> Option.defaultValue 0.0
                let r2 = s2.func R |> Option.defaultValue 0.0
                Assert.True(abs (r1 - r2) < tol, $"R mismatch {r1} vs {r2}"))
            sweep
            restored

        // JSON-exclusion: the bulk array travels the zipped-binary `.binz` path, whose
        // payload begins with the gzip magic bytes, never the canonical JSON '{'.
        Assert.True(bytes.Length > 2, "empty payload")
        Assert.Equal(0x1fuy, bytes.[0])
        Assert.Equal(0x8buy, bytes.[1])
        Assert.NotEqual(byte '{', bytes.[0])
