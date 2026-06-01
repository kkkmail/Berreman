namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization
open Xunit

/// Part G §G.1–G.3 optimization-infrastructure tests (slice 009). Prove the two
/// boundary invariants: AC-G1 — `optimize` returns an `OptimizationResult` (in
/// our own types) within `maxIterations`, with no ALGLIB type or exception
/// crossing the §G.1 boundary, and `NelderMead` surfaces as the deferred
/// `Failed "simplex backend unresolved"`; AC-G2 — folding a `DesignParameter`
/// list + a `float[]` vector lands canonical-meter `Thickness` values.
module OptimizationInterfaceTests =

    let private noBounds n : ParameterBounds =
        { lower = Array.create n -infinity; upper = Array.create n infinity }

    // ----------------------------------------------------------------- AC-G1

    [<Fact>]
    let ``AC-G1 LevenbergMarquardt returns an OptimizationResult within maxIterations`` () =
        // r(x) = [x0 - 3; x1 + 1]; the least-squares minimum is at (3, -1).
        let residual (x : float[]) = [| x.[0] - 3.0; x.[1] + 1.0 |]
        let request =
            {
                residual = residual
                jacobian = None
                initial = [| 0.0; 0.0 |]
                bounds = noBounds 2
                inequalityTargets = []
                method = LevenbergMarquardt
                maxIterations = 100
                epsX = 1.0e-9
            }
        match AlglibAdapter.optimize request with
        | Ok result ->
            // The returned value is the G.1 `OptimizationResult` — no ALGLIB type
            // is in its surface — and the run stayed within the iteration cap.
            Assert.True(result.iterations <= request.maxIterations)
            Assert.Equal(2, result.solution.Length)
            Assert.Equal(3.0, result.solution.[0], 4)
            Assert.Equal(-1.0, result.solution.[1], 4)
            Assert.True(result.success)
        | Error e -> failwith $"expected Ok, got Error {e}"

    [<Fact>]
    let ``AC-G1 LevenbergMarquardt with an analytic Jacobian also converges`` () =
        let residual (x : float[]) = [| x.[0] - 2.0; 2.0 * x.[1] - 4.0 |]
        let jacobian (_ : float[]) = [| [| 1.0; 0.0 |]; [| 0.0; 2.0 |] |]
        let request =
            {
                residual = residual
                jacobian = Some jacobian
                initial = [| 0.0; 0.0 |]
                bounds = noBounds 2
                inequalityTargets = []
                method = LevenbergMarquardt
                maxIterations = 100
                epsX = 1.0e-9
            }
        match AlglibAdapter.optimize request with
        | Ok result ->
            Assert.Equal(2.0, result.solution.[0], 4)
            Assert.Equal(2.0, result.solution.[1], 4)
        | Error e -> failwith $"expected Ok, got Error {e}"

    [<Fact>]
    let ``AC-G1 NelderMead surfaces as the deferred Failed reason (G.2)`` () =
        let request =
            {
                residual = (fun x -> [| x.[0] |])
                jacobian = None
                initial = [| 1.0 |]
                bounds = noBounds 1
                inequalityTargets = []
                method = NelderMead
                maxIterations = 10
                epsX = 1.0e-9
            }
        match AlglibAdapter.optimize request with
        | Ok result ->
            Assert.Equal(Failed "simplex backend unresolved", result.terminationReason)
            Assert.False(result.success)
        | Error e -> failwith $"expected Ok carrying the deferred Failed reason, got Error {e}"

    [<Fact>]
    let ``AC-G1 an exception inside the optimization stays behind the Result boundary`` () =
        // A throwing residual stands in for any ALGLIB-side failure: it MUST come
        // back as `Result.Error`, never as an escaped exception.
        let request =
            {
                residual = (fun _ -> failwith "boom")
                jacobian = None
                initial = [| 0.0 |]
                bounds = noBounds 1
                inequalityTargets = []
                method = LevenbergMarquardt
                maxIterations = 10
                epsX = 1.0e-9
            }
        match AlglibAdapter.optimize request with
        | Ok _ -> failwith "expected the failure to surface as Result.Error, not an escaped exception"
        | Error _ -> ()

    // ----------------------------------------------------------------- AC-G2

    let private vacuum = OpticalProperties.vacuum
    let private mkLayer (t : float<nm>) : Layer = { properties = vacuum; thickness = Thickness.nm t }

    let private baseSys : OpticalSystem =
        {
            description = None
            upper = vacuum
            films = [ mkLayer 100.0<nm>; mkLayer 200.0<nm> ]
            substrate = None
            lower = vacuum
        }

    [<Fact>]
    let ``AC-G2 folding a DesignParameter list lands canonical-meter thicknesses`` () =
        let parameters = [ DesignParameters.layerThickness 0 1.0e-6; DesignParameters.layerThickness 1 1.0e-6 ]
        let vector = [| 1.5e-7; 3.0e-7 |]   // canonical meters
        let result = DesignParameters.applyVector parameters vector baseSys

        match result.films.[0].thickness, result.films.[1].thickness with
        | Thickness t0, Thickness t1 ->
            // Each parameter is reflected as `Thickness (v * 1.0<meter>)` — the DU
            // stores `double<meter>`, so the value is canonical SI by construction.
            Assert.Equal(1.5e-7, t0 / 1.0<meter>, 15)
            Assert.Equal(3.0e-7, t1 / 1.0<meter>, 15)
        | _ -> failwith "expected finite Thickness values for both films"

        // The negative-thickness prohibition (R-3) is the lower bound 0, carried
        // into the G.1 ParameterBounds rather than a separate validator.
        let b = DesignParameters.bounds parameters
        Assert.Equal<float[]>([| 0.0; 0.0 |], b.lower)
        Assert.Equal<float[]>([| 1.0e-6; 1.0e-6 |], b.upper)

    [<Fact>]
    let ``AC-G2 the fold applies parameters in vector order and leaves the base system unmutated`` () =
        let parameters = [ DesignParameters.layerThickness 1 1.0e-6 ]
        let result = DesignParameters.applyVector parameters [| 4.2e-7 |] baseSys
        match result.films.[1].thickness with
        | Thickness t1 -> Assert.Equal(4.2e-7, t1 / 1.0<meter>, 15)
        | _ -> failwith "expected a finite Thickness"
        // Film 0 untouched; the base system is unchanged (immutable update).
        Assert.Equal<Thickness>(Thickness.nm 100.0<nm>, result.films.[0].thickness)
        Assert.Equal<Thickness>(Thickness.nm 200.0<nm>, baseSys.films.[1].thickness)
