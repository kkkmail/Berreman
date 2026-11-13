namespace OpticalConstructor.Tests

open System.Threading
open Berreman.Constants
open Berreman.MaterialProperties
open Berreman.Media
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.AlglibAdapter
open OpticalConstructor.Optimization.DesignParameters
open OpticalConstructor.Optimization.MuellerInverseSolver
open OpticalConstructor.Optimization
open Xunit

/// Part G §G.1–G.3 optimization-infrastructure tests (slice 009). Prove the two
/// boundary invariants: AC-G1 — `optimize` returns an `OptimizationResult` (in
/// our own types) within `maxIterations`, with no ALGLIB type or exception
/// crossing the §G.1 boundary, and `NelderMead` surfaces as the deferred
/// `SimplexBackendUnresolved`; AC-G2 — folding a `DesignParameter`
/// list + a `DesignVector` lands canonical-meter `Thickness` values.
module OptimizationInterfaceTests =

    let private noBounds n : ParameterBounds =
        { lower = Array.create n -infinity |> DesignVector; upper = Array.create n infinity |> DesignVector }

    // ----------------------------------------------------------------- AC-G1

    // r(x) = [x0 - 3; x1 + 1]; the least-squares minimum is at (3, -1).
    let private acG1Request : OptimizationRequest =
        {
            residual = fun (DesignVector x) -> ResidualVector [| x.[0] - 3.0; x.[1] + 1.0 |]
            jacobian = None
            initial = DesignVector [| 0.0; 0.0 |]
            bounds = noBounds 2
            inequalityTargets = []
            method = LevenbergMarquardt
            maxIterations = IterationCount 100
            epsX = StepTolerance 1.0e-9
        }

    [<Fact>]
    let ``AC-G1 LevenbergMarquardt returns an OptimizationResult within maxIterations`` () =
        let request = acG1Request
        match request.optimize () with
        | Ok result ->
            // The returned value is the G.1 `OptimizationResult` — no ALGLIB type
            // is in its surface — and the run stayed within the iteration cap.
            Assert.True(result.iterations.value <= request.maxIterations.value)
            Assert.Equal(2, result.solution.value.Length)
            Assert.Equal(3.0, result.solution.value.[0], 4)
            Assert.Equal(-1.0, result.solution.value.[1], 4)
            Assert.Equal(OptimizationSucceeded, result.outcome)
        | Error e -> failwith $"expected Ok, got Error {e}"

    [<Fact>]
    let ``AC-G1 LevenbergMarquardt with an analytic Jacobian also converges`` () =
        let residual (DesignVector x) = ResidualVector [| x.[0] - 2.0; 2.0 * x.[1] - 4.0 |]
        let jacobian (_ : DesignVector) = ResidualJacobian [| [| 1.0; 0.0 |]; [| 0.0; 2.0 |] |]
        let request =
            {
                residual = residual
                jacobian = Some jacobian
                initial = DesignVector [| 0.0; 0.0 |]
                bounds = noBounds 2
                inequalityTargets = []
                method = LevenbergMarquardt
                maxIterations = IterationCount 100
                epsX = StepTolerance 1.0e-9
            }
        match request.optimize () with
        | Ok result ->
            Assert.Equal(2.0, result.solution.value.[0], 4)
            Assert.Equal(2.0, result.solution.value.[1], 4)
        | Error e -> failwith $"expected Ok, got Error {e}"

    [<Fact>]
    let ``AC-G1 NelderMead surfaces as the deferred Failed reason (G.2)`` () =
        let request =
            {
                residual = (fun (DesignVector x) -> ResidualVector [| x.[0] |])
                jacobian = None
                initial = DesignVector [| 1.0 |]
                bounds = noBounds 1
                inequalityTargets = []
                method = NelderMead
                maxIterations = IterationCount 10
                epsX = StepTolerance 1.0e-9
            }
        match request.optimize () with
        | Ok result ->
            Assert.Equal(SimplexBackendUnresolved, result.terminationReason)
            Assert.Equal(OptimizationFailed, result.outcome)
        | Error e -> failwith $"expected Ok carrying the deferred Failed reason, got Error {e}"

    [<Fact>]
    let ``AC-G1 an exception inside the optimization stays behind the Result boundary`` () =
        // A throwing residual stands in for any ALGLIB-side failure: it MUST come
        // back as `Result.Error`, never as an escaped exception.
        let request =
            {
                residual = (fun _ -> failwith "boom")
                jacobian = None
                initial = DesignVector [| 0.0 |]
                bounds = noBounds 1
                inequalityTargets = []
                method = LevenbergMarquardt
                maxIterations = IterationCount 10
                epsX = StepTolerance 1.0e-9
            }
        match request.optimize () with
        | Ok _ -> failwith "expected the failure to surface as Result.Error, not an escaped exception"
        | Error _ -> ()

    // ----------------------------------------------------------------- spec 0050 S11.3, the watched solve

    // r(x) = [10 (x1 - x0²); 1 - x0]: the Rosenbrock valley, far from its minimum (1, 1) at the start (-1.2, 1).
    let private rosenbrock (x : float[]) : ResidualVector = ResidualVector [| 10.0 * (x.[1] - x.[0] * x.[0]); 1.0 - x.[0] |]

    let private rosenbrockRequest : OptimizationRequest =
        {
            residual = fun (DesignVector x) -> rosenbrock x
            jacobian = None
            initial = DesignVector [| -1.2; 1.0 |]
            bounds = noBounds 2
            inequalityTargets = []
            method = LevenbergMarquardt
            maxIterations = IterationCount 400
            epsX = StepTolerance 1.0e-12
        }

    /// Records every report and cancels its source at the first.
    let private cancellingAtFirstReport
        (source : CancellationTokenSource)
        (reports : ResizeArray<IterationCount>)
        : OptimizerIterationObserver =
        {
            reportIteration =
                fun count ->
                    reports.Add count
                    match count with
                    | IterationCount 1 -> source.Cancel()
                    | _ -> ()
            cancellation = source.Token
        }

    [<Fact>]
    let ``a watched Levenberg-Marquardt solve reports each iteration and answers what the unwatched solve answers`` () =
        let reports = ResizeArray<IterationCount>()
        let observer = { reportIteration = reports.Add; cancellation = CancellationToken.None }
        match acG1Request.optimize (), acG1Request.optimizeWatched (WatchedIterations observer) with
        | Ok unwatched, Ok watched ->
            Assert.Equal(unwatched.solution.value.Length, watched.solution.value.Length)
            Array.iter2 (fun (e : float) (a : float) -> Assert.Equal(e, a, 1e-12)) unwatched.solution.value watched.solution.value
            Assert.Equal<IterationCount>(unwatched.iterations, watched.iterations)
            Assert.Equal<TerminationReason>(unwatched.terminationReason, watched.terminationReason)
            Assert.NotEmpty reports
            // ALGLIB also reports its start point, so every iteration is reported and the count runs ahead of it.
            Assert.True(reports.Count >= watched.iterations.value, $"{reports.Count} reports for {watched.iterations.value} iterations")
            Assert.Equal<IterationCount list>([ for k in 1 .. reports.Count -> IterationCount k ], List.ofSeq reports)
        | unwatched, watched -> failwith $"expected two Ok results, got {unwatched} and {watched}"

    [<Fact>]
    let ``a watched Levenberg-Marquardt solve stops when its cancellation is requested`` () =
        use direct = new CancellationTokenSource()
        let directReports = ResizeArray<IterationCount>()
        match rosenbrockRequest.optimizeWatched (WatchedIterations (cancellingAtFirstReport direct directReports)) with
        | Ok result ->
            Assert.Equal<TerminationReason>(SearchCancelled, result.terminationReason)
            Assert.Equal<OptimizationOutcome>(OptimizationFailed, result.outcome)
            Assert.InRange(directReports.Count, 1, 2)
        | Error e -> failwith $"expected Ok carrying SearchCancelled, got Error {e}"

        use proxied = new CancellationTokenSource()
        let proxiedReports = ResizeArray<IterationCount>()
        let request : NonlinearRequest =
            {
                residual = fun (ScaledVector x) -> rosenbrock x
                initial = ScaledVector [| -1.2; 1.0 |]
                lowerBounds = ScaledVector [| -10.0; -10.0 |]
                upperBounds = ScaledVector [| 10.0; 10.0 |]
                maxIterations = IterationCount 400
                epsX = StepTolerance 1.0e-12
            }
        let solver = (WatchedIterations (cancellingAtFirstReport proxied proxiedReports)).levenbergMarquardtSolver
        match solver.solveNonlinearLeastSquares request with
        | Error (DidNotConvergeErr _) -> Assert.InRange(proxiedReports.Count, 1, 2)
        | other -> failwith $"expected DidNotConvergeErr, got %A{other}"

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
        let parameters =
            [ DesignParameters.layerThickness (FilmIndex 0) (Thickness.mkm 1.0<mkm>); DesignParameters.layerThickness (FilmIndex 1) (Thickness.mkm 1.0<mkm>) ]
        let vector = DesignVector [| 1.5e-7; 3.0e-7 |]   // canonical meters
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
        Assert.Equal<float[]>([| 0.0; 0.0 |], b.lower.value)
        Assert.Equal<float[]>([| 1.0e-6; 1.0e-6 |], b.upper.value)

    [<Fact>]
    let ``AC-G2 the fold applies parameters in vector order and leaves the base system unmutated`` () =
        let parameters = [ DesignParameters.layerThickness (FilmIndex 1) (Thickness.mkm 1.0<mkm>) ]
        let result = DesignParameters.applyVector parameters (DesignVector [| 4.2e-7 |]) baseSys
        match result.films.[1].thickness with
        | Thickness t1 -> Assert.Equal(4.2e-7, t1 / 1.0<meter>, 15)
        | _ -> failwith "expected a finite Thickness"
        // Film 0 untouched; the base system is unchanged (immutable update).
        Assert.Equal<Thickness>(Thickness.nm 100.0<nm>, result.films.[0].thickness)
        Assert.Equal<Thickness>(Thickness.nm 200.0<nm>, baseSys.films.[1].thickness)
