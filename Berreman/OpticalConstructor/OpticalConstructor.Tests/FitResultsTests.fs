namespace OpticalConstructor.Tests

open System
open System.Threading
open Xunit
open Berreman.Constants
open Berreman.Fields
open Berreman.Geometry
open Berreman.MathNetNumericsMath
open OpticalConstructor.Domain
open OpticalConstructor.Domain.ItemModels
open OpticalConstructor.Domain.Kit
open OpticalConstructor.Domain.MuellerReconstruction
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Domain.Studies
open OpticalConstructor.Domain.StudyForward
open OpticalConstructor.Domain.FitRuns
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.MuellerFitting
open OpticalConstructor.Optimization.FittedProblems

/// Spec 0050 S11.4 — a fit's results over their result shapes and synthetic least-squares problems.
module FitResultsTests =

    [<Fact>]
    let ``a Jacobian's rank counts the directions its data constrains, above forward-difference noise`` () : unit =
        let independent = ResidualJacobian [| [| 1.0; 0.0 |]; [| 0.0; 1.0 |]; [| 1.0; 1.0 |] |]
        let proportional = ResidualJacobian [| [| 1.0; 1.0 |]; [| 2.0; 2.0 |]; [| 3.0; 3.0 |] |]
        let proportionalToNoise = ResidualJacobian [| [| 1.0; 1.0 + 1e-9 |]; [| 2.0; 2.0 - 1e-9 |]; [| 3.0; 3.0 + 2e-9 |] |]
        let weakDirection = ResidualJacobian [| [| 1.0; 0.0 |]; [| 0.0; 1e-4 |] |]
        let noRow = ResidualJacobian [||]

        Assert.Equal(MatrixRank 2, independent.rank)
        Assert.Equal(MatrixRank 1, proportional.rank)
        Assert.Equal(MatrixRank 1, proportionalToNoise.rank)
        Assert.Equal(MatrixRank 2, weakDirection.rank)
        Assert.Equal(MatrixRank 0, noRow.rank)

    [<Fact>]
    let ``the residuals of a solution over a least-squares system are its design times the solution less its right-hand side`` () : unit =
        let system : LeastSquaresSystem =
            {
                design = [| DesignRow [| 1.0; 0.0 |]; DesignRow [| 0.0; 1.0 |]; DesignRow [| 1.0; 1.0 |] |]
                rhs = [| 0.1; 2.1; 1.9 |]
            }
        let solution = RealVector.create [ 0.0; 2.0 ]
        let (RealVector residuals) = system.residualsAt solution

        Assert.Equal(3, residuals.Count)
        Assert.Equal(-0.1, residuals.[0], 1e-15)
        Assert.Equal(-0.1, residuals.[1], 1e-15)
        Assert.Equal(0.1, residuals.[2], 1e-15)
        Assert.Equal(0.1, (system.residualRmseAt solution).value, 1e-15)

    [<Fact>]
    let ``a fitted Mueller matrix is realizable when no coherency eigenvalue is negative beyond round-off`` () : unit =
        let item = ItemId (Guid.Parse "39600000-0000-4000-8000-000000000001")

        let realizabilityOf (eigenvalues : CoherencyEigenvalue list) : MuellerRealizability =
            ({ item = item; eigenvalues = eigenvalues } : ObjectRealizability).realizability

        let eigenvaluesOf (values : float list) : CoherencyEigenvalue list = values |> List.map CoherencyEigenvalue

        let diagonal =
            RealMatrix4x4.create
                [ [ 1.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 1.5; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; 0.0 ]
                  [ 0.0; 0.0; 0.0; 0.0 ] ]
            |> MuellerMatrix

        let retarder = retarderMueller (Angle.degree 30.0<deg>) (Retardance.degree 40.0<deg>)

        List.iter2
            (fun (expected : float) (actual : CoherencyEigenvalue) -> Assert.Equal(expected, actual.value, 1e-12))
            [ 0.625; 0.625; -0.125; -0.125 ]
            diagonal.cloudeEigenvalues

        Assert.Equal(RealizableMueller, realizabilityOf Propagation.identityMueller.cloudeEigenvalues)
        Assert.Equal(RealizableMueller, realizabilityOf retarder.cloudeEigenvalues)
        Assert.Equal(UnrealizableMueller, realizabilityOf diagonal.cloudeEigenvalues)
        Assert.Equal(RealizableMueller, realizabilityOf (eigenvaluesOf [ 1.0; 0.0; 0.0; -1e-14 ]))
        Assert.Equal(UnrealizableMueller, realizabilityOf (eigenvaluesOf [ 1.0; 0.0; 0.0; -1e-6 ]))

    /// The rows [1; 0], [0; 1], [1; 1] against [0.1; 2.1; 1.9]: least squares at [0; 2], χ² 0.03, (AᵀA)⁻¹ = [2 −1; −1 2] / 3.
    let private independentSystem : LeastSquaresSystem =
        {
            design = [| DesignRow [| 1.0; 0.0 |]; DesignRow [| 0.0; 1.0 |]; DesignRow [| 1.0; 1.0 |] |]
            rhs = [| 0.1; 2.1; 1.9 |]
        }

    /// The rows [1; 1], [2; 2], [3; 3] against [1; 2; 3.1]: only x₀ + x₁ = 14.3 / 14 is determined.
    let private proportionalSystem : LeastSquaresSystem =
        {
            design = [| DesignRow [| 1.0; 1.0 |]; DesignRow [| 2.0; 2.0 |]; DesignRow [| 3.0; 3.0 |] |]
            rhs = [| 1.0; 2.0; 3.1 |]
        }

    let private fittedItem = ItemId (Guid.Parse "39700000-0000-4000-8000-000000000001")

    let private entryNamed (name : string) : FitEntryRef =
        { parameter = ItemParameterRef (fittedItem, ParameterName name); owner = ItemOwner fittedItem }

    /// The system's solve reached at `solution`: coordinate k the entry `x<k>`, centre zero, unit one and unbounded.
    let private fittedOver (system : LeastSquaresSystem) (solution : float list) : FittedProblem =
        let dimension = solution.Length

        let search : ScaledSearch =
            {
                start = ScaledVector (Array.ofList solution)
                lowerBounds = ScaledVector (Array.create dimension -infinity)
                upperBounds = ScaledVector (Array.create dimension infinity)
            }

        {
            subject = StudyEntriesProblem
            coordinates = List.init dimension (fun k -> EntryCoordinate [ entryNamed $"x{k}" ])
            residual = fun (ScaledVector s) -> let (RealVector r) = system.residualsAt (RealVector.create s) in ResidualVector (r.ToArray())
            search = search
            centre = StudyParameterVector (Array.zeroCreate dimension)
            unit = StudyParameterVector (Array.create dimension 1.0)
        }

    let private withBounds (lower : float list) (upper : float list) (problem : FittedProblem) : FittedProblem =
        let bounded : ScaledSearch =
            { problem.search with lowerBounds = ScaledVector (Array.ofList lower); upperBounds = ScaledVector (Array.ofList upper) }

        { problem with search = bounded }

    let private profileOf (result : Result<ProfileRun, FitProfileError>) : ProfileRun =
        match result with
        | Ok run -> run
        | Error error -> failwith $"the profile was refused: {error.describe}"

    let private chiSquaredOf (point : ProfilePoint) : float =
        match point.outcome with
        | ProfileConverged chiSquared -> chiSquared.value
        | ProfileFailed failure -> failwith $"a profile re-fit failed: {failure.value}"

    [<Fact>]
    let ``a fitted problem's Jacobian, sigmas, correlation, rank and condition are those of its least-squares design`` () : unit =
        let problem = fittedOver independentSystem [ 0.0; 2.0 ]
        let sigma = sqrt 0.02

        // Columns [1; 0; 1] and [0; 1; 1]: the design itself.
        let jacobian = problem.jacobian.value
        Assert.Equal(3, jacobian.Length)

        Array.iter2
            (fun (expected : float[]) (row : float[]) ->
                Assert.Equal(2, row.Length)
                Array.iter2 (fun (e : float) (actual : float) -> Assert.Equal(e, actual, 1e-6)) expected row)
            [| [| 1.0; 0.0 |]; [| 0.0; 1.0 |]; [| 1.0; 1.0 |] |]
            jacobian

        // √(χ² / (m − n) · (AᵀA)⁻¹ₖₖ) = √(0.03 · 2 / 3).
        for k in 0 .. 1 do
            match problem.sigmaAt (FitCoordinateIndex k) with
            | Some found -> Assert.Equal(1.0, found.value / sigma, 1e-6)
            | None -> Assert.Fail($"coordinate {k} has no sigma")

        let quality = problem.quality
        Assert.Equal(StudyEntriesProblem, quality.subject)
        Assert.Equal<FitCoordinate list>(problem.coordinates, quality.coordinates)
        Assert.Equal(MatrixRank 2, quality.rank)
        Array.iter (fun (norm : float) -> Assert.Equal(sqrt 2.0, norm, 1e-6)) quality.columnNorms.value
        Assert.Equal(0.03, quality.chiSquared.value, 1e-12)
        Assert.Equal(0.1, quality.rmse.value, 1e-12)

        match quality.correlationOpt with
        | Some correlation ->
            Assert.Equal(-0.5, correlation.value.[0].[1], 1e-6)
            Assert.Equal(-0.5, correlation.value.[1].[0], 1e-6)
            Assert.Equal(1.0, correlation.value.[0].[0], 1e-12)
        | None -> Assert.Fail("a full-rank solve has a correlation")

        match quality.conditionOpt with
        | Some condition -> Assert.Equal(sqrt 3.0, condition.value, 1e-6)
        | None -> Assert.Fail("a full-rank solve has a condition")

        match problem.entrySigmas with
        | [ first; second ] ->
            Assert.Equal(entryNamed "x0", first.entry)
            Assert.Equal(entryNamed "x1", second.entry)
            Assert.Equal(problem.sigmaAt (FitCoordinateIndex 0), first.sigmaOpt)
            Assert.Equal(problem.sigmaAt (FitCoordinateIndex 1), second.sigmaOpt)
        | sigmas -> Assert.Fail($"expected two entry sigmas, got {sigmas.Length}")

        // A coordinate of unit 2 stands for twice its scaled value, so its σ doubles.
        let doubled = { problem with unit = StudyParameterVector [| 2.0; 1.0 |] }

        match doubled.sigmaAt (FitCoordinateIndex 0) with
        | Some found -> Assert.Equal(1.0, found.value / (2.0 * sigma), 1e-6)
        | None -> Assert.Fail("coordinate 0 of unit 2 has no sigma")

        Assert.Equal(None, problem.sigmaAt (FitCoordinateIndex 2))

    [<Fact>]
    let ``a problem whose Jacobian is rank-deficient has no sigma, correlation or condition`` () : unit =
        let problem = fittedOver proportionalSystem [ 14.3 / 28.0; 14.3 / 28.0 ]
        let quality = problem.quality

        Assert.Equal(MatrixRank 1, quality.rank)
        Assert.Equal(None, problem.sigmaAt (FitCoordinateIndex 0))
        Assert.Equal(None, problem.sigmaAt (FitCoordinateIndex 1))
        Assert.Equal(None, quality.conditionOpt)

        match quality.correlationOpt with
        | Some _ -> Assert.Fail("a rank-deficient solve has no correlation")
        | None -> ()

        Assert.Equal<ParameterSigma option list>([ None; None ], problem.entrySigmas |> List.map (fun entrySigma -> entrySigma.sigmaOpt))

    [<Fact>]
    let ``a linear problem's profile rises by the reduced chi-squared times the step squared, within the coordinate's bounds`` () : unit =
        let problem = fittedOver independentSystem [ 0.0; 2.0 ]
        let sigma = sqrt 0.02
        let run = profileOf (problem.profileAt (FitCoordinateIndex 0) (IterationCount 0) UnwatchedIterations)

        Assert.Equal(7, run.points.Length)

        run.points
        |> List.iteri (fun (i : int) (point : ProfilePoint) ->
            let j = float (i - 3)
            Assert.Equal(j * sigma, point.value.value, 1e-9)
            Assert.Equal(1.0, chiSquaredOf point / (0.03 * (1.0 + j * j)), 1e-9))

        // The middle point is the solve's own χ², not a re-fit's.
        Assert.Equal(ProfileConverged problem.quality.chiSquared, run.points.[3].outcome)

        // −3σ ≈ −0.424 lies below a lower bound of −0.3; −2σ ≈ −0.283 does not.
        let bounded = problem |> withBounds [ -0.3; -infinity ] [ infinity; infinity ]
        let boundedRun = profileOf (bounded.profileAt (FitCoordinateIndex 0) (IterationCount 0) UnwatchedIterations)

        Assert.Equal<float list>(
            [ -2.0; -1.0; 0.0; 1.0; 2.0; 3.0 ],
            boundedRun.points |> List.map (fun (point : ProfilePoint) -> Math.Round(point.value.value / sigma, 9)))

    [<Fact>]
    let ``a sigma-less profile steps across the coordinate's bounds and is flat along a confounded direction`` () : unit =
        let problem = fittedOver proportionalSystem [ 14.3 / 28.0; 14.3 / 28.0 ] |> withBounds [ -1.0; -2.0 ] [ 1.0; 3.0 ]
        let solved = problem.quality.chiSquared.value
        let run = profileOf (problem.profileAt (FitCoordinateIndex 0) (IterationCount 0) UnwatchedIterations)

        Assert.Equal(7, run.points.Length)

        run.points
        |> List.iteri (fun (i : int) (point : ProfilePoint) ->
            Assert.Equal(-1.0 + float i / 3.0, point.value.value, 1e-12)
            Assert.Equal(1.0, chiSquaredOf point / solved, 1e-9))

    [<Fact>]
    let ``a cancelled profile answers ProfileCancelledErr, and a watch after earlier iterations counts on from them`` () : unit =
        let problem = fittedOver independentSystem [ 0.0; 2.0 ]

        use cancelled = new CancellationTokenSource()
        cancelled.Cancel()
        let silent = ResizeArray<IterationCount>()
        let stopped = WatchedIterations { reportIteration = silent.Add; cancellation = cancelled.Token }

        match problem.profileAt (FitCoordinateIndex 0) (IterationCount 0) stopped with
        | Error (ProfileCancelledErr reason) -> Assert.Contains("coordinate 0: 0 of 7", reason)
        | Ok run -> Assert.Fail($"expected ProfileCancelledErr, got {run.points.Length} point(s)")

        Assert.Empty(silent)

        // Told 1 and 2, the watch after five tells its observer 6 and 7.
        let told = ResizeArray<IterationCount>()
        let observer : OptimizerIterationObserver = { reportIteration = told.Add; cancellation = CancellationToken.None }

        match (WatchedIterations observer).after (IterationCount 5) with
        | WatchedIterations shifted ->
            shifted.reportIteration (IterationCount 1)
            shifted.reportIteration (IterationCount 2)
            Assert.Equal(observer.cancellation, shifted.cancellation)
        | UnwatchedIterations -> Assert.Fail("a watched solve stays watched")

        Assert.Equal<IterationCount list>([ IterationCount 6; IterationCount 7 ], List.ofSeq told)

        match UnwatchedIterations.after (IterationCount 5) with
        | UnwatchedIterations -> ()
        | WatchedIterations _ -> Assert.Fail("an unwatched solve stays unwatched")

        // A watched profile after five iterations counts its six re-fits on from six, and its run counts them all.
        let reports = ResizeArray<IterationCount>()
        let watch = WatchedIterations { reportIteration = reports.Add; cancellation = CancellationToken.None }
        let run = profileOf (problem.profileAt (FitCoordinateIndex 0) (IterationCount 5) watch)

        Assert.True(reports.Count >= 6, $"{reports.Count} iteration(s) were reported over six re-fits")
        Assert.Equal<IterationCount list>([ for n in 6 .. 5 + reports.Count -> IterationCount n ], List.ofSeq reports)
        Assert.Equal(IterationCount reports.Count, run.iterations)
