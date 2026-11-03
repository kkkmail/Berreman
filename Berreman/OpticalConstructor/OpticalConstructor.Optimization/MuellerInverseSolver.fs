namespace OpticalConstructor.Optimization

open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.OptimizationInterface

/// Spec 0044 §5.3 — the REAL nonlinear-solve backend behind the `NonlinearSolverProxy` seam that
/// `OpticalConstructor.Domain.MuellerInverse` declares.
///
/// Why the implementation lives HERE and the seam lives in the Domain: the optimizer is ALGLIB, and
/// `AlglibAdapter` is the single file in the solution allowed to name it. `OpticalConstructor.Optimization`
/// references the Domain, never the reverse, so the Domain can declare what a nonlinear solve LOOKS like
/// without acquiring a dependency on how one is performed. This is the same split spec 0042 used for its
/// linear solve, and it is what lets the inverse-problem tests exercise the whole Stage-B pipeline against
/// a mock proxy with no optimizer present at all.
///
/// The Levenberg–Marquardt choice is the standard one for this class of problem and the one the generalized-
/// ellipsometry literature uses throughout; `AlglibAdapter` maps it onto `minlm`.
module MuellerInverseSolver =

    /// Build the real ALGLIB Levenberg–Marquardt-backed `NonlinearSolverProxy`.
    ///
    /// The request's residual, start point and bounds are passed straight through to the existing
    /// `AlglibAdapter.optimize` entry point, which already traps every exception — ALGLIB's or otherwise —
    /// and returns a `Result`. No ALGLIB type crosses this function in either direction.
    ///
    /// A CRITICAL precondition, and the reason `ParameterScaling` exists at all: `AlglibAdapter.runLm`
    /// differentiates numerically with a FIXED ABSOLUTE step of 1e-6. That is only a small perturbation if
    /// the vector handed in is O(1), which is exactly what the scaled parameter vector guarantees and what
    /// raw physical parameters (refractive indices ≈ 1.5 alongside gyration components ≈ 1e-4) do not. This
    /// proxy therefore expects the SCALED vector; handing it physical parameters produces a meaningless
    /// Jacobian and a confident, wrong answer.
    ///
    /// `terminationReason` is folded into the `Result`: a run that ALGLIB reports as unsuccessful becomes
    /// `DidNotConvergeErr` carrying that reason, rather than a solution the caller might mistake for converged.
    let createAlglibLevenbergMarquardt () : NonlinearSolverProxy =
        {
            solveNonlinearLeastSquares =
                fun (request : NonlinearRequest) ->
                    let (ScaledVector initial) = request.initial
                    let (ScaledVector lower) = request.lowerBounds
                    let (ScaledVector upper) = request.upperBounds
                    let (IterationCount maxIterations) = request.maxIterations
                    let (StepTolerance epsX) = request.epsX
                    if initial.Length = 0 then
                        Error (InvalidNonlinearRequestErr "the initial parameter vector is empty")
                    elif lower.Length <> initial.Length || upper.Length <> initial.Length then
                        Error (
                            InvalidNonlinearRequestErr
                                $"the bounds must match the parameter count: {initial.Length} parameters, {lower.Length} lower, {upper.Length} upper")
                    else
                        let optimizationRequest : OptimizationRequest =
                            {
                                residual = fun x -> let (ResidualVector r) = request.residual (ScaledVector x) in r
                                jacobian = None
                                initial = Array.copy initial
                                bounds = { lower = Array.copy lower; upper = Array.copy upper }
                                inequalityTargets = []
                                method = LevenbergMarquardt
                                maxIterations = maxIterations
                                epsX = epsX
                            }
                        match AlglibAdapter.optimize optimizationRequest with
                        | Error reason -> Error (DidNotConvergeErr reason)
                        | Ok result ->
                            if result.success then
                                Ok
                                    {
                                        solution = ScaledVector result.solution
                                        finalResiduals = ResidualVector result.finalResiduals
                                        iterations = IterationCount result.iterations
                                    }
                            else Error (DidNotConvergeErr $"%A{result.terminationReason}")
        }
