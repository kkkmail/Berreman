namespace OpticalConstructor.Optimization

open Berreman.Media
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.DesignParameters
open OpticalConstructor.Optimization.MeritFunction

/// §G.5 — local refinement optimization (spec 0022 Part G §G.5, [Core]).
///
/// `refine`/`refineWith` take a base `OpticalSystem`, a `DesignParameter list`
/// (§G.3), the initial design vector, and a `FitTarget list` (§G.4); they build
/// the residual closure (§G.4) and call `optimize` (§G.1/§G.2). Levenberg–
/// Marquardt is the default ("the workhorse", 010 §II.5); quasi-Newton
/// (`QuasiNewtonLbfgs`) and Nelder–Mead simplex (`NelderMead`) are selectable
/// via `refineWith`. Bounded parameters are honoured by passing the §G.3 bounds
/// as `ParameterBounds` (lower bound 0 for thickness IS the negative-thickness
/// prohibition, not a separate validator), and the FitTargets' inequality
/// targets flow through to the request as G.1 `InequalityTarget`s.
///
/// On success the optimizer's `solution` vector is folded back through the §G.3
/// mapping (`applyVector`) to produce the refined `OpticalSystem`. Robust
/// normal-equation handling is delegated to ALGLIB's `minlm` internals via §G.2
/// and is NOT re-implemented here. This file does NOT spawn its own thread:
/// long-running execution and cancellation are owned by §G.10 / Part J §J.10
/// (slices 011/016).
module LocalRefinement =

    /// LM is the default refinement method (010 §II.5 "the workhorse").
    let defaultMethod = LevenbergMarquardt

    /// Conservative defaults for the unconstrained termination knobs.
    let defaultMaxIterations = 200
    let defaultEpsX = 1.0e-9

    /// The FitTargets' inequality targets, surfaced as a G.1 `InequalityTarget`
    /// list (R-2); equality targets contribute no constraint.
    let inequalityTargets (targets : FitTarget list) : InequalityTarget list =
        targets
        |> List.choose (fun t -> match t.kind with | Inequality it -> Some it | Equality -> None)

    /// Refine with an explicit method / termination knobs. `initial` is the
    /// starting design vector (the §G.3 `DesignParameter` carries no value
    /// getter, so the start point is supplied by the caller).
    let refineWith
        (method : OptimizationMethod)
        (maxIterations : int)
        (epsX : float)
        (baseSystem : OpticalSystem)
        (parameters : DesignParameter list)
        (initial : float[])
        (targets : FitTarget list) : Result<OpticalSystem * OptimizationResult, string> =
        let request =
            {
                residual = MeritFunction.buildResidual baseSystem parameters targets
                jacobian = None
                initial = Array.copy initial
                bounds = DesignParameters.bounds parameters
                inequalityTargets = inequalityTargets targets
                method = method
                maxIterations = maxIterations
                epsX = epsX
            }
        AlglibAdapter.optimize request
        |> Result.map (fun result ->
            let refined = DesignParameters.applyVector parameters result.solution baseSystem
            refined, result)

    /// Refine with the Levenberg–Marquardt default and the default termination
    /// knobs — the [Core] entry point §G.5 names.
    let refine
        (baseSystem : OpticalSystem)
        (parameters : DesignParameter list)
        (initial : float[])
        (targets : FitTarget list) : Result<OpticalSystem * OptimizationResult, string> =
        refineWith defaultMethod defaultMaxIterations defaultEpsX baseSystem parameters initial targets
