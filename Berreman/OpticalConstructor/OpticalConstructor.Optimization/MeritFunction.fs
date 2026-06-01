namespace OpticalConstructor.Optimization

open Berreman.Fields
open Berreman.Media
open Berreman.Solvers
open Berreman.FieldFunctions
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.DesignParameters
open OpticalConstructor.Optimization.Ellipsometry

/// §G.4 — target / merit-function editor model (spec 0022 Part G §G.4, [Core]).
///
/// A `FitTarget` carries the quantity to evaluate (photometric `OpticalFunction`
/// reused from `FieldFunctions.fs:109`, or the §G.6 `EllipsometricFunction`),
/// the independent-variable sample point (an `IncidentLightInfo` whose
/// `waveLength : WaveLength` reduces to canonical meters via `.value`,
/// `Fields.fs:284`), the desired value, a `weight`, a `tolerance`, and a
/// `kind : TargetKind` (`Equality | Inequality of InequalityTarget`) — covering
/// 010 §II.5's weights, tolerances, and one-sided/inequality targets.
///
/// The merit function is a weighted least-squares residual vector. `modelValue`
/// runs `OpticalSystemSolver` (`Solvers.fs:199`) on the system produced by the
/// G.3 mapping at the target's sample point and reads `Solution.func`
/// (`FieldFunctions.fs:192`) — the SAME evaluator the charts use, so fit and
/// chart never diverge. The assembled closure satisfies the G.1 `Residual`
/// signature directly. No merit-evaluation cache is admitted here (§G.4).
module MeritFunction =

    /// Floor on the residual denominator so a zero `tolerance` cannot divide by 0.
    let private epsilon = 1.0e-12

    /// The quantity a `FitTarget` evaluates: a photometric `OpticalFunction`
    /// (reused, not forked) or a §G.6 ellipsometric Ψ/Δ.
    type TargetQuantity =
        | Photometric of OpticalFunction
        | Ellipsometric of EllipsometricFunction

    /// Equality target, or a one-sided inequality carrying the G.1
    /// `InequalityTarget` bound (010 §II.5 one-sided/inequality targets).
    type TargetKind =
        | Equality
        | Inequality of InequalityTarget

    /// One fit target / merit term.
    type FitTarget =
        {
            quantity : TargetQuantity
            /// The independent-variable sample point. Its `waveLength` is stored
            /// canonical-SI (meters via `WaveLength.value`); unit conversion
            /// happens only at the editor/import boundary (§0 constraint 3).
            samplePoint : IncidentLightInfo
            desiredValue : float
            weight : float
            tolerance : float
            kind : TargetKind
        }

        /// The sample point's wavelength; `.value` is the canonical-SI datum (meters).
        member this.waveLength = this.samplePoint.waveLength

    /// `modelValue` — solve the G.3-mapped system at the target's sample point
    /// and read the requested quantity through `Solution.func` (the chart
    /// evaluator) for photometric quantities, or the §G.6 evaluator for Ψ/Δ.
    let modelValue
        (baseSystem : OpticalSystem)
        (parameters : DesignParameter list)
        (vector : float[])
        (target : FitTarget) : float =
        let system = DesignParameters.applyVector parameters vector baseSystem
        let solution = OpticalSystemSolver(target.samplePoint, system).solution
        match target.quantity with
        | Photometric f -> solution.func f |> Option.defaultValue 0.0
        | Ellipsometric e -> e.evaluate solution

    /// Per-target residual entry: `weight × (model − desired) / max(tolerance, ε)`
    /// for `Equality`, a one-sided hinge (zero when satisfied) for `Inequality`.
    let targetResidual (model : float) (target : FitTarget) : float =
        let denom = max target.tolerance epsilon
        match target.kind with
        | Equality -> target.weight * (model - target.desiredValue) / denom
        | Inequality (LessOrEqual bound) -> target.weight * (max 0.0 (model - bound)) / denom
        | Inequality (GreaterOrEqual bound) -> target.weight * (max 0.0 (bound - model)) / denom

    /// Assemble the weighted-LSQ residual closure over the design vector — one
    /// entry per `FitTarget`, in target order. This IS the G.1 `Residual`
    /// (`float[] -> float[]`); it is handed unchanged to the §G.5 entry point.
    let buildResidual
        (baseSystem : OpticalSystem)
        (parameters : DesignParameter list)
        (targets : FitTarget list) : Residual =
        fun (vector : float[]) ->
            targets
            |> List.map (fun t -> targetResidual (modelValue baseSystem parameters vector t) t)
            |> Array.ofList
