namespace OpticalConstructor.Optimization

open OpticalConstructor.Domain.MuellerInverse

/// §G.1 — the library-agnostic optimization interface (spec 0022 Part G §G.1).
///
/// Expressed entirely in our own F# records and discriminated unions (§0
/// constraint 1); NO ALGLIB type appears in this surface, so the physics/fitting
/// code compiles without the library (§A.8: "the physics/fitting code never
/// depends on ALGLIB directly"). The single entry point `OptimizationRequest.optimize`
/// is realised in `AlglibAdapter` (the only file permitted to reference ALGLIB);
/// §0 constraint 6 forbids an extra indirection layer here just to declare it.
///
/// No caching, retry, or progress-callback registry lives here — progress
/// reporting is wired separately in G.10 (slice 011).
module OptimizationInterface =

    /// One number per design parameter, in the parameters' order and canonical units (§0 constraint 3):
    /// a start point, a solution, a bound, a step or a per-parameter spread.
    [<Softellect.Sys.NumericCarrier("the optimizer's parameter vector (R3): handed whole to Alglib as its start point, bounds and solution, and read on every residual evaluation")>]
    type DesignVector =
        | DesignVector of float[]

        member this.value = let (DesignVector v) = this in v

        /// The vector with no parameters — the design of a concrete system.
        static member empty : DesignVector = DesignVector [||]

        /// A copy that the optimizer may overwrite without touching this vector.
        member this.copy : DesignVector = let (DesignVector v) = this in DesignVector (Array.copy v)

    /// The vectorized residual: one entry per target sample (§A.8).
    type Residual = DesignVector -> ResidualVector

    type ResidualVector with

        /// Sum of squared residuals — the single least-squares definition Σ residualᵢ² shared by the §G.2
        /// adapter's scalar objective, the §G.9 synthesis merit, and the §G.8 fit-quality χ² (reuse-critic F1).
        member this.sumOfSquares : float = let (ResidualVector r) = this in Array.fold (fun acc v -> acc + v * v) 0.0 r

    /// Optional analytic Jacobian (rows = residual entries, cols = parameters).
    /// `None` requests numerical differentiation from the adapter.
    type Jacobian = (DesignVector -> ResidualJacobian) option

    /// Per-parameter lower and upper vectors: the box a fit is held to (an unbounded side is `infinity` /
    /// `-infinity`), or the confidence interval a fit reports (§G.8).
    type ParameterBounds =
        {
            lower : DesignVector
            upper : DesignVector
        }

    /// One value of a fit target's quantity — a desired value, an inequality bound, or the model's
    /// evaluation (R and T as fractions, Ψ and Δ in radians).
    type TargetValue =
        | TargetValue of float

        member this.value = let (TargetValue v) = this in v

    /// One-sided target from 010 §II.5 (e.g. "R ≤ 0.5%"), carrying the bound.
    type InequalityTarget =
        | LessOrEqual of TargetValue
        | GreaterOrEqual of TargetValue

    /// Method selector. Each case maps onto the ALGLIB routine named in
    /// §A.8 / 010 §III.4 (resolved in `AlglibAdapter`): `LevenbergMarquardt → minlm`,
    /// `QuasiNewtonLbfgs → minlbfgs`, `BoundedLinearConstrained → minbleic`,
    /// `NonlinearConstrained → minnlc`. `NelderMead` is the 010-required simplex
    /// method whose concrete backend 010 leaves unnamed; its routine binding is
    /// deferred per G.2 and MUST NOT be resolved to a spec-invented routine.
    type OptimizationMethod =
        | LevenbergMarquardt
        | QuasiNewtonLbfgs
        | NelderMead
        | BoundedLinearConstrained
        | NonlinearConstrained

    /// The single call payload.
    type OptimizationRequest =
        {
            residual : Residual
            jacobian : Jacobian
            initial : DesignVector
            bounds : ParameterBounds
            inequalityTargets : InequalityTarget list
            method : OptimizationMethod
            maxIterations : IterationCount
            epsX : StepTolerance
        }

    /// The completion code an optimizer backend reported (ALGLIB's `terminationtype`).
    type CompletionCode =
        | CompletionCode of int

        member this.value = let (CompletionCode c) = this in c

    /// Why the optimizer stopped.
    type TerminationReason =
        | Converged
        | MaxIterationsReached
        | StepTooSmall
        /// The backend stopped on a failure code.
        | BackendFailed of CompletionCode
        /// `NelderMead`'s simplex backend is the deferred G.2 decision.
        | SimplexBackendUnresolved
        /// A §G.9 loop observed its cancellation flag (AC-G7).
        | SearchCancelled

    /// Whether a run's solution can be used.
    type OptimizationOutcome =
        | OptimizationSucceeded
        | OptimizationFailed

    /// The optimization outcome.
    type OptimizationResult =
        {
            solution : DesignVector
            finalResiduals : ResidualVector
            iterations : IterationCount
            terminationReason : TerminationReason
        }

        /// A run succeeded unless it stopped on a failure, an unresolved backend or a cancellation.
        member this.outcome : OptimizationOutcome =
            match this.terminationReason with
            | Converged | MaxIterationsReached | StepTooSmall -> OptimizationSucceeded
            | BackendFailed _ | SimplexBackendUnresolved | SearchCancelled -> OptimizationFailed

    /// A typed failure of an optimization run — never a throw across the G.1 boundary.
    type OptimizationError =
        /// The backend (ALGLIB or the caller's residual) threw; the exception's message.
        | OptimizationRunExn of reason : string
