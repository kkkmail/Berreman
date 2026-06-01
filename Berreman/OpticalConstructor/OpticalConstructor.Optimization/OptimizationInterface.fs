namespace OpticalConstructor.Optimization

/// §G.1 — the library-agnostic optimization interface (spec 0022 Part G §G.1).
///
/// Expressed entirely in our own F# records and discriminated unions (§0
/// constraint 1); NO ALGLIB type appears in this surface, so the physics/fitting
/// code compiles without the library (§A.8: "the physics/fitting code never
/// depends on ALGLIB directly"). The single `optimize` entry point with the
/// signature `OptimizationRequest -> Result<OptimizationResult, string>` is
/// realised in `AlglibAdapter` (the only file permitted to reference ALGLIB);
/// §0 constraint 6 forbids an extra indirection layer here just to declare it.
///
/// No caching, retry, or progress-callback registry lives here — progress
/// reporting is wired separately in G.10 (slice 011).
module OptimizationInterface =

    /// The vectorized residual: one entry per target sample (§A.8 `float[] -> float[]`).
    type Residual = float[] -> float[]

    /// Optional analytic Jacobian (rows = residual entries, cols = parameters).
    /// `None` requests numerical differentiation from the adapter.
    type Jacobian = (float[] -> float[][]) option

    /// Per-parameter box bounds in canonical SI units (§0 constraint 3). An
    /// unbounded side is encoded as `infinity` / `-infinity`.
    type ParameterBounds =
        {
            lower : float[]
            upper : float[]
        }

    /// One-sided target from 010 §II.5 (e.g. "R ≤ 0.5%"), carrying the bound.
    type InequalityTarget =
        | LessOrEqual of float
        | GreaterOrEqual of float

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
            initial : float[]
            bounds : ParameterBounds
            inequalityTargets : InequalityTarget list
            method : OptimizationMethod
            maxIterations : int
            epsX : float
        }

    /// Why the optimizer stopped.
    type TerminationReason =
        | Converged
        | MaxIterationsReached
        | StepTooSmall
        | Failed of string

    /// The optimization outcome.
    type OptimizationResult =
        {
            solution : float[]
            finalResiduals : float[]
            iterations : int
            terminationReason : TerminationReason
            success : bool
        }
