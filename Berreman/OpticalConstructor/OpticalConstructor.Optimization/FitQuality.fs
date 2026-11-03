namespace OpticalConstructor.Optimization

open Berreman.MathNetNumericsMath
open Softellect.Sys.Primitives
open Softellect.Sys.Core
open Softellect.Sys.Errors
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.OptimizationInterface

/// §G.8 — fit-quality reporting (spec 0022 Part G §G.8, [Standard]).
///
/// From the §G.1 `OptimizationResult.finalResiduals` and the residual Jacobian
/// at the solution, this module computes χ²/MSE, parameter confidence intervals,
/// and the parameter correlation matrix, and retains the per-sample residuals
/// for the §G.10 residual plots / fit-vs-measured overlay (`plotComparison`,
/// `Charting.fs:38`). It returns PLAIN ARRAYS only — it does NOT render charts
/// (that is the G.10 UI / `Charting` path).
///
/// The confidence intervals and correlation matrix are derived from the
/// covariance estimate Cov = reducedχ² · (JᵀJ)⁻¹. The real-matrix multiply and
/// inverse go through the Part A math seam `MathNetNumericsMath.fs:5`
/// (`RealMatrix.create`/`*`/`.inverse`) — this is engine-side linear algebra, so
/// ALGLIB is NOT used here (§A.8). No second matrix library is introduced.
///
/// The aggregated `FitReport` is persistable as a `.binz` fit-history sidecar via
/// `Softellect.Sys.Core.serialize`/`tryDeserialize` with `BinaryZippedFormat`
/// (`Core.fs:257`); per §0 constraint 4 this derived artefact MUST NOT be carried
/// inside the canonical JSON project (it is referenced as a sidecar from it).
module FitQuality =

    /// χ² = Σ residualᵢ² over the per-sample residuals.
    type ChiSquared =
        | ChiSquared of float

        member this.value = let (ChiSquared c) = this in c

    /// Mean squared residual (the MSE) = χ² / sampleCount.
    type MeanSquaredResidual =
        | MeanSquaredResidual of float

        member this.value = let (MeanSquaredResidual e) = this in e

    /// Reduced χ² = χ² / degreesOfFreedom (the covariance scale factor).
    type ReducedChiSquared =
        | ReducedChiSquared of float

        member this.value = let (ReducedChiSquared c) = this in c

    /// sampleCount − parameterCount (floored at 1 to keep the scale finite).
    type DegreesOfFreedom =
        | DegreesOfFreedom of int

        member this.value = let (DegreesOfFreedom d) = this in d

    /// An n × n matrix over the design parameters, row and column in the design vector's order.
    [<Softellect.Sys.NumericCarrier("an n x n matrix over a fit's parameters (R3): the covariance reducedChi2 * (JtJ)^-1 the MathNet inverse yields, and the correlation derived from it whole")>]
    type ParameterMatrix =
        | ParameterMatrix of float[][]

        member this.value = let (ParameterMatrix m) = this in m

    /// Aggregated fit-quality report (§G.8 / AC-G5). Plain carriers so the G.10
    /// UI and `plotComparison` can consume them without a charting dependency here.
    type FitReport =
        {
            chiSquared : ChiSquared
            mse : MeanSquaredResidual
            reducedChiSquared : ReducedChiSquared
            degreesOfFreedom : DegreesOfFreedom
            /// The converged parameter vector (canonical SI units, §0 constraint 3).
            parameters : DesignVector
            /// √(diag Cov) — the per-parameter standard errors.
            standardErrors : DesignVector
            /// 95% confidence interval (lower, upper) per parameter.
            confidenceIntervals : ParameterBounds
            /// The parameter covariance matrix Cov = reducedχ² · (JᵀJ)⁻¹.
            covariance : ParameterMatrix
            /// The parameter correlation matrix Corrᵢⱼ = Covᵢⱼ / (σᵢ σⱼ).
            correlation : ParameterMatrix
            /// The per-sample residuals at the solution (for residual plots / overlay).
            residuals : ResidualVector
        }

        /// The zipped-binary `.binz` payload (§0 constraint 4; `serialize … BinaryZippedFormat`,
        /// `Core.fs:257`). The bytes are written to a fit-history sidecar referenced from the
        /// canonical JSON, NEVER embedded in the canonical JSON project.
        member this.toBinz : SerializedData = serialize BinaryZippedFormat this

        /// Read a `FitReport` back from a `.binz` sidecar payload; a payload of another shape is an `Error`.
        static member tryFromBinz (data : SerializedData) : Result<FitReport, SerializationError> =
            tryDeserialize BinaryZippedFormat data

    /// Forward-difference residual Jacobian at `x`: rows = residual entries,
    /// columns = parameters. `None` analytic Jacobian is the common case for the
    /// fit-quality estimate, mirroring the adapter's numerical-differentiation path.
    ///
    /// The per-parameter step is scaled to that parameter's OWN magnitude —
    /// `h_p = √eps · max(|x_p|, stepFloor)` — so a canonical meter-scale thickness
    /// (|x| ~ 1e-7 m) is perturbed by ~1e-15 m (a part in ~1e8), not the ~1e-7 m
    /// (~40 %) the old absolute `max 1.0 |x|` floor forced. Without this the
    /// covariance / CI / correlation derived from JᵀJ are numerically meaningless at
    /// the project's canonical scale (§0 #3).
    let residualJacobian (residual : Residual) (point : DesignVector) : ResidualJacobian =
        // √(machine epsilon) for `float`, the canonical relative forward-difference step; `2.22e-16` is the
        // IEEE-754 double epsilon.
        let sqrtMachineEps = sqrt 2.220446049250313e-16
        // Keeps the step nonzero for an exactly-zero parameter only: a sub-picometre value, 5+ orders below
        // any canonical meter-scale thickness, so it never perturbs the step of a real fit.
        let stepFloor = 1.0e-12
        let (DesignVector x) = point
        let n = x.Length
        let (ResidualVector f0) = residual point
        let m = f0.Length
        // Per-parameter forward-difference residual columns (one residual eval each).
        let cols =
            [| for p in 0 .. n - 1 ->
                let h = sqrtMachineEps * (max (abs x.[p]) stepFloor)
                let xp = Array.copy x
                xp.[p] <- xp.[p] + h
                let (ResidualVector fp) = residual (DesignVector xp)
                [| for i in 0 .. m - 1 -> (fp.[i] - f0.[i]) / h |] |]
        // Assemble rows (residual entry i, parameter p).
        [| for i in 0 .. m - 1 -> [| for p in 0 .. n - 1 -> cols.[p].[i] |] |] |> ResidualJacobian

    /// Cov = reducedχ² · (JᵀJ)⁻¹, with the multiply and inverse routed through the
    /// `MathNetNumericsMath` `RealMatrix` seam (NOT ALGLIB, §A.8).
    let private covarianceMatrix (jacobian : ResidualJacobian) (reduced : ReducedChiSquared) : ParameterMatrix =
        let (ResidualJacobian jac) = jacobian
        let (ReducedChiSquared reducedChiSquared) = reduced
        let m = jac.Length
        let n = if m = 0 then 0 else jac.[0].Length
        if n = 0 then ParameterMatrix [||]
        else
            let jM = RealMatrix.create jac
            let jtRows = [| for p in 0 .. n - 1 -> [| for i in 0 .. m - 1 -> jac.[i].[p] |] |]
            let jtM = RealMatrix.create jtRows
            let inv = (jtM * jM).inverse
            [| for i in 0 .. n - 1 -> [| for j in 0 .. n - 1 -> reducedChiSquared * inv.[i, j] |] |] |> ParameterMatrix

    /// Build a `FitReport` from the residual closure, the solution vector, and the
    /// `OptimizationResult.finalResiduals` (R-1: "from `finalResiduals` and the
    /// residual Jacobian at the solution"). The Jacobian is taken at `solution`.
    let reportFrom (residual : Residual) (solution : DesignVector) (finalResiduals : ResidualVector) : FitReport =
        // Two-sided normal multiplier for a ~95% interval: a normal approximation (rather than a Student-t
        // table) is the minimum §0 #6 admits; the covariance scale already carries the reduced-χ² correction.
        let z95 = 1.959963984540054
        let (DesignVector x) = solution
        let (ResidualVector r) = finalResiduals
        let m = r.Length
        let n = x.Length
        let chi = finalResiduals.sumOfSquares
        let dof = max 1 (m - n)
        let reduced = chi / float dof
        let covariance = covarianceMatrix (residualJacobian residual solution) (ReducedChiSquared reduced)
        let (ParameterMatrix cov) = covariance
        let stdErr =
            [| for i in 0 .. n - 1 ->
                let v = if i < cov.Length then cov.[i].[i] else 0.0
                if v > 0.0 then sqrt v else 0.0 |]
        let corr =
            [| for i in 0 .. n - 1 ->
                [| for j in 0 .. n - 1 ->
                    let d = stdErr.[i] * stdErr.[j]
                    if d > 0.0 then cov.[i].[j] / d
                    elif i = j then 1.0
                    else 0.0 |] |]
        {
            chiSquared = ChiSquared chi
            mse = MeanSquaredResidual (chi / float (max 1 m))
            reducedChiSquared = ReducedChiSquared reduced
            degreesOfFreedom = DegreesOfFreedom dof
            parameters = solution.copy
            standardErrors = DesignVector stdErr
            confidenceIntervals =
                {
                    lower = Array.init n (fun i -> x.[i] - z95 * stdErr.[i]) |> DesignVector
                    upper = Array.init n (fun i -> x.[i] + z95 * stdErr.[i]) |> DesignVector
                }
            covariance = covariance
            correlation = ParameterMatrix corr
            residuals = ResidualVector (Array.copy r)
        }

    /// Build a `FitReport` directly from a §G.1 `OptimizationResult`, reusing its
    /// `finalResiduals` and `solution` (the adapter sets `finalResiduals =
    /// residual solution`, so the report is consistent with the optimizer's stop).
    let fromResult (residual : Residual) (result : OptimizationResult) : FitReport =
        reportFrom residual result.solution result.finalResiduals
