namespace OpticalConstructor.Optimization

open Berreman.MathNetNumericsMath
open Softellect.Sys.Primitives
open Softellect.Sys.Core
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
/// `Softellect.Sys.Core.serialize`/`deserialize` with `BinaryZippedFormat`
/// (`Core.fs:257`); per §0 constraint 4 this derived artefact MUST NOT be carried
/// inside the canonical JSON project (it is referenced as a sidecar from it).
module FitQuality =

    /// Aggregated fit-quality report (§G.8 / AC-G5). All plain arrays so the G.10
    /// UI and `plotComparison` can consume them without a charting dependency here.
    type FitReport =
        {
            /// χ² = Σ residualᵢ² over the per-sample residuals.
            chiSquared : float
            /// Mean squared error = χ² / sampleCount.
            mse : float
            /// Reduced χ² = χ² / degreesOfFreedom (the covariance scale factor).
            reducedChiSquared : float
            /// sampleCount − parameterCount (floored at 1 to keep the scale finite).
            degreesOfFreedom : int
            /// The converged parameter vector (canonical SI units, §0 constraint 3).
            parameters : float[]
            /// √(diag Cov) — the per-parameter standard errors.
            standardErrors : float[]
            /// 95% confidence interval (lower, upper) per parameter.
            confidenceIntervals : (float * float)[]
            /// The parameter covariance matrix Cov = reducedχ² · (JᵀJ)⁻¹.
            covariance : float[][]
            /// The parameter correlation matrix Corrᵢⱼ = Covᵢⱼ / (σᵢ σⱼ).
            correlation : float[][]
            /// The per-sample residuals at the solution (for residual plots / overlay).
            residuals : float[]
        }

    /// Two-sided normal multiplier for a ~95% interval. A normal approximation
    /// (rather than a Student-t table) is the minimum implementation §0 #6 admits;
    /// the covariance scale already carries the reduced-χ² correction.
    let private z95 = 1.959963984540054

    /// `sumSq` (Σ residualᵢ²) is the shared least-squares definition from
    /// `OptimizationInterface` (reuse-critic F1) — `reportFrom`'s χ² calls it.

    /// √(machine epsilon) for `float` — the canonical relative step for a
    /// forward-difference derivative (the truncation/round-off optimum balances at
    /// ≈√eps · scale). `2.22e-16` is the IEEE-754 double epsilon.
    let private sqrtMachineEps = sqrt 2.220446049250313e-16

    /// Floor used ONLY to keep the step nonzero for an exactly-zero parameter — a
    /// sub-picometre value, 5+ orders below any canonical meter-scale thickness
    /// (~1e-7 m), so it never perturbs the step for a real fit. It is NOT a
    /// meter-scale `1.0` floor (the old `max 1.0 |x|` forced a ~40 % perturbation of
    /// canonical thicknesses — the defect this round fixes).
    let private stepFloor = 1.0e-12

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
    let residualJacobian (residual : Residual) (x : float[]) : float[][] =
        let n = x.Length
        let f0 = residual x
        let m = f0.Length
        // Per-parameter forward-difference residual columns (one residual eval each).
        let cols =
            [| for p in 0 .. n - 1 ->
                let h = sqrtMachineEps * (max (abs x.[p]) stepFloor)
                let xp = Array.copy x
                xp.[p] <- xp.[p] + h
                let fp = residual xp
                [| for i in 0 .. m - 1 -> (fp.[i] - f0.[i]) / h |] |]
        // Assemble rows (residual entry i, parameter p).
        [| for i in 0 .. m - 1 -> [| for p in 0 .. n - 1 -> cols.[p].[i] |] |]

    /// Cov = reducedχ² · (JᵀJ)⁻¹, with the multiply and inverse routed through the
    /// `MathNetNumericsMath` `RealMatrix` seam (NOT ALGLIB, §A.8).
    let private covarianceMatrix (jac : float[][]) (reducedChiSquared : float) : float[][] =
        let m = jac.Length
        let n = if m = 0 then 0 else jac.[0].Length
        if n = 0 then [||]
        else
            let jM = RealMatrix.create jac
            let jtRows = [| for p in 0 .. n - 1 -> [| for i in 0 .. m - 1 -> jac.[i].[p] |] |]
            let jtM = RealMatrix.create jtRows
            let inv = (jtM * jM).inverse
            [| for i in 0 .. n - 1 -> [| for j in 0 .. n - 1 -> reducedChiSquared * inv.[i, j] |] |]

    /// Build a `FitReport` from the residual closure, the solution vector, and the
    /// `OptimizationResult.finalResiduals` (R-1: "from `finalResiduals` and the
    /// residual Jacobian at the solution"). The Jacobian is taken at `solution`.
    let reportFrom (residual : Residual) (solution : float[]) (finalResiduals : float[]) : FitReport =
        let m = finalResiduals.Length
        let n = solution.Length
        let chi = sumSq finalResiduals
        let dof = max 1 (m - n)
        let reduced = chi / float dof
        let cov = covarianceMatrix (residualJacobian residual solution) reduced
        let stdErr =
            [| for i in 0 .. n - 1 ->
                let v = if i < cov.Length then cov.[i].[i] else 0.0
                if v > 0.0 then sqrt v else 0.0 |]
        let ci = [| for i in 0 .. n - 1 -> (solution.[i] - z95 * stdErr.[i], solution.[i] + z95 * stdErr.[i]) |]
        let corr =
            [| for i in 0 .. n - 1 ->
                [| for j in 0 .. n - 1 ->
                    let d = stdErr.[i] * stdErr.[j]
                    if d > 0.0 then cov.[i].[j] / d
                    elif i = j then 1.0
                    else 0.0 |] |]
        {
            chiSquared = chi
            mse = chi / float (max 1 m)
            reducedChiSquared = reduced
            degreesOfFreedom = dof
            parameters = Array.copy solution
            standardErrors = stdErr
            confidenceIntervals = ci
            covariance = cov
            correlation = corr
            residuals = Array.copy finalResiduals
        }

    /// Build a `FitReport` directly from a §G.1 `OptimizationResult`, reusing its
    /// `finalResiduals` and `solution` (the adapter sets `finalResiduals =
    /// residual solution`, so the report is consistent with the optimizer's stop).
    let fromResult (residual : Residual) (result : OptimizationResult) : FitReport =
        reportFrom residual result.solution result.finalResiduals

    /// Serialize a `FitReport` to the zipped-binary `.binz` payload (§0 constraint
    /// 4; `serialize … BinaryZippedFormat`, `Core.fs:257`). The bytes are written
    /// to a fit-history sidecar referenced from the canonical JSON, NEVER embedded
    /// in the canonical JSON project.
    let toBinz (report : FitReport) : byte[] = serialize BinaryZippedFormat report

    /// Inverse of `toBinz` — read a `FitReport` back from a `.binz` sidecar payload.
    let fromBinz (bytes : byte[]) : FitReport = deserialize BinaryZippedFormat bytes
