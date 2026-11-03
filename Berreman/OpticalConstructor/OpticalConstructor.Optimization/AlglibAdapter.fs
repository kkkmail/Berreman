namespace OpticalConstructor.Optimization

open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.OptimizationInterface

/// §G.2 — the single ALGLIB adapter behind the G.1 interface (spec 0022 Part G §G.2).
///
/// This is the ONLY file in the solution permitted to reference ALGLIB. The
/// `alglib.net` (GPL free edition, §A.8) `PackageReference` is on
/// `OpticalConstructor.Optimization.fsproj`. Every method maps to the ALGLIB
/// routine named in §A.8 / 010 §III.4; `NelderMead`'s simplex backend is the
/// carried-forward deferred decision and is surfaced as
/// `SimplexBackendUnresolved` rather than dispatched to a guessed routine. No
/// ALGLIB type or exception crosses the G.1 boundary: the whole body runs under
/// `try/with` and returns `Result.Error` on any throw.
///
/// Linear algebra inside the LM step is ALGLIB's own — no matrix op is routed
/// through `MathNetNumericsMath.fs`, and ALGLIB is NOT used for engine linear
/// algebra (§A.8). Extreme Optimization (`ExtremeNumericsMath.fs`) is not
/// referenced here.
module AlglibAdapter =

    // The scalar objective for the scalar minimizers is the shared `ResidualVector.sumOfSquares`
    // (Σ residualᵢ²) from `OptimizationInterface` (reuse-critic F1).

    type CompletionCode with

        /// ALGLIB's reading of its completion code. Positive codes are successful stops; code 5 is the
        /// iteration cap, code 7 is "further improvement impossible" (a too-small step); negative codes are
        /// failures.
        member this.terminationReason : TerminationReason =
            match this with
            | CompletionCode 5 -> MaxIterationsReached
            | CompletionCode 7 -> StepTooSmall
            | CompletionCode c when c > 0 -> Converged
            | code -> BackendFailed code

    let private resultOf
        (request : OptimizationRequest)
        (solution : DesignVector)
        (iterations : IterationCount)
        (code : CompletionCode)
        : OptimizationResult =
        {
            solution = solution
            finalResiduals = request.residual solution
            iterations = iterations
            terminationReason = code.terminationReason
        }

    // The ALGLIB entry points speak ALGLIB's own arrays: each unpacks the request's carriers once, and
    // wraps an array ALGLIB hands a callback as the `DesignVector` the residual takes.
    type OptimizationRequest with

        // ------------------------------------------------------ LevenbergMarquardt → minlm

        /// `LevenbergMarquardt → minlm`. `InequalityTarget`s become one extra hinge
        /// penalty row each (a one-sided penalty on the achieved residual L2 norm);
        /// box bounds go through `minlmsetbc`. An analytic Jacobian (when supplied
        /// and no penalty rows are present) selects `minlmcreatevj`; otherwise
        /// `minlmcreatev` with numerical differentiation.
        member private this.runLm () : OptimizationResult =
            let (DesignVector initial) = this.initial
            let (DesignVector lower) = this.bounds.lower
            let (DesignVector upper) = this.bounds.upper
            let (IterationCount maxIterations) = this.maxIterations
            let (StepTolerance epsX) = this.epsX
            let x0 = Array.copy initial
            let n = x0.Length
            let (ResidualVector r0) = this.residual (DesignVector x0)
            let m0 = r0.Length
            let targets = Array.ofList this.inequalityTargets
            let nIneq = targets.Length
            let m = m0 + nIneq

            let fillResiduals (arg : float[]) (fi : float[]) =
                let residuals = this.residual (DesignVector arg)
                let (ResidualVector r) = residuals
                for i in 0 .. m0 - 1 do fi.[i] <- r.[i]
                if nIneq > 0 then
                    let norm = sqrt residuals.sumOfSquares
                    for k in 0 .. nIneq - 1 do
                        fi.[m0 + k] <-
                            match targets.[k] with
                            | LessOrEqual (TargetValue b) -> max 0.0 (norm - b)
                            | GreaterOrEqual (TargetValue b) -> max 0.0 (b - norm)

            let useAnalyticJac =
                match this.jacobian with
                | Some _ -> nIneq = 0
                | None -> false

            let mutable state = Unchecked.defaultof<alglib.minlmstate>
            if useAnalyticJac then alglib.minlmcreatevj(n, m, x0, &state)
            else alglib.minlmcreatev(n, m, x0, 1.0e-6, &state)

            alglib.minlmsetcond(state, epsX, maxIterations)
            alglib.minlmsetbc(state, lower, upper)

            let fvec = alglib.ndimensional_fvec(fun (arg : float[]) (fi : float[]) (_ : obj) -> fillResiduals arg fi)

            match this.jacobian with
            | Some j when useAnalyticJac ->
                let jac =
                    alglib.ndimensional_jac(fun (arg : float[]) (fi : float[]) (jacM : float[,]) (_ : obj) ->
                        fillResiduals arg fi
                        let (ResidualJacobian rows) = j (DesignVector arg)
                        for i in 0 .. m0 - 1 do
                            for p in 0 .. n - 1 do
                                jacM.[i, p] <- rows.[i].[p])
                alglib.minlmoptimize(state, fvec, jac, (null : alglib.ndimensional_rep), null)
            | _ ->
                alglib.minlmoptimize(state, fvec, (null : alglib.ndimensional_rep), null)

            let mutable xr = Array.empty<float>
            let mutable rep = Unchecked.defaultof<alglib.minlmreport>
            alglib.minlmresults(state, &xr, &rep)
            resultOf this (DesignVector xr) (IterationCount rep.iterationscount) (CompletionCode rep.terminationtype)

        // ------------------------------------------------------ numerical gradient of the SSR objective

        /// Forward-difference gradient of `f(x) = ||residual(x)||²` into ALGLIB's `g`, for the scalar
        /// minimizers (minlbfgs / minbleic) that consume an objective + gradient; returns `f(x)`.
        member private this.ssrGradient (arg : float[]) (g : float[]) : float =
            let f0 = (this.residual (DesignVector arg)).sumOfSquares
            let h = 1.0e-7
            for p in 0 .. arg.Length - 1 do
                let xp = Array.copy arg
                xp.[p] <- xp.[p] + h
                g.[p] <- ((this.residual (DesignVector xp)).sumOfSquares - f0) / h
            f0

        // ------------------------------------------------------ QuasiNewtonLbfgs → minlbfgs

        /// `QuasiNewtonLbfgs → minlbfgs`. minlbfgs is unconstrained (no box-bound
        /// setter), so `bounds` are not applied here; constrained fits should use
        /// `BoundedLinearConstrained` / `NonlinearConstrained`.
        member private this.runLbfgs () : OptimizationResult =
            let (DesignVector initial) = this.initial
            let (IterationCount maxIterations) = this.maxIterations
            let (StepTolerance epsX) = this.epsX
            let x0 = Array.copy initial
            let n = x0.Length
            let corrections = max 1 (min n 10)
            let mutable state = Unchecked.defaultof<alglib.minlbfgsstate>
            alglib.minlbfgscreate(n, corrections, x0, &state)
            alglib.minlbfgssetcond(state, 0.0, 0.0, epsX, maxIterations)
            let grad =
                alglib.ndimensional_grad(fun (arg : float[]) (f : byref<float>) (g : float[]) (_ : obj) ->
                    f <- this.ssrGradient arg g)
            alglib.minlbfgsoptimize(state, grad, (null : alglib.ndimensional_rep), null)
            let mutable xr = Array.empty<float>
            let mutable rep = Unchecked.defaultof<alglib.minlbfgsreport>
            alglib.minlbfgsresults(state, &xr, &rep)
            resultOf this (DesignVector xr) (IterationCount rep.iterationscount) (CompletionCode rep.terminationtype)

        // ------------------------------------------------------ BoundedLinearConstrained → minbleic

        /// `BoundedLinearConstrained → minbleic`. Box bounds via `minbleicsetbc`.
        member private this.runBleic () : OptimizationResult =
            let (DesignVector initial) = this.initial
            let (DesignVector lower) = this.bounds.lower
            let (DesignVector upper) = this.bounds.upper
            let (IterationCount maxIterations) = this.maxIterations
            let (StepTolerance epsX) = this.epsX
            let x0 = Array.copy initial
            let n = x0.Length
            let mutable state = Unchecked.defaultof<alglib.minbleicstate>
            alglib.minbleiccreate(n, x0, &state)
            alglib.minbleicsetbc(state, lower, upper)
            alglib.minbleicsetcond(state, 0.0, 0.0, epsX, maxIterations)
            let grad =
                alglib.ndimensional_grad(fun (arg : float[]) (f : byref<float>) (g : float[]) (_ : obj) ->
                    f <- this.ssrGradient arg g)
            alglib.minbleicoptimize(state, grad, (null : alglib.ndimensional_rep), null)
            let mutable xr = Array.empty<float>
            let mutable rep = Unchecked.defaultof<alglib.minbleicreport>
            alglib.minbleicresults(state, &xr, &rep)
            resultOf this (DesignVector xr) (IterationCount rep.iterationscount) (CompletionCode rep.terminationtype)

        // ------------------------------------------------------ NonlinearConstrained → minnlc

        /// `NonlinearConstrained → minnlc`. The objective is the SSR; each
        /// `InequalityTarget` becomes a nonlinear inequality constraint c(x) ≤ 0 on
        /// the residual L2 norm. Box bounds via `minnlcsetbc`; SLP algorithm. The
        /// `ndimensional_jac` callback fills the objective+constraint values and
        /// their forward-difference gradients.
        member private this.runNlc () : OptimizationResult =
            let (DesignVector initial) = this.initial
            let (DesignVector lower) = this.bounds.lower
            let (DesignVector upper) = this.bounds.upper
            let (IterationCount maxIterations) = this.maxIterations
            let (StepTolerance epsX) = this.epsX
            let x0 = Array.copy initial
            let n = x0.Length
            let targets = Array.ofList this.inequalityTargets
            let nlic = targets.Length
            let nfun = 1 + nlic
            let norm (arg : float[]) = sqrt (this.residual (DesignVector arg)).sumOfSquares

            let evalFi (arg : float[]) (fi : float[]) =
                fi.[0] <- (this.residual (DesignVector arg)).sumOfSquares
                let g = norm arg
                for k in 0 .. nlic - 1 do
                    fi.[1 + k] <-
                        match targets.[k] with
                        | LessOrEqual (TargetValue b) -> g - b
                        | GreaterOrEqual (TargetValue b) -> b - g

            let mutable state = Unchecked.defaultof<alglib.minnlcstate>
            alglib.minnlccreate(n, x0, &state)
            alglib.minnlcsetbc(state, lower, upper)
            alglib.minnlcsetnlc(state, 0, nlic)
            alglib.minnlcsetcond(state, epsX, maxIterations)
            alglib.minnlcsetalgoslp(state)

            let h = 1.0e-7
            let jac =
                alglib.ndimensional_jac(fun (arg : float[]) (fi : float[]) (jacM : float[,]) (_ : obj) ->
                    evalFi arg fi
                    let baseFi = Array.copy fi
                    for p in 0 .. n - 1 do
                        let xp = Array.copy arg
                        xp.[p] <- xp.[p] + h
                        let fp = Array.zeroCreate nfun
                        evalFi xp fp
                        for r in 0 .. nfun - 1 do
                            jacM.[r, p] <- (fp.[r] - baseFi.[r]) / h)
            alglib.minnlcoptimize(state, jac, (null : alglib.ndimensional_rep), null)
            let mutable xr = Array.empty<float>
            let mutable rep = Unchecked.defaultof<alglib.minnlcreport>
            alglib.minnlcresults(state, &xr, &rep)
            resultOf this (DesignVector xr) (IterationCount rep.iterationscount) (CompletionCode rep.terminationtype)

        // ------------------------------------------------------ entry point

        /// The single G.1 entry point. Dispatches to the ALGLIB routine for the
        /// chosen method; `NelderMead` is the deferred simplex decision and is
        /// surfaced as `SimplexBackendUnresolved`. Any exception (ALGLIB or
        /// otherwise) is trapped and returned as `OptimizationRunExn`, so no ALGLIB
        /// type or exception crosses this boundary.
        member this.optimize () : Result<OptimizationResult, OptimizationError> =
            try
                match this.method with
                | NelderMead ->
                    Ok
                        {
                            solution = this.initial
                            finalResiduals = this.residual this.initial
                            iterations = IterationCount 0
                            terminationReason = SimplexBackendUnresolved
                        }
                | LevenbergMarquardt -> this.runLm () |> Ok
                | QuasiNewtonLbfgs -> this.runLbfgs () |> Ok
                | BoundedLinearConstrained -> this.runBleic () |> Ok
                | NonlinearConstrained -> this.runNlc () |> Ok
            with
            | e -> Error (OptimizationRunExn e.Message)
