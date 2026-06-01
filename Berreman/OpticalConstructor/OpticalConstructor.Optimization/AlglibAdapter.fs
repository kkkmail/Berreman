namespace OpticalConstructor.Optimization

open OpticalConstructor.Optimization.OptimizationInterface

/// §G.2 — the single ALGLIB adapter behind the G.1 interface (spec 0022 Part G §G.2).
///
/// This is the ONLY file in the solution permitted to reference ALGLIB. The
/// `alglib.net` (GPL free edition, §A.8) `PackageReference` is on
/// `OpticalConstructor.Optimization.fsproj`. Every method maps to the ALGLIB
/// routine named in §A.8 / 010 §III.4; `NelderMead`'s simplex backend is the
/// carried-forward deferred decision and is surfaced as
/// `Failed "simplex backend unresolved"` rather than dispatched to a guessed
/// routine. No ALGLIB type or exception crosses the G.1 boundary: the whole body
/// runs under `try/with` and returns `Result.Error` on any throw.
///
/// Linear algebra inside the LM step is ALGLIB's own — no matrix op is routed
/// through `MathNetNumericsMath.fs`, and ALGLIB is NOT used for engine linear
/// algebra (§A.8). Extreme Optimization (`ExtremeNumericsMath.fs`) is not
/// referenced here.
module AlglibAdapter =

    // The scalar objective for the scalar minimizers is the shared `sumSq`
    // (Σ residualᵢ²) from `OptimizationInterface` (reuse-critic F1).

    /// Map an ALGLIB completion code to our `TerminationReason`. Positive codes
    /// are successful stops; code 5 is the iteration cap, code 7 is "further
    /// improvement impossible" (a too-small step); negative codes are failures.
    let private terminationReason (code : int) : TerminationReason =
        match code with
        | 5 -> MaxIterationsReached
        | 7 -> StepTooSmall
        | c when c > 0 -> Converged
        | c -> Failed (sprintf "ALGLIB termination code %d" c)

    let private result (xr : float[]) (request : OptimizationRequest) (iterations : int) (code : int) : OptimizationResult =
        {
            solution = xr
            finalResiduals = request.residual xr
            iterations = iterations
            terminationReason = terminationReason code
            success = code > 0
        }

    // ---------------------------------------------------------- LevenbergMarquardt → minlm

    /// `LevenbergMarquardt → minlm`. `InequalityTarget`s become one extra hinge
    /// penalty row each (a one-sided penalty on the achieved residual L2 norm);
    /// box bounds go through `minlmsetbc`. An analytic Jacobian (when supplied
    /// and no penalty rows are present) selects `minlmcreatevj`; otherwise
    /// `minlmcreatev` with numerical differentiation.
    let private runLm (request : OptimizationRequest) : OptimizationResult =
        let x0 = Array.copy request.initial
        let n = x0.Length
        let m0 = (request.residual x0).Length
        let targets = Array.ofList request.inequalityTargets
        let nIneq = targets.Length
        let m = m0 + nIneq

        let fillResiduals (arg : float[]) (fi : float[]) =
            let r = request.residual arg
            for i in 0 .. m0 - 1 do fi.[i] <- r.[i]
            if nIneq > 0 then
                let norm = sqrt (sumSq r)
                for k in 0 .. nIneq - 1 do
                    fi.[m0 + k] <-
                        match targets.[k] with
                        | LessOrEqual b -> max 0.0 (norm - b)
                        | GreaterOrEqual b -> max 0.0 (b - norm)

        let useAnalyticJac =
            match request.jacobian with
            | Some _ -> nIneq = 0
            | None -> false

        let mutable state = Unchecked.defaultof<alglib.minlmstate>
        if useAnalyticJac then alglib.minlmcreatevj(n, m, x0, &state)
        else alglib.minlmcreatev(n, m, x0, 1.0e-6, &state)

        alglib.minlmsetcond(state, request.epsX, request.maxIterations)
        alglib.minlmsetbc(state, request.bounds.lower, request.bounds.upper)

        let fvec = alglib.ndimensional_fvec(fun (arg : float[]) (fi : float[]) (_ : obj) -> fillResiduals arg fi)

        match request.jacobian with
        | Some j when useAnalyticJac ->
            let jac =
                alglib.ndimensional_jac(fun (arg : float[]) (fi : float[]) (jacM : float[,]) (_ : obj) ->
                    fillResiduals arg fi
                    let J = j arg
                    for i in 0 .. m0 - 1 do
                        for p in 0 .. n - 1 do
                            jacM.[i, p] <- J.[i].[p])
            alglib.minlmoptimize(state, fvec, jac, (null : alglib.ndimensional_rep), null)
        | _ ->
            alglib.minlmoptimize(state, fvec, (null : alglib.ndimensional_rep), null)

        let mutable xr = Array.empty<float>
        let mutable rep = Unchecked.defaultof<alglib.minlmreport>
        alglib.minlmresults(state, &xr, &rep)
        result xr request rep.iterationscount rep.terminationtype

    // ---------------------------------------------------------- numerical gradient of the SSR objective

    /// Forward-difference gradient of `f(x) = ||residual(x)||²`, for the scalar
    /// minimizers (minlbfgs / minbleic) that consume an objective + gradient.
    let private ssrGrad (request : OptimizationRequest) (n : int) (arg : float[]) (g : float[]) : float =
        let f0 = sumSq (request.residual arg)
        let h = 1.0e-7
        for p in 0 .. n - 1 do
            let xp = Array.copy arg
            xp.[p] <- xp.[p] + h
            g.[p] <- (sumSq (request.residual xp) - f0) / h
        f0

    // ---------------------------------------------------------- QuasiNewtonLbfgs → minlbfgs

    /// `QuasiNewtonLbfgs → minlbfgs`. minlbfgs is unconstrained (no box-bound
    /// setter), so `bounds` are not applied here; constrained fits should use
    /// `BoundedLinearConstrained` / `NonlinearConstrained`.
    let private runLbfgs (request : OptimizationRequest) : OptimizationResult =
        let x0 = Array.copy request.initial
        let n = x0.Length
        let corrections = max 1 (min n 10)
        let mutable state = Unchecked.defaultof<alglib.minlbfgsstate>
        alglib.minlbfgscreate(n, corrections, x0, &state)
        alglib.minlbfgssetcond(state, 0.0, 0.0, request.epsX, request.maxIterations)
        let grad =
            alglib.ndimensional_grad(fun (arg : float[]) (f : byref<float>) (g : float[]) (_ : obj) ->
                f <- ssrGrad request n arg g)
        alglib.minlbfgsoptimize(state, grad, (null : alglib.ndimensional_rep), null)
        let mutable xr = Array.empty<float>
        let mutable rep = Unchecked.defaultof<alglib.minlbfgsreport>
        alglib.minlbfgsresults(state, &xr, &rep)
        result xr request rep.iterationscount rep.terminationtype

    // ---------------------------------------------------------- BoundedLinearConstrained → minbleic

    /// `BoundedLinearConstrained → minbleic`. Box bounds via `minbleicsetbc`.
    let private runBleic (request : OptimizationRequest) : OptimizationResult =
        let x0 = Array.copy request.initial
        let n = x0.Length
        let mutable state = Unchecked.defaultof<alglib.minbleicstate>
        alglib.minbleiccreate(n, x0, &state)
        alglib.minbleicsetbc(state, request.bounds.lower, request.bounds.upper)
        alglib.minbleicsetcond(state, 0.0, 0.0, request.epsX, request.maxIterations)
        let grad =
            alglib.ndimensional_grad(fun (arg : float[]) (f : byref<float>) (g : float[]) (_ : obj) ->
                f <- ssrGrad request n arg g)
        alglib.minbleicoptimize(state, grad, (null : alglib.ndimensional_rep), null)
        let mutable xr = Array.empty<float>
        let mutable rep = Unchecked.defaultof<alglib.minbleicreport>
        alglib.minbleicresults(state, &xr, &rep)
        result xr request rep.iterationscount rep.terminationtype

    // ---------------------------------------------------------- NonlinearConstrained → minnlc

    /// `NonlinearConstrained → minnlc`. The objective is the SSR; each
    /// `InequalityTarget` becomes a nonlinear inequality constraint c(x) ≤ 0 on
    /// the residual L2 norm. Box bounds via `minnlcsetbc`; SLP algorithm. The
    /// `ndimensional_jac` callback fills the objective+constraint values and
    /// their forward-difference gradients.
    let private runNlc (request : OptimizationRequest) : OptimizationResult =
        let x0 = Array.copy request.initial
        let n = x0.Length
        let targets = Array.ofList request.inequalityTargets
        let nlic = targets.Length
        let nfun = 1 + nlic
        let norm (arg : float[]) = sqrt (sumSq (request.residual arg))

        let evalFi (arg : float[]) (fi : float[]) =
            fi.[0] <- sumSq (request.residual arg)
            let g = norm arg
            for k in 0 .. nlic - 1 do
                fi.[1 + k] <-
                    match targets.[k] with
                    | LessOrEqual b -> g - b
                    | GreaterOrEqual b -> b - g

        let mutable state = Unchecked.defaultof<alglib.minnlcstate>
        alglib.minnlccreate(n, x0, &state)
        alglib.minnlcsetbc(state, request.bounds.lower, request.bounds.upper)
        alglib.minnlcsetnlc(state, 0, nlic)
        alglib.minnlcsetcond(state, request.epsX, request.maxIterations)
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
        result xr request rep.iterationscount rep.terminationtype

    // ---------------------------------------------------------- entry point

    /// The single G.1 entry point. Dispatches to the ALGLIB routine for the
    /// chosen method; `NelderMead` is the deferred simplex decision and is
    /// surfaced as `Failed "simplex backend unresolved"`. Any exception (ALGLIB
    /// or otherwise) is trapped and returned as `Result.Error`, so no ALGLIB
    /// type or exception crosses this boundary.
    let optimize (request : OptimizationRequest) : Result<OptimizationResult, string> =
        try
            match request.method with
            | NelderMead ->
                Ok
                    {
                        solution = request.initial
                        finalResiduals = request.residual request.initial
                        iterations = 0
                        terminationReason = Failed "simplex backend unresolved"
                        success = false
                    }
            | LevenbergMarquardt -> runLm request |> Ok
            | QuasiNewtonLbfgs -> runLbfgs request |> Ok
            | BoundedLinearConstrained -> runBleic request |> Ok
            | NonlinearConstrained -> runNlc request |> Ok
        with
        | e -> Error e.Message
