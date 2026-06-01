namespace OpticalConstructor.Tests

open Berreman.Constants
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Fields
open Berreman.Solvers
open Berreman.FieldFunctions
open OpticalConstructor.Optimization
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.MeritFunction
open OpticalConstructor.Optimization.FitQuality
open OpticalConstructor.Ui
open Xunit

/// Part G §G.8 / §G.10 page-level ACs (slice 011), in the gated, UI-less
/// `OpticalConstructor.Tests` host (the page model/`update` carries NO Avalonia
/// type per P3, so these run headlessly):
///   AC-G5 — a `FitReport` exposes χ²/MSE, the correlation matrix, confidence
///           intervals, and per-sample residuals, and round-trips through the
///           `.binz` sidecar serializer (never the canonical JSON).
///   AC-G7 — Cancel stops the G.9 loop cooperatively (it observes the flag) and is
///           a DISTINCT message from Start; Revert restores the pre-fit system.
///   AC-G8 — on completion the fit-vs-measured overlay (the `plotComparison`
///           payload) refreshes from the model with no manual-reload message.
module SynthesisFitPageTests =

    // ----------------------------------------------------------------- fixtures

    let private glass = OpticalProperties.fromRefractionIndex (RefractionIndex.create 1.52)

    let private mkSystem (thickness : float<nm>) : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = [ { properties = glass; thickness = Thickness.nm thickness } ]
            substrate = None
            lower = OpticalProperties.vacuum
        }

    let private reflectanceOf (system : OpticalSystem) (w : WaveLength) : float =
        OpticalSystemSolver(IncidentLightInfo.create w, system).solution.func R |> Option.get

    /// A small R-fit problem and the resulting (refined system, FitReport).
    let private fittedProblem () =
        let trueSystem = mkSystem 250.0<nm>
        let baseSystem = mkSystem 1.0<nm>
        let parameters = [ DesignParameters.layerThickness 0 1.0e-6 ]
        let targets =
            [ 450.0; 520.0; 590.0; 660.0 ]
            |> List.map (fun nm ->
                let w = WaveLength.nm (nm * 1.0<nm>)
                {
                    quantity = Photometric R
                    samplePoint = IncidentLightInfo.create w
                    desiredValue = reflectanceOf trueSystem w
                    weight = 1.0
                    tolerance = 1.0
                    kind = Equality
                })
        let initial = [| 220.0e-9 |]
        match LocalRefinement.refine baseSystem parameters initial targets with
        | Ok (refined, result) ->
            let residual = MeritFunction.buildResidual baseSystem parameters targets
            baseSystem, parameters, initial, targets, refined, FitQuality.fromResult residual result
        | Error e -> failwith $"fixture refine failed: {e}"

    // ----------------------------------------------------------------- AC-G5

    [<Fact>]
    let ``AC-G5 FitReport exposes chi-squared, CI, correlation, residuals and round-trips through .binz`` () =
        let (_, _, _, targets, _, report) = fittedProblem ()

        // Quality metrics are present and well-shaped.
        Assert.True(report.chiSquared >= 0.0)
        Assert.True(System.Double.IsFinite report.mse)
        Assert.True(System.Double.IsFinite report.reducedChiSquared)
        Assert.Equal(1, report.parameters.Length)
        Assert.Equal(1, report.standardErrors.Length)
        Assert.Equal(1, report.confidenceIntervals.Length)
        // Correlation matrix is n×n with a unit diagonal.
        Assert.Equal(report.parameters.Length, report.correlation.Length)
        report.correlation |> Array.iter (fun row -> Assert.Equal(report.parameters.Length, row.Length))
        Assert.True(abs (report.correlation.[0].[0] - 1.0) < 1.0e-9)
        // Per-sample residuals retained for the residual plot / overlay.
        Assert.Equal(targets.Length, report.residuals.Length)
        // Each CI brackets its parameter estimate.
        let (lo, hi) = report.confidenceIntervals.[0]
        Assert.True(lo <= report.parameters.[0] && report.parameters.[0] <= hi)

        // .binz round-trip: derived sidecar payload, NOT canonical JSON.
        let bytes = FitQuality.toBinz report
        Assert.Equal(0x1fuy, bytes.[0])              // gzip magic — the .binz payload
        Assert.Equal(0x8buy, bytes.[1])
        Assert.NotEqual(byte '{', bytes.[0])         // never the canonical JSON '{'
        let restored = FitQuality.fromBinz bytes
        Assert.Equal(report.chiSquared, restored.chiSquared, 12)
        Assert.Equal(report.parameters.[0], restored.parameters.[0], 15)
        Assert.Equal(report.residuals.Length, restored.residuals.Length)

    // -------------------------------------- mis-scaled-Jacobian regression (R-1)

    /// Catches a mis-scaled finite-difference step in `residualJacobian` at the
    /// project's canonical meter scale (§0 #3). A NONLINEAR residual is required:
    /// the old step `h = 1e-7·max(1, |x|)` floors at 1e-7 m, a ~40 % perturbation of
    /// a ~2.5e-7 m thickness, so the forward difference is a poor derivative and the
    /// covariance / std-error / CI / correlation built from JᵀJ are numerically wrong
    /// — yet AC-G5 (CI brackets the estimate, unit diagonal) stays green either way.
    /// This asserts the Jacobian matches the analytic derivative AND the standard
    /// error matches its analytic closed form; a mis-scaled step fails both.
    [<Fact>]
    let ``residualJacobian and covariance stay accurate for a meter-scale parameter`` () =
        // A single canonical meter-scale parameter (~2.5e-7 m, a 250 nm thickness).
        let x0 = 2.5e-7
        // Nonlinear residual rᵢ(x) = sin(ωᵢ x) − dᵢ with ωᵢ x0 = baseᵢ ~ O(1), so the
        // forward difference is sensitive to the step size.
        let bases = [| 0.5; 1.0; 1.5; 2.0; 2.5 |]
        let omegas = bases |> Array.map (fun b -> b / x0)
        // Known small residuals at the solution: dᵢ = sin(baseᵢ) + epsᵢ ⇒ rᵢ(x0) = −epsᵢ.
        let eps = [| 0.01; -0.02; 0.015; -0.005; 0.012 |]
        let data = Array.map2 (fun b e -> sin b + e) bases eps
        let residual (x : float[]) = Array.map2 (fun om d -> sin (om * x.[0]) - d) omegas data

        // Analytic Jacobian column at x0: drᵢ/dx = ωᵢ cos(ωᵢ x0) = ωᵢ cos(baseᵢ).
        let analyticJac = Array.map2 (fun om b -> om * cos b) omegas bases
        let jac = FitQuality.residualJacobian residual [| x0 |]
        Assert.Equal(omegas.Length, jac.Length)
        jac |> Array.iteri (fun i row ->
            Assert.Equal(1, row.Length)
            let rel = abs (row.[0] - analyticJac.[i]) / abs analyticJac.[i]
            Assert.True(rel < 1.0e-5, $"Jacobian row {i} rel error {rel} — mis-scaled step"))

        // Analytic standard error of the single parameter:
        //   reducedχ² = Σeps²/(m−1);  Cov = reducedχ²/Σ(ωᵢ cos baseᵢ)²;  σ = √Cov.
        let finalResiduals = residual [| x0 |]
        let m = finalResiduals.Length
        let reduced = (eps |> Array.sumBy (fun v -> v * v)) / float (m - 1)
        let analyticStdErr = sqrt (reduced / (analyticJac |> Array.sumBy (fun v -> v * v)))

        let report = FitQuality.reportFrom residual [| x0 |] finalResiduals
        let sigma = report.standardErrors.[0]
        // Non-degenerate covariance: finite and strictly positive.
        Assert.True(System.Double.IsFinite sigma && sigma > 0.0, "std error must be finite and positive")
        Assert.True(System.Double.IsFinite report.covariance.[0].[0] && report.covariance.[0].[0] > 0.0)
        // And it matches the analytic closed form — a mis-scaled Jacobian misses this.
        let relSigma = abs (sigma - analyticStdErr) / analyticStdErr
        Assert.True(relSigma < 1.0e-3, $"std error {sigma} vs analytic {analyticStdErr} (rel {relSigma}) — mis-scaled Jacobian")

    // ----------------------------------------------------------------- AC-G7

    [<Fact>]
    let ``AC-G7 Cancel is distinct from Start and Revert restores the pre-fit system`` () =
        let (baseSystem, parameters, initial, targets, _, _) = fittedProblem ()
        let model0 = SynthesisFitPage.init baseSystem parameters initial targets LevenbergMarquardt "C:/work"

        // Start is one-click and non-destructive: snapshots the pre-fit system and
        // does NOT overwrite the working system.
        let started = SynthesisFitPage.update SynthesisFitPage.StartFit model0
        Assert.True(started.running)
        Assert.Equal<OpticalSystem>(model0.workingSystem, started.workingSystem)
        Assert.Equal(Some model0.workingSystem, started.preFit)

        // Cancel is a DISTINCT message from Start; it sets the cooperative flag the
        // G.9 loop observes, and is not the same control as Start.
        let cancelled = SynthesisFitPage.update SynthesisFitPage.CancelFit started
        Assert.True(cancelled.cancelRequested)
        Assert.NotEqual<SynthesisFitPage.Msg>(SynthesisFitPage.StartFit, SynthesisFitPage.CancelFit)

        // A completed-then-accepted fit replaces the working system only after an
        // explicit confirm (Accept is gated, Start is not).
        let candidateSystem = mkSystem 250.0<nm>
        let report = (let (_, _, _, _, _, r) = fittedProblem () in r)
        let completed = SynthesisFitPage.update (SynthesisFitPage.FitCompleted(candidateSystem, report)) cancelled
        Assert.False(completed.running)
        let requested = SynthesisFitPage.update SynthesisFitPage.RequestAccept completed
        Assert.True((SynthesisFitPage.acceptConfirmationPrompt requested).IsSome)   // confirm gate
        Assert.Equal<OpticalSystem>(model0.workingSystem, requested.workingSystem)  // not yet replaced
        let confirmed = SynthesisFitPage.update SynthesisFitPage.ConfirmAccept requested
        Assert.Equal<OpticalSystem>(candidateSystem, confirmed.workingSystem)       // now replaced

        // Revert restores the pre-fit working system.
        let reverted = SynthesisFitPage.update SynthesisFitPage.Revert confirmed
        Assert.Equal<OpticalSystem>(model0.workingSystem, reverted.workingSystem)

    [<Fact>]
    let ``AC-G7 the global synthesis loop stops cooperatively when the cancel flag is set`` () =
        let (baseSystem, parameters, _, targets, _, _) = fittedProblem ()
        let residual = MeritFunction.buildResidual baseSystem parameters targets
        let bounds = DesignParameters.bounds parameters
        let initial = [| 220.0e-9 |]
        let saConfig = { Synthesis.AnnealingConfig.defaultValue with iterations = 5000 }

        // Cancel immediately: the SA loop must stop without exhausting its iterations.
        let cancelledRun = Synthesis.simulatedAnnealing saConfig (fun () -> true) bounds residual initial
        Assert.True(cancelledRun.iterations < saConfig.iterations, "the loop must stop early on cancel")
        Assert.Equal(Failed "cancelled", cancelledRun.terminationReason)
        Assert.False(cancelledRun.success)

        // The GA loop observes the same flag.
        let gaConfig = { Synthesis.GeneticConfig.defaultValue with generations = 5000 }
        let cancelledGa = Synthesis.geneticAlgorithm gaConfig (fun () -> true) bounds residual initial
        Assert.True(cancelledGa.iterations < gaConfig.generations)
        Assert.Equal(Failed "cancelled", cancelledGa.terminationReason)

        // With no cancellation the same loop runs to completion — proving the flag
        // (not some other early-exit) is what stopped it above.
        let fullRun = Synthesis.simulatedAnnealing { saConfig with iterations = 50 } (fun () -> false) bounds residual initial
        Assert.Equal(50, fullRun.iterations)

    // ----------------------------------------------------------------- AC-G8

    [<Fact>]
    let ``AC-G8 the fit-vs-measured overlay refreshes from the model on completion without a manual reload`` () =
        let (baseSystem, parameters, initial, targets, refined, report) = fittedProblem ()
        let model0 = SynthesisFitPage.init baseSystem parameters initial targets LevenbergMarquardt "C:/work"

        // No candidate yet: no overlay.
        Assert.True((SynthesisFitPage.comparisonOverlay model0).IsNone)

        // Start then complete — the overlay refreshes WITHOUT any extra reload message.
        let running = SynthesisFitPage.update SynthesisFitPage.StartFit model0
        let completed = SynthesisFitPage.update (SynthesisFitPage.FitCompleted(refined, report)) running

        match SynthesisFitPage.comparisonOverlay completed with
        | Some (infos, funcs) ->
            // Two models overlaid (pre-fit reference + fitted candidate) for the
            // targets' photometric channel — the exact `plotComparison` inputs.
            Assert.Equal(2, infos.Length)
            Assert.True(List.contains R funcs, "the overlay must compare the targets' R channel")
        | None -> failwith "expected a fit-vs-measured overlay payload after completion"

    // ----------------------------------------------------------------- nav (item 1)

    [<Fact>]
    let ``G10 the page exposes a named Synthesis/Fit navigation entry`` () =
        Assert.Equal("Synthesis / Fit", SynthesisFitPage.navEntry.title)
