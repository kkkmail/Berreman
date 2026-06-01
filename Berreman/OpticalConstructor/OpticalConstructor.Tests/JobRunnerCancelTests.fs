namespace OpticalConstructor.Tests

open System.IO
open System.Threading
open Berreman.Constants
open Berreman.Fields
open Berreman.MaterialProperties
open Berreman.Media
open Berreman.Solvers
open Analytics.Variables
open OpticalConstructor.Ui.JobRunner
open Xunit

/// §J.10 background-job harness — AC-J10 (slice 016). A started job reports
/// increasing completed-points/iteration progress on a background worker; a
/// `CancellationToken` observed BETWEEN points stops the run within one inter-point
/// check and leaves prior committed results intact (the partial is dropped); and a
/// job's derived artefacts are written only under the project sidecar directory,
/// never the repo root. All headless — no Avalonia surface.
module JobRunnerCancelTests =

    // A representative "sweep point" the harness hosts: one engine single-system
    // solve through the REUSED OpticalSystemSolver (no parallel solver). The harness
    // is generic — it just drives this step per point and counts progress.
    let private glass = OpticalProperties.fromRefractionIndex (RefractionIndex 1.5)

    let private sampleSystem : OpticalSystem =
        {
            description = None
            upper = OpticalProperties.vacuum
            films = []
            substrate = None
            lower = glass
        }

    let private light = IncidentLightInfo.create (WaveLength.nm 600.0<nm>)

    /// A sweep over 10 wavelength points → the engine iterates 0..length inclusive.
    let private sweep : RangedVariable =
        WaveLengthRange(Range<_>.create 10 (WaveLength.nm 500.0<nm>) (WaveLength.nm 700.0<nm>))

    let private solvePoint (_ : int) : float =
        OpticalSystemSolver(light, sampleSystem).solution.emSys.reflected.e.value.norm

    // ----------------------------------------------------------------- AC-J10.1

    [<Fact>]
    let ``AC-J10 sweepTotalPoints is the engine's 0..length inclusive count`` () =
        // Granularity is driven by the RangedVariable point count (Variables.fs:42).
        Assert.Equal(11, sweepTotalPoints sweep)
        Assert.Equal(sweep.length + 1, sweepTotalPoints sweep)

    [<Fact>]
    let ``AC-J10 a run reports increasing completed-over-total progress to completion`` () =
        let total = sweepTotalPoints sweep
        let ticks = ResizeArray<JobProgress>()
        use cts = new CancellationTokenSource()
        let outcome = runPoints cts.Token total ticks.Add solvePoint

        match outcome with
        | RanToCompletion results -> Assert.Equal(total, List.length results)
        | CancelledWith _ -> Assert.Fail("an uncancelled run must run to completion")

        // One tick per point, completed strictly increasing 1..total, total known.
        Assert.Equal(total, ticks.Count)
        let completed = ticks |> Seq.map (fun p -> p.completed) |> List.ofSeq
        Assert.Equal<int list>([ 1 .. total ], completed)
        Assert.All(ticks, fun p -> Assert.Equal(Some total, p.total))
        Assert.All(ticks, fun p -> Assert.False(p.isIndeterminate))
        // The last tick is a full completion (fraction = 1).
        Assert.Equal(Some 1.0, ticks.[ticks.Count - 1].fraction)

    // ----------------------------------------------------------------- AC-J10.2

    [<Fact>]
    let ``AC-J10 a cancel observed between points stops within one inter-point check`` () =
        let total = sweepTotalPoints sweep
        let cancelAfter = 3
        use cts = new CancellationTokenSource()
        let ticks = ResizeArray<JobProgress>()
        // Cancel from inside the step body once `cancelAfter` points have run; the
        // loop checks the token at the TOP of the next iteration and stops there.
        let step (i : int) : float =
            let v = solvePoint i
            if i = cancelAfter - 1 then cts.Cancel()
            v

        let outcome = runPoints cts.Token total ticks.Add step

        match outcome with
        | CancelledWith partial ->
            // Stopped within one inter-point check: exactly `cancelAfter` points ran,
            // not the full sweep.
            Assert.Equal(cancelAfter, List.length partial)
            Assert.True(List.length partial < total)
        | RanToCompletion _ -> Assert.Fail("a cancelled run must stop early")

        Assert.Equal(cancelAfter, ticks.Count)

    [<Fact>]
    let ``AC-J10 a cancel leaves prior committed results intact (partial dropped)`` () =
        // Prior committed results exist in the model from an earlier completed run.
        let priorResults = [ 1.0; 2.0; 3.0 ]
        let model0 : JobModel<float list> =
            { initModel with status = Completed; committedResults = Some priorResults }

        // A new run is started (non-destructive) then cancelled.
        let started = updateJob StartRequested model0
        Assert.Equal(Running, started.status)
        Assert.Equal<float list option>(Some priorResults, started.committedResults)

        let cancelled = updateJob RunCancelled started
        // The cancelled job's partial output is dropped; prior results are UNTOUCHED.
        Assert.Equal(Cancelled, cancelled.status)
        Assert.Equal<float list option>(Some priorResults, cancelled.committedResults)

    [<Fact>]
    let ``AC-J10 completion refreshes committed results through the MVU cycle`` () =
        let model0 : JobModel<float list> =
            { initModel with status = Completed; committedResults = Some [ 1.0 ] }
        let started = updateJob StartRequested model0
        let newResults = [ 9.0; 8.0; 7.0 ]
        let completed = updateJob (RunCompleted newResults) started
        Assert.Equal(Completed, completed.status)
        Assert.Equal<float list option>(Some newResults, completed.committedResults)

    // ----------------------------------------------------------------- button/toggle

    [<Fact>]
    let ``AC-J10 the single Start-Cancel button shows two states`` () =
        let idle : JobModel<float list> = initModel
        Assert.Equal("Start", buttonLabel idle)
        Assert.False(canCancel idle)

        let running = updateJob StartRequested idle
        Assert.Equal("Cancel", buttonLabel running)
        Assert.True(canCancel running)

    // ----------------------------------------------------------------- indeterminate

    [<Fact>]
    let ``AC-J10 an unknown-total job reports an indeterminate-but-running state`` () =
        // Optimizer-style: iterate while hasMore holds, no known total.
        let ticks = ResizeArray<JobProgress>()
        use cts = new CancellationTokenSource()
        let outcome = runIterations cts.Token (fun i -> i < 4) ticks.Add (fun i -> float i)
        match outcome with
        | RanToCompletion rs -> Assert.Equal(4, List.length rs)
        | CancelledWith _ -> Assert.Fail("an uncancelled run must complete")
        Assert.All(ticks, fun p -> Assert.True(p.isIndeterminate))
        Assert.All(ticks, fun p -> Assert.Equal(None, p.fraction))

    // ----------------------------------------------------------------- AC-J10.6

    [<Fact>]
    let ``AC-J10 derived artefacts go under the project sidecar dir, never the repo root`` () =
        let projectDir = Path.Combine(Path.GetTempPath(), "oc-project-sample")
        let repoRoot = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", ".."))

        let sidecarDir = sidecarDirectory projectDir
        let path = derivedArtefactPath projectDir "sweep-result"

        // The artefact lives under the project's sidecar directory…
        Assert.StartsWith(Path.GetFullPath sidecarDir, Path.GetFullPath path)
        Assert.StartsWith(Path.GetFullPath projectDir, Path.GetFullPath sidecarDir)
        // …carries the Softellect .binz extension (reused, not hand-written)…
        Assert.Equal(".binz", Path.GetExtension path)
        // …and is NEVER the repo root or a direct child of it.
        Assert.NotEqual<string>(Path.GetFullPath repoRoot, Path.GetFullPath path)
        Assert.NotEqual<string>(Path.GetFullPath repoRoot, Path.GetFullPath(Path.GetDirectoryName path))
