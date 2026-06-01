/// §J.10 — long-running job progress, cancellation & timing [Standard] (010 Part
/// II §3). `JobRunner` is the SINGLE shared background-job harness that hosts heavy
/// sweeps (`Variables.calculate`/`calculate3D`, `Variables.fs:237,270`) and Part G
/// fit runs (the ALGLIB path) on a background worker, reporting progress,
/// cooperative cancellation, and elapsed time as MVU messages. Part G §G.10 (slice
/// 011) supplies the per-iteration optimizer progress payload to it; an active job
/// surfaces in the `AppShell` (J.8) shared status area (slice 015).
///
/// The harness is GENERIC over the per-step work (`step : int -> 'a`): it CONSUMES
/// the engine sweep and the optimizer through an injected closure rather than
/// forking `Variables.calculate`'s per-point routing or the ALGLIB loop (§0
/// constraint 2 — reuse before invention). Sweep progress granularity stays tied
/// to the `RangedVariable` point count (`Variables.fs:42`) via `sweepTotalPoints`.
///
/// Per §0/P3 the model and the cooperative loop carry NO Avalonia type, so the
/// whole §J.10 operator-UX checklist — background execution, completed-points/
/// iteration progress, the one-click Start→Cancel toggle, the MVU completion
/// refresh, and sidecar-only derived artefacts — is unit-testable headlessly
/// (AC-J10). The FuncUI view that binds this model to controls is a later
/// UI-wiring slice; this module takes no Avalonia dependency and adds NO
/// job-queue, retry, or scheduling abstraction (§J.10 non-requirement — one job
/// per worker, cancel-and-restart).
module OpticalConstructor.Ui.JobRunner

open System
open System.Diagnostics
open System.IO
open System.Threading
open Analytics.Variables
open OpticalConstructor.Storage

// ---------------------------------------------------------------------------
// Progress payload (R-1 item 2). Completed-points-over-total for sweeps (total
// known) and iteration count for optimization (total unknown -> indeterminate-
// but-running, never a frozen UI). Carries elapsed time for the §J.10 timing
// report.
// ---------------------------------------------------------------------------

/// A single progress tick: how many steps have completed, the total when known
/// (`None` => indeterminate-but-running, R-1 item 2), and the elapsed run time.
type JobProgress =
    {
        completed : int
        total : int option
        elapsed : TimeSpan
    }

    /// Completed fraction in [0,1] when the total is known; `None` for an
    /// indeterminate job (the view shows a marquee/running state instead).
    member p.fraction =
        match p.total with
        | Some t when t > 0 -> Some(float p.completed / float t)
        | _ -> None

    /// True when the total is unknown — the view must show running, not frozen.
    member p.isIndeterminate = Option.isNone p.total

/// The total number of points a sweep over `x` runs: the engine's own
/// `Variables.calculate`/`calculate3D` iterate `0 .. x.length` INCLUSIVE
/// (`Variables.fs:267,298`), so the total is `x.length + 1`. Reading the count
/// straight off `RangedVariable.length` (`Variables.fs:42`) keeps progress
/// granularity DRIVEN BY the engine's point count — it is not re-derived here.
let sweepTotalPoints (x : RangedVariable) : int = x.length + 1

// ---------------------------------------------------------------------------
// The cooperative run core (R-1 / cancellation). One job per worker; the
// CancellationToken is checked BETWEEN steps so a cancel stops the run within one
// inter-step check and the partial output is returned for the caller to DROP
// (prior committed results stay intact — item 5). No thread-kill, no retry loop.
// ---------------------------------------------------------------------------

/// The outcome of a cooperative run: either it ran every step to completion, or a
/// cancellation was observed and the run stopped early carrying its PARTIAL
/// results — which the MVU layer drops, leaving prior committed results intact.
type JobOutcome<'a> =
    | RanToCompletion of 'a list
    | CancelledWith of 'a list

let private runLoop
    (token : CancellationToken)
    (total : int option)
    (shouldContinue : int -> bool)
    (onProgress : JobProgress -> unit)
    (step : int -> 'a) : JobOutcome<'a> =
    let sw = Stopwatch.StartNew()
    let results = ResizeArray<'a>()
    let mutable i = 0
    let mutable cancelled = false
    while not cancelled && shouldContinue i do
        if token.IsCancellationRequested then
            // Observed between steps: stop within one inter-step check (item 5).
            cancelled <- true
        else
            let r = step i
            results.Add r
            i <- i + 1
            onProgress { completed = i; total = total; elapsed = sw.Elapsed }
    sw.Stop()
    let collected = List.ofSeq results
    if cancelled then CancelledWith collected else RanToCompletion collected

/// Run a determinate job of exactly `total` steps (a sweep: `total =
/// sweepTotalPoints x`), checking `token` between points and reporting
/// completed-over-total progress with elapsed time (R-1 item 2).
let runPoints
    (token : CancellationToken)
    (total : int)
    (onProgress : JobProgress -> unit)
    (step : int -> 'a) : JobOutcome<'a> =
    runLoop token (Some total) (fun i -> i < total) onProgress step

/// Run an indeterminate job (optimizer iterations) while `hasMore` holds and the
/// token is not cancelled, reporting iteration-count progress with NO known total
/// (`total = None`, the indeterminate-but-running state of R-1 item 2). The
/// optimizer's own termination drives `hasMore`; the loop adds no retry.
let runIterations
    (token : CancellationToken)
    (hasMore : int -> bool)
    (onProgress : JobProgress -> unit)
    (step : int -> 'a) : JobOutcome<'a> =
    runLoop token None hasMore onProgress step

/// Start `run` on a thread-pool background worker (R-1 item 1: the long job never
/// freezes the UI). When it finishes — completion, cancellation, OR an exception —
/// `onDone` is invoked with the result; an exception becomes `Error message`
/// (mapped to a `RunFailed` message) rather than crashing the worker.
let startBackground (run : unit -> JobOutcome<'a>) (onDone : Result<JobOutcome<'a>, string> -> unit) : unit =
    Async.Start(
        async {
            let outcome =
                try Ok(run ())
                with e -> Error e.Message
            onDone outcome
        })

// ---------------------------------------------------------------------------
// The MVU harness (Avalonia-free, per P3). Start keeps the prior committed
// results; completion refreshes them through the normal update->view cycle
// (item 4); a cancel leaves them untouched (item 5 — the dropped partial is the
// undo). The Start control becomes a Cancel control for the run's duration
// (single button, two states — item 5).
// ---------------------------------------------------------------------------

/// The status of the single hosted job.
type JobStatus =
    | Idle
    | Running
    | Completed
    | Cancelled
    | Failed of string

/// The harness model, generic over the job's result type `'r` (a sweep result
/// table, a fit report, …). `committedResults` are the LAST completed run's
/// results that the owning page renders; they survive a cancel.
type JobModel<'r> =
    {
        status : JobStatus
        progress : JobProgress option
        committedResults : 'r option
    }

/// The idle initial model: no run, no progress, no committed results yet.
let initModel<'r> : JobModel<'r> =
    {
        status = Idle
        progress = None
        committedResults = None
    }

/// Messages the runner emits/consumes. `StartRequested`/`CancelRequested` are the
/// two states of the single Start↔Cancel button (item 5); the remaining three are
/// posted by the background worker via `startBackground`'s `onDone`.
type JobMsg<'r> =
    | StartRequested
    | CancelRequested
    | ProgressReported of JobProgress
    | RunCompleted of 'r
    | RunCancelled
    | RunFailed of string

/// Pure MVU update (Avalonia-free).
let updateJob (msg : JobMsg<'r>) (model : JobModel<'r>) : JobModel<'r> =
    match msg with
    // One-click, non-destructive Start (item 3): flip to Running, clear the
    // progress line; the prior committed results stay so a cancel can fall back.
    | StartRequested -> { model with status = Running; progress = None }
    // Cooperative Cancel (item 3 immediate one-click): the host cancels the token;
    // status flips to Cancelled when the worker reports `RunCancelled`.
    | CancelRequested -> model
    | ProgressReported p -> { model with progress = Some p }
    // Completion -> the new results are committed and the owning page refreshes
    // through the normal update->view cycle, no manual reload (item 4).
    | RunCompleted r -> { model with status = Completed; committedResults = Some r }
    // Cancel -> the partial output is dropped; prior committed results UNTOUCHED
    // (item 5 — the natural undo).
    | RunCancelled -> { model with status = Cancelled }
    | RunFailed e -> { model with status = Failed e }

// ---------------------------------------------------------------------------
// Presentation seams (Avalonia-free). The strings/flags a FuncUI view binds, so
// the §J.10 UX commitments are provable headless.
// ---------------------------------------------------------------------------

/// True while a run is in flight (the Cancel state of the button).
let isRunning (model : JobModel<'r>) : bool = model.status = Running

/// Whether the immediate one-click Cancel is available (item 3/5): a run is in
/// flight.
let canCancel (model : JobModel<'r>) : bool = model.status = Running

/// The single Start↔Cancel button's label (item 5 — one button, two states):
/// "Cancel" while running, otherwise "Start".
let buttonLabel (model : JobModel<'r>) : string =
    if isRunning model then "Cancel" else "Start"

/// The progress line (item 2): completed/total + elapsed for a sweep, or an
/// indeterminate-but-running iteration count for optimization.
let progressText (model : JobModel<'r>) : string option =
    model.progress
    |> Option.map (fun p ->
        match p.total with
        | Some t -> sprintf "%d / %d  (%.1f s)" p.completed t p.elapsed.TotalSeconds
        | None -> sprintf "%d iterations  (%.1f s)…" p.completed p.elapsed.TotalSeconds)

// ---------------------------------------------------------------------------
// Derived-artefact sidecar confinement (R-1 item 6). Any artefact a job writes
// (sweep/field-map `.binz`, fit histories) goes UNDER the project's sidecar
// directory referenced from the project JSON (§A.7), NEVER the repository root;
// these derived outputs are not committable project files (Part J adds none).
// ---------------------------------------------------------------------------

/// The project's sidecar directory for derived/bulk artefacts: a `.sidecars`
/// subfolder beside the project file's own directory. NEVER the repository root.
/// This is the SINGLE shared sidecar-location seam `Storage.Sidecar.sidecarDirectory`
/// (reuse finding F2): the fit page's `fitHistorySidecarPath` routes through the same
/// seam, so the flat-vs-subfolder choice is decided in exactly one place.
let sidecarDirectory (projectDir : string) : string =
    Sidecar.sidecarDirectory projectDir

/// A derived-artefact `.binz` path under the project sidecar directory (item 6),
/// via the single `Storage.Sidecar.derivedArtefactPath` seam, which appends the
/// drift-proof Softellect `.binz` extension. The path is always under
/// `sidecarDirectory projectDir`, never the repo root — the canonical JSON project
/// that references it is the committable artefact (§0 constraint 4).
let derivedArtefactPath (projectDir : string) (name : string) : string =
    Sidecar.derivedArtefactPath projectDir name
