/// §G.10 — Synthesis / Fit operator page (spec 0022 Part G §G.10).
///
/// Hosts the G.4 target editor, the G.3 parameter/bound vector, the G.1 method
/// selector, and the G.8 fit report, and commits the six-item operator-UX
/// checklist for a page with a long-running, cancellable job. Per §0/P3 the model
/// and `update` carry NO Avalonia type, so the whole checklist — nav entry, the
/// per-iteration progress payload, one-click non-destructive Start, confirm-gated
/// Accept, the `plotComparison` fit-vs-measured overlay refresh, and the distinct
/// Cancel + Revert controls — is unit-testable headlessly (AC-G7/AC-G8). The
/// FuncUI view that binds this model to controls is a later UI-wiring slice; this
/// module carries no Avalonia type.
///
/// Long-running execution itself is the Part J §J.10 background-job runner (slice
/// 016): this page supplies only the per-iteration progress payload and a single
/// cancellable run at a time — NO job-queue, retry, or scheduling abstraction is
/// added here (§G.10 non-requirement). The G.5/G.9 loops observe the model's
/// `cancelRequested` flag through the runner.
module OpticalConstructor.Ui.SynthesisFitPage

open Berreman.Media
open Berreman.Dispersion
open Berreman.FieldFunctions
open Analytics.Variables
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.DesignParameters
open OpticalConstructor.Optimization.MeritFunction
open OpticalConstructor.Optimization.FitQuality

// ---------------------------------------------------------------------------
// Navigation entry (§G.10 item 1): a named top-level entry reachable from the
// main navigation alongside the existing stack/chart pages, without a dialog.
// ---------------------------------------------------------------------------

type NavEntry =
    {
        title : string
        isDefaultLanding : bool
    }

let navEntry : NavEntry = { title = "Synthesis / Fit"; isDefaultLanding = false }

// ---------------------------------------------------------------------------
// The page model & messages (Avalonia-free, per P3).
// ---------------------------------------------------------------------------

type Model =
    {
        /// The working `OpticalSystem` being fitted. Start is non-destructive — it
        /// never overwrites this; only a confirmed Accept replaces it (item 3).
        workingSystem : OpticalSystem
        /// The G.3 design variables and their start vector for the next run.
        parameters : DesignParameter list
        initial : float[]
        /// The G.4 target-editor model (the merit targets).
        targets : FitTarget list
        /// The G.1 method selector.
        method : OptimizationMethod
        /// The candidate refined system from the last completed run — produced by
        /// Start, NOT yet folded into `workingSystem` (item 3).
        candidate : OpticalSystem option
        /// The G.8 fit report for the last completed run.
        report : FitReport option
        /// Per-iteration progress payload (iteration count, current χ²/MSE) on a
        /// channel DISTINCT from the callback-free G.1 interface (item 2).
        progress : (int * float) option
        /// Whether a run is in flight.
        running : bool
        /// Cooperative cancellation flag the G.5/G.9 loops observe (item 5 / AC-G7).
        cancelRequested : bool
        /// The working system snapshot before the last Start — the Revert target (item 5).
        preFit : OpticalSystem option
        /// A pending Accept awaiting explicit confirmation (item 3, destructive).
        pendingAccept : bool
        /// The project working folder — fit-history `.binz` sidecars are written
        /// here, never the repository root (item 6).
        workingFolder : string
    }

type Msg =
    /// Edit the G.1 method / the G.4 targets / the G.3 start vector (the editors).
    | SetMethod of OptimizationMethod
    | SetTargets of FitTarget list
    | SetInitial of float[]
    /// One-click Start (item 3): non-destructive — produces a candidate, snapshots
    /// the working system for Revert, and never overwrites saved files.
    | StartFit
    /// Per-iteration progress from the §J.10 runner (item 2).
    | ReportProgress of int * float
    /// The runner reports completion: the refined system + its G.8 report.
    | FitCompleted of OpticalSystem * FitReport
    /// The runner reports failure / a cancelled run.
    | FitFailed of string
    /// Cooperative Cancel (item 5) — a DISTINCT control from Start (AC-G7).
    | CancelFit
    /// Accept the candidate (item 3): opens the explicit confirmation gate.
    | RequestAccept
    /// Confirm the Accept: fold the candidate back into the working system (item 3).
    | ConfirmAccept
    | CancelAccept
    /// Revert to the pre-fit working system (item 5 / AC-G7).
    | Revert

// ---------------------------------------------------------------------------
// update (pure, Avalonia-free).
// ---------------------------------------------------------------------------

let update (msg : Msg) (model : Model) : Model =
    match msg with
    | SetMethod m -> { model with method = m }
    | SetTargets ts -> { model with targets = ts }
    | SetInitial v -> { model with initial = Array.copy v }

    | StartFit ->
        // One-click, non-destructive: snapshot the working system for Revert, clear
        // any prior candidate/report/progress, arm the cancellation flag to false.
        if model.running then model
        else
            { model with
                running = true
                cancelRequested = false
                preFit = Some model.workingSystem
                candidate = None
                report = None
                progress = None
                pendingAccept = false }

    | ReportProgress (iter, chi) -> { model with progress = Some (iter, chi) }

    | FitCompleted (system, report) ->
        // Completion → the candidate + report are folded in and the comparison
        // overlay (below) refreshes automatically — no manual reload message (item 4).
        { model with running = false; candidate = Some system; report = Some report }

    | FitFailed _ -> { model with running = false }

    | CancelFit ->
        // Cooperative: set the flag the G.5/G.9 loops observe. Distinct from Start.
        { model with cancelRequested = true }

    | RequestAccept ->
        match model.candidate with
        | Some _ -> { model with pendingAccept = true }   // open the confirm gate
        | None -> model

    | ConfirmAccept ->
        match model.candidate with
        | Some system -> { model with workingSystem = system; candidate = None; pendingAccept = false }
        | None -> { model with pendingAccept = false }

    | CancelAccept -> { model with pendingAccept = false }

    | Revert ->
        match model.preFit with
        | Some system -> { model with workingSystem = system; candidate = None; report = None; progress = None }
        | None -> model

// ---------------------------------------------------------------------------
// init.
// ---------------------------------------------------------------------------

let init
    (workingSystem : OpticalSystem)
    (parameters : DesignParameter list)
    (initial : float[])
    (targets : FitTarget list)
    (method : OptimizationMethod)
    (workingFolder : string) : Model =
    {
        workingSystem = workingSystem
        parameters = parameters
        initial = Array.copy initial
        targets = targets
        method = method
        candidate = None
        report = None
        progress = None
        running = false
        cancelRequested = false
        preFit = None
        pendingAccept = false
        workingFolder = workingFolder
    }

// ---------------------------------------------------------------------------
// Avalonia-free presentation / seam helpers (§G.10). These compute the strings,
// flags, and chart-input payloads a FuncUI view would render or hand to the
// charting path — so the page's UX commitments are unit-testable per P3.
// ---------------------------------------------------------------------------

/// Whether a run can be cancelled (item 5): a run is in flight.
let canCancel (model : Model) : bool = model.running

/// Whether the Revert action is available (item 5): a pre-fit snapshot exists.
let canRevert (model : Model) : bool = Option.isSome model.preFit

/// Whether a candidate is available to accept (item 3).
let canAccept (model : Model) : bool = Option.isSome model.candidate

/// Accepting a fit result that REPLACES the working system requires an explicit
/// confirm (item 3); Start does not. This is the prompt for that gate (`None`
/// unless an Accept is pending).
let acceptConfirmationPrompt (model : Model) : string option =
    if model.pendingAccept then Some "Replace the working optical system with the fitted result?"
    else None

/// The per-iteration progress line (item 2): iteration count + current χ²/MSE.
let progressText (model : Model) : string option =
    model.progress |> Option.map (fun (iter, chi) -> sprintf "Iteration %d — χ²/MSE = %g" iter chi)

/// The distinct photometric `OpticalFunction`s among the targets — the channels
/// the fit-vs-measured overlay compares.
let private overlayFunctions (targets : FitTarget list) : OpticalFunction list =
    targets
    |> List.choose (fun t -> match t.quantity with | Photometric f -> Some f | Ellipsometric _ -> None)
    |> List.distinct

/// §G.10 item 4 / AC-G8 — the fit-vs-measured overlay payload that the result
/// charts refresh through the existing `plotComparison` path (`Charting.fs:38`,
/// `list<FixedInfo> -> List<OpticalFunction> -> RangedVariable -> _`). On a
/// completed run this is populated from the model with NO manual-reload message:
/// the FixedInfo list overlays the pre-fit (measured-reference) system and the
/// fitted candidate system, for the targets' photometric channels. The view binds
/// `plotComparison (fst payload) (snd payload) <wavelength range>`; the measured
/// sample points themselves are surfaced by the G.8 `FitReport.residuals`.
let comparisonOverlay (model : Model) : (FixedInfo list * OpticalFunction list) option =
    match model.candidate, model.targets with
    | Some fitted, (t0 :: _) ->
        let mkInfo (system : OpticalSystem) : FixedInfo =
            { incidentLightInfo = t0.samplePoint; opticalSystem = system.dispersive }
        let reference = model.preFit |> Option.defaultValue model.workingSystem
        Some ([ mkInfo reference; mkInfo fitted ], overlayFunctions model.targets)
    | _ -> None

// ---------------------------------------------------------------------------
// Fit-history sidecar (§G.10 item 6). The G.8 `.binz` report is written under the
// project working folder (never the repo root); `*.binz` is already in the root
// .gitignore (Part I / slice 012), so the sidecar is NOT committed — the canonical
// JSON project that references it is the committable artefact (§0 constraint 4).
// ---------------------------------------------------------------------------

/// The fit-history `.binz` sidecar path under the working folder (never the repo root).
let fitHistorySidecarPath (model : Model) (name : string) : string =
    System.IO.Path.Combine(model.workingFolder, name + ".fit-history.binz")

/// The `(path, bytes)` to write for the last completed run's G.8 report, via the
/// `.binz` serializer (`FitQuality.toBinz`). `None` until a run completes.
let saveFitReport (model : Model) (name : string) : (string * byte[]) option =
    model.report |> Option.map (fun report -> fitHistorySidecarPath model name, toBinz report)
