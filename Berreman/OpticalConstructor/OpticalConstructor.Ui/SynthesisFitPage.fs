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
/// `cancelRequested` state through the runner.
module OpticalConstructor.Ui.SynthesisFitPage

open Berreman.Media
open Berreman.Dispersion
open Berreman.FieldFunctions
open Analytics.Variables
open OpticalConstructor.Domain.MuellerInverse
open OpticalConstructor.Optimization.OptimizationInterface
open OpticalConstructor.Optimization.DesignParameters
open OpticalConstructor.Optimization.MeritFunction
open OpticalConstructor.Optimization.FitQuality
open OpticalConstructor.Optimization.Synthesis
open OpticalConstructor.Controls
open Softellect.Sys.Primitives

// ---------------------------------------------------------------------------
// Navigation entry (§G.10 item 1): a named top-level entry reachable from the
// main navigation alongside the existing stack/chart pages, without a dialog.
// ---------------------------------------------------------------------------

/// Whether a navigation entry is the page the app lands on.
type NavLanding =
    | DefaultLandingPage
    | NotDefaultLandingPage

type NavEntry =
    {
        title : ControlCaption
        landing : NavLanding
    }

let navEntry : NavEntry = { title = ControlCaption "Synthesis / Fit"; landing = NotDefaultLandingPage }

// ---------------------------------------------------------------------------
// The page model & messages (Avalonia-free, per P3).
// ---------------------------------------------------------------------------

/// Whether a fit run is in flight.
type FitRunState =
    | FitRunning
    | FitNotRunning

/// Whether an Accept is waiting for its explicit confirmation (item 3, destructive).
type AcceptGate =
    | AcceptGateOpen
    | AcceptGateClosed

type Model =
    {
        /// The working `OpticalSystem` being fitted. Start is non-destructive — it
        /// never overwrites this; only a confirmed Accept replaces it (item 3).
        workingSystem : OpticalSystem
        /// The G.3 design variables and their start vector for the next run.
        parameters : DesignParameter list
        initial : DesignVector
        /// The G.4 target-editor model (the merit targets).
        targets : FitTarget list
        /// The G.1 method selector.
        method : OptimizationMethod
        /// The candidate refined system from the last completed run — produced by
        /// Start, NOT yet folded into `workingSystem` (item 3).
        candidate : OpticalSystem option
        /// The G.8 fit report for the last completed run.
        report : FitReport option
        /// Per-iteration progress payload (iteration count, current χ²) on a
        /// channel DISTINCT from the callback-free G.1 interface (item 2).
        progress : (IterationCount * ChiSquared) option
        running : FitRunState
        /// Cooperative cancellation the G.5/G.9 loops observe (item 5 / AC-G7).
        cancelRequested : SearchCancellation
        /// The working system snapshot before the last Start — the Revert target (item 5).
        preFit : OpticalSystem option
        acceptGate : AcceptGate
        /// The project working folder — fit-history `.binz` sidecars are written
        /// here, never the repository root (item 6).
        workingFolder : FolderName
    }

type Msg =
    /// Edit the G.1 method / the G.4 targets / the G.3 start vector (the editors).
    | SetMethod of OptimizationMethod
    | SetTargets of FitTarget list
    | SetInitial of DesignVector
    /// One-click Start (item 3): non-destructive — produces a candidate, snapshots
    /// the working system for Revert, and never overwrites saved files.
    | StartFit
    /// Per-iteration progress from the §J.10 runner (item 2).
    | ReportProgress of IterationCount * ChiSquared
    /// The runner reports completion: the refined system + its G.8 report.
    | FitCompleted of OpticalSystem * FitReport
    /// The runner reports failure / a cancelled run.
    | FitFailed of OptimizationError
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

let updateSynthesisFit (msg : Msg) (model : Model) : Model =
    match msg with
    | SetMethod m -> { model with method = m }
    | SetTargets ts -> { model with targets = ts }
    | SetInitial v -> { model with initial = v.copy }

    | StartFit ->
        // One-click, non-destructive: snapshot the working system for Revert, clear
        // any prior candidate/report/progress, disarm the cancellation.
        match model.running with
        | FitRunning -> model
        | FitNotRunning ->
            { model with
                running = FitRunning
                cancelRequested = SearchNotCancelled
                preFit = Some model.workingSystem
                candidate = None
                report = None
                progress = None
                acceptGate = AcceptGateClosed }

    | ReportProgress (iter, chi) -> { model with progress = Some (iter, chi) }

    | FitCompleted (system, report) ->
        // Completion → the candidate + report are folded in and the comparison
        // overlay (below) refreshes automatically — no manual reload message (item 4).
        { model with running = FitNotRunning; candidate = Some system; report = Some report }

    | FitFailed _ -> { model with running = FitNotRunning }

    | CancelFit ->
        // Cooperative: the request the G.5/G.9 loops observe. Distinct from Start.
        { model with cancelRequested = SearchCancelRequested }

    | RequestAccept ->
        match model.candidate with
        | Some _ -> { model with acceptGate = AcceptGateOpen }
        | None -> model

    | ConfirmAccept ->
        match model.candidate with
        | Some system -> { model with workingSystem = system; candidate = None; acceptGate = AcceptGateClosed }
        | None -> { model with acceptGate = AcceptGateClosed }

    | CancelAccept -> { model with acceptGate = AcceptGateClosed }

    | Revert ->
        match model.preFit with
        | Some system -> { model with workingSystem = system; candidate = None; report = None; progress = None }
        | None -> model

// ---------------------------------------------------------------------------
// init.
// ---------------------------------------------------------------------------

let initSynthesisFit
    (workingSystem : OpticalSystem)
    (parameters : DesignParameter list)
    (initial : DesignVector)
    (targets : FitTarget list)
    (method : OptimizationMethod)
    (workingFolder : FolderName) : Model =
    {
        workingSystem = workingSystem
        parameters = parameters
        initial = initial.copy
        targets = targets
        method = method
        candidate = None
        report = None
        progress = None
        running = FitNotRunning
        cancelRequested = SearchNotCancelled
        preFit = None
        acceptGate = AcceptGateClosed
        workingFolder = workingFolder
    }

// ---------------------------------------------------------------------------
// Avalonia-free presentation / seam helpers (§G.10). These compute the captions,
// control availabilities, and chart-input payloads a FuncUI view would render or
// hand to the charting path — so the page's UX commitments are unit-testable per P3.
// ---------------------------------------------------------------------------

/// The distinct photometric `OpticalFunction`s among the targets — the channels
/// the fit-vs-measured overlay compares.
let private overlayFunctions (targets : FitTarget list) : OpticalFunction list =
    targets
    |> List.choose (fun t -> match t.quantity with | Photometric f -> Some f | Ellipsometric _ -> None)
    |> List.distinct

type Model with

    /// The Cancel control (item 5): offered while a run is in flight.
    member this.cancelAvailability : ControlAvailability =
        match this.running with
        | FitRunning -> ControlOffered
        | FitNotRunning -> ControlWithheld

    /// The Revert control (item 5): offered once a pre-fit snapshot exists.
    member this.revertAvailability : ControlAvailability =
        match this.preFit with
        | Some _ -> ControlOffered
        | None -> ControlWithheld

    /// The Accept control (item 3): offered while a candidate is available to accept.
    member this.acceptAvailability : ControlAvailability =
        match this.candidate with
        | Some _ -> ControlOffered
        | None -> ControlWithheld

    /// Accepting a fit result that REPLACES the working system requires an explicit
    /// confirm (item 3); Start does not. This is the prompt for that gate (`None`
    /// unless an Accept is pending).
    member this.acceptConfirmationPrompt : ControlCaption option =
        match this.acceptGate with
        | AcceptGateOpen -> Some (ControlCaption "Replace the working optical system with the fitted result?")
        | AcceptGateClosed -> None

    /// The per-iteration progress line (item 2): iteration count + current χ².
    member this.progressText : ControlCaption option =
        this.progress |> Option.map (fun (iter, chi) -> ControlCaption $"Iteration %d{iter.value} — χ²/MSE = %g{chi.value}")

    /// §G.10 item 4 / AC-G8 — the fit-vs-measured overlay payload that the result
    /// charts refresh through the existing `plotComparison` path (`Charting.fs:38`,
    /// `list<FixedInfo> -> List<OpticalFunction> -> RangedVariable -> _`). On a
    /// completed run this is populated from the model with NO manual-reload message:
    /// the FixedInfo list overlays the pre-fit (measured-reference) system and the
    /// fitted candidate system, for the targets' photometric channels. The view binds
    /// `plotComparison (fst payload) (snd payload) <wavelength range>`; the measured
    /// sample points themselves are surfaced by the G.8 `FitReport.residuals`.
    member this.comparisonOverlay : (FixedInfo list * OpticalFunction list) option =
        match this.candidate, this.targets with
        | Some fitted, (t0 :: _) ->
            let mkInfo (system : OpticalSystem) : FixedInfo =
                { incidentLightInfo = t0.samplePoint; opticalSystem = system.dispersive }
            let reference = this.preFit |> Option.defaultValue this.workingSystem
            Some ([ mkInfo reference; mkInfo fitted ], overlayFunctions this.targets)
        | _ -> None

// ---------------------------------------------------------------------------
// Fit-history sidecar (§G.10 item 6). The G.8 `.binz` report is written under the
// project working folder (never the repo root); `*.binz` is already in the root
// .gitignore (Part I / slice 012), so the sidecar is NOT committed — the canonical
// JSON project that references it is the committable artefact (§0 constraint 4).
// The location/extension choice goes through the SINGLE shared sidecar seam
// `Storage.Sidecar.derivedArtefactPath` (slice 016, reuse finding F2): the §J.10
// `JobRunner` that hosts this fit run and this page now agree on the `.sidecars`
// subfolder and the drift-proof `.binz` extension, decided in one place.
// ---------------------------------------------------------------------------

/// The fit-history `.binz` sidecar path, under the working folder's `.sidecars`
/// subfolder via the shared `Storage.Sidecar.derivedArtefactPath` seam (never the
/// repo root). The `.fit-history` infix names the artefact; the seam appends `.binz`.
let fitHistorySidecarPath (model : Model) (name : FileName) : FileName =
    OpticalConstructor.Storage.Sidecar.derivedArtefactPath model.workingFolder (name.addExtension (FileExtension ".fit-history"))

/// The `(path, bytes)` to write for the last completed run's G.8 report, via the
/// `.binz` serializer (`FitReport.toBinz`). `None` until a run completes.
let saveFitReport (model : Model) (name : FileName) : (FileName * SerializedData) option =
    model.report |> Option.map (fun report -> fitHistorySidecarPath model name, report.toBinz)
