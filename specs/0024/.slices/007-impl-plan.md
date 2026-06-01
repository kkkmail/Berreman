# 007 impl-plan — Synthesis / Fit page + background jobs (Part U7)

## Approach

Slice 007 is the last UI-wiring slice and the only one with a long-running,
cancellable background job. The page model (`SynthesisFitPage`), the merit /
optimization seams (`MeritFunction`, `LocalRefinement`, `FitQuality`,
`DesignParameters`, `AlglibAdapter`), and the background-dispatch primitive
(`JobRunner.startBackground` / `runIterations`) all already exist and are frozen
(§0.1). This slice (a) opens the page on demand into the root model's `fit` field,
(b) renders a new `FitView.fs` wired to `SynthesisFitPage.Msg`, and (c) wires the
background run with UI-thread-marshaled progress/completion, cooperative cancel, and
the accept-confirmation gate.

### The optimization run loop (R-2)

ALGLIB's internal LM iterations are not exposed through the frozen G.1 interface, so
genuine *per-iteration* progress is produced by an outer loop in the host layer over
the frozen `LocalRefinement.refineWith` seam: each cooperative `JobRunner.runIterations`
step runs ONE LM iteration (`refineWith method 1 epsX …`) resumed from the prior
step's solution vector, reports `(iteration, χ²)` via a UI-thread-marshaled
`ReportProgress`, and the loop stops when ALGLIB reports anything other than
`MaxIterationsReached` (converged / step-too-small) or the outer cap is hit. The
token is checked between steps, so a cancel stops within one inter-step check and
resolves to `FitFailed "cancelled"`. No frozen module is edited.

### §0.4 / §0.5 discipline

- The `CancellationTokenSource` is a host-layer module field in `Shell.fs`
  (`fitCts`), never a field of the root model (§0.5). `SynthesisFitPage.Model` is the
  single source of truth for run/progress state.
- Every progress/completion dispatch from the background worker is marshaled onto the
  UI thread via `Dispatcher.UIThread.Post` (§0.4) — reusing the slice-003 node-solve
  marshaling discipline.

## Files to modify

New:
- `OpticalConstructor.Ui/FitView.fs` — the fit page view: per-iteration progress
  line, single Start↔Cancel button (two states), same-row Accept/Revert/Cancel with
  distinct CTA colours, the accept-confirmation gate, and the fit-vs-measured overlay
  (`comparisonOverlay`) in the slice-002 AvaPlot host.
- `OpticalConstructor.Ui.Tests/FitPanelTests.fs` — headless fit-page view tests.

Edited:
- `OpticalConstructor.Ui/Shell.fs` — `RootMsg.Fit`; open the page on navigation;
  the marshaled `StartFit`/`CancelFit` effectful `Cmd`s; route `Fit` through
  `SynthesisFitPage.update`; render `FitView` in `pageBody`.
- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — register `FitView.fs`.
- `OpticalConstructor.Ui.Tests/OpticalConstructor.Ui.Tests.fsproj` — register the test.
- `OpticalConstructor.Ui.Tests/SmokeTests.fs` — navigate to open the fit page so
  `FitView` renders one frame headlessly (ui-smoke).

## Risks

- **Threading-sensitive.** A direct cross-thread dispatch or a run-state copy in the
  root model is a defect (§0.4/§0.5). Mitigated by reusing the slice-003 marshaling
  pattern and keeping the CTS + run state out of the root model.
- **Headless fit determinism.** The `StartFit` Cmd test runs a real (tiny) ALGLIB fit
  whose single target the seed system already satisfies, so it converges in ~1
  iteration; the test pumps the dispatcher for the marshaled `FitCompleted` exactly as
  `ConstructionEditTests` pumps for `NodeSolved`.
