# 007 state-of-the-world — Synthesis / Fit page + background jobs (Part U7)

```yaml
gates:
  berreman_unit_tests: 84
  constructor_unit_tests: 204
  ui_smoke: 1
  ui_tests: 19
```

# Where we are

Slice 007 carries Part U7 of the spec-0024 UI-wiring arc — the synthesis/fit page and
the only long-running, cancellable background job in the arc. U1 (slice 001) stood up
the root MVU loop; U5 (slice 002) added the renderer-host seam (`ChartHosts.fs`); U2–U6
(slices 003–006) wired the stack, materials, sources, and results panels. This slice
opens the fit page on demand into the root model's `fit` field, renders a new
`FitView.fs` wired to `SynthesisFitPage.Msg`, and wires the background fit run with
UI-thread-marshaled per-iteration progress and completion (§0.4), a host-held
`CancellationTokenSource` (§0.5), cooperative cancel, and the accept-confirmation gate.
The page model, the optimization seams, and the `JobRunner` background primitive
already existed and are frozen (§0.1); this slice renders and wires them.

# What's working

- Add `FitView.fs`: renders the per-iteration progress line (`progressText`), a single
  Start↔Cancel control (two states), same-row Accept/Revert with distinct CTA colours,
  the `acceptConfirmationPrompt` gate, and the fit-vs-measured `comparisonOverlay` in
  the slice-002 AvaPlot host (R-1/R-3/R-4).
- Wire the background fit in `Shell.fs`: `Fit StartFit` attaches a `Cmd` that creates a
  host-held `CancellationTokenSource` (§0.5), runs the optimization run loop off-thread,
  and marshals `ReportProgress`/`FitCompleted`/`FitFailed` onto the UI thread (§0.4).
- Run the optimization run loop as one LM iteration per cooperative `JobRunner`
  step over the frozen `LocalRefinement.refineWith`, so progress is genuinely
  per-iteration and a cancel is observed between iterations (no frozen edit, §0.1).
- Open the fit page on navigation (R-1) and route `Fit` through `SynthesisFitPage.update`;
  `Fit CancelFit` cancels the host token, resolving to `FitFailed "cancelled"` (R-3).
- Add 4 `ui-tests` fit-page tests and extend the `ui-smoke` page sweep to open the fit
  page; keep every slice gate green.

# Tests

All gates in the slice roster pass locally (run in order; first failure short-circuits):

- `build` — solution builds, 0 errors (only the pre-existing MSB3277 WindowsBase/WebView2
  conflict, NU1902 log4net, FS1125 SeriesDataTests warnings — inherited, not introduced).
- `unit-tests` (BerremanTests) — 84 passed / 5 skipped; baseline `berreman_unit_tests = 84`.
- `constructor-unit-tests` — 204 passed; the fit page keeps the pure
  `OpticalConstructor.Tests` suite Avalonia-free.
- `ui-smoke` — 1 passed: navigating to the fit page opens its model and `FitView`
  renders one frame headlessly via `Shell.view` without throwing.
- `ui-tests` — 19 passed (the prior 15 plus 4 new fit-page tests): Start dispatches
  `Fit StartFit` (R-3); `StartFit` runs off-thread and marshals per-iteration
  `ReportProgress` + `FitCompleted`, the completion refreshing the `comparisonOverlay`
  with no manual reload (AC-U7.1); a cooperative Cancel resolves to `FitFailed
  "cancelled"` leaving the prior system intact with Revert available (AC-U7.2); Accept
  routes through `acceptConfirmationPrompt` (RequestAccept → Confirm → ConfirmAccept),
  rendering Accept/Revert/Start same-row (AC-U7.3).

# Architecture

- `FitView.fitPanel` follows the slice-002..006 view precedent: it takes its sub-state
  (the `SynthesisFitPage.Model`) plus a `SynthesisFitPage.Msg` dispatch — NOT
  `RootModel`, which lives in `Shell.fs` — so it composes under the root without a
  module cycle. `Shell` passes `RootMsg.Fit >> dispatch`.
- The `CancellationTokenSource` is a `Shell.fs` host-layer module field (`fitCts`),
  never a field of the pure/serializable root model (§0.5). `SynthesisFitPage.Model`'s
  `running`/`progress` remain the single source of truth for run state.
- The optimization run loop is an outer loop in the host over the frozen
  `LocalRefinement.refineWith` (one LM iteration per `JobRunner.runIterations` step,
  resumed from the prior step's solution vector). ALGLIB's internal iterations are not
  exposed through the frozen G.1 interface, so this is the only way to surface genuine
  per-iteration progress without editing the frozen Optimization modules (§0.1). The
  loop stops when ALGLIB reports anything other than `MaxIterationsReached` or the
  `LocalRefinement.defaultMaxIterations` cap is hit; the token is checked between steps.
- Completion builds the `FitReport` through the frozen `FitQuality.fromResult`; the
  progress callback is threaded through the run loop (`step`), NOT through
  `startBackground` (whose signature is `run`/`onDone` only), per the spec.
- The fit-vs-measured overlay reuses the slice-002 `ChartHosts.scottPlotHost` and the
  Part H §H.4 `Plot1DView.renderComparison` over the `comparisonOverlay` payload — no
  new hosting seam and no recomputed geometry. The only `OpticalConstructor.Ui` changes
  are the new `FitView.fs` and the composition-root `Shell.fs`.

# Deferred

- **Target / parameter / method editor UI.** The fit page opens with empty
  parameters/targets (a fresh page); the merit-target editor, the design-parameter
  picker, and the method selector are out of this slice's threading/wiring scope. The
  headless test builds a fully-populated fit model to exercise the run path.
- **Per-ALGLIB-iteration progress hooks.** Genuine intra-`minlm` iteration reporting
  would require an ALGLIB `xrep` callback exposed through the frozen G.1 interface; the
  outer-loop-of-single-iterations approach is used instead (see Gotchas).
- **Live AvaPlot rendering headlessly.** On the headless platform `scottPlotHost` may
  degrade to the §U1.8 placeholder; the overlay renders live on the desktop host.
- The clone-swap decision (§0.2 / spec 0022 §A.9 / AC-A8) — untouched.

# Gotchas

- **The run loop re-creates ALGLIB state per outer iteration.** Each cooperative step
  calls `refineWith … maxIterations 1 …` from the prior solution vector, so ALGLIB
  state is rebuilt each iteration. This is the deliberate cost of getting per-iteration
  progress + between-iteration cancellation out of a frozen one-shot optimizer; the
  design vector genuinely advances each step (it is continued refinement, not
  from-scratch). A future slice that exposes an ALGLIB `xrep` callback through the G.1
  interface could replace it with a single `optimize` call.
- **`Fit CancelFit` does not itself flip `running`.** It cancels the host token and sets
  the cooperative `cancelRequested` flag; the background `onDone` posts `FitFailed
  "cancelled"`, which is what clears `running` and leaves the prior committed system
  intact (Revert available). The Cmd is built from the PRE-update model so the StartFit
  guard sees the prior `running` state.
- **The Start↔Cancel button and the Accept confirmation gate never both show a
  "Cancel".** The action bar's button reads "Cancel" only while running; the accept gate
  (which opens only when a candidate exists and the page is idle) reads "Start" in the
  action bar, so its own "Cancel" (CancelAccept) is unambiguous. The fit-page tests rely
  on this.
- **`ui_tests` baseline rose 15 → 19** (the 4 new fit-page tests). `berreman_unit_tests`
  (84), `constructor_unit_tests` (204), and `ui_smoke` (1) are unchanged. This is the
  final slice of the arc; the baseline is emitted for completeness.

# Changelog

- 2026-06-01 — Slice 007: wired the synthesis/fit page (Part U7) — new `FitView.fs`
  renders the per-iteration progress line, the single Start↔Cancel control, same-row
  Accept/Revert with distinct CTA colours, the `acceptConfirmationPrompt` gate, and the
  fit-vs-measured `comparisonOverlay` in the slice-002 AvaPlot host. `Shell.fs` opens the
  page on navigation (R-1), routes `Fit` through `SynthesisFitPage.update`, and attaches
  the §0.4-marshaled background-run `Cmd` (host-held `CancellationTokenSource` per §0.5;
  optimization run loop over the frozen `LocalRefinement.refineWith`; cooperative cancel
  → `FitFailed "cancelled"`). Added 4 `ui-tests`; extended the `ui-smoke` page sweep. All
  slice gates green (build 0 errors; berreman 84, constructor 204, ui-smoke 1, ui-tests 19).
