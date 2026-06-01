# Architecture critique -- 007.slice-md cycle 1

## Summary

Clean implementation with one or two minor issues. The threading discipline
the slice is built around (§0.4 UI-thread marshaling, §0.5 non-serializable
handle out of the root model) is correctly honoured and is consistent with the
slice-003 node-solve precedent. The single most important finding: on the arc's
explicitly highest-risk path — cooperative cancellation — the test only
exercises the pure `update` reduction, not the real `cts.Cancel()` →
`runIterations` → `CancelledWith` → `FitFailed "cancelled"` wiring that the
slice actually adds.

## Layering

No violations. The only edited `OpticalConstructor.Ui` modules are the
composition root `Shell.fs` and the `.fsproj` registration — neither is in the
§0.1 frozen list (which names `SynthesisFitPage`, `JobRunner`, `Workspace`,
the `Charts/`/`Sources/` builders, etc., all of which are consumed unchanged).
`FitView.fs` is a new sibling `*View.fs`, exactly the seam §0.1 prescribes for
new view code. `FitView.fitPanel` takes its sub-state (`SynthesisFitPage.Model`)
plus a `Fit.Msg` dispatch rather than `RootModel`, so it composes under the root
with no module cycle — matching the slice-002..006 view precedent. The new
`open OpticalConstructor.Optimization[.OptimizationInterface]` in `Shell.fs` is
the correct direction (composition root depends on the frozen optimizer, never
the reverse).

## Separation of concerns

The run loop is appropriately placed in the host (`startFitCmd`), not in the
frozen page model: the page model stays the pure `running`/`progress` source of
truth while the non-serializable `CancellationTokenSource` and the
ALGLIB-driving loop live in `Shell.fs`. `marshalFit` is a single, named
marshaling seam reused by both the progress and completion dispatches, which
keeps the §0.4 obligation in one place rather than scattered `Post` calls.

## Consistency

Strong fit with the established patterns. `startFitCmd` mirrors
`nodeSolveCmd`'s shape (off-thread work, then `Dispatcher.UIThread.Post` to
marshal the result message back); the difference — a `CancellationTokenSource`
held in a module field rather than the CTS-free `Task.Run` of node-solve — is
warranted because the fit is cancellable and node-solve is not. The CTA-colour
and same-row action-bar conventions are applied the way the UX commitments
describe.

One small naming slip: in `FitView.overlaySection` the destructured
`comparisonOverlay` payload is bound as `(systems, fns)`, but the first element
is a `FixedInfo list`, not a list of systems (the frozen seam documents it as
the FixedInfo overlay list). Renaming to `infos`/`overlay` would avoid a
misleading read at the one place the seam is consumed. Cosmetic only.

## Spec fit

Matches what the slice asked for. R-1 (open the page into `fit` on navigation,
once), R-2 (host-held CTS, run-loop progress threaded through `step` not
`startBackground`, marshaled completion), R-3 (single Start↔Cancel control,
`cts.Cancel()` on the host token, `progressText` line), and R-4 (same-row
Accept/Revert gated by `canAccept`/`canRevert`, Accept routed through
`acceptConfirmationPrompt`, overlay in the slice-002 `scottPlotHost`) are all
present. The §0 binding constraints are respected: no frozen edit, public FuncUI
DSL only, no clone reference.

Two scoped-down behaviours worth noting, both defensible. (1) `openFit` opens the
page with empty parameters/targets/initial; a user-facing Start with no targets
will fall straight through to `FitFailed`. The slice's own Deferred list and
Scope ("this slice wires the threading") sanction this, and the headless test
builds a fully-populated model to exercise the real run — so it is documented
under-delivery, not a defect. (2) The overlay renders only the first photometric
channel (`fns |> head`, fallback `OpticalFunction.R`) because
`Plot1DView.renderComparison` takes a single `OpticalFunction`; that matches the
existing `ResultsView`/`Workspace`/`ChartView` calls to the same seam, so it is
consistent rather than a regression, even though `comparisonOverlay` surfaces the
full channel list.

## Evolvability

The overlay's x-domain is hardcoded — `Analytics.StandardLightVariables.wavelength200to800Range 12`
— and ignores the targets' actual `samplePoint`s. With the targets editor
deferred this is harmless today, but it leaves a future slice that wires real
measured targets needing to revisit `overlaySection` so the fit-vs-measured plot
actually spans the measured domain rather than a generic 200–800 nm sweep. A
brief comment marking the range as a placeholder pending the targets-editor slice
would signal the intent; it is not cornering, just a known follow-up.

The single module-level `fitCts` encodes "one cancellable run at a time," which
is exactly the §G.10 non-requirement ("NO job-queue / scheduling abstraction").
If a later slice adds a second long-running job (e.g. a cancellable sweep), this
one-global-per-job-type shape will want consolidating, but that is out of scope
here and the slice is right not to invent the abstraction now.

## Risks

- **Cancel path is unit-tested, not integration-tested (key finding).** The
  `cancel during a run` test calls `SynthesisFitPage.update` directly with
  `StartFit` → `CancelFit` → `FitFailed "cancelled"` and asserts the pure model
  transitions. It never drives the wiring this slice introduces: that
  `cancelFitCmd` calls `cts.Cancel()`, that `runIterations` observes the token
  between iterations and yields `CancelledWith`, and that `onDone` maps
  `CancelledWith` → `FitFailed "cancelled"`. By contrast the completion test
  *does* run the real background `Cmd` end to end. Given the slice flags
  threading as its only high risk and cancellation as the live cooperative path,
  the asymmetry is notable — the `CancelledWith` arm at `Shell.fs:263` is covered
  only by reading. A test that starts the real `Cmd`, cancels the host token, and
  pumps until `FitFailed "cancelled"` lands would close it.

- **`fitCts` is never disposed or cleared.** Each `StartFit` does
  `fitCts <- Some (new CancellationTokenSource())`, overwriting the prior handle
  without `Dispose()`, and nothing nulls the field on completion. The leak is one
  `CancellationTokenSource` per run (low severity), and a `CancelFit` arriving
  after a run has finished would call `Cancel()` on a stale, unobserved token
  (harmless). Disposing/clearing on `onDone` would make the host field's
  lifecycle explicit; not blocking.

## Bottom line

I would lean ship. The architecturally load-bearing requirements — §0.4
marshaling, §0.5 handle placement, no frozen edits, the new-sibling-view seam —
are all met and the code reads consistently with the prior slices. The findings
are a test-coverage gap on the cancel path (the one I would most want addressed,
given the slice's own risk framing) plus two cosmetic/cleanup notes (CTS
disposal, overlay range/naming). None of these blocks correctness of what
landed; the judge may reasonably accept as-is and file the cancel-path
integration test as a follow-up, or hold one cycle to add it. The verdict is the
judge's; my read is "accept with the cancel-path test noted."
