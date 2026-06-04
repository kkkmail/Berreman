# Code judge -- 004.slice-md cycle 1

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0024\.slices\004.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0024\.slices\004-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0024\.slices\004-impl-log.md`
- Gate results: build pass, unit-tests pass, constructor-unit-tests pass, impl-log-structure pass, state-of-world-structure pass, ui-smoke pass, ui-tests pass
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0024\.artifacts\architecture_critic\004-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0024\.artifacts\reuse_critic\004-02-reuse-critic.md`

## Rationale

Every gate is green and the slice meets each acceptance criterion it owns.
R-1 (browse/filter through `MaterialLibrary.byCategory` / `byNameContains`,
no new index), AC-U3.1 (dispersion preview routed to the WebView2 host with
zero `AvaPlot`), and AC-U3.2 (drop builds the edit via the frozen
`StackEditor.layerMaterialDrop` seam, dispatches
`Construction (EditStack (SetLayerMaterial …))`, thickness unchanged) are all
satisfied and directly exercised by the four new `ui-tests` in
`MaterialsPanelTests.fs`. The new public surface — `materialsPanel`,
`materialDrop`, `filteredEntries`, `update`, the `Filter` sub-model — is
covered: the list/filter test drives R-1, the WebView2/no-`AvaPlot` test
drives AC-U3.1, and the drop test drives AC-U3.2 plus the unknown-id no-op.
The done-green test-coverage criterion is met.

The single substantive deviation is R-2's literal wording ("render the charts
returned by `MaterialPreview.show*`"). I verified this independently:
`Charting.plotDispersion` ends in `|> Chart.show` (`Charting.fs:92`) and
returns `unit`, and the whole `plot*` / `show*` family inherits that, so the
spec's premise that these return `Plotly.NET.GenericChart` is false. The
worker rerouted through the public data calculator
`Analytics.Variables.calculateN11Re` + `Chart.Line` (the identical
construction minus the `Chart.show` side-effect), still sourcing the spectral
range through the real `MaterialPreview.spectralRange` seam, and recorded the
call in the SoW/impl-log `Gotchas`. This is exactly the "pick the most
defensible reading and record it" behavior the project prompt mandates. The
architecture critic concurs and explicitly asked the judge to bless it so it
is not re-litigated in a later preview slice: **I accept the
`show*`-returns-`unit` deviation.** AC-U3.1's intent (Plotly chart → WebView2
host, never an `AvaPlot`) is fully honoured and nothing is lost today.

§0.1/§0.2/§0.5 hold: the only edits to existing files are `Shell.fs` and the
two `.fsproj` files (all new-from-this-arc or explicitly permitted); no frozen
model/update/projection module is touched; the `Filter` sub-model is pure and
serializable with no renderer handle in the model. The SoW and impl-log line
up with the diff, including the candid §7 contradiction write-up.

The advisory findings do not reach `route-back-to-worker` ground. Reuse F1
(verbatim copy of `ConstructionView.selectedNode`) is a genuine 4-line
duplication of a "view-total" invariant, but the project prompt forbids no
such duplication, the underlying lookup seam `ConstructionPage.tryGetNode` is
already reused, and the reuse critic itself states it "does not by itself
warrant a re-spawn." F2/F3/F4 are optional consolidations where the worker
largely followed existing repo precedent (privatized per-module test helpers,
no shared seam exists to call). The architecture critic's drag-on-press
interaction risk is real but headless-invisible, depends on no AC this slice
asserts, and is correctly a followup for the first slice that runs the UI
live. None of these is a correctness defect or an unmet slice requirement.

With all gates green, both critics recommending ship, full AC test coverage,
the one spec divergence being a verified-correct and documented §7 call, the
defensible verdict is `done-green`. F1 and F2 are recorded here as low-cost
follow-up reuse wins for a future slice, not blockers.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All seven gates pass; AC-U3.1/AC-U3.2/R-1/R-3 are met and exercised by the four new ui-tests; new public surface (materialsPanel, materialDrop, filteredEntries, update, Filter) is covered. The one spec divergence -- R-2's premise that MaterialPreview.show* return GenericChart -- is verified false (they return unit via Chart.show at Charting.fs:92); the worker's reroute through calculateN11Re + Chart.Line honours AC-U3.1's intent and is documented per the project's record-the-call rule, which I accept. Binding constraints 0.1/0.2/0.5 hold; no frozen module edited. Reuse F1 (verbatim selectedNode copy) and F2 (chartRow row chrome) are advisory low-cost follow-ups, not project-forbidden duplications; the drag-on-press UX risk is headless-invisible and AC-independent. No unmet slice requirement remains.", "retry_hint": ""}
```
