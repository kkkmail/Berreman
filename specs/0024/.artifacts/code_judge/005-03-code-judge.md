# Code judge -- 005.slice-md cycle 1

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0024\.slices\005.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0024\.slices\005-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0024\.slices\005-impl-log.md`
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / impl-log-structure pass / state-of-world-structure pass / ui-smoke pass / ui-tests pass
- Critic critiques:
  - `C:\GitHub\Berreman\specs\0024\.artifacts\architecture_critic\005-01-architecture-critic.md`
  - `C:\GitHub\Berreman\specs\0024\.artifacts\reuse_critic\005-02-reuse-critic.md`

## Rationale

All seven gates in the slice roster pass, and the slice delivers its binding
contract. Both acceptance criteria are met with dedicated coverage: AC-U4.1
(a field edit routes the frozen `SourceEditorView.update` and the live Stokes
readout refreshes) and AC-U4.2 (a preset routes `PolarizationPicker.applyPreset`
and the ellipse/Poincaré refresh) each have a `ui-tests` case, lifting the
`ui_tests` count 9 → 13. R-1 surfaces all six `SourceMsg` cases plus the
five-way unit selector (`SourceView.fs:117-147`); R-2 offers all six presets
(`SourceView.fs:163-183`) and reads the live projections straight from the
picker (`SourceView.fs:194-208`). The Non-requirement mapping holds —
ellipse → `ChartHosts.scottPlotHost` (AvaPlot), Poincaré → `webView2Host`
(WebView2), never reversed (`SourceView.fs:221-233`) — and the headless routing
test asserts exactly one WebView2 placeholder, the only headless-observable
proof of the mapping. The freeze mandate (§0.1) is honoured: the only
`OpticalConstructor.Ui` changes are the new `SourceView.fs` and the
new-from-this-arc `Shell.fs` composition root; §0.5 holds (`source : SourceSpec`
is pure; live controls are host-layer locals); §0.4 is vacuously satisfied (the
`RootMsg.Source` case is pure, no effectful `Cmd`). The SoW and impl-log
reconcile with the diff — the architecture critic verified this independently and
its bottom line is "I would ship this."

The reuse critic raises three findings, all explicitly advisory with "no gate
authority." I verified the substantive one, F1, against source: `SourceView.plotRow`
(`SourceView.fs:236-245`) is a line-for-line copy of `ChartView.chartRow`
(`ChartView.fs:162-172`) — same `StackPanel`/`TextBlock`/`Border` shape, the same
baked-in `220.0` plot height, and the carried-over doc comment. This is genuine
duplication and worth consolidating, but it is not an unmet slice requirement: the
slice's reuse discipline targets *renderer hosting*, and the actual hosting
(`scottPlotHost`/`webView2Host`) is reused — only the titled-row chrome wrapping it
was copied. F2 (`radToDeg` re-derives the engine's `degree` conversion,
`SourceView.fs:81`) is a real but smaller single-seam divergence in a display-layer
helper, numerically equivalent to `/ Berreman.Geometry.degree`. F3 (a third
invariant-culture float parser) the critic itself would not re-spawn over — the
two existing copies sit inside frozen Storage modules (§0.1), and the UI-vs-import
layering is a defensible reason to keep them separate.

Weighing the rubric: every `done-green` condition is satisfied — all gates green,
no critic finding implies an unmet slice-spec requirement, SoW/impl-log align with
the diff, and every new public surface is exercised by a test in the diff. The
`route-back-to-worker` triggers are not cleanly met: F1 is not "duplication the
project prompt's conventions forbid" — the Berreman project prompt declares no
anti-duplication convention (its Code style section defaults to standard F# and
floating-point-tolerance precedent only). The remaining lever is the discretionary
"one cheap re-spawn away" clause, but the system prompt is explicit that a
non-empty critique is not automatic grounds for re-spawn and that minor,
nice-to-have findings get noted and the cycle moves on. F1/F2 are maintainability
cleanups, not correctness, architecture, or contract defects, and the architecture
critic — the structural authority here — would ship as-is. Blocking an
otherwise-complete, all-green, spec-faithful render-and-wire slice over a nine-line
private helper dedup is the over-strictness that clause guards against.

I am therefore calling this `done-green` and recording F1+F2 prominently as the
recommended consolidation for a future touch: extract one shared titled-plot-row
helper (the reuse critic's `ChartHosts.plotRow title host` is the natural home,
next to the adapters every caller pairs it with) so `ChartView` and `SourceView`
both delegate and the `220.0` constant lives once, and while there route
`radToDeg` through `Berreman.Geometry.degree` (already `open`ed at
`SourceView.fs:29`). F3 is accepted as a conscious, layering-justified separation.
None of these rise to a re-spawn this cycle.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All seven gates pass; R-1/R-2 and AC-U4.1/AC-U4.2 are met with dedicated ui-tests (9->13); ellipse->AvaPlot, Poincare->WebView2 mapping is held and headless-asserted; freeze mandate (§0.1) and §0.5 honoured; SoW/impl-log reconcile with the diff and the architecture critic would ship. The reuse critic's findings are advisory: F1 (SourceView.plotRow is a verbatim copy of ChartView.chartRow incl. the 220.0 magic height) is genuine duplication but not an unmet requirement and not duplication the project prompt forbids (it declares no anti-dup convention); F2 (radToDeg re-derives Geometry.degree) is a small display-layer divergence; F3 (third invariant-culture float parser) the critic itself would not re-spawn over. Recommended future-touch cleanup: lift a shared titled-plot-row helper (e.g. ChartHosts.plotRow) so ChartView and SourceView both delegate and the 220.0 constant lives once, and route radToDeg through Berreman.Geometry.degree. None rise to a re-spawn.", "retry_hint": ""}
```
