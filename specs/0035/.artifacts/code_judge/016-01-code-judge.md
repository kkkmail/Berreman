# Code judge -- 016.slice-md cycle 1

## Inputs read

- Slice spec: `C:\GitHub\Berreman\specs\0035\.slices\016.slice-md`
- State-of-the-world: `C:\GitHub\Berreman\specs\0035\.slices\016-state-of-the-world.md`
- Impl-log: `C:\GitHub\Berreman\specs\0035\.slices\016-impl-log.md`
- Gate results: build pass / unit-tests pass / constructor-unit-tests pass / ui-smoke pass / ui-tests pass
- Critic critiques: (none — no architecture/reuse critic ran this cycle)

## Rationale

Every deterministic gate is green (`build`, `unit-tests`,
`constructor-unit-tests`, `ui-smoke`, `ui-tests`), and no critic
critiques were supplied this cycle, so the decision turns on
slice-spec conformance, SoW/impl-log fidelity, and test coverage of
the new public surface. I verified all three against `git diff HEAD`.

The slice's binding requirements are met. (1) An embeddable form of
the dual-axis chart now exists in `OpticalConstructor.Controls`:
`EmbeddedChart.create` (new `EmbeddedChart.fs`) builds a live
`ScottPlot.Avalonia.AvaPlot` through the newly-extracted shared
`ChartPlot.renderCartesian` rebuild path (`ChartWindow.fs`), reusing
`ExperimentChart`/`ChartStyle` with n on the LEFT axis and k on the
RIGHT — exactly the slice's "reuse ExperimentChart / ChartStyle and
the ChartWindow rebuild path" instruction. (2) Both host sites embed
it: `MaterialEditorView.previewCanvas` and
`TableAndElementRotationView.materialViewPanel` now call
`EmbeddedChart.create … (NkDispersionChart.nkDispersionStyle chart)`,
replacing the prior `inlineCanvas` calls. (3) The pure model builders
`nkDispersionChart`/`nkDispersionStyle` are untouched (only their
now-unused `Avalonia*` opens were trimmed). (4) The primitive
`NkDispersionChart.inlineCanvas` and its private canvas helpers are
deleted; a repo grep confirms no product/test *code* still references
it (only doc comments remain).

The `ChartWindow` refactor is behaviour-preserving, which matters
because a regression there would silently break the interactive
pop-out. The surviving `applyLegendAndFonts`/`applyAxisFormat`/
`applyAxisLimits`/`applySeriesStyle` closures delegate to the new
`ChartPlot` functions while still reading the mutable
`style`/`scatters`/`polar` at call time; `applySeriesStyle` forwards
`(not polar)` as `assignAxis`, preserving the original guard that
skips Y-axis re-binding under the polar projection. The cartesian
rebuild branch now calls `ChartPlot.buildCartesianScatters`, which
reproduces the original per-series axis-side binding and legend text.
The green `build` gate corroborates that this all compiles clean under
`--warnaserror`.

Test coverage of the new public surface satisfies the `done-green`
criterion. `EmbeddedChartTests.fs` adds six `ui-smoke` render proofs
that exercise `EmbeddedChart.create` (and, transitively,
`ChartPlot.renderCartesian`): the embedded control carries exactly the
n (left) / k (right) scatters and rasterizes one real Skia frame via
`Plot.GetImage` for a non-dispersive entry, a dispersive built-in, and
each of the four transcendental model kinds (Tauc–Lorentz, Gaussian
oscillator, Forouhi–Bloomer, Brendel–Bormann); the real Material
editor preview embeds and renders the AvaPlot for a non-dispersive
default and a transcendental pick; and the real Materials View panel
embeds and renders it for a non-dispersive (glass) and a dispersive
(silicon) entry driven through the workbench. This directly discharges
the slice's "render one frame carrying the embedded chart, for a
dispersive (including transcendental) and a non-dispersive entry"
testing plan and the "renders under the headless ui-smoke frame
without throwing" acceptance clause. The `try`/`with` graceful-degrade
placeholder in `EmbeddedChart.create` is defence-in-depth; the tests
assert the live AvaPlot path (they `Assert.Fail` if the host degraded
to the placeholder), so the render path is genuinely proven, not
merely tolerated.

The SoW and impl-log line up with the diff, including the attempt-02
changelog entry describing the test-file-only opens fix (`open
Avalonia.Headless`, dropping the bare `open Avalonia.FuncUI`) that the
green build now reflects. The `.manifest.state.json` churn is
supervisor bookkeeping, not worker scope. No requirement is unmet, no
layering or duplication problem is present (the refactor removes
per-host duplication rather than adding it), and nothing warrants a
re-spawn or escalation. Verdict: `done-green`.

## Verdict

done-green

```json
{"verdict": "done-green", "rationale": "All five gates pass and no critic ran. The diff meets every slice requirement: EmbeddedChart.create exposes an embeddable AvaPlot host built through the newly-shared ChartPlot.renderCartesian (n-left/k-right), embedded at both host sites (MaterialEditorView preview and Materials View panel) over the unchanged nkDispersionChart/nkDispersionStyle builders; the primitive NkDispersionChart.inlineCanvas and its helpers are deleted with no remaining code references. The ChartWindow refactor is behaviour-preserving (closures still read mutable style/scatters/polar; applySeriesStyle forwards not-polar as assignAxis). Six new ui-smoke tests exercise the new public surface end-to-end, rasterizing a real frame for non-dispersive, dispersive, and all four transcendental model kinds and for both real host sites. SoW and impl-log align with the diff.", "retry_hint": ""}
```
