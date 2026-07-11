# State of the world — step 032 (IMPLEMENT: two-pane Material editor + tabbed multi-curve preview)

## Where we are

Spec 0038 Part I's Optical Constructor arc. The Material editor (`MaterialEditorView` over the Domain
`MaterialComplexityEditor` ladder) is now a **two-pane vertical split** — the identity/ladder pane
(scrolling, actions pinned below) beside a **full-height tabbed multi-curve chart** (per-principal-axis
n/k always, Gyration when active, μ when magnetic) — delivered across attempts 01–02. This retry
(attempt 03) closes the one gap the review flagged: the **per-series show/hide toggle** shipped without
a direct test. This round adds exactly that test (and the minimal exposure it needs); the two-pane
layout and the chart builders — correct and well-covered — are untouched.

## What's working

- Test the per-series show/hide toggle end to end in one focused pure `[<Fact>]`: the
  `ToggleSeriesVisibility` handler round-trips a `UiIds.seriesToggle "nk" "n₁"` key in and out of
  `m.hiddenSeries` through `update`.
- Prove the lowering: `applyHidden` flips exactly the toggled curve — hiding `n₁` makes series index 0
  invisible and leaves 1..5 visible; hiding `k₂` flips index 4 (the index is read from the series name,
  so a wrong index cannot pass); the same key under a mismatched tabCode hides nothing (so a wrong
  tabCode cannot pass either).
- Expose `MaterialEditorView.applyHidden` (drop `private`) so the test asserts against the real
  lowering rather than a re-derivation — a hint-sanctioned, behaviour-neutral change.

## Tests

Per Invariant 6 the worker runs no gates; the arc-runner gate engine is the sole authority. As
advisory diagnostic verification — and heeding the attempt-02 lesson that only a real `dotnet build`
proves a module compiles — the worker built and ran the two touched-project test sets:

- **`build`** — `dotnet build Berreman.slnx -c Release` → Build succeeded, 0 errors (only the exempt
  third-party `NU1701` Wolfram.NETLink advisories remain).
- **`ui-tests`** (`Category!=ui-smoke`) — 432/432 passed (was 431; the one new pure test is the +1).
- **`ui-smoke`** (`Category=ui-smoke`) — 164/164 passed (unchanged — this round adds no headless test).

The solver `unit-tests` (119) and `constructor-unit-tests` (636) projects are untouched by this slice
and were not re-run. The new test is PURE (no window), so it counts under `ui-tests`, not `ui-smoke`.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 636
  ui_smoke_tests: 164
  ui_tests: 432
```

## Architecture

- **The per-series toggle is proved at both ends against real code.** The `ToggleSeriesVisibility`
  handler (pure `Set` flip on `hiddenSeries`) is exercised through `update`; the `applyHidden` lowering
  (`hiddenSeries` key → `ChartStyle.setSeriesVisible i false`, keyed by `UiIds.seriesToggle tabCode
  seriesName`) is exercised directly. Making `applyHidden` non-private keeps the test asserting the
  shipped mapping instead of a copy — the deliberate, minimal exposure the retry hint calls for.
- **No production behaviour changed this round.** The only source edit is the visibility of a helper
  whose single call site (`chartTab`) is unaffected; the layout, tab logic, and builders are as
  delivered in attempts 01–02.
- (Unchanged, from earlier rounds) One n/k builder reused by the editor tab and the Materials View
  panel; tabs track the ladder toggles in edit mode and the assembled tensors in view-only mode;
  layout via a `3*,Auto,2*` Grid with a vertical `GridSplitter`.

## Deferred

- **Interactive legend / axis formatting for the embedded tabs.** The pop-out `ChartWindow` already
  offers full per-series colour / thickness / axis-range editing; the embedded preview keeps the
  read-only shared render plus the show/hide toggles. Unchanged this round.
- **View-only presets driving optional tabs by data.** The classifiers exist (`hasGyration` /
  `hasMagnetic`) but no shipped view-only preset is magnetic; broadening preset coverage is separate.

## Gotchas

- **`ChartStyle` is a TOP-LEVEL module in `OpticalConstructor.Controls`** (a sibling of the
  `ExperimentChart` module, NOT nested in it). Qualify it as `OpticalConstructor.Controls.ChartStyle.X`;
  `…ExperimentChart.ChartStyle.X` binds `ExperimentChart` to the like-named TYPE and fails to compile.
- **Never `open OpticalConstructor.Controls.ChartStyle` in `MaterialEditorWindowTests.fs`.** Its
  `defaultState` would shadow the `MaterialComplexityEditor.defaultState` the file uses unqualified
  everywhere (the attempt-02 collision). The test references `ChartStyle` fully qualified instead.
- **The n/k series names are subscript-digit unicode** (`n₁…k₃`). The toggle key must match the
  builder's exact glyphs; a mismatch fails the lowering assert loudly rather than passing silently.
- (Unchanged) the n/k builder emits SIX series (`nSeriesIndex`/`kSeriesIndex` are gone — select k
  curves by the `"k"` name prefix); an isotropic medium draws three coincident n and three coincident k
  curves; the left ladder pane is inside a `ScrollViewer` and the window is 1400×1000 so a single-aspect
  ladder fits the viewport without scrolling (keeping the id-driven `clickOn` ui-smoke tests hit-testable).
- The task file's system-prompt path is stale (`.../implement_worker.system-md` at the repo root); the
  real file is under `.../src/ai_strategy_generator/multistep/`.

## Changelog

- 2026-07-11 — step 032 (attempt 03, retry): added one focused pure test for the per-series show/hide
  toggle — the `ToggleSeriesVisibility` handler round-trip through `update` plus the `applyHidden`
  lowering (right curve / wrong-index-can't-pass / wrong-tabCode-can't-pass); exposed
  `MaterialEditorView.applyHidden` (non-private) so the lowering is asserted against real code. Build
  0 errors; ui-tests 431 → 432; ui-smoke 164 unchanged.
- 2026-07-11 — step 032 (attempt 02, retry): fixed the two `build`-gate compile errors from round 1 —
  added `open Berreman.Constants` to `NkDispersionChart.fs` and qualified
  `MaterialComplexityEditor.defaultState` in `NkDispersionChartTests.fs`. Build succeeds (0 errors);
  ui-smoke 164/164 and ui-tests 431/431 pass.
- 2026-07-11 — step 032: reworked the Material editor into a two-pane vertical-GridSplitter layout
  (identity + ladder in a ScrollViewer with actions/status pinned below | full-height tabbed preview);
  replaced the single n/k curve with a per-principal-axis n/k chart and added Gyration (when active) and
  μ (when magnetic) tabs that track the ladder toggles, all embedded through EmbeddedChart and rendering
  headless; retired the old 2-series builder (the Materials View panel reuses the new per-axis n/k
  builder); added builder + headless tab tests.
