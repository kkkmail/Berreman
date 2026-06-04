# 004 state-of-the-world — Materials panel (Part U3)

```yaml
gates:
  berreman_unit_tests: 84
  constructor_unit_tests: 204
  ui_smoke: 1
  ui_tests: 9
```

# Where we are

Slice 004 carries Part U3 of the spec-0024 UI-wiring arc. U1 (slice 001) stood up the
root MVU loop and the read-only `stack` panel; U5 (slice 002) added the renderer-host
seam (`ChartHosts.fs`) and chart panel; U2 (slice 003) made the `stack` panel editable
with the §0.4-marshaled async node-solve. This slice wires the `materials` panel: it
browses the material library (filtered through the existing library seams), previews a
material's dispersion hosted in the slice-002 WebView2 adapter (Plotly, never an
`AvaPlot`), and assigns a material to a layer by drag-drop through the frozen
`StackEditor.layerMaterialDrop` seam — dispatching `Construction (EditStack …)` and
resolving nothing itself.

# What's working

- Add `MaterialsView.fs`: the `materials` panel browses `MaterialLibrary` entries filtered
  through the existing `byCategory` / `byNameContains` seams (no new search index), with a
  pure `Filter`/`MaterialsMsg`/`update` sub-model held in `RootModel`.
- Host the dispersion preview in the slice-002 `ChartHosts.webView2Host` (Plotly
  `GenericChart` built from the reused `Analytics.Variables.calculateN11Re` data), never an
  `AvaPlot`; it degrades to the §U1.8 "unavailable" placeholder headlessly (AC-U3.1).
- Wire drag-drop: library entries are real Avalonia drag sources carrying the stable
  material id; the selected node's layer rows are `allowDrop`/`onDrop` targets that route
  `StackEditor.layerMaterialDrop` and dispatch `Construction (EditStack …)`, thickness
  unchanged (AC-U3.2). The view resolves no material itself.
- Wire the `materials` panel id + a `Materials` `RootMsg` case into `Shell.fs`; add 4
  `ui-tests` materials-panel tests; keep every slice gate green.

# Tests

All gates in the slice roster pass locally (run in order; first failure short-circuits):

- `build` — solution builds, 0 errors (only pre-existing MSB3277 WindowsBase/WebView2
  version-conflict + FS1125 warnings, inherited not introduced).
- `unit-tests` (BerremanTests) — 84 passed / 5 skipped; baseline `berreman_unit_tests = 84`.
- `constructor-unit-tests` — 204 passed; the materials panel keeps the pure
  `OpticalConstructor.Tests` suite Avalonia-free.
- `ui-smoke` — 1 passed: the `materials` panel renders one frame headlessly via
  `Shell.view`; the WebView2 preview degrades to the placeholder without throwing.
- `ui-tests` — 9 passed: the prior 5 plus 4 new materials-panel tests — the library list
  renders and a Glass category filter narrows it (R-1); the dispersion preview routes to
  the WebView2 host with no `AvaPlot` in the tree (AC-U3.1); a drop builds
  `EditStack (path, SetLayerMaterial (0, _))` via `layerMaterialDrop` and the frozen
  `update` leaves layer 0's thickness unchanged (AC-U3.2); and the pure `update`/
  `filteredEntries` route filter edits while an unknown drop id dispatches nothing.

# Architecture

- `MaterialsView.materialsPanel` follows the slice-002/003 view precedent: it takes its
  sub-state (the `MaterialLibrary`, the `Filter` UI state, the `ConstructionPage.Model`
  whose layer rows are drop targets) plus dispatchers — NOT `RootModel` — so it composes
  under `Shell` without a module cycle. `Shell` passes `RootMsg.Materials >> dispatch`
  (filter/selection) and `RootMsg.Construction >> dispatch` (the drop edit).
- The materials filter is a pure, serializable `RootModel` sub-model
  (`MaterialsView.Filter`) with its own `MaterialsMsg` + pure `update`, the same shape as
  `ChartView`'s chart sub-state. No renderer handle / token source enters the model (§0.5).
- Filtering reuses ONLY the existing `MaterialLibrary.byCategory` / `byNameContains` seams,
  composed by a plain set intersection — no new search index (R-1 / Non-requirements).
- Drag-drop resolution flows through the single seam `StackEditor.layerMaterialDrop`
  (→ `MaterialLibrary.resolveMaterial`); the view never resolves a dragged id itself. The
  drop logic is factored into `MaterialsView.materialDrop`, which the `onDrop` handler
  wraps and the headless test drives directly.
- The dispersion preview is hosted through the slice-002 `ChartHosts.webView2Host`
  (Plotly/WebView2), reusing the renderer-host seam rather than embedding its own host
  (Non-requirements / §0.1). No frozen core module is edited; the only `OpticalConstructor.Ui`
  changes are the new `MaterialsView.fs` and the new-from-this-arc `Shell.fs` composition root.

# Deferred

- **Active-wavelength source lift.** The preview and a drop resolve dispersive entries at
  a single reference wavelength (`Templates.defaultLight.waveLength`, 550 nm). The
  model-driven active wavelength from the project source (a later part's `OpticalSystem →
  FixedInfo` lift) is deferred; until then the seed source's canonical light is used.
- **Richer preview.** The preview shows one trace (Re[ε₁₁]); the full `show*` family
  (Xi/N22/N33/Rho…) and a spectral-unit toggle are later UI polish. `MaterialPreview` and
  the engine calculators cover them when wired.
- **Live WebView2 rendering.** On this `net10.0` target `ChartHosts.tryHostPlotly` returns
  `None` (no Avalonia-hostable WebView2 binding), so the preview is the placeholder until
  the deferred Windows-desktop WebView bridge lands (slice-002 carry-over, unchanged here).
- The clone-swap decision (§0.2 / spec 0022 §A.9 / AC-A8) — untouched.

# Gotchas

- **§7 skepticism rule — the spec's `MaterialPreview.show*` premise is wrong.** The slice
  (and claim-check) assert `MaterialPreview.show*` return `Plotly.NET.GenericChart`. They
  do NOT: each `show*` delegates to `Analytics.Charting.plotDispersion`, which pipes
  through `Chart.show` (`Charting.fs:92`) and returns **`unit`** — it opens a browser
  window and is unembeddable (and unsafe to call in a headless render). The whole
  `Charting` `plot*` family ends in `Chart.show`. So the preview cannot consume `show*`.
  It instead reuses the public engine DATA calculator `Analytics.Variables.calculateN11Re`
  (no dispersion re-derived) and constructs the chart with `Chart.Line` — exactly the
  construction `plotDispersion` performs, minus the `Chart.show` side-effect — and still
  builds the spectral range through the existing `MaterialPreview.spectralRange` seam. The
  AC-U3.1 intent (Plotly `GenericChart` → WebView2 host, never an `AvaPlot`) is honoured;
  the literal "render the charts returned by `show*`" wording could not be, because those
  charts are `unit`.
- **The dispersion chart is passed lazily** to `webView2Host`; on this TFM the host always
  degrades to the placeholder and never forces it, so the dispersion calculation never runs
  headlessly. Drag gestures (`DoDragDrop`, `onDrop`) never fire headlessly either, so the
  smoke/view tests are unaffected.
- **`Control.allowDrop` / `Control.onDrop`** (not `DragDrop.allowDrop`) are the FuncUI DSL
  attached-property setters; `Avalonia.Input.DragDrop.DoDragDrop` is the static drag
  initiator (referenced fully-qualified to avoid the simple-name clash with the DSL).
- **`ui_tests` baseline rose 5 → 9** (the 4 new materials-panel tests); the next slice's
  baseline should track this.

# Changelog

- 2026-06-01 — Slice 004: wired the `materials` panel (Part U3) — new `MaterialsView.fs`
  browses `MaterialLibrary` filtered through the existing `byCategory`/`byNameContains`
  seams (no new index), previews a material's dispersion in the slice-002 WebView2 host
  (Plotly `GenericChart` from the reused `calculateN11Re` data, never an `AvaPlot`,
  degrading to the §U1.8 placeholder headlessly, AC-U3.1), and assigns a material to a
  layer by real Avalonia drag-drop routed through the frozen `StackEditor.layerMaterialDrop`
  seam → `Construction (EditStack …)` with thickness unchanged (AC-U3.2). Added a pure
  `Filter`/`MaterialsMsg`/`update` sub-model + a `Materials` `RootMsg` case in `Shell.fs`;
  added 4 `ui-tests`. Surfaced the §7 contradiction that `MaterialPreview.show*` return
  `unit` (they call `Chart.show`), not `GenericChart`. All slice gates green (build 0
  errors; berreman 84, constructor 204, ui-smoke 1, ui-tests 9).
