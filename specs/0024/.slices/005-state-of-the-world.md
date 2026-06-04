# 005 state-of-the-world — Sources panel (Part U4)

```yaml
gates:
  berreman_unit_tests: 84
  constructor_unit_tests: 204
  ui_smoke: 1
  ui_tests: 13
```

# Where we are

Slice 005 carries Part U4 of the spec-0024 UI-wiring arc. U1 (slice 001) stood up the
root MVU loop; U5 (slice 002) added the renderer-host seam (`ChartHosts.fs`) with the
AvaPlot and WebView2 adapters; U2 (slice 003) made the `stack` panel editable; U3 (slice
004) wired the `materials` panel. This slice wires the `sources` panel: it renders the
light-source / polarization field editors, offers the polarization presets, shows a live
Stokes / Poincaré / ellipse readout, and hosts the polarization ellipse in the slice-002
`AvaPlot` adapter and the Poincaré sphere in the slice-002 WebView2 adapter. The editor
reducer, the presets, and the live projections already existed and are frozen (§0.1);
this slice only renders and wires them.

# What's working

- Add `SourceView.fs`: the `sources` panel renders the source field editors (wavelength
  in the chosen display unit, incidence angle, polarization azimuth, ellipticity,
  intensity, unit selector), each dispatching a `SourceEditorView.SourceMsg` routed
  through the frozen `SourceEditorView.update` (R-1 / AC-U4.1).
- Offer the polarization presets (s, p, 45°, RCP, LCP, unpolarized) via the frozen
  `PolarizationPicker.applyPreset`, and render the live readout from
  `PolarizationPicker.liveStokes` / `poincareMarker` / `ellipseParameters` (R-2 / AC-U4.2).
- Host the polarization ellipse (`PolarizationPlots.renderEllipse`, a `ScottPlot.Plot`)
  in the slice-002 `ChartHosts.scottPlotHost` (AvaPlot) and the Poincaré sphere
  (`PolarizationPlots.poincareSphere`, a lazy Plotly `GenericChart`) in
  `ChartHosts.webView2Host` (WebView2, degrading to the §U1.8 placeholder headlessly) —
  ellipse → AvaPlot, Poincaré → WebView2, never the reverse (AC-U4.2).
- Wire a `Source` `RootMsg` case + a pure `source : SourceSpec` model field into
  `Shell.fs`, seeded from `SourceEditorView.defaultSource`; add 4 `ui-tests`
  sources-panel tests; keep every slice gate green.

# Tests

All gates in the slice roster pass locally (run in order; first failure short-circuits):

- `build` — solution builds, 0 errors (only the pre-existing MSB3277 WindowsBase/WebView2
  version-conflict + FS1125 `Range` warnings, inherited not introduced).
- `unit-tests` (BerremanTests) — 84 passed / 5 skipped; baseline `berreman_unit_tests = 84`.
- `constructor-unit-tests` — 204 passed; the sources panel keeps the pure
  `OpticalConstructor.Tests` suite Avalonia-free.
- `ui-smoke` — 1 passed: the `sources` panel renders one frame headlessly via `Shell.view`;
  the ellipse AvaPlot renders and the Poincaré WebView2 host degrades to the placeholder
  without throwing.
- `ui-tests` — 13 passed: the prior 9 plus 4 new sources-panel tests — the field editors
  and live Stokes readout render (R-1); a field edit routes `SourceEditorView.update` and
  the live Stokes refreshes (AC-U4.1); a preset button dispatches `Source (ApplyPreset …)`
  and `applyPreset` refreshes the Poincaré marker (AC-U4.2); and exactly one WebView2
  placeholder appears (the Poincaré) while the ellipse routes through `scottPlotHost`
  (AvaPlot), proving the ellipse is never in WebView2 (AC-U4.2).

# Architecture

- `SourceView.sourcePanel` follows the slice-002/003/004 view precedent: it takes its
  sub-state (the edited `SourceSpec`) plus a dispatch — NOT `RootModel`, which lives in
  `Shell.fs` — so it composes under the root without a module cycle. `Shell` passes
  `RootMsg.Source >> dispatch`.
- The source is a pure, serializable `RootModel` field (`source : SourceSpec`), seeded
  from `SourceEditorView.defaultSource "source-1"`. No renderer handle / token source
  enters the model (§0.5); the live `AvaPlot`/`WebView2` controls are host-layer locals
  created inside the `ChartHosts` adapters.
- Field edits and presets are routed through a thin view-local
  `SourceViewMsg = Edit of SourceMsg | ApplyPreset of PolarizationPreset` (the same shape
  as the materials panel's `MaterialsMsg`), dispatched through the single `Source`
  `RootMsg` case. `Edit` delegates to the frozen `SourceEditorView.update` (R-1);
  `ApplyPreset` delegates to the frozen `PolarizationPicker.applyPreset` (R-2). The view
  re-implements neither the reducer nor the presets nor the projections.
- The two plots reuse the slice-002 `ChartHosts` adapters rather than embedding their own
  hosting (Non-requirements): ellipse → `scottPlotHost` (AvaPlot), Poincaré →
  `webView2Host` (WebView2). The Poincaré chart is passed lazily so the placeholder path
  never pays to build it. No frozen core module is edited; the only `OpticalConstructor.Ui`
  changes are the new `SourceView.fs` and the new-from-this-arc `Shell.fs` composition root.

# Deferred

- **Active-wavelength / multi-source list.** This slice edits a single seed source held in
  the root model. The §E.8 multi-source list, the spectral-profile / cone / Gaussian-beam
  orchestration controls, and the source-driven active-wavelength lift consumed by the
  materials preview (slice 004 carry-over) are later parts' UI work; the reducer and the
  expansion seams (`SourceSpec.expand`) already exist for them.
- **Bidirectional ellipse / Poincaré dragging.** `PolarizationPicker.setAzimuthEllipticity`
  (the §E.4 write-back from a drag on the ellipse / Poincaré marker) is wired neither here;
  the readout is currently one-way (model → view). Hooking pointer drags to that seam is a
  later polish step (and the live 3-D Poincaré needs the deferred WebView2 bridge anyway).
- **Live WebView2 rendering.** On this `net10.0` target `ChartHosts.tryHostPlotly` returns
  `None` (no Avalonia-hostable WebView2 binding), so the Poincaré sphere is the placeholder
  until the deferred Windows-desktop WebView bridge lands (slice-002 carry-over, unchanged).
- The clone-swap decision (§0.2 / spec 0022 §A.9 / AC-A8) — untouched.

# Gotchas

- **The polarization presets cannot be expressed as a `SourceMsg`.**
  `PolarizationPicker.applyPreset` sets `coherence` (`Unpolarized`) and clears
  ellipticity/coherence on the linear presets — and `SourceEditorView.SourceMsg` has no
  `coherence` case — so the presets are a genuinely separate seam from the field reducer.
  That is why `SourceView` introduces the local `SourceViewMsg` wrapper (`Edit` vs
  `ApplyPreset`) instead of folding everything into `SourceMsg`. R-1's "dispatch
  `SourceMsg` as `Source …` routed through `SourceEditorView.update`" is honoured by the
  `Edit` arm; R-2's presets are honoured by the `ApplyPreset` arm. This mirrors slice 004,
  which also gave the panel a view-local message union under one `RootMsg` case.
- **Ellipse → AvaPlot, Poincaré → WebView2 is asserted by the placeholder text, not by an
  AvaPlot count.** Whether a live `AvaPlot` actually constructs on the headless platform is
  not guaranteed (it may degrade to `scottPlotHost`'s "Plot — renderer unavailable"
  placeholder). The AC-U4.2 view test therefore asserts there is exactly ONE WebView2
  placeholder (text containing "Plotly chart (WebView2)") — the Poincaré — proving the
  ellipse never routes to WebView2, and accepts either a live AvaPlot or the ScottPlot
  placeholder for the ellipse. The two adapters emit distinct placeholder text, which makes
  the routing observable headlessly.
- **The ellipse plot is rebuilt each render** (cheap: 180 parametric points via
  `ellipseCurve`); the Poincaré chart is built lazily and never forced on this TFM (the
  WebView2 host degrades first), so no Plotly 3-D sweep runs headlessly.
- **`ui_tests` baseline rose 9 → 13** (the 4 new sources-panel tests); the next slice's
  baseline should track this.

# Changelog

- 2026-06-01 — Slice 005: wired the `sources` panel (Part U4) — new `SourceView.fs` renders
  the source field editors dispatching `SourceEditorView.SourceMsg` through the frozen
  `SourceEditorView.update` (R-1 / AC-U4.1), offers the polarization presets via
  `PolarizationPicker.applyPreset` with a live Stokes/Poincaré/ellipse readout from
  `liveStokes`/`poincareMarker`/`ellipseParameters` (R-2 / AC-U4.2), and hosts the ellipse
  (`renderEllipse`, ScottPlot) in the slice-002 AvaPlot adapter and the Poincaré sphere
  (`poincareSphere`, Plotly) in the slice-002 WebView2 adapter (degrading to the §U1.8
  placeholder headlessly). Added a `Source` `RootMsg` case + a pure `source : SourceSpec`
  model field in `Shell.fs` seeded from `SourceEditorView.defaultSource`; added a
  view-local `SourceViewMsg` wrapper because presets touch `coherence` (no `SourceMsg`
  case); added 4 `ui-tests`. All slice gates green (build 0 errors; berreman 84,
  constructor 204, ui-smoke 1, ui-tests 13).
