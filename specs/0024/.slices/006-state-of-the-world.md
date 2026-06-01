# 006 state-of-the-world — Results + schematic + workspace (Part U6)

```yaml
gates:
  berreman_unit_tests: 84
  constructor_unit_tests: 204
  ui_smoke: 1
  ui_tests: 15
```

# Where we are

Slice 006 carries Part U6 of the spec-0024 UI-wiring arc. U1 (slice 001) stood up the
root MVU loop; U5 (slice 002) added the renderer-host seam (`ChartHosts.fs`) with the
AvaPlot / WebView2 / Canvas adapters; U2 (slice 003) made the `stack` panel editable; U3
(slice 004) wired the `materials` panel; U4 (slice 005) wired the `sources` panel. This
slice wires the `results` panel: it renders the schematic cross-section (band layout +
ray overlay) of the active system on the slice-002 Canvas host, and the multi-system
comparison overlay in the slice-002 AvaPlot host, routing the workspace
visibility/active selection through the frozen `Workspace.update`. It is the first
end-to-end exercise of the Canvas adapter (ahead of U8's 2-D orthographic 3-D view, which
reuses the same adapter). The schematic geometry, the ray geometry, and the overlay
builder already existed and are frozen (§0.1); this slice only renders and wires them.

# What's working

- Add `ResultsView.fs`: the `results` panel renders the schematic band layout
  (`Schematic.layout`) as Canvas `Rectangle`s and the ray overlay (`Schematic.rayGeometry`)
  as incident/reflected/transmitted `Line`s, hosted via the slice-002
  `ChartHosts.canvasHost` — no geometry recomputed (R-1 / AC-U6.1).
- Wire the multi-system comparison: per-system visibility/active controls dispatch
  `Workspace.ToggleVisible`/`SetActive` through the frozen `Workspace.update`, and the
  overlay (`Workspace.renderOverlay`, a `ScottPlot.Plot`) hosts in the slice-002
  `ChartHosts.scottPlotHost` (AvaPlot), degrading to the §U1.8 placeholder headlessly
  (R-2 / AC-U6.2).
- Wire the `results` panel id in `Shell.fs` to `ResultsView.resultsPanel`; the `Workspace`
  `RootMsg` case already routed through `Workspace.update` since slice 001.
- Add 2 `ui-tests` results-panel tests; keep every slice gate green.

# Tests

All gates in the slice roster pass locally (run in order; first failure short-circuits):

- `build` — solution builds, 0 errors (only the pre-existing MSB3277 WindowsBase/WebView2
  version-conflict, NU1902 log4net, and NU1701 Wolfram warnings — inherited, not introduced).
- `unit-tests` (BerremanTests) — 84 passed / 5 skipped; baseline `berreman_unit_tests = 84`.
- `constructor-unit-tests` — 204 passed; the results panel keeps the pure
  `OpticalConstructor.Tests` suite Avalonia-free.
- `ui-smoke` — 1 passed: the `results` panel (visible by default, docked right) now renders
  one frame headlessly via `Shell.view`; the Canvas host renders the schematic and the
  AvaPlot overlay host degrades gracefully without throwing.
- `ui-tests` — 15 passed: the prior 13 plus 2 new results-panel tests — the schematic bands
  and ray render on a Canvas (R-1 / AC-U6.1); a visibility toggle dispatches
  `Workspace (ToggleVisible 0)`, the visible set changes through `Workspace.update`, and the
  overlay routes to the AvaPlot adapter (live `AvaPlot` or its placeholder) and never a
  WebView2 host (R-2 / AC-U6.2).

# Architecture

- `ResultsView.resultsPanel` follows the slice-002..005 view precedent: it takes its
  sub-state (the `Workspace.Model` and the edited `SourceSpec`) plus a `Workspace.Msg`
  dispatch — NOT `RootModel`, which lives in `Shell.fs` — so it composes under the root
  without a module cycle. `Shell` passes `RootMsg.Workspace >> dispatch`.
- The schematic projection is pure rendering: `Schematic.layout` / `Schematic.rayGeometry`
  produce the band/ray values and the view only maps them to Avalonia `Rectangle`/`Line`
  shapes positioned on the slice-002 `ChartHosts.canvasHost`. No parallel geometry model
  and no second hosting seam are introduced (Non-requirements). A `topPad` offset keeps
  the upward-mirrored reflected ray on the canvas.
- The active system the schematic draws is `Workspace.active` (falling back to the first
  system, then a note) — the workspace owns the active/visible selection (§J.5), so the
  active index is the natural source.
- The overlay reuses the frozen `Workspace.renderOverlay` (which delegates to the Part H
  §H.4 `Plot1DView.renderComparison`) over the visible systems, with an `OpticalSystem ->
  FixedInfo` lift `{ incidentLightInfo = source.light; opticalSystem = system.dispersive }`
  — the same shape as `SynthesisFitPage.comparisonOverlay`'s `mkInfo`, so the edited source
  drives the overlay's incident light and no new source store is added. No frozen core
  module is edited; the only `OpticalConstructor.Ui` changes are the new `ResultsView.fs`
  and the new-from-this-arc `Shell.fs` composition root.

# Deferred

- **Per-layer material-id colouring.** The schematic is coloured by a representative
  per-index key (`layer-N`) because the `OpticalSystem.films` `Layer` carries no material
  id and the §A.7 per-layer material-id assignment lift is a later part — the same
  representative-inputs precedent as `ChartView`'s sample sweep and slice-004's
  `referenceWavelength`. `Schematic.colorForMaterial` is total for any string, so the bands
  still colour deterministically.
- **Model-driven sweep range / source-driven overlay axis.** The overlay plots over a fixed
  `wavelength200to800Range 12` x-axis (the `ChartView` precedent); the model-driven sweep
  range and the spectral-profile-driven overlay axis are later parts' work.
- **Live AvaPlot rendering headlessly.** On the headless platform `scottPlotHost` may
  degrade to the §U1.8 placeholder (slice-002 carry-over); the overlay renders live on the
  desktop host.
- The clone-swap decision (§0.2 / spec 0022 §A.9 / AC-A8) — untouched.

# Gotchas

- **The `Workspace` `RootMsg` case was already wired through `Workspace.update` in
  `Shell.update` since slice 001**, so R-2's "dispatched as `Workspace …` routed through
  `Workspace.update`" needed only the `results` panel-id wiring in `Shell`, not a second
  reducer route. The gate reviewer should not expect a new `Shell.update` case.
- **The FuncUI Shapes DSL setters resolve only when the shape *type* is in scope.**
  `ResultsView.fs` must `open Avalonia.Controls.Shapes` so `Rectangle`/`Line` name the
  Avalonia types (not just the `Avalonia.FuncUI.DSL` modules); `Rectangle.left`/`top`/
  `width`/`height`/`fill`/`stroke` and `Line.startPoint`/`endPoint` are static-member
  extensions on those types (and on inherited `Control`/`Layoutable`/`Shape`), so without
  the type in scope they fail with `FS0039 … not defined`. The Canvas-position attached
  properties are applied on the child shape (`Rectangle.left`/`Rectangle.top`), mirroring
  the existing `Border.dock` precedent in `Shell.fs`.
- **The AvaPlot routing is asserted by the placeholder text, not by an AvaPlot count.**
  Whether a live `AvaPlot` constructs on the headless platform is not guaranteed (it may
  degrade to `scottPlotHost`'s "Plot — renderer unavailable" placeholder), so the AC-U6.2
  test accepts either a live `AvaPlot` or that placeholder and additionally asserts the
  overlay never produces WebView2 text — mirroring the slice-005 ellipse-routing test.
- **`ui_tests` baseline rose 13 → 15** (the 2 new results-panel tests); the next slice's
  baseline should track this. `berreman_unit_tests` (84), `constructor_unit_tests` (204),
  and `ui_smoke` (1) are unchanged.

# Changelog

- 2026-06-01 — Slice 006: wired the `results` panel (Part U6) — new `ResultsView.fs` renders
  the schematic band layout (`Schematic.layout`) and ray overlay (`Schematic.rayGeometry`)
  as Canvas `Rectangle`/`Line` shapes on the slice-002 `ChartHosts.canvasHost` (R-1 /
  AC-U6.1), and the multi-system comparison overlay (`Workspace.renderOverlay`) in the
  slice-002 AvaPlot adapter with per-system visibility/active controls routed through the
  frozen `Workspace.update` (R-2 / AC-U6.2). Wired the `results` panel id in `Shell.fs`
  (the `Workspace` `RootMsg` case was already routed since slice 001); added 2 `ui-tests`.
  All slice gates green (build 0 errors; berreman 84, constructor 204, ui-smoke 1,
  ui-tests 15).
