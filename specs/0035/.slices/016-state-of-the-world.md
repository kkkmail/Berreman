# Step 016 — state of the world

## Where we are

Step 016 of spec 0035 closes the 0033 implementation gap where the Material editor's live
n/k preview and the Materials-bay View panel both drew through a primitive hand-rolled
FuncUI canvas (`NkDispersionChart.inlineCanvas`) that neither looked nor behaved like the
experiment chart. 0033 Part D had already REAL-MOVED the shared dual-axis ScottPlot chart
into `OpticalConstructor.Controls`, but only as the pop-out `ChartWindow`. This slice exposes
an **embeddable form** of that same control and embeds it at both host sites, so a dispersion
curve there IS the experiment chart; the primitive `inlineCanvas` is deleted. Every model
kind — including the four transcendental made first-class in step 15 — draws n and k, and the
embedded control renders under the headless `ui-smoke` frame without throwing.

## What's working

- Extract the cartesian dual-axis (n-left / k-right) rendering into a shared `ChartPlot` module in
  `Controls/ChartWindow.fs`; `ChartWindow`'s closures delegate to it (behaviour-identical).
- Add `OpticalConstructor.Controls.EmbeddedChart.create` — an embeddable `AvaPlot` host that builds its
  `Plot` through `ChartPlot.renderCartesian` and degrades gracefully to a placeholder headlessly.
- Embed the shared chart as the Material editor preview and the Materials-bay View panel through the
  unchanged `NkDispersionChart.nkDispersionChart` builder and its `nkDispersionStyle` seed.
- Delete the primitive `NkDispersionChart.inlineCanvas` and its canvas helpers; trim the now-unused opens.
- Add six headless `ui-smoke` render proofs: the embedded chart draws n (left) / k (right) and rasterizes
  for a non-dispersive, a dispersive built-in, and every transcendental model kind; both host sites embed
  the AvaPlot and render.

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). This session ran no gate commands.
- Step 016 roster: `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- `build` covers the whole `.slnx`: the new `ChartPlot` module + `ChartWindow` delegation, the new
  `EmbeddedChart` host, the `inlineCanvas` deletion, and the two TestWindows consumers.
- `ui-smoke`: SIX new render proofs in `EmbeddedChartTests.fs` — the embedded chart carries n on the LEFT
  axis / k on the RIGHT and rasterizes one frame (`Plot.GetImage`) for a non-dispersive (vacuum), a
  dispersive built-in (silicon), and each of the four transcendental models (Tauc–Lorentz, Gaussian
  oscillator, Forouhi–Bloomer, Brendel–Bormann); the real Material editor preview embeds the AvaPlot and
  renders for a non-dispersive default AND a transcendental pick; the real Materials View panel embeds the
  AvaPlot and renders for a non-dispersive (glass) and a dispersive (silicon) entry.
- Unchanged ui-smoke tests stay green: the `MaterialEditorWindow` tests `Show()` the (now AvaPlot) preview
  without asserting a `Canvas`; `MainWorkbenchTests` asserts `materialNkChart`, which the embedded host
  carries; the `ChartWindow` render/polar/axis/series-flip tests exercise the delegating path unchanged.
- No tests were removed (six added), so no `count_at_least` regression.

## Architecture

- **One dual-axis rendering path.** The cartesian scatter-build + per-side auto-bounds + legend/font +
  number-format mapping is factored into `ChartPlot` and used by BOTH the interactive pop-out `ChartWindow`
  (its closures delegate) and the new static `EmbeddedChart` host (`renderCartesian`), so there is no
  per-host reimplementation of the ScottPlot mapping. The window keeps its polar / crosshair / property-
  panel interactivity layered on top of these shared primitives.
- **The embeddable host mirrors the `ChartHosts.scottPlotHost` seam.** A live `AvaPlot` hosted in a
  `ContentControl` carrying the caller's automation id, inside a `try`/`with` that degrades to a bordered
  "renderer unavailable" placeholder — so a native-render failure can never break the headless gate.
- **The chart MODEL is untouched.** `nkDispersionChart` / `nkDispersionStyle` are reused verbatim; only the
  RENDERER changed (primitive canvas → embedded ScottPlot control). n → left, k → right stays STYLE (the
  paired `nkDispersionStyle` seed), so every model kind — transcendental included — draws n and k through
  the same model-agnostic `getEps` builder.

## Deferred

- **Interactivity in the embedded preview.** The embedded host is a static render (no element picker,
  polar toggle, or in-place axis editing); those remain the pop-out `ChartWindow`'s job. Double-clicking
  to open the interactive window from the preview is not wired this slice (out of scope).

## Gotchas

- **Headless native ScottPlot render is proven on this platform.** The existing baseline ui-smoke test
  `ChartWindow constructs and really renders a live experiment chart` opens an AvaPlot-hosting window
  headlessly and forces Skia rasterization via `ava.Plot.GetImage(...)`, so an embedded AvaPlot renders
  here; the new tests use the same `GetImage` proof, and the host additionally keeps the graceful-degrade
  `try`/`with` for defence in depth.
- **`inlineCanvas` deletion trims opens, not behaviour.** Removing the private canvas geometry made the
  `Avalonia*` / `Avalonia.FuncUI.*` opens in `NkDispersionChart.fs` unused; they were dropped (they were
  used ONLY by `inlineCanvas`). The pure `nkDispersionChart` / `nkDispersionStyle` builders are unchanged.
- **`ChartPlot` lives in `ChartWindow.fs`, beside `ChartRender`.** It is placed after `ChartRender` (which
  it reuses for the hex→ScottPlot colour) and before the window type, so it can be shared without a new
  file dependency edge; only the genuinely new host (`EmbeddedChart.fs`) is a new file.
- **Silicon is searchable by name.** The dispersive View-panel test surfaces silicon via the workbench
  search (`byNameContains "Silicon"`) then clicks its stable row id — the proven `MainWorkbenchTests` path.

## Changelog

- 2026-07-08 — Step 016 (IMPLEMENT, retry 02): fixed the build gate. The failure was three
  `error`-level diagnostics confined to the new `EmbeddedChartTests.fs` (production code compiled
  clean): `FS0893` from a bare `open Avalonia.FuncUI` making `open Elmish` a partial path, and two
  `FS0039`s for `MouseDown`/`MouseUp` missing `open Avalonia.Headless`. Aligned the opens with the
  green `MainWorkbenchTests.fs` precedent (added `open Avalonia.Headless`, dropped bare
  `open Avalonia.FuncUI`, fully-qualified `Avalonia.FuncUI.Component`). Build now green (0 errors, no
  `warning FS` from our code); ui-smoke 104/104.
- 2026-07-08 — Step 016 (IMPLEMENT): exposed an embeddable form of the shared dual-axis ScottPlot chart
  (`OpticalConstructor.Controls.EmbeddedChart`, rendering through a new shared `ChartPlot` module that
  `ChartWindow` now delegates to); embedded it as the Material editor's live n/k preview and the
  Materials-bay View panel through the unchanged `nkDispersionChart` / `nkDispersionStyle`; deleted the
  primitive `NkDispersionChart.inlineCanvas` and its canvas helpers; added six ui-smoke render proofs
  (dual-axis n/k rasterization for non-dispersive, dispersive, and all four transcendental model kinds,
  plus both host sites).

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 447
  ui_smoke_tests: 104
  ui_tests: 329
```
