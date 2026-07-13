# State of the world — 047 (WIRE_UI / AC-W1: finalize the composition root, drive the whole surface headless)

## Where we are

This is the CLOSING step of spec 0038 Part N — the WIRE_UI composition finale. Prior slices
built every component (the Part-B relocation, the Materials/Library/editor/SolverHandoff windows,
the five stores + the experiment-data/collection proxies, the four-button launcher, the §0.6
warning sweep). Step 047 finalizes the composition root so every proxy and the launcher seam are
built exactly once at the root, and ships the consolidated headless wiring assertion that drives
the WHOLE root-wired surface — launcher, both constructor states, both faceted windows in Browse
and Select, the three editors, and the SolverHandoff — over ONE app scope, without throwing.

## What's working

- Compose the step-027 `SceneProxy.createInMemory` at the app scope (`AppContext.create`), the last
  STORE_XDUO_000x proxy missing from "every proxy built once at the root" — beside the five stores,
  the file-backed `ExperimentDataProxy` (36) and the in-memory `ExperimentCollectionProxy` (29).
- Add the closing composition acceptance (`WireUiFinalCompositionTests`): a ui-tests pin that the
  finalized app scope carries every store proxy — including the scene seam — built once at the root.
- Drive the whole root-wired surface headless in one wiring assertion: render the launcher, the Main
  and Inverse constructor windows, the three editor windows and the SolverHandoff through the ONE
  root window-launcher factory over the shared `WindowRegistry`, and both faceted windows in Select
  state — each over one app scope composed exactly like `Startup.context`, without throwing.
- Leave `Program.fs` unchanged: it already creates the `AppSettingsProvider` once, builds the app
  scope once, and hands the context to the launcher window and both constructor states; the new
  proxy flows through the unchanged `AppContext.create` signature transparently.

## Tests

- **build** — GREEN locally: full Release build of `Berreman.slnx` reports **0 Error(s),
  4 Warning(s)**, all four exempt/third-party (`NU1701` ×2 Wolfram.NETLink; `SYSLIB0051` ×2
  vendored MathNet). `OpticalConstructor.Ui` (the AppContext change) and `OpticalConstructor.Ui.Tests`
  (the new file) both compile; no new our-code warning (§0.6 holds). Captured `.artifacts/047-build.log`.
- **the new wiring assertion** — GREEN locally: filtered to `WireUiFinalCompositionTests` →
  **Passed 4, Failed 0** (1 ui-tests + 3 ui-smoke). Captured `.artifacts/047-wire-tests.log`.
- **unit-tests / constructor-unit-tests / full ui-smoke / ui-tests** — deferred to the arc-runner
  gate engine (Invariant 6 / WIRE_UI "act only": the worker runs no gates). All edits are additive
  (a new `AppContext` field with no record-literal site; a new test file), and the solution builds
  clean, so the baselines carry forward and rise by this round's four new tests (+1 ui-tests,
  +3 ui-smoke).

```yaml
gates:
  berreman_unit_tests:    119
  constructor_unit_tests: 674
  ui_smoke_tests:         180
  ui_tests:               462
```

## Architecture

- **The scene seam joins the app scope.** `AppContext` now holds `scenes : Scene.SceneProxy`,
  built once via `Scene.SceneProxy.createInMemory ()`. This closes the composition contract's
  "every proxy built once at the root" for STORE_XDUO_0004, mirroring how `experimentCollections`
  / `experimentData` sit in the scope. A future disk-backed `create` swaps in with no consumer
  change.
- **No launcher restructuring.** The launcher seam (`EditorLaunchers.defaults` + `WindowLauncher.create`
  over the ONE host-layer `WindowRegistry`) was already built once and threaded into the launcher
  window and both constructor states. The wiring assertion demonstrates that ONE factory covers
  every `WindowKey` (Materials, Library, the three editors, SolverHandoff) under the app's modality
  switch — the "construct the WindowLauncher with every window factory" seam — without changing the
  production launcher architecture.

## Deferred

- **A product trigger that opens the SolverHandoff from the inverse workbench.** The step wires the
  SolverHandoff into the launcher factory (proven by the sweep) but adds no workbench button — that
  would be new product behaviour in `OpticalConstructor.Ui`, outside this step's `touches`
  [App, Ui.Tests] and outside the WIRE_UI "assemble, add no new behaviour" mandate. The
  `SolverHandoffWindow` composes over the root-wired proxies and renders; a later cycle owns the
  inverse-flow "hand off to solver" verb.
- **A scene save/load surface.** The `SceneProxy` is composed at the root but has no product consumer
  yet (the workbench holds pure `captureScene` / `restoreScene` but no save/load verb). A later cycle
  owns that surface; the seam is ready.

## Gotchas

- **`touches` vs the how_to.** The declared `touches` is [App, Ui.Tests], but the how_to explicitly
  requires the SceneProxy in "the app scope," which lives in `AppContext.fs` (`OpticalConstructor.Ui`).
  The more-specific how_to governs; `AppContext.fs` was touched to compose the proxy. `Program.fs`
  needed no change.
- **Registry hygiene in the sweep.** The factory sweep opens each window through the REAL launcher
  over the app-global `WindowRegistry` and CLOSES each (pumping the close so the `Closed` hook
  unregisters the key), so no key leaks into a later suite. The Select-state proofs use direct
  construction (no registry). Verified: the four new tests pass together and leave the registry clean.
- **Modality.** Select-state opens run under `AppContext.settings.selectWindowModality` (default
  `ModelessSelectWindows` → unowned `Show`), so no modal `ShowDialog` blocks the headless session.
- **Empty handoff snapshot.** The sweep hands the `SolverHandoffWindow` an EMPTY collection, so it
  renders its summary + solver-comes-later message with no measured-data load against the real
  file-backed proxy; the rich per-file validation is already proven by `SolverHandoffWindowTests`.
- **Line endings.** The edited/added files are pure LF (0 CRLF), matching `.gitattributes`.

## Changelog

- 2026-07-11 — 047 WIRE_UI (AC-W1): composed the step-027 `SceneProxy.createInMemory` at the app
  scope (`AppContext.create`) so every proxy is built once at the root; added
  `WireUiFinalCompositionTests` — the closing composition acceptance driving the whole root-wired
  surface headless (launcher, both constructor states, both faceted windows in Browse and Select,
  the three editors, and the SolverHandoff) without throwing. `Program.fs` unchanged. Full Release
  build 0 errors / 0 new warnings; the four new tests pass (1 ui-tests + 3 ui-smoke).
