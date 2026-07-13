# Step 003 — state of the world

## Where we are

Step 003 executes the spec 0038 Part B REAL-MOVE: the workbench that step 002's retirement left
stranded in `OpticalConstructor.TestWindows` now lives in `OpticalConstructor.Ui`, the project that
is supposed to BE the product UI. Twelve modules moved — the scene spine (`TableScene`,
`SceneInput`), the shared catalogue + element renderer, the pure n/k dispersion chart builder, the
three editor view/window pairs (Sample / Material / Category), and the workbench scene itself
(`TableAndElementRotationView`) — all under `OpticalConstructor.Ui` namespaces, real files in the
Ui folder (no file-linking, one fsproj per folder). `OpticalConstructor.TestWindows` keeps only the
seven diagnostic scenes and gains a project reference to Ui; the App composition root still opens
both namespaces and its launcher still reaches Main plus every test window. No test was added or
deleted — the four suites re-pin the same behaviour under the new namespaces.

## What's working

- REAL-MOVE the 12 workbench modules from OpticalConstructor.TestWindows into
  OpticalConstructor.Ui under OpticalConstructor.Ui namespaces, ordered after
  UserEnvironment.fs with the original relative order preserved.
- Keep only the diagnostic scenes in OpticalConstructor.TestWindows; add its
  project reference to Ui and open the moved namespaces in the 8 files that
  render the shared scene/renderer/workbench modules.
- Drop TestWindows' direct Analytics reference (its only consumer moved; it
  still flows transitively via Ui); no MSB3277 and zero touched-project
  warnings reintroduced.
- Re-point Ui.Tests opens: 12 workbench suites to OpticalConstructor.Ui, the
  diagnostic-scene suites unchanged, mixed suites open both.
- Launcher Main + all seven test windows still open (LauncherTests' eight
  ui-smoke click-opens proofs pass); all four suites green at the checkpoint
  counts (457 / 111 / 339 / 119).

## Tests

- Gates are executed by the arc-runner's deterministic gate engine after this worker exits
  (IMPLEMENT Invariant 6 — the worker acts, it runs no checks). The roster for this step is
  `build`, `unit-tests`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`.
- Diagnostic verification (not gate authority): `dotnet build Berreman.slnx -c Release` succeeded
  with 0 errors, **no MSB3277**, and zero warnings from any touched project — the only warnings
  are the step-001-catalogued pre-existing set in untouched files (FS1125 SeriesDataTests, FS3873
  Dispersion, FS0044 ChartWindow, SYSLIB0051 vendored MathNet, NU1701 Wolfram.NETLink). Suites:
  OpticalConstructor.Tests **457/457**, ui-smoke **111/111**, ui-tests **339/339**, BerremanTests
  **119 passed / 5 pre-existing skips** — every count exactly at the step-002 checkpoint (a pure
  move; nothing added or deleted).
- Nothing deferred.

## Architecture

- **The move is one-directional by construction**: pre-move dependency analysis (opens +
  word-boundary identifier greps) showed no moved file references a staying diagnostic module and
  no existing Ui module references a moved one. The only new edge is
  `OpticalConstructor.TestWindows → OpticalConstructor.Ui` — the same direction the App already
  uses, so no cycle can form.
- **The diagnostic scenes are now thin consumers of the product UI**: `RendererTestView`,
  `SnapToBeamView`, `SnapToReflectedView` render through the Ui `ElementRenderer` / `Catalogue` /
  `TableScene`, and `TableAndElementRotationWindow` hosts the Ui workbench view — a change to the
  shared scene shows up in the product and the diagnostics alike.
- **Compile placement**: the moved block sits at the end of the Ui compile list (after
  `UserEnvironment.fs`, as the slice orders). The workbench currently consumes no existing Ui
  module, so this placement leaves every later slice free to wire the environment/theme seams into
  the workbench without reordering.
- **Editor windows keep their view-before-window, windows-before-workbench order** inside Ui —
  the workbench's Add/Edit verbs construct `SampleEditorWindow` / `MaterialEditorWindow` /
  `CategoryEditorWindow` directly, so those types must precede `TableAndElementRotationView`.

## Deferred

- The possibly under-used Ui package references (`ScottPlot.Avalonia`, `Plotly.NET`, `OpenTK`)
  remain as-is (step 002's deferral) — the workbench move brought FuncUI/Elmish consumers into Ui
  but still no direct ScottPlot/Plotly/OpenTK ones; the Part N sweep can prune what's still idle.
- TestWindows' Avalonia package set was left untouched (Desktop/Fluent themes are runtime assets
  for the diagnostic hosts); pruning it is not this slice's mandate.
- The pre-existing warnings in untouched files (FS1125, FS3873, FS0044, SYSLIB0051) stay for
  spec 0038 Part N's final warning sweep (§0.6).

## Gotchas

- **`Program.fs` needed no edit** despite the slice's "update its opens": step 002's trim already
  left it opening BOTH `OpticalConstructor.Ui` (for `AppShell`/`UserEnvironment`) and
  `OpticalConstructor.TestWindows`; after the move the former open silently supplies
  `TableAndElementRotationView` and the latter the seven diagnostic windows.
- **Three Ui.Tests suites test BOTH sides now**: TableRotationTests / ElementMovementTests /
  RendererTestTests exercise staying diagnostic views that render moved modules — they open both
  namespaces. TableAndElementRotationTests opens the Ui VIEW module but the TestWindows WINDOW.
- **`SnapToReflectedWindow` (not just the views) references the moved `TableScene`** for its
  canvas dimensions — easy to miss when auditing which staying files need the Ui open.
- **The moved files were verified LF/no-BOM and rewritten byte-safely**; the namespace rewrite is
  a plain filesystem move + one-token replace, so git's rename detection will pair the delete/add
  at commit time.
- Step 002's carried-over gotchas remain valid (baselines come from `.checkpoints-json`, not the
  SoW YAML; several Ui modules still have only OpticalConstructor.Tests consumers).

## Changelog

- 2026-07-10 — Step 003 (IMPLEMENT, attempt 1): REAL-MOVEd the 12 workbench modules
  (TableScene, SceneInput, Catalogue, ElementRenderer, NkDispersionChart, the Sample/Material/
  Category editor view+window pairs, TableAndElementRotationView) from
  OpticalConstructor.TestWindows into OpticalConstructor.Ui under OpticalConstructor.Ui
  namespaces; TestWindows keeps the seven diagnostic scenes and references Ui; Ui.Tests opens
  re-pointed; App unchanged. Build clean (no MSB3277, no touched-project warnings); suites
  457 / 111 / 339 / 119 — all at the prior checkpoint.

```yaml
gates:
  berreman_unit_tests: 119
  constructor_unit_tests: 457
  ui_smoke_tests: 111
  ui_tests: 339
```
