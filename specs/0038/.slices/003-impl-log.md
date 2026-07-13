# Step 003 — impl-log (attempt 1)

## Progress

- [x] Read task file, worker system prompt, project prompt, slice spec; dependency
      analysis of the move set (see impl-plan).
- [x] Move the 12 workbench files TestWindows → Ui (Move-Item; LF/no-BOM verified
      before and preserved by byte-safe rewrites); namespaces rewritten to
      `OpticalConstructor.Ui`; the one stale in-file comment ("TestWindows does not
      reference the Ui project", TableAndElementRotationView) refreshed.
- [x] Ui.fsproj: 12 compile items appended after UserEnvironment.fs (end of list),
      original relative order preserved; header comment updated.
- [x] TestWindows.fsproj: moved items removed, ProjectReference to Ui added, the
      direct Analytics reference dropped (only consumer NkDispersionChart moved;
      still flows transitively via Ui); comments updated.
- [x] `open OpticalConstructor.Ui` added to the 8 staying files that reference
      moved modules (6 diagnostic views + TableAndElementRotationWindow +
      SnapToReflectedWindow). The other 5 window files reference no moved symbol.
- [x] App Program.fs: no change needed — it already opens BOTH
      `OpticalConstructor.Ui` (now supplies TableAndElementRotationView) and
      `OpticalConstructor.TestWindows` (the seven diagnostic windows).
- [x] Ui.Tests: 12 moved-only files' opens re-pointed to `OpticalConstructor.Ui[...]`;
      3 mixed files (TableRotation/ElementMovement/RendererTest tests) keep the
      TestWindows opens and gain the Ui open; TableAndElementRotationTests re-points
      only the view open (window stayed); 3 stale "window in TestWindows" doc
      comments fixed; staying-only test files untouched.
- [x] Diagnostic build + all four suites (non-gate verification) — all green, all
      counts exactly at the step-002 checkpoint.

## Files modified

Moved (TestWindows → Ui, namespace rewritten to `OpticalConstructor.Ui`, content
otherwise unchanged; plain filesystem move — the arc-runner's commit will pair
delete+add as renames):

- `TableScene.fs`, `SceneInput.fs`, `Catalogue.fs`, `ElementRenderer.fs`,
  `NkDispersionChart.fs`, `SampleEditorView.fs`, `SampleEditorWindow.fs`,
  `MaterialEditorView.fs`, `MaterialEditorWindow.fs`, `CategoryEditorView.fs`,
  `CategoryEditorWindow.fs`, `TableAndElementRotationView.fs` (also: one stale
  comment about TestWindows-not-referencing-Ui refreshed at former line 1773).

Edited:

- `OpticalConstructor.Ui/OpticalConstructor.Ui.fsproj` — the 12 compile items
  appended after `UserEnvironment.fs` (end of list; internal order = the
  TestWindows original), header comment updated.
- `OpticalConstructor.TestWindows/OpticalConstructor.TestWindows.fsproj` — moved
  compile items removed; `<ProjectReference>` to Ui added; the direct Analytics
  reference dropped (its only consumer here, NkDispersionChart, moved; Analytics
  still flows transitively through Ui); comments updated.
- Staying TestWindows files gaining `open OpticalConstructor.Ui`:
  `TableRotationView.fs`, `ElementRotationView.fs`, `ElementMovementView.fs`,
  `RendererTestView.fs`, `SnapToBeamView.fs`, `SnapToReflectedView.fs`,
  `TableAndElementRotationWindow.fs`, `SnapToReflectedWindow.fs`. (The other five
  window files reference no moved symbol — untouched.)
- `OpticalConstructor.Ui.Tests` — opens re-pointed: CatalogueTests,
  NkDispersionChartTests, MainSceneMsgTests, WireUiCompositionTests,
  MainWorkbenchTests, EmbeddedChartTests, ExperimentControlsTests,
  MaterialEditorWindowTests, SampleEditorWindowTests, CategoryEditorWindowTests,
  LibraryControlsTests, LayerBandsControlsTests (TestWindows → Ui);
  TableRotationTests, ElementMovementTests, RendererTestTests (keep TestWindows
  opens + gain `open OpticalConstructor.Ui`); TableAndElementRotationTests (view
  open → Ui, window open stays); 3 stale doc comments fixed; fsproj comment
  refreshed.
- `OpticalConstructor.App/Program.fs` — NOT modified: it already opens both
  `OpticalConstructor.Ui` (now supplies `TableAndElementRotationView`) and
  `OpticalConstructor.TestWindows` (the seven diagnostic windows), so the
  slice's "update its opens" is already satisfied by the existing pair.

## Testing state

Gate execution belongs to the arc-runner's gate engine after this worker exits
(IMPLEMENT Invariant 6). Diagnostic verification only, not gate authority:

- `dotnet build Berreman.slnx -c Release` — **Build succeeded, 0 errors**;
  **no MSB3277 anywhere in the log**; zero warnings from any touched project
  (Ui / TestWindows / Ui.Tests / App). The 10 remaining warning lines are exactly
  the step-001-catalogued pre-existing set in untouched files (FS1125
  SeriesDataTests, FS3873 Dispersion.fs, FS0044 ChartWindow.fs, SYSLIB0051
  vendored MathNet, NU1701 Wolfram.NETLink).
- `OpticalConstructor.Tests`: **457/457 passed** (checkpoint 457).
- `OpticalConstructor.Ui.Tests --filter Category=ui-smoke`: **111/111 passed**
  (checkpoint 111) — includes LauncherTests' eight click-opens proofs: Main →
  `MainConstructorWindow` and all seven diagnostic test windows still open.
- `OpticalConstructor.Ui.Tests --filter Category!=ui-smoke`: **339/339 passed**
  (checkpoint 339).
- `BerremanTests`: **119 passed, 5 skipped** (the pre-existing skips;
  checkpoint 119).
- Line endings: `git diff --numstat` equals `--ignore-cr-at-eol`; all 12 moved
  files verified LF/no-BOM before the move and rewritten byte-safely (0 CR bytes
  after).

## Artifacts

- `specs/0038/.artifacts/003-diag-build.log` — the diagnostic Release build log
  (the MSB3277 / touched-project warning sweep was run against this capture).

## Gotchas

- No operator note in flight (the project prompt's Operator note section is empty).
- The moved set is cleanly one-directional: no moved file references a staying
  diagnostic module (verified by word-boundary grep before moving), so
  TestWindows → Ui is the only new edge and no cycle is possible.
- `RendererTestView` (staying) reuses the moved workbench's public `kindCode`,
  and `SnapToBeam/SnapToReflected` views print it too — that is why those views
  need the Ui open, not just the windows.
- The slice's "Order the moved files … after UserEnvironment.fs" was implemented
  by appending the block at the END of the Ui compile list (which is after
  UserEnvironment.fs); the moved files depend on no existing Ui module and
  nothing existing depends on them, so end-placement keeps every current ordering
  intact and leaves later slices free to wire UserEnvironment/AppShell into the
  workbench without reordering.
- `Program.fs` needed no edit: both namespaces were already opened (the Ui open
  survived step 002 for `AppShell`/`UserEnvironment`), so the moved
  `TableAndElementRotationView` re-resolves silently. Recorded here because the
  slice text says "update its opens" — the update turned out to be a no-op.
