# Step 003 — impl-plan (attempt 1)

## Slice

REAL-MOVE the workbench modules from `OpticalConstructor.TestWindows` into
`OpticalConstructor.Ui` (no file-linking, one fsproj per folder, namespaces become
`OpticalConstructor.Ui`). The seven diagnostic scenes stay in TestWindows, which
gains a project reference to Ui.

## Approach

Dependency analysis performed up-front (greps over opens + word-boundary
identifier references):

- The 12 moved files (`TableScene`, `SceneInput`, `Catalogue`, `ElementRenderer`,
  `NkDispersionChart`, `SampleEditorView/Window`, `MaterialEditorView/Window`,
  `CategoryEditorView/Window`, `TableAndElementRotationView`) depend ONLY on
  Avalonia/FuncUI, `Berreman.*`, `Analytics`, `OpticalConstructor.Domain*` and
  `OpticalConstructor.Controls*` — never on a staying diagnostic module and never
  on an existing Ui module. The move is acyclic in the required direction.
- Staying diagnostic files reference moved modules (e.g. `TableScene`,
  `Catalogue`, `ElementRenderer`, `TableAndElementRotationView.kindCode`,
  `TableAndElementRotationView.init/update/view`) — TestWindows must reference Ui
  and the 8 affected files gain `open OpticalConstructor.Ui`.
- No module-name collision exists in the Ui namespace (checked against every
  `module`/type declaration in Domain/Controls/Ui).

Steps:

1. `Move-Item` the 12 `.fs` files to `OpticalConstructor.Ui\` (plain FS move; git
   mutations are out of scope for the worker — rename detection happens at commit).
2. Rewrite each moved file's top declaration `OpticalConstructor.TestWindows` →
   `OpticalConstructor.Ui`; refresh stale in-file comments that say the code lives
   beside the test windows / that "TestWindows does not reference the Ui project".
3. `OpticalConstructor.Ui.fsproj`: add the 12 `<Compile>` items at the END of the
   compile list (i.e. after `UserEnvironment.fs`, as the slice orders), preserving
   the original relative order: TableScene → SceneInput → Catalogue →
   ElementRenderer → NkDispersionChart → SampleEditorView/Window →
   MaterialEditorView/Window → CategoryEditorView/Window →
   TableAndElementRotationView.
4. `OpticalConstructor.TestWindows.fsproj`: remove the 12 compile items, add a
   `<ProjectReference>` to Ui, drop the now-unconsumed direct Analytics reference
   (its only TestWindows consumer, NkDispersionChart, moves; it still flows
   transitively via Ui), refresh the header comment.
5. Staying files gaining `open OpticalConstructor.Ui`: TableRotationView,
   ElementRotationView, ElementMovementView, RendererTestView, SnapToBeamView,
   SnapToReflectedView, TableAndElementRotationWindow, SnapToReflectedWindow
   (verify each file's actual refs when editing).
6. `OpticalConstructor.App/Program.fs` already opens BOTH `OpticalConstructor.Ui`
   and `OpticalConstructor.TestWindows`, so `TableAndElementRotationView` (moved)
   and the seven diagnostic windows keep resolving; update the comments to the new
   split.
7. `OpticalConstructor.Ui.Tests`: re-point opens per file —
   moved-only files (WireUiComposition, MainWorkbench, MaterialEditorWindow,
   SampleEditorWindow, CategoryEditorWindow, NkDispersionChart, Catalogue,
   MainSceneMsg, EmbeddedChart, ExperimentControls, LibraryControls,
   LayerBandsControls tests) switch `OpticalConstructor.TestWindows[...]` opens to
   `OpticalConstructor.Ui[...]`; mixed files (TableRotationTests,
   ElementMovementTests, RendererTestTests, TableAndElementRotationTests) keep the
   TestWindows opens and add/redirect the Ui ones; staying-only files
   (LauncherTests, ElementRotationTests, SnapToBeam/SnapToReflected/
   RayPositionControls tests) are untouched.
8. Diagnostic (non-gate) verification: `dotnet build Berreman.slnx -c Release`
   with zero our-code warnings (MSB3277 explicitly watched), then the four test
   suites; counts must hold at/above the step-002 checkpoint (119 / 457 / 111 /
   339 — no test is added or deleted, only opens change, so counts are expected
   unchanged). LF check via `git diff --numstat` vs `--ignore-cr-at-eol`.

## Files to modify

- Move: 12 `.fs` files TestWindows → Ui (namespace rewrite in each).
- Edit: `OpticalConstructor.Ui.fsproj`, `OpticalConstructor.TestWindows.fsproj`,
  8 staying TestWindows `.fs` files, `OpticalConstructor.App/Program.fs` (comments
  only), ~16 `OpticalConstructor.Ui.Tests/*.fs` open blocks, Ui.Tests fsproj
  comment (cosmetic).

## Risks

- F# compile order: the moved block goes after every existing Ui file; internal
  relative order preserved, so no forward references can appear.
- Namespace-open shadowing in staying files (`open OpticalConstructor.Ui` exposes
  `Validation`, `Localization`, … alongside Domain opens) — no identifier overlap
  found, watch the build.
- MSB3277 regression: nothing re-adds WebView2; Ui's package set is unchanged;
  TestWindows only gains a project reference — expect none, verify in build log.
- `count_at_least` baselines: no tests deleted/added, counts must equal 457 /
  111 / 339 / 119 exactly.
