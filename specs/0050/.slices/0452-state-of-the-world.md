# Where we are

Step 0452 wires the Online sources window into the Materials window, spec 0050 S13.1, obligation
S131-MATERIALS-WIRE.

- Step 450 built the Online sources surface, and step 451 hosted it in the single-instance `MaterialSourcesWindow`.
- Here the Materials window gains `Online sources…` in Browse mode, and opens that window over its own import seam.
- What a pull or an update adds comes back as the Materials window's import status. The window's next render lists the
  new material with no reload.

# What's working

- Added `Online sources…` to the Materials window's Browse verbs. It is offered while the import seam carries a source
  and never in Select mode. Its id is `ConstructorUiId.materialsOnlineSourcesButton`.
- Added `MaterialsWindowContext.openOnlineSources` and `MaterialsWindowMsg.OpenOnlineSources`. The update calls the
  context and answers the model unchanged.
- `MaterialsWindow` opens `MaterialSourcesWindow` under `MaterialSourcesWindowKey`. Each pull's or update's caption
  becomes the window's `MaterialsImportApplied`.
- Added three ui-tests facts and one ui-smoke acceptance fact to `MaterialsWindowTests`. The stub context records
  Online sources opens.

# Tests

These are development checks. The gate engine runs the roster after this session and is the authority.

- **build:** exits 0 in 85 s, against a 600 s deadline, with 0 errors. The warnings are the exempt NU advisories and
  the vendored MathNet's SYSLIB0051, which predates this step.
- **constructor-unit-tests, the whole suite as the gate selects it:** `Passed!`, 1801 of 1801, in 107 s against a 180 s
  deadline. The count is unchanged.
- **ui-smoke, the whole suite as the gate selects it:** `Passed!`, 267 of 267, in 226 s against a 600 s deadline. That
  is checkpoint 451's 266 plus 1. The new fact is
  `acceptance: Online sources… opens one Online sources window over the app's refractiveindex.info source`.
- **ui-tests, filtered to `MaterialsWindowTests`:** `Passed!`, 48 of 48, in 9 s against a 360 s deadline. That is the
  class's earlier 45 plus the step's three:
  - `the Online sources UiId of the Materials window is the stable intent-named id`;
  - `Online sources… is offered in Browse mode while the import seam carries a source`;
  - `a pull's caption is the Materials window's status and the pulled material is listed`.
- **elevation, all 13 projects:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, in 234 s against a 600 s
  deadline.
- **The twenty-one s43 invariants:** each decoded `git grep`, run with `--untracked`, finds nothing.
- No development run exceeded its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1801
  ui_smoke_tests: 267
  ui_tests: 48
```

# Architecture

- **The Materials window's context gets one more opener field,** beside `openMaterialEditor` and
  `openCategoryEditor`.
  - The composition root (`MaterialsWindow`) bakes it over the shared `WindowLauncher` registry. Tests record it.
  - The update reaches IO only through that field, so it stays pure.
- **The pulls come back through the opener's callback.** `MaterialSourcesWindow`'s `pullsApplied` sends the Materials
  window's existing `MaterialsImportApplied`.
  - No new message or model field was needed.
  - The window already re-queries its proxies on every render, so the pulled material and its new versions list
    themselves.
- **Single instance is the process-wide `WindowRegistry`,** read by every launcher `openBrowse` creates. A second press
  activates the live window.
- No stored format, schema, package, gate or analyzer configuration changed. R0001–R0005 are untouched.

# Deferred

Nothing.

# Gotchas

- **The Silicon double in `MaterialsWindowTests` is a private mirror of step 451's
  `MaterialSourcesWindowFixture.siliconSource`.**
  - That fixture lives in `ScrollExtentTests.fs`, which compiles after `MaterialsWindowTests.fs`.
  - Neither that file nor the test project file is in this step's edits, so moving it was not authorized.
  - Each earlier source test module keeps its own Silicon record. The mirror's doc names the fixture it copies.
- **The acceptance fact does not settle start-up imports.** Online sources depends only on the import seam's sources.
  `TestAppScope.fileLocations` mints a fresh sources folder, so the window reads
  `No snapshot yet; Update snapshot downloads the database.`. Nothing presses Update snapshot, so nothing is
  downloaded.
- **`MaterialsWindowTests` runs in the non-parallel `Library picker isolation` collection,** so its registry-sharing
  acceptance fact never races `MaterialSourcesWindowTests`' registry fact.

# Changelog

- 2026-11-17, step 0452: wired `Online sources…` into the Materials window's Browse verbs. It opens the single-instance
  `MaterialSourcesWindow`, whose pull and update captions become the Materials import status. Added
  `materialsOnlineSourcesButton`, `openOnlineSources` and `OpenOnlineSources`. Added three ui-tests facts and one
  ui-smoke fact; ui-smoke goes from 266 to 267.
