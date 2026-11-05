# Step 0013 — IMPLEMENT — state of the world

## Where we are

This is step 13 of spec 0050, in section S1.2 ("A material's physics is its complexity"), under
obligation `S12-MANDATORY`. Steps 10 and 11 made every construction set
`properties = complexity.toProperties` with a present complexity. This step makes the complexity
mandatory: `MaterialEntry.complexity : MaterialComplexity`, and `properties` is a member derived
from it (001 §B, .spec-md §0.1 and §3.2). The record and every consumer changed together, so the
build stayed green. Every `None` arm that stood for a coded, view-only preset is deleted. The
view-only editor carries the complexity it shows. Step 14 sweeps the last three test-side
mentions of the option.

## What's working

- `MaterialEntry.complexity` is mandatory, and `properties` is a member computed from it; entries compare structurally
- Every construction and copy-and-update drops `properties = …` and `Some` (Storage, Ui and test fixtures)
- Facets, the store payload, the out-of-band diagnostic and the Materials window read the complexity directly; `previewAnisotropy` is deleted
- The view-only editor case is `ViewOnlyMaterial of reason * shown : MaterialComplexity`, and `presetProperties` / `viewOnlyComplexity` are deleted
- Two facts that lost their subject are deleted, and three Save / view-only facts are renamed

## Tests

These runs are local observations, made to confirm the change. The arc-runner's gate engine runs
and records the gates.

- `build`: 0 errors. The only warnings are the two vendored MathNet SYSLIB0051 ones already on
  the start tree (`.artifacts/0013-build.log`).
- `constructor-unit-tests` (whole suite): 736/736. That is the 737 at checkpoint `"10"` (slice
  0011) less the deleted `every built-in carries Some complexity after the step-004 re-seed`.
  **The gate engine failed this gate**: `constructor_unit_tests count 736 < baseline 737`.
  - The identity check passed: the census expects exactly these 736.
  - The floor was not credited for the authorized delete, because `authorized_count_credits`
    credits only `"granularity": "row"` suites, and this suite is `"method"`.
  - The fix lies outside this step's authority (the arc-runner's checkpoint record, or ASG).
    This attempt is therefore `commit_ready: false`, an authority conflict. The impl-log's
    Testing state ("Attempt 2") has the evidence and the operator's options.
- `ui-smoke` (whole suite): 208/208. Both renamed facts pass under their new names.
- `ui-tests` (MaterialEditorWindowTests, MaterialsWindowTests, NkDispersionChartTests,
  OutOfBandBadgeTests, FsxAcceptanceTests): 93/93. That is one fewer for the deleted
  `spec 0040 006: a coded preset with no stored tree still renders all components`, and
  `Save adds a NEW material with its complexity, …` passes under its new name.
- `elevation`: `ELEV-GATE: OK - 0 new finding(s) in 8 project(s)` (`.artifacts/0013-elevation.log`).
  Domain, Storage and Ui changed, and Optimization, Seeding, App and the two TestWindows heads
  were analysed with them. The test files are outside the analyzers' scope.
- The four s02 invariants (`elev-list-empty`, `no-vstest`, `no-kit-kit-json` and
  `no-eps-axis-evaluated`) each find nothing (`.artifacts/0013-invariants.log`).

```yaml
gates:
  constructor_unit_tests: 736
  ui_smoke_tests:         208
  ui_tests:                93
```

`constructor_unit_tests` falls by one by design: the deleted fact's subject is now guaranteed by
the type. `ui_tests` is the count under this step's five-class filter, so it is not comparable
with step 12's MaterialsControlsTests filter.

## Architecture

- **A material's physics is its complexity.** `MaterialEntry` holds only data:
  - `properties` is the engine composition (`complexity.toProperties`), derived on every read;
  - the record has F#'s structural equality;
  - `MaterialEntry.dispersion` binds `properties` once.
- **The view-only editor shows a complexity, never preset properties.** Its preview reads the
  `shown` complexity: properties, gyration components and μ kind. Editability is the single
  predicate `MaterialComplexity.isEditable`, in the editor's `init` and in the Materials window
  alike.
- **`MaterialEditorModel` stays reference-compared**, so the Elmish re-render behaviour is
  unchanged.

## Deferred

- Nothing in this step's scope is deferred. The three remaining `complexity = None/Some` mentions
  (`MaterialsWindowTests.fs:459`, `NkDispersionChartTests.fs:27`,
  `OpticalConstructor.Ui.Tests.fsproj:124`) are step 14's stated scope, not this step's.

## Gotchas

- **`ExportImportTests.fs` was edited outside the listed effects.** Its
  `Assert.Equal(Some vacuumPlaceholderComplexity, e.complexity)` no longer type-checks once the
  field is not an option. The fix drops `Some` and nothing else. This is recorded as a deviation
  in the impl-log.
- **The view-only caption changed in both windows.** The Materials window's caption is now
  ` (view-only)`, and the editor's readout says `view-only`. The editor's reason text is now
  "this entry's physics is not editable term data — view-only".
- **`properties` is recomputed on each read.** A caller that needs it more than once in a tight
  path should bind it once, as `MaterialEntry.dispersion` now does.
- **A deletion in a method-granularity suite never lowers its `count_at_least` floor.** ASG's
  `authorized_count_credits` and `write_checkpoint_counts` credit only `"row"` suites. All four
  of this repo's suites are `"method"` in `.repository-inventory-json`. As a result:
  - this step fails at 736 < 737;
  - accepting it alone leaves the floor at 737, and the refused checkpoint write keeps the
    older floor;
  - every later whole-suite run of `constructor-unit-tests` would then fail the same way.

## Changelog

- 2026-11-05 — step 0013:
  - `MaterialEntry.complexity` is mandatory, and `properties` is a derived member.
  - The option readers, the `previewAnisotropy` fallback and the view-only editor's preset
    fields are deleted.
  - The tests follow: two deleted, three renamed, the rest read the value.
- 2026-11-05 — step 0013, attempt 2:
  - Diagnosed the `constructor-unit-tests` failure (736 < 737): the drop is exactly the
    authorized delete, which the engine does not credit for a method-granularity suite.
  - No code changed. Reported as an authority conflict (`commit_ready: false`).
