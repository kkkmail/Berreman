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

The step's tree was judged review-green as `969e23357ffd`. Its only red was an engine floor that
did not credit the authorized delete. The operator committed the tree as `d8a56f5` and fixed the
ASG credit, and this relaunch hands that tree, unchanged, to the gates.

## What's working

- `MaterialEntry.complexity` is mandatory, and `properties` is a member computed from it; entries compare structurally
- Every construction and copy-and-update drops `properties = …` and `Some` (Storage, Ui and test fixtures)
- Facets, the store payload, the out-of-band diagnostic and the Materials window read the complexity directly; `previewAnisotropy` is deleted
- The view-only editor case is `ViewOnlyMaterial of reason * shown : MaterialComplexity`, and `presetProperties` / `viewOnlyComplexity` are deleted
- Two facts that lost their subject are deleted, and three Save / view-only facts are renamed

## Tests

**This relaunch ran no gate.** The gate engine is the only gate authority (Invariant 6), and the
operator's note hands the tree to it. This round made read-only checks only.

- **The tree is the judged tree.** The `cleaned_sha256` of
  `git diff --binary f53174e d8a56f5 -- Berreman/` is
  `969e23357ffdabcbfb7e6f02b042dfd0672f6ae7002799903925d378e89ddf6e`. That is the fingerprint the
  code-judge reviewed green and the engine measured. The diff is saved as
  `.artifacts/0013-relaunch-step-diff.txt`.
- **The tree meets the slice.**
  - The record holds `complexity : MaterialComplexity` and `member this.properties`.
  - `previewAnisotropy`, `presetProperties` and `viewOnlyComplexity` occur nowhere.
  - The editor case is `ViewOnlyMaterial of reason : ControlCaption * shown : MaterialComplexity`.
  - The test attributes drop by one in each test project.
  - The three successor names are present, and the old names and the two deleted facts are gone.
  - No changed file says `complexity = None`, `complexity = Some` or `Some complexity`.
  - Line endings are LF.
- **The floor fix is in ASG.** `authorized_count_credits` (`executability/runtime_tests.py:106`)
  now credits `"method"` suites. Both the gate baseline (`evaluate_gate.py:408-412`) and the
  checkpoint write (`run_arc_runner.py:4826-4831`) read it. So `constructor-unit-tests` is held
  to 737 − 1 = 736.
- **What the gates should find** on this tree, from attempt 1's local runs and the engine's
  attempt-2 measurement:
  - `build` green, 0 errors;
  - `elevation` `ELEV-GATE: OK`, 0 new findings in 8 projects;
  - `constructor-unit-tests` 736/736, with the identity census accepted by the engine;
  - `ui-smoke` 208/208, both renamed facts passing;
  - `ui-tests` 93/93 under the five-class filter;
  - the four s02 invariants find nothing.

  The engine has not yet run ui-smoke, ui-tests or the invariants on this tree.

```yaml
gates:
  constructor_unit_tests: 736
  ui_smoke_tests:         208
  ui_tests:                93
```

`constructor_unit_tests` falls by one by design: the deleted fact's subject is now guaranteed by
the type, and the engine credits that delete. `ui_tests` is the count under this step's
five-class filter, so it is not comparable with step 12's MaterialsControlsTests filter.

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
  field is not an option. The fix drops `Some` and nothing else. It is recorded as a deviation
  in the impl-log, and the code-judge accepted it.
- **The view-only caption changed in both windows.** The Materials window's caption is now
  ` (view-only)`, and the editor's readout says `view-only`. The editor's reason text is now
  "this entry's physics is not editable term data — view-only".
- **`properties` is recomputed on each read.** A caller that needs it more than once in a tight
  path should bind it once, as `MaterialEntry.dispersion` now does.
- **The relaunch's `BASELINE` line still logs `constructor_unit_tests: 737`.** That is expected.
  The credit for the authorized delete is applied inside the gate evaluation
  (`credited_counts`), not at baseline resolution.
- **The boot's "worktree fingerprint drift" comes from the operator's commit, not from an
  edit.** The product content still hashes to `969e23357ffd`.
- **A local test count is not the gate.** Attempt 1 read 736 as a pass without comparing it with
  the engine's credited floor. Only the engine's floor decides the gate.

## Changelog

- 2026-11-05 — step 0013:
  - `MaterialEntry.complexity` is mandatory, and `properties` is a derived member.
  - The option readers, the `previewAnisotropy` fallback and the view-only editor's preset
    fields are deleted.
  - The tests follow: two deleted, three renamed, the rest read the value.
- 2026-11-05 — step 0013, attempt 2:
  - Diagnosed the `constructor-unit-tests` failure (736 < 737): the drop is exactly the
    authorized delete, which the engine did not credit for a method-granularity suite.
  - No code changed. Reported as an authority conflict (`commit_ready: false`).
- 2026-11-05 — step 0013, relaunch after the operator reply:
  - No product or test edit. Confirmed that the committed tree `d8a56f5` is byte-identical to the
    judged tree `969e23357ffd` and meets every clause of the slice.
  - Confirmed that ASG now credits the authorized delete (737 − 1 = 736), and handed the tree to
    the gates.
