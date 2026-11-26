# Step 0042 state of the world

## Where we are

Spec 0050 S1.6 now composes the default Main and table/element test scenes from
the shipped Built-in catalogue, satisfying the S16-TEST-SCENES implementation
scope. The shared fixture introduced in step 0041 supplies the new acceptance
fact's expectations. Formal gate acceptance remains with the arc-runner.

## What's working

- Imported fresh scene stores from the shipped Built-in catalogue through the existing proxies.
- Logged import failures with their descriptions while retaining successfully imported entries.
- Preserved the Select bay's By-kind tree and composed live samples and user sources over imported presets.
- Added the default-scene catalogue parity fact for all materials, samples, categories and presets.

## Tests

Local Release/x64 solution build succeeded with zero warnings and zero errors.
All 799 constructor tests, 208 UI smoke tests and 209 selected UI tests passed,
with zero failures and zero skips. The selected run covers exactly the eleven
classes in the slice roster, including LibraryControlsTests and the new
`the default test scene composes its stores from the Built-in catalogue` fact.
Logs and TRX reports are under `specs/0050/.artifacts/0042-local-*`; exact paths
are recorded in the implementation log.

The initial build identified an incorrect module qualifier in the new fact;
it was corrected to `MaterialLibrary.IncludeInactive` before the successful
build and test runs. The final diff has no whitespace errors or CRLF churn.

No formal gate was run or green-lit. The supervisor's pending roster is
`build`, `elevation`, `constructor-unit-tests`, `ui-smoke`, `ui-tests`,
`invariant-s06-elev-list-empty`, `invariant-s06-no-vstest`,
`invariant-s06-no-kit-kit-json`, `invariant-s06-no-eps-axis-evaluated`,
`invariant-s06-complexity-mandatory`, and `invariant-s06-no-seeded-string-preset-ids`.

These are the existing supervisor checkpoint 40 counts, unchanged by this
worker. No `ui_tests` checkpoint exists; zero denotes that absence, not this
round's observed result. The supervisor owns baseline updates.

```yaml
gates:
  constructor_unit_tests: 799
  ui_smoke_tests: 208
  ui_tests: 0
```

## Architecture

`DefaultStores.create` uses `CatalogueImportProxy.createInMemory` and imports
`CatalogueProxy.createBuiltIn` through `importFrom`. Its five-field result uses
the import's material, sample and category stores; user sources are fresh and
empty, and `liveLibrary` combines them with imported samples and presets. An
import error is logged once through `Logger.logError` with `e.describe`; the
scene receives whatever was imported before that error.

The import bundle overrides only its library read proxy's `libraryTrees` field
with `(Library.createInMemory ()).libraryTrees`. Entry queries still close over
the imported store. The existing tree and its frozen ids are reused directly;
no grouping, ordering or fallback reader was added. The test sorts complete
records by id for comparison without imposing production order. No serialized
shape changed and no migration applies.

## Deferred

Nothing within this slice's implementation scope. The supervisor's formal
acceptance is pending, with no waived requirement.

## Gotchas

- Local validation follows the coding-session instructions despite the worker
  role's act-only wording; formal gate collections remain empty.
- The catalogue must be available in the output's `Catalogue/Builtin/` folder,
  copied by the existing Storage content wiring.
- The existing grouping function remains the source of Select-bay tree order;
  imported preset queries preserve catalogue insertion order.
- The pre-existing supervisor manifest-state modification is untouched. No
  version bump, git mutation, catalogue write or external-repository edit occurred.
- The initial rebuild's existing vendored MathNet warnings are recorded in the
  log; the final build reported zero warnings. No suppressions were added.

## Changelog

- 2026-11-05: Imported default scene stores from Built-in catalogue files, retained the By-kind tree, and added catalogue parity coverage; local build and 799 constructor, 208 smoke and 209 selected UI tests passed.
