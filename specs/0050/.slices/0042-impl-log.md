# Step 0042 implementation log

## Progress

- Read the worker protocols, project prompt, root instructions, naming rules,
  slice, gate snapshot and S16-TEST-SCENES acceptance oracle.
- Before implementation: confirmed `DefaultStores` still self-seeds and the
  catalogue import's preset proxy currently returns an empty tree. The shared
  `BuiltinCatalogue` fixture is already linked into the UI suite.
- Added the required composition fact before the production edits. It compares
  complete sample, material and category records sorted only in the test by id,
  and asserts every imported preset resolves to itself through the scene library.
- Before production changes: reuse the existing catalogue reader/importer and
  grouping function directly, preserving the live store queries and failure data.
- Replaced self-seeding in `DefaultStores.create` with a fresh catalogue import,
  descriptive error logging and the existing live-library composition. Removed
  the obsolete opens and replaced the outdated composition narrative.
- Updated the import bundle's library read proxy to reuse the existing By-kind
  tree function, retaining its imported entry queries and insertion order.
- Before validation: normalize the three edited source files to LF and perform
  local build/test verification required by the coding-session instructions.
- First local build: production projects compiled; the new fact used the wrong
  module qualifier for `IncludeInactive` (FS0039). Correct its qualifier before
  rebuilding; the assertions and their acceptance scope remain unchanged.
- Corrected the qualifier to `MaterialLibrary.IncludeInactive`. The full
  Release/x64 solution build then succeeded with zero warnings and zero errors.
- Local constructor tests passed: 799 succeeded, zero failed or skipped.
- Local UI smoke tests passed: 208 succeeded, zero failed or skipped.
- Started local non-smoke verification over exactly the eleven UI classes in
  the slice's selected roster, including the required composition fact.
- Selected UI verification passed: 209 succeeded, zero failed or skipped.
  The TRX contains all eleven declared classes and the exact new fact name.
- Reviewed the final source diff: only the three authorized F# files changed;
  no whitespace errors or CRLF churn. No production ordering was added.
- Wrote the prescribed state-of-the-world file with the inherited checkpoint
  counts, local validation observations and pending supervisor acceptance.

## Files modified

- Updated `OpticalConstructor.Domain/CatalogueImport.fs`.
- Updated `OpticalConstructor.Ui/TableAndElementRotationView.fs`.
- Added the declared fact in `OpticalConstructor.Ui.Tests/TableAndElementRotationTests.fs`.
- Created the prescribed implementation plan and this log.
- Created `specs/0050/.slices/0042-state-of-the-world.md`.

## Testing state

Local Release/x64 solution build passed with zero warnings and errors after
correcting the test's module qualifier. All 799 constructor tests, 208 UI smoke
tests and 209 selected UI tests passed, with no failures or skips. The new fact
checks complete records by id, not only counts or presence.

No formal gate was run or green-lit. Formal execution and acceptance belong to
the arc-runner: build, elevation, constructor-unit-tests, ui-smoke, selected
ui-tests, invariant-s06-elev-list-empty, invariant-s06-no-vstest,
invariant-s06-no-kit-kit-json, invariant-s06-no-eps-axis-evaluated,
invariant-s06-complexity-mandatory and invariant-s06-no-seeded-string-preset-ids.
There is no implementation blocker or deferred slice requirement.

## Artifacts

Persistent run artifacts belong only in `specs/0050/.artifacts/`.

- `0042-local-build.log`: initial build with the corrected FS0039 diagnostics.
- `0042-local-build-retry.log`: successful full solution build.
- `0042-local-constructor-tests.log`, `0042-local-constructor-results/constructor.trx`.
- `0042-local-ui-smoke.log`, `0042-local-smoke-results/smoke.trx`.
- `0042-local-ui-tests.log`, `0042-local-ui-results/ui.trx`: selected UI run.

## Gotchas

- The existing dirty `specs/0050/.manifest.state.json` belongs to the supervisor
  and must remain untouched.
- The existing grouping is reused directly; catalogue row order is intentionally
  not normalized in production.
- The coding-session instructions require local validation despite the worker
  protocol's act-only wording. Local observations will be recorded separately
  from formal gate results; no gate acceptance will be claimed by this worker.
- The initial rebuild also reported the existing vendored MathNet SYSLIB0051
  warnings. The final build reported zero warnings; no warning suppressions or
  vendored-code changes were introduced.
- The required test is a composition parity assertion; it was added first, but
  no pre-change red result is claimed. This slice changes the source of values
  while preserving the shipped built-ins' content.
- No version bump, git mutation, external-repository write or catalogue change
  occurred. No deviation from S16-TEST-SCENES was needed.
