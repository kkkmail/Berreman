# Where we are

Slice 0099 completes S31-REGISTRATION on top of step 0098's pending catalogue imports. Extra-folder registration now rejects reserved or already registered catalogue identities and returns every row for explicit confirmation. Settlement validates the complete selection before applying and logging it. All checks in the resolved slice roster passed locally; supervisor acceptance and commit remain external.

## What's working

- Added read-only folder registration with diagnostic errors for Built-in and already registered catalogue identities.
- Required exactly one choice for every pending row, including reuse, before settlement applies any change.
- Applied confirmed actions against authoritative pending decisions and logged successful imports with the settlement time.
- Listed extra registered folders and preserved imported entries when their registration was forgotten.
- Added four shared facts over both stores and replaced startup fixtures' explicit apply-and-log sequence with settle.

## Tests

- Expected-red compilation confirmed the absent registration, settlement, listing and error surface before implementation.
- Release solution build passed with zero warnings and zero errors in 45.31 seconds.
- Elevation passed across seven affected/dependent product projects with zero new findings in 142 seconds; ELEV004 excluded as prescribed.
- All 968 constructor cases passed, zero failed/skipped, in 33.660 seconds: exactly eight above the previous measured 960.
- TRX audit confirmed all nine specified test effects: eight new inherited cases and the updated error-rendering expectation.
- All eleven resolved s12 section invariants passed. Source whitespace and line-ending diff checks passed.

These are measured local results; supervisor checkpoint and acceptance-baseline files were not edited.

```yaml
gates:
  constructor_unit_tests: 968
```

Evidence is under specs/0050/.artifacts: 0099-build-red.log, 0099-build.log, 0099-elevation.log and 0099-elevation-reports/, 0099-constructor-unit-tests.log/.trx, 0099-test-coverage.log and 0099-invariants.log.

## Architecture

CatalogueImportContext.registerFolder reads the manifest through the existing folder proxy, checks the catalogue identity against the log and the reserved Built-in identity, then reuses importFolder. CatalogueImportContext.settle validates membership and uniqueness by entryId, pairs the chosen actions with the original pending decisions, delegates action/reference validation and writes to apply, and logs only after success using now. CatalogueImportProxy.registeredFolders filters loggedImports without changing its order; forgetImport remains the unregister operation. No durable schema, stored contract or migration changed.

## Deferred

None within this slice. Every specified implementation, test effect and required local check is complete.

## Gotchas

- The explicit all-row confirmation requirement supersedes the slice's older CatalogueImported wording: registerFolder returns CatalogueAwaitsChoices and does not write. Registration fixtures settle the full pending list before expecting completed registration effects.
- The supplied choice's action is checked against the pending decision, so replacing a choice's decision metadata cannot authorize a different action.
- A Built-in catalogue is refused even before it has a log row; when a catalogue is logged, duplicate diagnostics include both the requested folder and its existing folder.
- Settlement inherits apply's partial-write failure contract; it adds no rollback transaction and never logs success after an apply failure.
- The common fixture confirmation helper now calls settle. All fresh folders remain under the test output directory; committed catalogues and data were not modified.
- Higher-priority workspace verification instructions require local checks despite the worker prompt's act-only wording. These results do not replace supervisor acceptance.
- The supervisor's pre-existing .manifest.state.json edit was left untouched. No Git mutation, version bump, external-repository write, baseline edit or analyzer-policy change occurred.

## Changelog

- 2026-11-06: Implemented catalogue folder registration, complete-choice settlement and extra-folder listing; Release build, elevation, all 968 constructor cases and eleven s12 invariants passed locally.
