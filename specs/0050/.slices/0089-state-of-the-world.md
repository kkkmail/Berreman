# Where we are

Slice 0089 completes S24-LAUNCHER-RECREATE in S2.4. The launcher can recover from an incompatible library schema using the existing backup-and-recreate boundary, then open the normal launcher over a freshly composed app scope. The requested implementation and both named acceptance facts are complete; all resolved local checks passed. Formal acceptance remains with the arc-runner supervisor.

## What's working

- Share database app composition between normal startup and library recreation.
- Offer the accessible Recreate library from catalogues action only for an initial schema mismatch.
- Preserve the retry action and show the new diagnostic when recreation fails.
- Preserve the old library in Backups, import the Built-in catalogue and enable all four launcher actions on success.
- Add schema-only visibility and file-backed recovery acceptance coverage; update every launcher construction site.

## Tests

- Release solution build passed in 62.97 seconds with zero errors and no warnings from product or test code. The two existing SYSLIB0051 warnings originate in vendored MathNetNumerics/Numerics/Exceptions.cs.
- Elevation passed over all five affected/dependent product projects in 129 seconds: zero new findings, zero known findings, ELEV004 excluded.
- All 209 ui-smoke cases passed, no failures/skips, in 40.159 seconds. The new named recreation acceptance fact raises the suite from 208 to 209.
- All 8 selected ui-tests cases passed, no failures/skips, in 4.917 seconds. LauncherTests and WireUiFinalCompositionTests gain the one required visibility fact, raising this selection from 7 to 8.
- All eleven s11 section invariants passed using the commands from the resolved slice snapshot.
- Source diffs have no whitespace errors or CRLF churn.

These are observed local counts, not edits to the supervisor's baseline/checkpoint records. Local verification follows the coding environment's higher-priority requirement; it does not replace formal supervisor acceptance.

```yaml
gates:
  ui_smoke_tests: 209
  ui_tests: 8
```

Build, analyzer, test and invariant logs, SARIF reports and TRX reports are under `specs/0050/.artifacts/0089-*`.

## Architecture

AppContext.compose is the single private composition over an open LibraryDatabase. Both create and recreateLibrary map their existing storage boundary result through it. The CreatedLibraryDatabase origin still triggers the existing CatalogueImportProxy over the configured shipped catalogue; settings and all session proxies retain their previous composition.

LauncherWindow now receives DataLocations, WorkbenchSettings and the initial open result. One local function constructs content from the current result. The original schema-mismatch result establishes remedy eligibility; subsequent errors retain the remedy for retry, while success removes the error and remedy and binds the four surface actions to the new context. The action asks no confirmation because the preserved backup is its undo. Existing scrolling, window sizing and shared editor launchers remain in use. There is no durable shape, schema version or migration change.

## Deferred

None within slice 0089. Supervisor acceptance and commit handling remain outside the worker's local verification.

## Gotchas

- Initial disk/database failures cannot be remedied by recreation and never offer the button. A backup/IO failure after an offered schema remedy must keep that button available.
- File-backed fixtures live under the test output via TestAppScope.fileLocations. Preparation and inspection connections disable pooling; the incompatible database is closed before recreation.
- The acceptance fact first blocks the backups path with a file, verifies the original bytes and disabled actions survive the failure, removes the obstruction and retries. The successful retry leaves exactly one byte-identical backup with schema 2, a current-schema live database and the Built-in catalogue's entry count.
- AppContext's database must remain alive through the store closures; do not introduce a use binding in compose.
- The supervisor-owned specs/0050/.manifest.state.json change is untouched. No Git mutations, version bumps, other-repository changes, schema migrations or analyzer policy changes were made.

## Changelog

- 2026-11-06: Added launcher schema-mismatch recreation with preserved backups, shared catalogue composition, retry diagnostics and the two required facts; build, elevation, 209 smoke cases, 8 selected UI cases and eleven invariants passed locally.
