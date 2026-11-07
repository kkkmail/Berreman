# Where we are

Slice 0090 completes S24-LIBRARY-BACKUP-UI in S2.4. Every product Library-window launch path receives the app-scope backup proxy. The six specified model/ID facts and the database-backed headless button acceptance fact are implemented. All resolved local checks passed; supervisor acceptance remains separate.

## What's working

- Compose the database backup store once and thread it through Launcher, Main, Inverse and Select Library windows.
- Add accessible backup and restore controls with stable IDs, typed refusals and named backup statuses.
- Require a listed backup selection before restore; preserve the current library through the existing store and clear stale entry selection after success.
- Keep restore, remove and lifecycle confirmations mutually exclusive, with distinct Restore/Cancel actions.
- Add six model/ID facts and one real-window, file-backed backup/restore acceptance fact.

## Tests

- Final Release solution build passed in 30.54 seconds with zero warnings and errors.
- Elevation passed over all five affected/dependent product projects in 135 seconds: zero new or known findings, ELEV004 excluded.
- All 210 ui-smoke cases passed, zero failures/skips, in 42.723 seconds. The new real-window backup/restore fact raises the suite from slice 0089's 209 to 210.
- All 82 selected ui-tests cases passed, zero failures/skips, in 17.419 seconds, including all six required model/ID facts. The selected classes are AppContextTests, LibraryWindowTests, LightSourceEditorTests, MainWorkbenchTests, ScrollExtentTests, WireUiFinalCompositionTests and WorkbenchInfoAndCaptureTests.
- All eleven s11 section invariants passed using the resolved snapshot commands.
- Source diffs have no whitespace errors or CRLF churn.

Local verification follows the higher-priority workspace requirement despite the task's act-only instruction. These are observed counts; no supervisor checkpoint/baseline file was edited.

```yaml
gates:
  ui_smoke_tests: 210
  ui_tests: 82
```

## Architecture

LibraryBackupProxy remains the sole backup/restore implementation. AppContext.compose captures its existing database, configured Backups folder and local-time provider. LibraryWindowView holds only typed confirmation state, selections, captions and the injected proxy. Its restore panel uses the existing button styling; the extracted background-aware verb helper also highlights the selected backup without copying button rendering. Both restore success and failure are projected through Elmish state. LibraryBackupName owns its uiKey and caption conversions; LibraryWindowModel owns the internal backup-status projection.

No durable shape, schema, package, version or migration changes are introduced. Source constructors and launcher signatures gain the required proxy with no compatibility path.

## Deferred

None within slice 0090. Supervisor acceptance and commit handling remain external.

## Gotchas

- In-memory backup proxies hold names only. The acceptance fact uses TestAppScope.fileLocations and AppContext.create to prove real SQLite content restoration through live store closures.
- Restore is inert with no valid pick. A failed restore keeps the panel open; cancellation changes only the restore gate.
- The pre-existing Remove path did not disarm lifecycle confirmation. Its arming branches now do so to preserve the slice's single-confirmation obligation in every direction.
- The spec's literal `(ControlCaption name.value)` row expression produced ELEV001. Following the repository's analyzer-precedence rule, LibraryBackupName.caption now owns that conversion; displayed text is unchanged. The initial single-argument status helper similarly moved onto LibraryWindowModel to satisfy ELEV013P.
- The supervisor-owned specs/0050/.manifest.state.json change is untouched. No Git mutations, other-repository changes or analyzer policy changes were made.

Final evidence is under specs/0050/.artifacts: 0090-build-repair-final.log, 0090-elevation-repair.log and its SARIF folder, 0090-ui-smoke.log/.trx, 0090-ui-tests.log/.trx and 0090-invariants.log. Earlier diagnostic and repair logs remain there for the audit trail.

## Changelog

- 2026-11-06: Wired library backup/restore through every product launch path and added the seven required facts; build, elevation, 210 smoke cases, 82 selected UI cases and eleven invariants passed locally.
