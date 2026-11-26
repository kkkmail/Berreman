# Implementation log — slice 0088

## Progress

- Read AGENTS.md, both worker protocols, the project prompt, slice 0088 and its
  resolved gate snapshot, naming conventions and the relevant spec authority.
- Inspected the app composition, launcher, database constructors and existing
  headless fixtures. The app currently imports into in-memory stores on every
  create; all dependent composition tests already use TestAppScope.
- Before implementation: add the three specified regression facts and adapt the
  fixture/result consumers, then wire the existing database and launcher seams.
- Added the three exact declared facts before product edits. File reopen compares
  every sample after removing one; mismatch checks both unchanged bytes and the
  persisted stamp; launcher failure checks the diagnostic/accessibility contract
  and raises each disabled button's Click event to prove there is no handler.
- Wired AppContext to the existing SQLite constructors and created-only catalogue
  import. Open errors remain unchanged Results. Updated the launcher and its
  consumers, and placed its content in a vertical scroller so wrapped errors do
  not make the lower buttons unreachable.
- Before verification: inspect the complete owned diff, normalize touched files
  to LF, then run the Release build and resolved test/analyzer/invariant roster.
- Release build completed successfully in 63.24 seconds with no errors. It
  emitted two existing SYSLIB0051 warnings from vendored MathNet's Exceptions.cs;
  the product/test F# changes emitted no warnings. The vendor source is outside
  this slice's authorized effects and was not changed.
- Owned diff has no whitespace errors or CRLF churn. The only CRLF warning is
  the supervisor-owned manifest state. Next: elevation, then both UI selections
  and the eleven invariant checks in snapshot order.
- Elevation passed in 137 seconds: zero findings across Controls, Ui, App and
  both dependent TestWindows projects, with only the mandated ELEV004 exclusion.
- All discovered UI smoke facts are running over the Release output. Local TRX
  reports are directed into the supplied artifacts folder; test database fixtures
  remain under the test output as required by the repository.
- UI smoke passed: 208 succeeded, zero failed/skipped, 40.464 seconds.
- Selected UI tests initially reported 39 passed / 1 failed. The new launcher
  fact counted every Button in the visual tree, including four scrollbar template
  buttons introduced when the error text overflows. The existing semantic probes
  already assert each of the exact four required IDs, disabled state and inert
  Click actions. Before repair: remove only the redundant all-descendants count;
  retain every required surface assertion. This corrects the test probe, not the
  launcher behavior or acceptance contract. The two database regressions passed.
- Removed that redundant count and retained all four explicit stable-ID probes,
  disabled-state assertions and synthetic-click checks. Rebuilt the complete
  Release solution: zero warnings/errors, 11.78 seconds.
- Selected UI tests passed after the probe correction: 40 succeeded, zero
  failed/skipped, 5.697 seconds. Exactly three facts were added; none were removed
  or renamed, taking this selection from 37 to 40.
- Ran all eleven invariant commands directly from the immutable 0088 gate
  snapshot, in order: all passed. No gate definitions or baselines were edited.
- Local verification is complete. The final change after elevation and smoke was
  confined to the non-smoke test probe, so those successful runs remain applicable.

## Files modified

- Planning and implementation log created at the supervisor-supplied paths.
- Product: `OpticalConstructor.Ui/AppContext.fs`,
  `OpticalConstructor.App/Program.fs`, `OpticalConstructor.Controls/UiIds.fs`.
- Tests: `TestAppScope.fs`, `AppContextTests.fs`, `LauncherTests.fs`,
  `WireUiFinalCompositionTests.fs` in `OpticalConstructor.Ui.Tests`.

## Testing state

- Final build, elevation, all 208 UI smoke tests, all 40 selected UI tests and all
  eleven s11 invariants passed locally. Formal acceptance and count checkpoints
  remain owned by the supervisor. The coding environment requires local verification;
  that higher-priority requirement governs local checks despite the worker role's
  act-only instruction. No supervisor acceptance or checkpoint is claimed here.
- The three declared identities are included in the passing ui-tests TRX report.
  There is no remaining failure or unfinished slice requirement.

## Artifacts

- Persistent run artifacts belong under `specs/0050/.artifacts/0088-*`.
- `0088-build.log`, `0088-elevation.log`, `0088-elevation-reports/`,
  `0088-ui-smoke.log` and `0088-test-results/`.
- Final build: `0088-build-after-fixture-fix.log`.
- Final selected tests: `0088-ui-tests.log` and `0088-test-results/ui-tests.trx`.
- Initial probe failure retained in `0088-ui-tests-before-fixture-fix.log` and
  `0088-test-results/ui-tests-before-fixture-fix.trx`.
- Invariants: `0088-invariants.log` and `0088-invariant-results.json`.

## Decisions and gotchas

- `LibraryDatabase` stays alive through the proxies' captured reference; do not
  use a scoped `use` binding in AppContext.create.
- Fresh file fixtures relocate writable locations under the owner's GUID folder.
  The shipped catalogue stays at the read-only test-output asset, matching
  testLocations and avoiding a duplicate catalogue fixture.
- Existing modification to `.manifest.state.json` belongs to the supervisor and
  is outside this implementation.
- ScrollViewer templates contain their own buttons. Launcher action assertions
  use the four stable surface IDs rather than counting framework descendants.
