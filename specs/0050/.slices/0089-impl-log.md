# Step 0089 implementation log

## Progress

- Read AGENTS.md, the cross-repository worker base and IMPLEMENT role, the project prompt, slice 0089 and its gate snapshot, and naming_conventions.md.
- Located the existing AppContext composition, launcher content, test fixtures and database recreation seam. The initial working tree contains a supervisor-owned change to specs/0050/.manifest.state.json; leave it untouched.
- Before implementation: add the specified acceptance facts and update launcher test construction, then extract composition and wire the remedy.
- Added both named LauncherTests facts before the product edits, including a blocked-backup/retry scenario in the acceptance fact, byte-preservation assertions, schema stamps and the imported catalogue entry count. Updated every product-launcher test call site.
- Extracted AppContext.compose and added recreateLibrary over the existing LibraryDatabase.recreate boundary. Rebuilt launcher content from each outcome, with initial schema-mismatch eligibility preserved across recreation failures. Added the centralized accessible button ID and wired startup locations/settings.
- Before verification: run local Release build, elevation, the resolved UI selections and section invariants. The coding environment's higher-priority verification instruction requires local checks despite the worker role's act-only text; formal acceptance and checkpoints remain supervisor-owned.
- Release solution build passed in 62.97 seconds, with zero errors and no warnings from product/test code. Two existing SYSLIB0051 warnings came from vendored MathNetNumerics/Numerics/Exceptions.cs. The five source diffs have matching normal/ignore-CR numstats and no whitespace errors. Begin elevation, then the UI selections.
- Elevation passed across all five affected/dependent product projects: zero new findings, zero known findings, ELEV004 excluded, 129 seconds. Begin the full ui-smoke selection, then the two selected ui-tests classes.
- All 209 ui-smoke cases passed with no failures/skips in 40.159 seconds, including the new file-backed recreation acceptance fact. All 8 selected ui-tests cases passed with no failures/skips in 4.917 seconds, including schema-only remedy visibility. The smoke suite grows from 208 to 209; the selected non-smoke classes grow from 7 to 8.
- All eleven s11 invariant commands passed without modifying their snapshot, analyzer rules or known findings. No source repairs were needed after the first build and checks.
- Before handoff: record the completed slice in the required state-of-the-world file and inspect final source scope, line endings and acceptance-test discovery in the captured TRX files.
- Handoff complete: saved 0089-state-of-the-world.md at the supplied path. Both exact required test names appear as Passed in their respective TRX files. Final source scope is exactly the five authorized files; owned diffs remain whitespace-clean with no CRLF churn. The existing manifest-state change remains untouched. No implementation requirement is deferred.

## Files modified

- OpticalConstructor.Ui/AppContext.fs: shared composition and recreation entry point.
- OpticalConstructor.App/Program.fs: launcher arguments, remedy and content replacement, startup wiring.
- OpticalConstructor.Controls/UiIds.fs: stable recreation button ID.
- OpticalConstructor.Ui.Tests/LauncherTests.fs: updated callers and the two specified facts.
- OpticalConstructor.Ui.Tests/WireUiFinalCompositionTests.fs: updated launcher construction.
- Worker plan and this log at the supervisor-provided paths.

## Testing state

All resolved local checks passed: Release solution build, elevation, 209 ui-smoke cases, 8 ui-tests cases from LauncherTests and WireUiFinalCompositionTests, and all eleven s11 invariants. No failures or skips were carried. The build's only warnings are the two existing SYSLIB0051 diagnostics in vendored MathNet, with none from product/test code.

The worker role's no-check instruction conflicts with the higher-priority coding-environment requirement to run appropriate tests and required checks; local runs satisfy the latter and do not substitute for supervisor acceptance. Test report destinations and elevation report destinations were redirected into the supplied artifacts folder; gate selections and commands' assertions were preserved. Baseline/checkpoint files remain supervisor-owned and untouched.

## Artifacts

- specs/0050/.artifacts/0089-build.log: Release solution build output.
- specs/0050/.artifacts/0089-elevation.log and 0089-elevation-reports/: analyzer output and SARIF reports.
- specs/0050/.artifacts/0089-ui-smoke.log and 0089-ui-smoke-results/ui-smoke.trx: all discovered smoke cases.
- specs/0050/.artifacts/0089-ui-tests.log and 0089-ui-tests-results/ui-tests.trx: selected non-smoke cases.
- specs/0050/.artifacts/0089-run-invariants.py: executes the eleven resolved invariant commands directly from the immutable slice gate snapshot, stopping on failure.
- specs/0050/.artifacts/0089-invariants.log: all eleven invariant results.

## Gotchas

- An initial non-schema open error must not offer recreation. Once a schema-mismatch remedy has been offered, an error from its click must keep the button available for retry, even when that new error is not a schema mismatch.
- Test counts and baseline checkpoints are supervisor-owned; this worker will not invent or update measured counts.
- No implementation deviation: the failed-backup/retry assertions extend the required acceptance fact to cover the slice's explicit failure behavior without adding an extra fact or changing gate membership.
- Do not dispose the database in AppContext.compose: the returned store closures retain it for the live app scope, as before extraction.
