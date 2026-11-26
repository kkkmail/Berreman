# Step 0082 state of the world

## Where we are

Slice 0082 implements S24-SETTINGS-READ: the composition-root reader for all seven data locations and isolated settings for UI test startup. All four authorized product/test files are implemented. The existing workbench loader and its callers remain unchanged as required by this slice. The supervisor owns acceptance gate execution and commit readiness.

## What's working

- Added six folder keys and readDataLocations using the existing settings provider and folder validation door.
- Preserved configured text and default write-backs while returning expanded locations rooted at the install folder when relative.
- Added test-owned appsettings.json with an in-memory library and folders under the test output.
- Added all four declared AppConfigTests facts; local configuration tests and all 208 smoke cases passed.

## Tests

The local Release/x64 build of the affected UI test project and its dependencies succeeded with zero warnings and zero errors. The four required AppConfigTests facts passed with no failures or skips, and all 208 existing headless smoke cases passed with no failures or skips. Logs and TRX reports are under `specs/0050/.artifacts/0082-local-*`.

The initial build found two percent-escaping errors in the new test's interpolated environment-variable strings; both were fixed before the successful build and test runs. That initial build also reported the two pre-existing vendored MathNet SYSLIB0051 warnings recorded in slice 0081; no vendored code changed.

No supervisor acceptance gate was run or certified. Build, elevation, ui-smoke, selected ui-tests, and the eleven s11 invariants remain the supervisor's assigned stage. No implementation blocker remains.

The YAML copies the existing supervisor checkpoint 73's smoke baseline. No prior `ui_tests` baseline is recorded in `.checkpoints-json`; this worker does not create one. The four new selected facts are local observations, not an authored baseline.

```yaml
gates:
  ui_smoke_tests: 208
```

## Architecture

DataLocations remains pure configured data in Domain. AppConfig reads its defaults through the existing provider doors, handles connection-string read/write failures with warnings, and resolves environment variables at a marked IO boundary. Relative folders use FolderName.combine under the injected install folder. The reader neither saves the settings file nor creates resolved locations; its caller retains save ownership. No durable format migration or new dependency was introduced.

The test project copies its own appsettings.json over the transitive App content item. Its integration fact executes inside HeadlessSession.run, after the only other writer, the real App startup, has finished. Other facts use fresh settings files beneath the test assembly output and a unique process environment variable removed in finally.

## Deferred

No implementation requirement was deferred. Supervisor acceptance gate execution remains the next assigned stage.

## Gotchas

- Higher-priority development instructions require local verification despite the worker protocol's act-only rule; local observations are separate from supervisor acceptance.
- The mandated readSetting reuse retains that existing helper's internal tryGet/getOrDefault calls. The new reader requests each folder once through that seam.
- The pre-existing manifest-state modification remains untouched. All four edited/new code/config files use LF, and no version, analyzer configuration, checkpoint, or git state was mutated.

## Changelog

- 2026-11-06: Added the data-location settings reader, isolated UI settings, and four configuration facts; local build, four new facts, and 208 smoke cases succeeded.
