# Step 0083 state of the world

## Where we are

Slice 0083 implements S24-SCOPE-LOCATIONS: startup reads settings and resolved data locations together, the app context receives both, and CSV exports use the configured folder. Every named UI test composition uses test-owned locations. All eleven authorized source/project paths are implemented; stores remain in memory as required. The supervisor owns acceptance gate execution and commit readiness.

## What's working

- Read workbench settings and resolved locations through one provider and one save, with defaults on configuration-open failure.
- Pass both values through Startup and write app-scope experiment exports into the configured exports folder.
- Isolate UI test compositions through TestAppScope under the test output directory.
- Add the configured CSV export regression while preserving existing assertions; pass 37 selected cases and 208 smoke cases locally.

## Tests

The local Release/x64 build of the affected UI test project and its dependencies succeeded with zero warnings and zero errors. All 37 cases in the four selected non-smoke classes passed, including the new `create writes experiment exports into the configured exports folder` fact. All 208 headless smoke cases passed. Neither run failed or skipped any case. Logs and TRX reports are under `specs/0050/.artifacts/0083-local-*`.

The initial build found a removed import still required by an existing settings assertion in WireUiFinalCompositionTests. Restoring it fixed the error. That initial build also reported the two existing vendored MathNet SYSLIB0051 warnings already recorded by slices 0081/0082; no vendored source changed.

No supervisor acceptance gate was run or certified. The build, elevation, ui-smoke, selected ui-tests and eleven s11 invariants remain the supervisor's assigned stage. No implementation blocker remains.

The YAML copies the existing supervisor checkpoint 81's smoke baseline. No prior `ui_tests` baseline appears in `.checkpoints-json`; this worker does not create one. The 37 selected local results are observations, not an authored checkpoint.

```yaml
gates:
  ui_smoke_tests: 208
```

## Architecture

AppConfig reuses readDataLocations and resolveLocations over its existing provider. Both workbench and location reads finish before the one save. The open-failure branch returns the standard workbench defaults with resolved DataLocations defaults. Startup passes both values into AppContext.create. The context retains its fields and in-memory stores; only the existing CSV export proxy's folder changes, while its clock remains unchanged. No durable format or migration changed.

TestAppScope composes an in-memory connection, five owner-specific output folders and the shipped catalogue path rooted at the test assembly output. The settings-verbatim fact uses testLocations directly; other named compositions use freshAppContext. The export regression clears its own exports folder's files and exercises the actual file-backed proxy with one two-point series.

## Deferred

No implementation requirement was deferred. Supervisor acceptance verification remains the next assigned stage.

## Gotchas

- Higher-priority development instructions require local verification despite the worker protocol's act-only restriction. Local observations are separate from supervisor gate results.
- The current .spec-md lacks the cited section 3.9; the explicit slice, immutable S24-SCOPE-LOCATIONS oracle and manual 001 section G agree. Fixture docs retain the slice's requested citation.
- The pre-existing manifest-state modification remains untouched. Source diff counts match with and without CR ignored, and all edited/new code files use LF.
- No version, package, analyzer configuration, checkpoint, git state or external repository was changed.

## Changelog

- 2026-11-06: Threaded resolved locations through startup and app scope, isolated UI test scopes, and added the real configured-export regression; local build, 37 selected cases and 208 smoke cases succeeded.
