# Step 0081 state of the world

## Where we are

Slice 0081 adds S2.4's shared data-location defaults and configurable shipped-catalogue factory. All requested production and test changes are implemented in the four authorized files, and local build/test verification succeeded. The supervisor owns acceptance gates and commit readiness.

## What's working

- Added the seven elevated DataLocations fields with pure configured defaults and a shared relative shipped-catalogue default.
- Added CatalogueProxy.createBuiltInUnder and routed createBuiltIn through the shared default and folder composition.
- Added the two required facts covering all default texts, the shipped catalogue, and typed refusal of an empty shipped folder.

## Tests

The affected constructor test project built successfully in Release/x64 with zero errors and no constructor-code warnings. The full local constructor suite passed 891 tests, zero failed/skipped, in 17 seconds. The TRX confirms both exactly named new facts passed, increasing the prior count by two.

The build reported the same two vendored MathNet SYSLIB0051 warnings at Exceptions.cs lines 24 and 77 recorded by slice 0080. No vendored source changed. Build/test logs and the TRX are under specs/0050/.artifacts/0081-local-build.log, 0081-local-tests.log and 0081-local-tests/constructor-tests.trx.

No acceptance gates have been run or reported green. The supervisor will run build, elevation, constructor-unit-tests and the eleven s11 invariants from 0081.gates. Commit readiness remains unclaimed pending that stage; no implementation blocker remains.

The YAML copies the existing supervisor checkpoint 79, not a newly authored acceptance baseline. The locally observed count is 891.

```yaml
gates:
  constructor_unit_tests: 889
```

## Architecture

DataLocations reuses Softellect.Sys ConnectionString and FolderName. It is separate from WorkbenchSettings, whose record and defaults are unchanged. Defaults retain environment-variable placeholders and compose Documents children using FolderName.combine without reading the environment or disk. No durable format changes or migrations are introduced.

CatalogueProxy.createBuiltInUnder reuses the existing folder proxy and its IO error mapping. The Builtin child name has one production home in that factory; the relative Catalogue default has one home in DataLocations. Environment expansion and rooting configured text remain outside this slice.

## Deferred

No implementation requirement was deferred. Acceptance gate execution remains the supervisor's assigned stage.

## Gotchas

- Higher-priority development instructions require local verification despite the worker's act-only protocol; local observations are separate from supervisor-owned acceptance results.
- The defaults test's D:/Docs is text only. The catalogue test reads the shipped output and creates its empty fixture only beneath the test assembly output.
- The pre-existing manifest-state modification remains untouched. No git mutation, version bump, checkpoint edit, analyzer change or project-file edit was made.
- The four code files have matching ordinary and ignore-cr-at-eol diff counts; no line-ending churn was introduced.

## Changelog

- 2026-11-06: Added DataLocations defaults, configurable shipped-catalogue composition, and the two required facts; all 891 local constructor tests passed.
