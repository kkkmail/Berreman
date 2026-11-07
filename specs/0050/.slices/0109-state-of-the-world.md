# Where we are

Slice 0109 implements S32-IMPORTS-THREADED in S3.2. The existing catalogue import
context now reaches every Materials and Sample editor window, including the
Sample editor's Select-state Materials picker. This is dependency threading
only. Implementation and all resolved checks are complete; formal acceptance
remains supervisor-owned.

# What's working

- Added CatalogueImportContext to MaterialsWindowContext and both window constructors.
- Forwarded the app-owned import context through Library, workbench and launcher paths.
- Updated all test constructions without changing assertions, identities or test cases.
- Preserved WorkbenchInfoAndCaptureTests through its existing defaults-based launchers.

# Tests

The Release build passed in 65.47 seconds with zero errors. Two SYSLIB0051
warnings originate in vendored MathNetNumerics/Numerics/Exceptions.cs, not our
code or project configuration.

- Elevation passed with zero findings across four projects in 101 seconds;
  ELEV004 excluded as required.
- All-discovered ui-smoke passed 212/212 cases, zero failures/skips.
- The ten selected ui-tests classes passed 173/173 cases, zero failures/skips.
- All eleven s13 invariants passed with their snapshot commands unchanged.
- Both TRX reports contain only passed outcomes. Smoke identities match the
  retained 0108 attempt-03 report; the four explicitly named acceptance facts
  each passed once in the selected UI suite.

Source review proves that reversing only the required wiring changes recovers
each changed test file exactly from HEAD. All 14 changed source files are LF-only
and the whitespace review passed. Formal acceptance and baseline updates remain
supervisor-owned; no baseline file was changed.

```yaml
gates:
  ui_smoke_tests: 212
  ui_tests: 173
```

These are measured counts for this slice's selections, not baseline edits.
Evidence lives in specs/0050/.artifacts/0109-attempt-01-*: build/elevation/test
logs, analyzer reports, both TRX reports, eleven invariant logs, source and
acceptance reviews, and verification-results.json. The implementation log
records their paths and the commands' artifact-output redirection.

# Architecture

Reuse CatalogueImportContext as a mandatory constructor argument immediately
after CategoryProxy. MaterialsWindow stores it in its MVU context; the Sample
editor captures and forwards the same instance into its Materials Select open.
EditorLaunchers.defaults, the workbench message, the application launcher and
Library's Sample-editor composition pass their existing app context. Test
fixtures use the specified in-memory import proxy; final composition tests pass
ctx.catalogueImports. No durable shape, migration, package or version changes.

# Deferred

None within slice 0109. The slice explicitly excludes file-drop behavior.

# Gotchas

- Keep the import dependency mandatory on both constructors and the launcher field.
- Preserve all assertions and test identities; this slice changes only their setup.
- Session-level verification requirements take precedence over the worker's act-only wording.
- The pre-existing supervisor edit to .manifest.state.json remains untouched.

# Changelog

- 2026-11-07: Threaded the catalogue import context through Materials and Sample-editor composition and updated all affected test fixtures.
