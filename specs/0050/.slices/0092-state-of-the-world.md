# Where we are

Slice 0092 implements S31-MANIFEST-READ in S3.1. CatalogueProxy now exposes a checked manifest-only read for the later startup hash comparison. Implementation and all resolved local checks are complete; supervisor acceptance remains separate.

## What's working

- Add manifest-only reads to the in-memory and folder catalogue proxies.
- Reuse the folder manifest parser in full catalogue reads, preserving validation order and typed errors.
- Centralize the shipped Builtin path in CatalogueProxy.builtInFolderUnder.
- Add two regression facts and extend the existing shipped-folder assertion.

## Tests

- The regression-first invocation failed as expected with FS0039 for the new readManifest and builtInFolderUnder members before implementation.
- Release solution build passed in 45.11 seconds with zero warnings/errors.
- Elevation passed over seven affected/dependent projects in 145 seconds with zero new/known findings; ELEV004 was excluded as required.
- All 912 constructor cases passed in 18.978 seconds with zero failures/skips, increasing the prior slice's 910 by exactly two. The TRX confirms both new facts and the extended builtInFolderUnder fact passed.
- All eleven s12 invariant commands from 0092.gates passed.
- Source diff counts match with and without ignoring CR-at-EOL; git diff --check passed.

Local verification follows the higher-priority workspace requirement. Observed results do not replace supervisor acceptance or modify checkpoints.

```yaml
gates:
  constructor_unit_tests: 912
```

Evidence is under specs/0050/.artifacts: 0092-regression-red.log, 0092-build.log, 0092-elevation.log and 0092-elevation-reports/, 0092-constructor-unit-tests.log/.trx, and 0092-invariants.log. Analyzer and TRX output paths were redirected there without changing check membership or test selection.

## Architecture

CatalogueProxy.readManifest precedes readCatalogue and returns Result<CatalogueManifest, CatalogueError>. The in-memory implementation reads its existing held catalogue through a shared closure. The folder implementation extracts the existing read/parse prefix into a single private member used by both paths, without listing or opening entry files for a manifest-only read. Existing parsing, schema/version checks and error mapping remain shared.

builtInFolderUnder owns the single production Builtin subfolder literal; createBuiltInUnder calls it. No cache, existence check, additional proxy field or durable-format change was introduced. No migration is needed.

## Deferred

None within the requested implementation. Supervisor acceptance and commit handling remain external.

## Gotchas

- Manifest-only reading validates the manifest itself; full catalogue reading still validates the entry set and hashes.
- The initial regression build emitted two existing SYSLIB0051 warnings from vendored MathNet Exceptions.cs. The completed Release solution build reported zero warnings/errors.
- The pre-existing supervisor-owned specs/0050/.manifest.state.json change remains untouched.
- No Git mutations, version bumps, other-repository changes, analyzer policy edits or checkpoint edits were made.

## Changelog

- 2026-11-06: Added manifest-only catalogue reads and shared Builtin path composition; Release build, elevation, all 912 constructor cases and eleven s12 invariants passed locally.
