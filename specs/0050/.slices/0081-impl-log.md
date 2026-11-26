# Step 0081 implementation log

## Progress

- Read the worker protocol, project rules, slice, gate snapshot, S24-LOCATIONS acceptance oracle, and existing settings/catalogue implementations.
- Confirmed `FolderName.combine` takes the parent as receiver and the child as argument. The existing catalogue factory hard-codes both `Catalogue` and `Builtin` in `Path.Combine`.
- Before test changes: add the two required facts to the existing test modules; expected constructor case count increases from 889 to 891.
- After test changes: both exactly named facts are present. The defaults fact compares every elevated field and independently fixes the shipped folder text; the catalogue fact compares decoded catalogues and requires a diagnostic `CatalogueIoErr` for an existing empty folder.
- Before production changes: add the pure Domain shape and replace the factory's inline path with two calls to `FolderName.combine` and delegation through `createBuiltInUnder`.
- After production changes: `DataLocations` owns all seven configured defaults without reading the environment or filesystem. `createBuiltInUnder` owns the `Builtin` child name, and `createBuiltIn` combines the install folder with `DataLocations.shippedCatalogueDefault` before delegating. Existing `WorkbenchSettings` values and catalogue read/error behavior are unchanged.
- Before local verification: started a Release/x64 build of `OpticalConstructor.Tests.fsproj` and its dependencies. Reviewed the four-file diff against S24-LOCATIONS; no unintended code changes or whitespace errors were found, and ordinary versus ignore-cr-at-eol diff counts match.
- Local build completed successfully in 55 seconds with zero errors and no constructor-code warnings. The only compiler warnings were the same two vendored MathNet `SYSLIB0051` warnings recorded in slice 0080, at `Exceptions.cs` lines 24 and 77. No vendored code changed.
- Before test execution: run the full local constructor suite over the built Release/x64 output and capture its TRX under this arc's artifacts directory.
- After test execution: all 891 constructor tests passed, zero failed/skipped, in 17 seconds. This is exactly two more than the 889-test supervisor checkpoint. The two required facts passed with their specified names.
- Final review: all four code files remain LF-only; the working tree contains only the four authorized code edits, three required slice outputs, and the untouched pre-existing manifest-state modification. No implementation deviation or blocker remains.

## Files modified

- Updated: `WorkbenchSettings.fs`, `CatalogueFolderStore.fs`, `WorkbenchSettingsTests.fs`, and `CatalogueFolderTests.fs`, plus the required slice outputs.

## Testing state

The local Release/x64 constructor test-project build passed with zero errors and no constructor-code warnings. The full local constructor suite passed 891 tests, zero failed/skipped; both required new facts passed. The two pre-existing vendored MathNet warnings are recorded above.

No acceptance gate was run or certified. The supervisor owns build, elevation, constructor-unit-tests and the eleven s11 acceptance invariants. Commit readiness is not claimed before that stage; there is no implementation blocker.

## Artifacts

- `specs/0050/.artifacts/0081-local-build.log`.
- `specs/0050/.artifacts/0081-local-tests.log`.
- `specs/0050/.artifacts/0081-local-tests/constructor-tests.trx`.

## Gotchas

- The worker protocol's act-only restriction conflicts with higher-priority development instructions to verify changes; local build/test observations will not be represented as acceptance gate results.
- `specs/0050/.manifest.state.json` was already modified before this round and will remain untouched.
- No version bump, git mutation, checkpoint edit, analyzer change or edit outside the authorized code files is planned.
