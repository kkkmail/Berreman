# Step 0298 — impl log

## Progress

- 2026-11-11: read the task, the system prompts, the project prompt, the slice, step 297's outputs, `Kit.fs`,
  `CanonicalJson.fs`, `KitTests.fs` and `CanonicalJsonTests.fs`. Wrote the impl plan.
- **Names checked first.** Every new type and case name (`DataDelimiter` … `PhysicalScanValue`) was grepped over
  `Berreman/**/*.fs` and STL's `*.fs` (read-only): none exists, so ELEV014 / ELEV016 cannot collide.
- **Kit.fs.** `ImportProfileName` moved, members unchanged, to directly before `NamingTemplate`, and gained
  `phoneRoiCsv`, `muellerObservations` and `builtIns`. After it: `DataDelimiter`, `HeaderRowChoice`, `ByteOrderMark`,
  `ScanColumnUnit`, `ScanReading` (each `value` / `tryOfWire` / `text` / `all`, plus `character` and
  `spectralUnitOpt`), then `DataColumnNumber` (`value` / `tryCreate` / `index`). `DataRowKey`, `DataRowSelection`
  and `MappedAttachment` stay where step 291 put them.
- **CanonicalJson.fs.** `open OpticalConstructor.Domain.Kit`; five `ClosedSetTagConverter` registrations after
  `ExperimentRole`'s, before the `JsonFSharpConverter`; the options' doc names them (S9.2).
- **Tests.** `KitTests` gained the private helper `assertClosedSet` and the fact `the closed sets of an import profile
  round-trip their disk tags and render their texts`; `CanonicalJsonTests` (now opening `Kit`) extends its two facts.
- LF only, no CRLF churn (`git diff --numstat` equals `--ignore-cr-at-eol`); `µ` is U+00B5 and `cm⁻¹` the same bytes
  as `Units.fs`.
- **First build and run: green.** Full build exit 0 (74 s); constructor-unit-tests 1496 / 1496 (1495 + the new fact),
  the new fact and both changed facts `Passed` in the TRX.
- **Mutation check (red).** `DataDelimiter.tryOfWire 3` was made `None` and the `ScanColumnUnit` converter dropped.
  The new `KitTests` fact failed with `Expected: Some(TabDelimiter)`, `Actual: null`; `every closed set with an int
  tag…` failed with `JsonException: '3' is not a DataDelimiter tag`; `an unknown disk tag is refused` stayed green.
  Both files were restored from the copies byte for byte (SHA-256 `583510a3…8fb0` and `f9037036…02c6e` before and
  after) and the solution rebuilt; the Domain and Storage DLLs are newer than the restored sources.
- **Final runs** over the restored build: build, elevation, constructor-unit-tests and the 16 s30 invariants, all green.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Kit.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Storage/CanonicalJson.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/CanonicalJsonTests.fs`

## Decisions

- **A spectral unit's text is `Units.UnitOfMeasure.abbreviation`.** `Units.fs` says its union is the only place a
  non-SI unit name is written, and AGENTS.md forbids a re-transcribed constant, so `ScanColumnUnit.text` answers
  `Units.Nanometer.abbreviation` … `Units.Wavenumber.abbreviation`. These are exactly the slice's `nm`, `µm`, `eV` and
  `cm⁻¹`; the test pins the literal texts. `°` and `rad` are written in `Kit.fs`, since `Units` has no angle unit.
- **Member order follows `ExperimentRole`** (`value`, `tryOfWire`, `text`, `all`), with the extra member
  (`character`, `spectralUnitOpt`) last.
- **`DataColumnNumber.index` pattern-matches the case** instead of reading `.value`, keeping `.value` to the boundary.
- **`CanonicalJson.fs` opens `OpticalConstructor.Domain.Kit`** so the five registrations read like the others. No
  name in `Kit` shadows one the file uses (ELEV names are unique).
- **The new fact's helper `assertClosedSet`** also checks, by reflection, that `all` holds every case of the union, so a
  case added later without a tag fails here. It asserts all three `character`s and all six `spectralUnitOpt`s, and
  `DataColumnNumber.tryCreate` of 1 and -1, beyond the slice's named ones; nothing the slice names is left out.

## Testing state

Final runs over the final (restored) build, with the gates' own commands:

- **build:** exit 0, 0 warnings, 0 errors, no `error` in stdout, 55 s.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 12 project(s), 200s`, known findings 0, ELEV004 excluded; 201 s.
- **constructor-unit-tests:** Passed!, 1496 / 1496 (baseline 1495, +1), 0 failed, 0 skipped, 70 s.
- **The 16 s30 invariants:** each prints `INVARIANT HOLDS`, exit 0.

No run went over its deadline. Commit-ready.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0298-build-dev-01.log` (first full build) and `0298-gate-build.log` (the final build);
- `0298-dev-constructor-unit-tests-01.log` (first green run);
- `0298-mutation/` (the two original files, the hashes before and after, the mutated build and its red run);
- `0298-gate-elevation.log`, `0298-gate-constructor-unit-tests.log`, `0298-trx/constructor-unit-tests.trx`;
- `0298-gate-invariants.log` (the 16 decoded invariant commands and their results).

## Gotchas

See the SoW.
