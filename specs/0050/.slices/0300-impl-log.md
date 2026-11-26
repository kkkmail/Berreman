# Step 0300 — implementation log

## Progress

- [x] Read the protocol, the project prompt, AGENTS.md, the slice and steps 298/299's state of the world.
- [x] `ImportProfiles.fs` written and wired into the Domain project (directly after `ItemEvaluation.fs`).
- [x] `ImportProfileTests.fs` written and wired into the test project (end of the compile list).
- [x] Development runs: build, elevation (first run red on ELEV010, fixed, re-run green), constructor-unit-tests,
  the sixteen s30 invariants, the mutation (red) check.
- [x] State of the world written.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ImportProfiles.fs` (new): `module ImportProfiles`.
  - `type Observable with` `value` (1 `TransmittedMueller`, 2 `ReflectedMueller`), `tryOfWire`, `tryOfCell`.
  - `type SampleCut with` `value` (1 to 4), `tilt`, `tryOfWire tag tilt`.
  - `BuiltInImportProfile` (`name`, `all`, `tryOfName`), `ChosenImportProfile` (`name`, `tryOfName`).
  - `MeasuredSigma`, `RowTimestamp`, `ScanCell` (`displayValue`), `MappedReading`, `MappedRow`, `KeyedObservation`,
    `MappedPoints` (`pointCount`).
  - `MuellerElementIndex` (`value`, `tryCreate`, `all`, `text`), `ColumnRoleChoice` (`text`), `ColumnChoice`,
    `type ColumnRoles with static member tryOfChoices`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one `<Compile>` line, no
  comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ImportProfileTests.fs` (new): `module ImportProfileTests`, the
  five facts the slice names.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: one `<Compile>` line at the end,
  no comment.

## Decisions

- **Deviation: `Observable.tryOfCell` takes a `DataFileCell`, not a `string`.** The slice asked for
  `tryOfCell (cell : string)` and said no other type would be added.
  - **Measured conflict.** The first elevation run failed on ELEV010:
    `ImportProfiles.fs:33:23 - tryOfCell mixes bare primitive(s) string with elevated types in one signature`
    (log `.artifacts/0300-elevation-dev-elev010.log`). ELEV010 exempts only the elevation-pattern names (`value`,
    `tryCreate`, `tryOfWire`, …), and no boundary mark silences it.
  - **Authority.** AGENTS.md says where any document disagrees with the analyzers, the analyzers win (spec 0048
    `.manual/002` R6). Adding to the known-findings list is forbidden.
  - **Fix.** One wrapper, `DataFileCell of string` with `.value`, declared first in `ImportProfiles.fs`, next to the
    existing `DataFileRow` / `DataFileText`. `tryOfCell (cell : DataFileCell) : Observable option` keeps the contract
    symbol `Observable.tryOfCell` and its reading rule unchanged.
  - **Effect on later steps.** Step 302, which reads lab files in Storage, will make each cell a `DataFileCell` where
    the CSV text arrives. No file outside this step's authorized edits changed.
  - **Name check.** `DataFileCell` is new in Berreman and STL (`git grep -w`).
- **`Lab scan` is `KitTests.labScanProfile`, reused.** The slice calls it a test-local profile and spells out its exact
  shape, which is the shape `KitTests.labScanProfile` already has. AGENTS.md forbids a copied fixture ("reuse, never
  copy"), and step 299's gotcha says to keep it in one place. `ImportProfileTests` compiles after `KitTests`, so it reads
  the fixture directly, and `KitTests.fs` is not edited.
- **`tryOfChoices` checks the repeated single roles first, then the reading.** The single roles are σ, timestamp,
  capture index, observable, key and throughput. With at most one column per single role, each role's column is simply
  the first of its list, so no nested `Result` binds are needed. The reading rule follows the slice:
  - any element or throughput column means a Mueller reading;
  - a signal or σ beside the elements is `MixedReadingColumnsErr`, checked before the element count;
  - the first index in index order with no column, or with two, is `MuellerElementCountErr` naming it;
  - otherwise exactly one signal column is required (`SignalColumnCountErr`).

  The slice does not fix the order in which these errors are reported. The order chosen satisfies every case the slice
  lists.
- **`ScanCell.displayValue` builds the `SweptValue` once, from the display number it computes.** It reads the
  magnitude through `.value`, the slice's own formula, so it never reaches into the single-case union by pattern
  (AGENTS.md).
  - `Propagation.SweptValue this.magnitude.value` would be ELEV001 (a cross-type re-wrap).
  - A `UnitMagnitude.toSweptValue` member would be the reverse of the existing `SweptValue.toUnitMagnitude`, which is
    ELEV006.
  - A spectral column takes its unit from `ScanColumnUnit.spectralUnitOpt` (step 298), so the column-to-unit mapping
    is not written a second time. A nanometre column returns its magnitude unchanged, with no round trip through
    metres, so `632.8 nm` displays exactly `632.8`.
- **No shared ordered Result collector is used.** `ItemEvaluation.sequenced` is fixed to `ItemEvaluationError`, and
  `ItemEvaluation.fs` is outside this step's edits. The element check needs only the first offending index, which
  `List.tryPick` gives, so no collector is copied.

## Testing state

These were development runs of each gate's own command over the final build. The arc-runner's gate engine runs the
gates themselves. No run went over its effective deadline.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors, no `error` in stdout | 15 s incremental (first full build 46 s) | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`, known findings 0, ELEV004 excluded | 208 s | 600 s |
| constructor-unit-tests | Passed!, 1503 of 1503, 0 failed, 0 skipped | 71 s | 180 s |
| invariant-s30-\* (16) | each `INVARIANT HOLDS` (`section_invariants --find-nothing`, exit 0) | < 1 s each | 300 s |

- **constructor-unit-tests: 1503**, which is the 1498 baseline plus the 5 new facts. The TRX shows each new identity
  `Passed`, exactly as the slice names it.
- **First elevation run: red.** It failed on ELEV010 for `tryOfCell (cell : string)`; see Decisions. After the
  `DataFileCell` fix, the re-run was green.
- **Mutation check (red).** One defect per fact, all in a single build:
  - `ChosenImportProfile.tryOfName` looked in the kit first;
  - `tryOfCell` did not trim;
  - radians were not converted;
  - `KeyColumnRole` was dropped from the repeated-role check;
  - `SeriesPoints` counted 0.

  All five facts failed, each naming its defect: `Actual: Some(KitProfileChoice …`, `Actual: [null, Some(ReflectedMueller), …]`,
  `Actual: 0.785…`, `two key columns: expected RepeatedRoleErr: the choices were accepted`, `Actual: MeasuredPointCount 0`
  (log `.artifacts/0300-mutation-red.log`). The file was restored byte for byte (SHA-256 `17de7db3…37f01c1` before
  and after), and the solution was rebuilt before the runs above.
- The new files have LF line endings. `git diff --numstat` matches `--ignore-cr-at-eol`.

## Artifacts

- `specs/0050/.artifacts/0300-build-dev.log`: the final build.
- `specs/0050/.artifacts/0300-elevation-dev-elev010.log`: the first elevation run, red on ELEV010.
- `specs/0050/.artifacts/0300-elevation-dev.log`: the final elevation run, OK.
- `specs/0050/.artifacts/0300-mutation-red.log`: the mutation run, all five facts red.
- `specs/0050/.artifacts/0300-constructor-unit-tests-dev.log`: the final suite run, 1503 passed.

## Gotchas

- **`ColumnRoles` and `MappedRow` share all six record labels** (`scan`, `reading`, `timestampOpt`, `captureIndexOpt`,
  `observableOpt`, `keyOpt`). A record literal is resolved to the later type, `MappedRow`, unless it is annotated, so
  `tryOfChoices` and the tests annotate `: ColumnRoles`.
- **`column` labels three records**: `ScanColumn`, `ScanCell` and `ColumnChoice`. Annotate the literal.
- **`Observable` is also an FSharp.Core module name.** The tests qualify the type as `MuellerInverse.Observable` when
  calling its static extension members, so name resolution never meets the module.
- The STL-wide name grep was stopped by its 10-minute background limit. The elevation analyzers' duplicate-name rules
  are the authoritative check over every referenced `Softellect.*` assembly. Every new type name, and every case name
  through `SeriesPoints`, was confirmed absent from STL's sources before the stop.
