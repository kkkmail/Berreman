# Step 0317 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice, AGENTS.md, and step 316's SoW and impl-log. Also
  read the how-to of steps 318 and 319, so this step's shapes fit what follows.
- Read ELEV001 and ELEV005 in STL's analyzers (read-only). `Ctor this.value` inside a member of the source type is
  sanctioned, so `rowKey`'s body is clean.
- Wrote the impl-plan.
- **Tests first.**
  - `ImportProfileTests`:
    - `observationOf` and `twoPointIntensity` are now public;
    - new public `capturesLabelled`, built over `captureAt`;
    - step 305's local `labScanRow` moved, unchanged, to module level and made public;
    - new fact `a point's key is the one selection reads, and a selection keeps the points of its key`.
  - New `DataSplitTests.fs` with two facts, appended last in the test project's compile list.
- **Red** (`.artifacts/0317-dev-red-build.log`, exit 1). Every error names a production symbol that does not exist yet:
  `rowKey`, `pointKeys`, `splitGroups`, `splitKeyOpt`, `SplitKeyColumn`, `KeyedGroup` and the `DataSplits` module.
- **Domain:**
  - `ImportProfiles.fs` gains `MuellerExperimentLabel.rowKey` and `MappedPoints.pointKeys`.
  - `selectedBy` now keeps the points whose `pointKeys` entry is `Some key`. Its contract and texts are unchanged.
  - New `DataSplits.fs` holds `SplitKeyColumn`, `ChosenImportProfile.splitKeyOpt`, `KeyedGroup` and
    `MappedPoints.splitGroups`. It is compiled after `ExperimentDataLoad.fs`.

- First full build green: 0 warnings, 0 errors. Constructor suite 1538 of 1538, which is 1535 + 3.
- **Mutation check** (`.artifacts/0317-dev-mutation.log`). Three mutations were applied together:
  - `splitGroups` sorted its keys;
  - `splitKeyOpt` answered `experiment` for the observation profile;
  - `pointKeys` read no key from an observation.

  Over `ImportProfileTests` and `DataSplitTests`, exactly 4 of 13 failed:
  - the three new facts;
  - step 305's selection fact, whose observations selection lost its K1 point.

  Each fact failed with a message naming its mutation. Both Domain files were restored byte-identical (`cmp`).
- The split fact's failure had printed only the `Result` type's name. `assertSplit` now fails with both values
  rendered, as `assertValidation` does.
- Final run of every gate command over the final tree (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/ImportProfiles.fs`: `MuellerExperimentLabel.rowKey`,
  `MappedPoints.pointKeys`, and `selectedBy` reading `pointKeys`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/DataSplits.fs` (new): `SplitKeyColumn`,
  `ChosenImportProfile.splitKeyOpt`, `KeyedGroup` and `MappedPoints.splitGroups`.
- `Berreman/OpticalConstructor/OpticalConstructor.Domain/OpticalConstructor.Domain.fsproj`: one `<Compile>` line after
  `ExperimentDataLoad.fs`, with no comment.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ImportProfileTests.fs`: shared fixture builders and the new
  fact.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/DataSplitTests.fs` (new): `module DataSplitTests`, two facts.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/OpticalConstructor.Tests.fsproj`: `DataSplitTests.fs` appended
  last, with no comment.

## Decisions

- **One reading of a point's key.** `selectedBy` zips `pointKeys` with the case's own point list and keeps the points
  whose entry is `Some key`. The case is kept, so the result is still a `CapturePoints` / `ObservationPoints` /
  `ColumnPoints` of the original points.
  - The `ColumnPoints` "maps no key column" refusal now reads `List.choose id pointKeys`, the same test as before.
  - Its three texts are byte-identical.
  - The capture comparison `capture.experiment.value = key.value` is gone; `rowKey` replaced it.
- **`splitGroups` keeps first-appearance order explicitly.** It uses `List.distinct`, which documents that later
  occurrences are dropped, and counts each key with a filter. `List.countBy` was not used, because its result order
  rests on `Dictionary` enumeration, which is not a documented contract.
- **`splitGroups` refusals mirror `selectedBy`'s:**
  - a series is refused: `an X,Y data file has no key column to split by`;
  - `ColumnPoints` whose rows hold no key (an empty list included) are refused: `the import profile maps no key column
    to split by`;
  - empty captures or observations split into no group (`Ok []`). This step's fact pins that for empty `CapturePoints`.
- **`splitKeyOpt`'s texts are the slice's:** `experiment`, `sample` and `column <n>`.
  - The CSV column names `experiment` (`MuellerDataStore.requiredColumns`) and `sample` (`LabFileImport`'s observation
    header) live in Storage. Storage sits above Domain, so Domain cannot read them, and the slice names the texts. They
    are what the split shows, not a second parser of either file.
- **`rowKey`'s body is `DataRowKey this.value`.** This is the conversion-body shape that ELEV001 and ELEV005 sanction
  (`isThisValue`), as `ElementId.fs:484` and `LibraryEntryFiles.fs:106` are.
  - It is not named `to…`, so ELEV006's census does not count it.
  - Nothing converts `DataRowKey` back to a label. Step 318 adds `DataRowKey.nameText`, which goes to `NameText`, not
    back.
- **Reuse in the tests, not copy.** `DataSplitTests` calls `ImportProfileTests`' fixture builders, as
  `ImportProfileTests` already calls `KitTests.labScanProfile`:
  - `observationOf` and `twoPointIntensity` lost `private`;
  - the new `capturesLabelled` labels rows over `captureAt`;
  - `labScanRow` moved out of step 305's fact, with its body unchanged.

## Testing state

These were development runs of each gate's own command over the final tree.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; 0 lines holding `error` | 36 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; suppressed 1432 (ELEV004 only) | 207 s | 600 s |
| constructor-unit-tests | Passed!, 1538 of 1538 (1535 + 3), 0 failed, 0 skipped | 72 s | 180 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- The TRX shows `Passed` for the three new facts and for step 305's `a row selection keeps every row or the rows of
  one key`.
- No run exceeded its deadline.
- Every touched file is LF only, and `git diff --numstat` equals `--ignore-cr-at-eol`.
- Every requirement of the slice landed. Commit-ready.

## Artifacts

- `.artifacts/0317-dev-red-build.log`: the red build, before the production code.
- `.artifacts/0317-dev-mutation.log`, `.artifacts/0317-dev-mutation-build.log`: the mutation run.
- `.artifacts/0317-dev-build.log`: the final build.
- `.artifacts/0317-dev-constructor-unit-tests.log` and `.artifacts/0317-dev-trx/constructor-unit-tests.trx`: the
  final constructor suite.
- `.artifacts/0317-dev-elevation.log`: the final elevation run.
- `.artifacts/0317-dev-invariants.log`: the 16 s31 invariants.

## Gotchas

- **Step 305's selection fact is unchanged in substance.** The slice says it "passes as written". Every assertion,
  value and expected text of it is untouched, and it passes. The only change is that its local `labScanRow` helper
  moved to module level with the same body, so the new facts reuse it instead of copying a function body (AGENTS.md
  "Reuse, never copy"). This round reads "as written" as "with its assertions as written".
- **`KeyedGroup.key` shares its label with `KeyedObservation.key`.** F# infers a record from its labels, and the most
  recent type wins. Every `KeyedGroup` construction is annotated (`groupOf` in `DataSplits` and in the test).
- **The red build and the mutation build printed SYSLIB0051 twice**, from the vendored `MathNetNumerics/Numerics` when
  it recompiles. This is pre-existing (step 316 recorded it). The final build reports 0 warnings.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.
