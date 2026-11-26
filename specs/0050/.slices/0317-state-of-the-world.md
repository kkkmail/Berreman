# Step 0317 — state of the world

## Where we are

This step is in spec 0050 section S9.3, which covers one file serving many experiments, exclusions, corrections, the
Data step and the experiment matrix (001 §K).

- Steps 313–316 gave an attachment its point exclusions, the Data step's state and setup corrections.
- This step starts "one file → many experiments: split by a key column" (001 §K, §P `SplitByColumn`) as pure Domain.
  - It adds one reading of a point's key, which the row selection and the split share.
  - It adds the key column each import profile splits by.
  - It adds the split of mapped points into keyed groups.
- Step 318 matches each group to an experiment through the naming template. Step 319 adds the `SplitByColumn` message.
  Step 320 applies a split.

## What's working

- Add `MappedPoints.pointKeys`, the one reading of a point's key:
  - `None` for a series point;
  - a Phone ROI row's label (`MuellerExperimentLabel.rowKey`);
  - an observation's key;
  - a mapped row's key column.
- `selectedBy` keeps the points whose `pointKeys` entry is the key. Its contract and refusal texts are unchanged.
- Add `MappedPoints.splitGroups`. It returns one `KeyedGroup` per key in first-appearance order with its point count,
  equal to what `selectedBy` keeps. A series, or rows without a key, are refused with `MalformedDataFileErr`.
- Add `ChosenImportProfile.splitKeyOpt`, the column a profile splits by:
  - `experiment` for Phone ROI CSV;
  - `sample` for Mueller-matrix observations;
  - `column <n>` for a kit profile's key column;
  - `None` otherwise.
- Tests: one new `ImportProfileTests` fact and the new `DataSplitTests` class with two facts.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 36 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1432) | 207 s | 600 s |
| constructor-unit-tests | Passed!, 1538 of 1538, 0 failed, 0 skipped | 72 s | 180 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- **constructor-unit-tests: 1538**, which is 1535 + 3, as the slice requires.
  - The TRX (`.artifacts/0317-dev-trx/constructor-unit-tests.trx`) shows the three new facts as `Passed`:
    - `a point's key is the one selection reads, and a selection keeps the points of its key`;
    - `a file splits into one group per key in first-appearance order, each counting its points`;
    - `a file without a key column does not split, and a profile names the key column it splits by`.
  - Step 305's `a row selection keeps every row or the rows of one key` passes, with its assertions as written.
- **Red first.** Before the production code, the test project failed to compile
  (`.artifacts/0317-dev-red-build.log`). Every error named a missing symbol: `rowKey`, `pointKeys`, `splitGroups`,
  `splitKeyOpt`, `SplitKeyColumn`, `KeyedGroup` and `DataSplits`.
- **Measured mutation** (`.artifacts/0317-dev-mutation.log`). Three mutations were applied together:
  - sorted groups;
  - `sample` changed to `experiment`;
  - observations read without a key.

  Exactly 4 of 13 facts failed in the two classes: the three new facts and step 305's selection fact. Each failed with
  a message naming its mutation. Both files were restored byte-identical (`cmp`).
- **Elevation's suppressed count fell from 1434 (step 316) to 1432.** These are ELEV004 `.value` reads, the rule the
  gate command excludes (spec 0048 `.manual/002` R7), so they are measured, not gated. `selectedBy` no longer compares
  `capture.experiment.value = key.value`. The known-findings list is untouched and empty.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes), and `git diff --numstat` equals the `--ignore-cr-at-eol` count.

```yaml
gates:
  constructor_unit_tests: 1538
```

## Architecture

- **`pointKeys` is the one reading of a point's key.** `selectedBy` and `splitGroups` both read it, so a group's
  points are, by construction, the points the selection of its key keeps. The selection zips `pointKeys` with the
  case's own point list, so it keeps the case and the original points.
- **The split lives in its own module, `DataSplits`.** It is compiled last in Domain, after `ExperimentDataLoad.fs`, so
  steps 318–320 can add the template matching over experiments, setups and the naming template there. This step adds
  only the four symbols the slice names. There is no cache, sort or option.
- **First-appearance order is explicit.** `splitGroups` uses `List.distinct`, which documents that later occurrences
  are dropped. It does not use `List.countBy`, whose result order rests on `Dictionary` enumeration.
- **`splitKeyOpt` names, it does not parse.** `experiment` and `sample` are the Phone ROI and observation files' key
  headers, which Storage reads. The kit profile's key column is shown as `column <n>`.

## Deferred

- Nothing. Every requirement of the slice landed in this round. Template matching (step 318), the `SplitByColumn`
  messages (step 319) and applying a split (step 320) are later slices, not deferrals.

## Gotchas

- **Step 305's selection fact keeps every assertion, value and text.** Its local `labScanRow` helper moved to module
  level with the same body, so the new facts reuse it instead of copying a function body (AGENTS.md "Reuse, never
  copy").
  - `observationOf` and `twoPointIntensity` lost `private`.
  - The new `capturesLabelled` is public.

  `DataSplitTests` calls them, as `ImportProfileTests` calls `KitTests.labScanProfile`.
- **`KeyedGroup.key` shares its label with `KeyedObservation.key`.** F# infers a record from its labels, and the most
  recent type wins, so every `KeyedGroup` construction is annotated. Step 318's `SplitGroup` will share `key` and
  `points` with it too.
- **Empty files:**
  - empty `CapturePoints` or `ObservationPoints` split into no group (`Ok []`);
  - empty `ColumnPoints` are refused as "maps no key column", the same answer `selectedBy` gives them.
- **The red and mutation builds printed SYSLIB0051 twice**, from the vendored MathNet C# project when it recompiles.
  This is pre-existing (step 316 recorded it). The final build reports 0 warnings.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0317: split one file by its key column (S9.3).
  - Domain: `MuellerExperimentLabel.rowKey` and `MappedPoints.pointKeys`, with `selectedBy` reading `pointKeys`; the
    new `DataSplits` module holds `SplitKeyColumn`, `ChosenImportProfile.splitKeyOpt`, `KeyedGroup` and
    `MappedPoints.splitGroups`.
  - Tests: one `ImportProfileTests` fact, plus the new `DataSplitTests` with two facts.
