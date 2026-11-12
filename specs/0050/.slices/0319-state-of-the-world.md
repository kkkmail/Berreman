# Step 0319 — state of the world

## Where we are

This step is in spec 0050 section S9.3, which covers one file serving many experiments, exclusions, corrections, the
Data step and the experiment matrix (001 §K).

- Step 317 split mapped points into keyed groups (`splitGroups`).
- Step 318 matched each group to an experiment through the kit's naming template (`DataSplit.ofGroups`).
- This step gives the window its split messages. They identify the groups and confirm nothing: no message attaches or
  creates anything until `MatchByTemplate` (step 320) applies the split (.spec-md §10.3).
- The panel that shows the split is step 324's.

## What's working

- Add `DataSplitState`: the file being split, its profile and key column, the split's matched groups, and the offered
  groups ticked for creation. The model gains `dataSplitOpt`, `None` in `initWith`.
- Add `SplitByColumn (file, profile)`.
  - It reads the picked file through the chosen profile, splits it by the profile's key column, and holds every group as
    matched, offered or unmatched.
  - It writes nothing, and the status line counts the groups.
  - It refuses typed: a profile the kit lacks, a profile without a key column, an unreadable or unmappable file, and a
    read-only kit.
  - A refusal keeps any split already held.
- Add `ToggleSplitOffer key`, which ticks or unticks an offered group only, and `CancelDataSplit`, which forgets the
  split. Neither writes.
- Tests: three new `DataSplitModelTests` facts over a read-write folder kit with step 272's template table and E1.
  `KitWindowModelTests`' read-only fact now also refuses `SplitByColumn`.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 38 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1434) | 216 s | 600 s |
| constructor-unit-tests | Passed!, 1545 of 1545, 0 failed, 0 skipped | 70 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 222 s | 600 s |
| ui-tests (`InverseConstructorTests`, `KitWindowTests`) | Passed!, 30 of 30 | 29 s | 360 s |
| invariant-s31-\* (16) | each decoded `git grep` finds nothing, untracked files included | under 1 s each | 300 s |

- **constructor-unit-tests: 1545**, which is 1542 + 3, as the slice requires.
  - The TRX (`.artifacts/0319-dev-trx/constructor-unit-tests.trx`) shows these as `Passed`:
    - `splitting a picked file lists its groups matched, offered and unmatched, and writes nothing`;
    - `a split offer is ticked and unticked, and cancelling the split forgets it`;
    - `a file the profile cannot split, or a profile the kit lacks, is told and nothing is split`;
    - the changed `a kit opened read-only refuses every write message and writes nothing`.
- **ui-smoke: 253**, equal to its checkpoint. **ui-tests: 30** over the two selected classes. No UI test changed, so the
  count is unchanged.
- **Mutations** (`.artifacts/0319-dev-mutation.log`):
  - ticking any group's key fails the toggle fact;
  - dropping `SplitByColumn`'s read-only guard fails the read-only fact.

  The view file was restored byte-identical (`cmp`), and the final tree was rebuilt and re-tested.
- ui-smoke ran beside the elevation run, which explains its 222 s. It stayed within its 600 s deadline, and no run
  exceeded its effective deadline.
- Every touched file is LF only, and `git diff --numstat` equals the `--ignore-cr-at-eol` count.

```yaml
gates:
  constructor_unit_tests: 1545
  ui_smoke_tests: 253
```

## Architecture

- **A split is window state, never kit state.** `DataSplitState` lives in the Ui model, beside the Data step's fields.
  `SplitByColumn` reads only the picked file, through `ExperimentDataProxy.readPickedFile`. Nothing is read from or
  written to the kit.
- **One `Result` chain** runs `readPickedFile`, then `content.kitFileText`, then `ChosenImportProfile.mapText`, then
  `splitGroups`. A `KitError` renders with `describe` and an `ExperimentDataError` with its Ui `caption`, so the status
  line has one shape: `Not split — <reason>`.
- **The split is matched against `DataSplitParam`.** The table there is `model.recordedSlots`, the same recorded slots a
  setup is written from, so the offer for a key naming the held setup is that setup itself. Item models resolve through
  the window's `tryItemModelChoice`.
- **The read-only guard is step 185's `guardKitWrite`, as `OpenImportWizard` uses it.** `ToggleSplitOffer` and
  `CancelDataSplit` write nothing, so they are not guarded. A read-only kit never holds a split.

## Deferred

Nothing. By the spec's plan, applying a split (attaching the matched groups, creating the ticked setups and
experiments) is step 320's `MatchByTemplate`, and the split panel is step 324's.

## Gotchas

- **The slice says "five groups in file order", but the file's five rows hold four keys**, because `LP1-(QZ#45)-LPd`
  has two captures. The groups the slice lists and its status (`1 matched, 2 offered, 1 unmatched`) both describe four,
  so the fact asserts four.
- **Opening `DataSplits` brings in record labels** that other records share: `key`, `points`, `setup`, `table` and
  `items`. Annotate every new record construction in the view, as this step does.
- **xUnit v3 filters of different kinds are AND-ed.** `--filter-class A --filter-method B` runs nothing. Run classes
  one at a time, or use the gate's `--filter-method "<Class>.*"` form.
- `DataSplitModelTests` copies `SetupModelTests`' private items and table with its own GUIDs, because that file is
  outside this step's edits. Keep the two in step if step 272's table changes.

## Changelog

- 2026-11-11 — Step 0319: added the window's split messages. `SplitByColumn` holds a picked file's groups matched,
  offered and unmatched, writing nothing and refusing typed. `ToggleSplitOffer` ticks offered groups, and
  `CancelDataSplit` forgets the split. Three `DataSplitModelTests` facts were added (1542 → 1545), and the read-only
  fact was extended.
