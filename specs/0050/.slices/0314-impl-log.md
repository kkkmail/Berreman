# Step 0314 — impl log

## Progress

- Read the task, the two system prompts, the project prompt, AGENTS.md and the slice.
- Read the code the slice touches:
  - `ImportProfiles.fs`, `ExperimentDataLoad.fs` and the badge caption in `ConstructorUiIdOfDomain.fs`;
  - the two callers of `loadAndValidate` and `Kit.fs`'s exclusion types (step 313);
  - the two test files.
- `ExperimentDataStore.readMapped` already applies the row selection (`selectedBy`), so the exclusions number the
  selected points, as `MappedPointNumber`'s doc says.
- Read the analyzer source in STL (read-only), `NoWideTuple.fs`. A union case whose fields repeat a type is an ELEV011
  finding. The pinned `Softellect.Analyzers 10.1.402.136` dll carries that message. See Gotchas.
- **Tests first.**
  - `ImportProfileTests`: lifted step 300's four test-local fixtures out of `mapped points count what they hold` into
    private module values, unchanged (`twoPointIntensity`, `threeCaptures`, `threeObservations`, `threeMappedRows`).
    The fact's four assertions read them as before.
  - `ImportProfileTests`: added `kept points leave out the excluded ones and keep the rest in order`.
  - `ExperimentDataProxyTests`: extended `the data badge follows the attachment, its file's hash and its validation`
    and added `loading data validates the kept points, so an excluded point outside the range does not refuse the
    data`.
- **Red.** The test project's build failed with exactly 10 errors, each naming the missing `MappedPoints.keptBy` or
  `ExcludedPointsBadge` (`.artifacts/0314-dev-build-red.log`).
- **Domain, `ImportProfiles.fs`.** Added `MappedPoints.keptBy` after `selectedBy`.
  - It numbers the points 1, 2, … in order and drops those an exclusion names.
  - A series keeps its kind through `{ series with points = … }`, and each list case keeps its kept items.
- **Domain, `ExperimentDataLoad.fs`.**
  - `loadAndValidate` is `readMapped`, then `keptBy data.exclusions`, then `validatedFor`.
  - Added `KeptPointsInfo = { kept; excluded }`.
  - Added `ExcludedPointsBadge of counts : KeptPointsInfo` after `ValidatedDataBadge`, with text
    `excluded points (<k> excluded, <n> validated)`.
  - The `Ok points` arm of `ofCheck` answers `ValidatedDataBadge` with no exclusions, else `ExcludedPointsBadge`.
    Every other arm and the arm order are unchanged.
- **Ui, `ConstructorUiIdOfDomain.fs`.** The caption is `Loaded — <n> point(s) kept, <k> excluded`.
- The two callers, `TableAndElementRotationView.fs` and `SolverHandoffView.fs`, are unchanged, as the slice says.
- **Green.** The solution built with 0 warnings and 0 errors. Every gate's own command passed; see Testing state.
- **Mutation check of the deviation.**
  - Wrote the slice's literal case `ExcludedPointsBadge of kept : MeasuredPointCount * excluded : MeasuredPointCount`
    into the source, adjusting `text` and `ofCheck` to the tuple.
  - Ran the analyzer over Domain alone (`-Project OpticalConstructor.Domain -NoDependents -ExcludeRules ELEV004`).
    It answered `ELEV-GATE: FAILED - 1 new finding(s)`: ELEV011 at `ExperimentDataLoad.fs:67:11`, "a 2-element tuple
    with a REPEATED element type … make it a record or give the positions distinct types".
  - Restored the file from its backup (`cmp`-identical). A rebuild was a no-op, which confirms the restore.
- **Caption pinned.** The changed badge fact now also asserts the new caption and the unchanged
  `ValidatedDataBadge` caption, through `open OpticalConstructor.Ui.ConstructorUiIdOfDomain`. The test project already
  references Ui. After a rebuild and a re-run, the suite passed again, 1529 of 1529.

## Files modified

- `OpticalConstructor.Domain/ImportProfiles.fs`: `MappedPoints.keptBy`.
- `OpticalConstructor.Domain/ExperimentDataLoad.fs`:
  - `loadAndValidate` validates the kept points, and its doc now cites S9.3;
  - `KeptPointsInfo`;
  - `ExperimentDataBadge.ExcludedPointsBadge`, with its `text` and its `ofCheck` arm; the type doc and the `ofCheck`
    doc now cite S9.3.
- `OpticalConstructor.Ui/ConstructorUiIdOfDomain.fs`: the `ExcludedPointsBadge` caption.
- `OpticalConstructor.Tests/ImportProfileTests.fs`:
  - step 300's fixtures moved to private module values, with the count fact's assertions unchanged;
  - the new kept-points fact.
- `OpticalConstructor.Tests/ExperimentDataProxyTests.fs`:
  - the changed badge fact;
  - the new loading fact;
  - one `open` for the caption.
- Not modified: `TableAndElementRotationView.fs` and `SolverHandoffView.fs`, authorized but needing no change.

## Testing state

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, `0 Warning(s)`, `0 Error(s)`; the red test-project build's only warnings were the 2 pre-existing vendored MathNet SYSLIB0051 | 57 s, then 23 s incremental | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; suppressed 1428, unchanged from step 313 | 202 s | 600 s |
| constructor-unit-tests | Passed!, 1529 of 1529 (1527 + 2) | 70 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 233 s | 600 s |
| ui-tests (selected: `InverseConstructorTests`, `SolverHandoffWindowTests`) | Passed!, 23 of 23 | 27 s | 360 s |
| invariant-s31-\* (16) | each exit 0, `INVARIANT HOLDS` | under 1 s each | 300 s |

- The TRX shows each named fact as `Passed`: the two new facts, the changed badge fact, and the count fact whose
  fixtures moved.
- After the ui-smoke, ui-tests and elevation runs, only the test project changed (the caption assertions). Elevation
  does not analyze test projects, and neither Ui suite reads that project.
- No run exceeded its effective deadline. `commit_ready: true`.

## Artifacts

All under `specs/0050/.artifacts/`:

- `0314-dev-build-red.log`: the red build. `0314-dev-build.log` and `0314-dev-build-final.log`: the green builds.
- `0314-dev-constructor-unit-tests.log`, `0314-dev-ui-smoke.log`, `0314-dev-ui-tests.log`, `0314-dev-elevation.log`
  and `0314-dev-invariants.log`.
- `0314-dev-mutation-literal-case.log`: the analyzer's ELEV011 finding against the slice's literal case.
- `0314-scripts/`: the invariant command list (the same 16 as step 313's), and `ExperimentDataLoad.fs.keep`, the
  backup the mutation check restored from.

## Gotchas

- **Deviation: the badge's two counts are held in a record.**
  - The slice prescribes `ExcludedPointsBadge of kept : MeasuredPointCount * excluded : MeasuredPointCount`. That
    shape is an ELEV011 finding (repeated element type on a union case), measured by the mutation check above.
  - The tree is ELEV-clean, so it would fail `elevation`, and "where any document disagrees with the analyzers, the
    analyzers win" (AGENTS.md; spec 0048 `.manual/002` R6).
  - So the case is `ExcludedPointsBadge of counts : KeptPointsInfo`, with `KeptPointsInfo = { kept :
    MeasuredPointCount; excluded : MeasuredPointCount }`. That is the analyzer's own first remedy, and step 214's
    precedent for same-typed pairs.
  - The field names, their types and their meanings are the slice's. So are the case's place, its `text`, its caption
    and `ofCheck`'s rule.
  - The test literal is `ExcludedPointsBadge { kept = MeasuredPointCount 2; excluded = MeasuredPointCount 1 }` in place
    of the tuple.
  - The record is the one type added. Beyond it, no member, case or option was added outside the slice's list.
- **`excluded` counts the exclusions, not the points they hit.** An exclusion of point 9 of 3 counts as one in the
  badge, though `keptBy` keeps every point. This is what the slice prescribes ("the number of exclusions").
- **The kept-points fact goes slightly beyond the slice's list, inside the declared identity:**
  - it also covers the observation list and an ellipsometric series, so every case of `keptBy` is exercised;
  - it checks two exclusions given out of point order.
  - The new loading fact also checks that `readMapped` still answers all three points (raw data is immutable), and the
    badge `ofCheck` gives for the kept outcome.
- **Step 300's fixtures moved; they were not copied.** They lived inside `mapped points count what they hold`. They are
  now private module values that both facts read, following "reuse, never copy". The count fact's assertions are
  unchanged.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.
