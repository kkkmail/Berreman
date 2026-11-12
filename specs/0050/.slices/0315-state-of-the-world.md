# Step 0315 — state of the world

## Where we are

This step is in spec 0050 section S9.3, which covers one file serving many experiments, exclusions, corrections, the
Data step and the experiment matrix (001 §K).

- Step 313 gave a mapped attachment its point exclusions.
- Step 314 made them count in validation and the badge.
- This step adds the Data step's model state and its semantic messages. They review one experiment's points, exclude a
  point with a reason, and take an exclusion back. Each write goes to the experiment file, never the lab file.
- Step 316 adds the setup corrections, and step 325 adds the views that send these messages.

## What's working

- Add `ReviewedData` and `DataStepDraft`, and the model's `reviewedDataOpt` and `dataDraft`. `initWith` starts them at
  `None` and blank (S9.3).
- `ReviewExperimentData` reads every point of an experiment's data, the excluded ones included, and writes nothing, so a
  read-only kit reviews too. `SetExclusionReason` sets the draft's reason.
- `ExcludePoint` records a point's exclusion and its reason in the experiment file. It refuses with a typed status, and
  writes nothing, for a blank reason, no data, a read error, or a point the data lacks. On success it re-badges through
  the one validation and re-reads the review.
- `IncludePoint` takes an exclusion back. Both are refused by step 185's read-only guard. Neither changes the lab file,
  the attachment, the profile or the row selection.
- `recordExperimentEdit` and the Data step's writes share one private record-and-store core, `withExperimentRecorded`.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 39 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1432) | 224 s | 600 s |
| constructor-unit-tests | Passed!, 1532 of 1532, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 219 s | 600 s |
| ui-tests (selected: `InverseConstructorTests`, `KitWindowTests`) | Passed!, 30 of 30 | 30 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- **constructor-unit-tests: 1532**, which is 1529 + 3, as the slice requires.
  - The TRX (`.artifacts/0315-dev-trx/experiment-model.trx`) shows the three new facts as `Passed`:
    - `reviewing an experiment's data reads every point, the excluded ones included, and writes nothing`;
    - `ExcludePoint records a point's exclusion and its reason in the experiment file, and IncludePoint takes it back`;
    - `an exclusion without a reason, of a point the data lacks, or of an experiment without data writes nothing`.
  - The changed fact `a kit opened read-only refuses every write message and writes nothing` passes. It also sends
    `ExcludePoint` and `IncludePoint` under the same assertions.
- **ui-tests: 30** over the two selected classes. No Ui test was touched, so their count is unchanged.
- **ui-smoke: 253**, equal to the baseline. It ran while the elevation run was still going, and stayed inside its
  deadline.
- **Elevation's suppressed count rose from 1428 to 1432.** These are ELEV004 `.value` reads, the rule the gate command
  excludes (spec 0048 `.manual/002` R7). They are measured, not gated. The known-findings list is untouched and empty.
- **Red by construction, then a measured mutation** (`.artifacts/0315-dev-mutation.log`).
  - The new facts name messages, fields and types that did not exist before this step.
  - Two mutations were applied together: `ExcludePoint`'s read-only guard bypassed, and the badge recompute dropped.
    Exactly two facts failed, each with the expected message:
    - the read-only fact got `Not excluded — E1 has no data file.`;
    - the exclusion fact got `ValidatedDataBadge 3` where `ExcludedPointsBadge {2, 1}` was expected.
  - The view was restored byte-identical (`cmp`), and the final runs above are over the restored tree.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes), and `git diff --numstat` equals the `--ignore-cr-at-eol` count.

```yaml
gates:
  constructor_unit_tests: 1532
  ui_smoke_tests: 253
  ui_tests: 30
```

## Architecture

- **The review is a read, never a write.** `reviewedDataOf` answers what `readMapped` reads: every point the row
  selection keeps, the excluded ones included, so they stay visible (001 §K). Only validation and the badge drop the
  excluded points, through step 314's `keptBy`. An experiment with no data reviews as
  `Error (EmptyDataFileErr "<name> has no data file")`.
- **One record-and-store core.** `withExperimentRecorded changed onRecorded` records through
  `KitLibraryContext.recordExperiment`. On success it stores through `withStoredExperiment` and clears the status, and
  `onRecorded` finishes the model. On failure it keeps the model with `Not saved to the kit — …`.
  - `recordExperimentEdit` (role, session, notes) passes the identity continuation, so its behaviour is unchanged.
  - The Data step's `withExperimentDataRecorded` re-badges through `checkExperimentData`, the one private validation
    (steps 136, 295). It re-reads the review when the review is of this experiment, and sets the told status.
- **The refusal order is the slice's.** First comes the read-only guard, then:
  1. an absent experiment, which changes nothing;
  2. a blank reason;
  3. no data;
  4. a `readMapped` error;
  5. a point above `pointCount`.
- **Point numbers are checked against the points as read**, not the kept points. Exclusions number the selected points,
  as `MappedPointNumber` (step 313) defines them.

## Deferred

- Nothing. Every requirement of the slice landed in this round. The setup corrections (step 316) and the Data step's
  views (step 325) are later slices, not deferrals.

## Gotchas

- **E1's fixture range.** The slice says E1 is "committed over the polarizer's R1 with the draft's range 0…90°". R1's
  default draft range is 0…360° in 73 points (`VariableRange.forVariable`). The fixture therefore sets the range to
  0…90° through the bay's messages, in 3 points to match the file's three rows. The bounds are what validation reads;
  the point count is not checked against the file.
- **E2 is the same scan as E1.** It is committed after `ExpNew` over an unchanged table, so it holds no data. The fixture
  asserts the names are `E1` and `E2`, which the fresh kit's template leaves to the `E<n>` fallback.
- **A successful `ExcludePoint` keeps `dataDraft.exclusionReason` as typed.** The slice does not say to clear it; step
  325's view may decide.
- **`IncludePoint` passes the read-only guard first.** On a read-only kit, even an absent experiment answers the
  read-only status. This is what `KitWindowModelTests`' loop asserts for every write message.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0315: the Data step reviews an experiment's points, and excludes or takes back a point with a
  reason in the experiment file (S9.3).
  - Ui: `ReviewedData`, `DataStepDraft`, `reviewedDataOpt`, `dataDraft`; the messages `ReviewExperimentData`,
    `SetExclusionReason`, `ExcludePoint` and `IncludePoint`; one shared record-and-store core.
  - Tests: three new `ExperimentModelTests` facts over a two-experiment data kit; the read-only fact also sends both
    write messages.
