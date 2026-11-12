# Step 0315 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice, AGENTS.md, and the SoWs of steps 313 and 314.
- Wrote the impl-plan.
- **View** (`TableAndElementRotationView.fs`):
  - Added `ReviewedData` and `DataStepDraft` (with `empty`) before the model.
  - Added the model fields `reviewedDataOpt` and `dataDraft`; `initWith` sets `None` and `DataStepDraft.empty`.
  - Added four messages, each with a one-line doc: `ReviewExperimentData`, `SetExclusionReason`, `ExcludePoint` and
    `IncludePoint`.
  - Added the private helpers `withExperimentRecorded`, `reviewedDataOf` and `withExperimentDataRecorded`, and the four
    update arms.
  - `recordExperimentEdit` now calls the shared `withExperimentRecorded`, with the same behaviour.
  - The Ui project built with 0 warnings from our code.
- **Tests:**
  - `ExperimentModelTests` gained the `DataStepKit` fixture (`withDataStepKit`) and the three new facts.
  - `KitWindowModelTests`' read-only fact also sends `ExcludePoint` and `IncludePoint`.
  - Both classes passed: 46 of 46. The TRX shows the 3 new facts as `Passed`.
- **Mutation check:**
  - With `ExcludePoint`'s read-only guard bypassed and the badge recompute dropped, exactly the two expected facts
    failed. The read-only fact got `Not excluded — E1 has no data file.` where the read-only status was expected. The
    exclusion fact got `ValidatedDataBadge 3` where `ExcludedPointsBadge {2, 1}` was expected.
  - The view was restored byte-identical (`cmp`).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`

## Decisions

- **One record-and-store core.** `recordExperimentEdit` (step 294) and the Data step's write both record through
  `KitLibraryContext.recordExperiment` and store through `withStoredExperiment`. Copying that body would breach AGENTS.md
  "Reuse, never copy", so it is one private `withExperimentRecorded` with an `onRecorded` continuation.
  `recordExperimentEdit` passes the identity continuation, so its behaviour is unchanged.
- **One read for the review.** `reviewedDataOf` serves `ReviewExperimentData` and the re-read after a write.
- **The refusal order is the slice's:**
  1. the read-only guard;
  2. an absent experiment, which changes nothing;
  3. a blank reason;
  4. no data;
  5. a `readMapped` error;
  6. a point above `pointCount`.
- `IncludePoint` changes nothing (after the guard) for:
  - an absent experiment;
  - an experiment with no data;
  - a point with no exclusion.
- A successful `ExcludePoint` keeps `dataDraft` as typed; the slice does not say to clear it.
- The fixture's E1 varies the polarizer's R1 over 0…90° in **3 points**. "The draft's range 0…90°" fixes the bounds but
  not the count, and 3 matches the file's three rows.
- The fixture's E2 is committed after `ExpNew` over the same scan, so it holds no data.

## Testing state

These were development runs of each gate's own command over the final, restored tree:

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 39 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; suppressed 1432 (ELEV004, excluded) | 224 s | 600 s |
| constructor-unit-tests | Passed!, 1532 of 1532 (1529 + 3) | 73 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 219 s | 600 s |
| ui-tests (`InverseConstructorTests`, `KitWindowTests`) | Passed!, 30 of 30 | 30 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- No run exceeded its deadline.
- Every touched file has 0 CR bytes, and `git diff --numstat` equals the `--ignore-cr-at-eol` count.
- The slice is ready for the supervisor's gate run.

## Artifacts

- `specs/0050/.artifacts/0315-dev-trx/experiment-model.trx`: the TRX of the class's 7 facts.
- `specs/0050/.artifacts/0315-dev-mutation.log`: the mutation run (2 expected failures).
- `specs/0050/.artifacts/0315-dev-build.log`, `0315-dev-elevation.log`, `0315-dev-constructor-unit-tests.log`,
  `0315-dev-ui-smoke.log`, `0315-dev-ui-tests.log` and `0315-dev-invariants.log`: the development gate runs.
