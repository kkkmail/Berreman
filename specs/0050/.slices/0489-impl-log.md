# Step 0489 — impl log

## Progress

- Read the task, both system prompts, the project prompt, rulings R0001–R0005 and the slice. The rulings bind earlier
  steps' behaviour (series dispatch, the circular analyzer, element identities, the study fit's runtime and allocation).
  None of it is touched here, and every one is preserved.
- Wrote the impl plan.
- **Red.** Appended the three census facts to `PlanWindowModelTests.fs`. The test project failed to compile on every
  `AddPlannedExperiment` (FS0039), captured in `.artifacts/0489-red-build.log`. A test-side `WaveLength` scope error was
  fixed by qualifying `Berreman.Fields.WaveLength`.
- **Production** (`TableAndElementRotationView.fs`):
  - `AddPlannedExperiment of MeasurementPlanner.PlanCandidate`, with a one-line doc, after `SetPlanCriterion`.
  - `GenerateSetupGrid`'s arm body moved into the private `withGridRecorded grid collection role model`. It answers
    `Ok (written, setups, appended)`, or `Error` with the model carrying step 276's text.
  - The private `unheldSetups` names "the setups the kit lacks" once. The recording writes them and
    `GenerateSetupGrid` counts them in its text.
  - `GenerateSetupGrid grid` now calls `withGridRecorded grid model.experimentCollection ScienceRun`, keeping its
    texts, its writes and its draft afterwards (the committed one).
  - The `AddPlannedExperiment` arm, under `guardKitWrite`, checks in order: the Object 1 slot, then `coverageIn`, then
    `availableCaptures`, then why the grid cannot be made. Only then does it record the planned draft over
    `SetupGrid.create` as `PlannedRun`, with the window's draft put back.
- **Green.** The build passed and `PlanWindowModelTests` ran 6 of 6.
- **Mutations**, each reverted and the file restored byte for byte (`cmp`):
  - dropping `withRole` fails fact 1 (`ScienceRun` ≠ `PlannedRun`);
  - not putting the draft back fails fact 1 (draft differs);
  - skipping the coverage check fails fact 2 (`Planned E2 …` instead of `Not added — E1 already plans it.`);
  - skipping the capture check fails fact 3: the reflected candidate was recorded as a transmitted `E1`.
- **Final local runs** over the restored tree, each with its gate's own command; see Testing state.

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`: +132 / −35.
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/PlanWindowModelTests.fs`: +139.

Both are LF, with 0 CR bytes, and `git diff --numstat` equals `--ignore-cr-at-eol`.

## Testing state

The supervisor's gate engine is the authority; these are my local runs.

| Gate | Outcome | Measured | Deadline |
|---|---|---|---|
| build | exit 0, 0 errors; only the exempt `NU1902` | 42 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0 | 242 s | 600 s |
| constructor-unit-tests (whole, as selected) | `Passed!`, 1905 of 1905 = 1902 + 3 | 137 s | 180 s |
| ui-smoke | `Passed!`, 268 of 268 | 223 s | 600 s |
| ui-tests (`KitWindowTests`, `StudyViewTests`) | `Passed!`, 24 of 24 | 34 s | 360 s |
| 21 × `invariant-s46-*` | each decoded `git grep` finds nothing, also with `--untracked` | — | 300 s |

- Elevation ran standalone, with no comparison base, so it covered all 13 projects. That is wider than the arc-origin
  run.
- The TRX names the three new facts exactly as the census ids.
- `SetupModelTests` passes unchanged.

`commit_ready: true`.

## Artifacts

- `0489-red-build.log`: the red compile.
- `0489-build.log`: the final solution build.
- `0489-plan-window-tests.log`: the class run.
- `0489-mutation-build.log`, `0489-mutation-tests.log`: the last mutation round.
- `0489-elevation.log`, `0489-constructor-unit-tests.log`, `0489-ui-smoke.log`, `0489-ui-tests.log`: the final gate runs.

## Gotchas

- **An impossible combination.** `SetupGrid.create` unticks a combination the table cannot make, and `generate` then
  answers `Ok []`. Recording that would say `Planned  over .`.
  - The arm asks the unticked combination's `slotsOver` and says `Not generated — <why>`, step 276's text for a grid
    that cannot be made.
  - This is the one check the brief does not list; it adds no message, field or option.
- **The branch word.** `transmitted` / `reflected` is a local match on `candidate.observable` in the arm.
  `PlanCandidate.caption` holds the same two words as a local of its own, in `MeasurementPlanner.fs`. That file is
  outside this step's authorized edits, so the words could not be lifted into a shared member.
- **The draft goes back on every path.** A failed write after the experiments began would otherwise leave the planned
  draft in the window.
- **`withGridRecorded` commits over the collection it is given.** It reads the draft's capture and the experiment
  targets from `{ model with experimentCollection = collection }`. `GenerateSetupGrid` passes the window's own
  collection, so its outcome is unchanged.
- **Names in the fixture.** The standard template does not name the inverse table, so the fixture's setup is
  `Setup 1` and its experiment `E1`. The facts read the names back rather than hard-coding them.
