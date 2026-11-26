# Step 0316 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice, AGENTS.md, and the impl-log and SoW of step 315.
- Wrote the impl-plan.
- **Domain** (`Experiments.fs`): added `Experiment.correctedTo` beside `withRole` / `withNotes`.
- **View** (`TableAndElementRotationView.fs`):
  - Added the messages `SetCorrectionNote` and `CorrectSetup`, each with a one-line doc.
  - Added the private alias `ExperimentCorrectionNote`, as `ExperimentSessionName` is, so `tryCreate` resolves.
  - Added the private `refusedWith`. `ExcludePoint`'s local `refused` closure now calls it directly, and so does
    `CorrectSetup`, so the closure is not copied.
  - Widened `withExperimentDataRecorded`'s doc to "its exclusions or its setup changed".
  - Added the two update arms.
- **Tests:**
  - `ExperimentProxyTests` gained the `correctedTo` fact.
  - `ExperimentModelTests` gained `withCorrectionKit`, built on `withDataStepKit`, and the two facts.
  - `KitWindowModelTests`' read-only fact also sends `CorrectSetup`.
- First build green. Constructor suite 1535 of 1535 (1532 + 3); the TRX shows the 3 new facts and the changed fact
  `Passed`.
- **Mutation check** (`.artifacts/0316-dev-mutation.log`):
  - Two mutations, applied together: `CorrectSetup`'s read-only guard was bypassed, and its "already references"
    refusal was dropped.
  - Exactly the two expected facts failed:
    - the refusal fact got `Corrected E1: 'Setup 1' revision 1 → 'Setup 1' revision 1 (LP1 was at 30).`;
    - the read-only fact got `Not saved to the kit — … the kit is open read-only: writeKindFile` where the guard's
      status was expected.
  - The view was restored byte-identical (`cmp`) and the solution rebuilt.
- Elevation, ui-smoke, ui-tests and the 16 invariants passed. Two doc comments were then rewrapped (comment-only), so
  every gate was run again over the final tree (see Testing state).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/Experiments.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Ui/TableAndElementRotationView.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentProxyTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/ExperimentModelTests.fs`
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/KitWindowModelTests.fs`

## Decisions

- **The kit check reuses the one lookup.** "A reference no setup of `model.kitSetups` answers at its revision
  (`slotsAt`)" is exactly `Experiment.slotsIn` (find by name, then `slotsAt`). The arm asks it of the corrected
  experiment, `corrected.slotsIn model.kitSetups`, instead of writing a second name-then-revision lookup.
- **The write reuses step 315's core.** A successful correction goes through `withExperimentDataRecorded`, which:
  - records through `KitLibraryContext.recordExperiment`;
  - stores with `withStoredExperiment`;
  - re-badges through `checkExperimentData`, the one private validation;
  - re-reads `reviewedDataOpt` when it reviews this experiment;
  - sets the told status.

  A failed write keeps the model with `Not saved to the kit — <e.describe>` (step 315's `withExperimentRecorded`).
  Only the helper's doc changed.
- **One refusal helper.** Step 315's local `refused` closure in `ExcludePoint` would have been copied into
  `CorrectSetup`. It is now one private `refusedWith`, with the same behaviour for `ExcludePoint`.
- **The refusal order is the slice's:**
  1. the read-only guard;
  2. an absent experiment, which changes nothing;
  3. a blank note;
  4. a reference the kit lacks;
  5. the experiment's own reference.

  An experiment whose own setup the kit lacks is therefore told "the kit holds no setup".
- **The status texts are the slice's**, with `reference.describe` (`'<name>' revision <n>`) and the note as given
  (`CorrectionNote.tryCreate` keeps the text).
- **The note is a type alias.** `ExperimentCorrectionNote` follows step 294's `ExperimentSessionName`, whose comment
  records that a qualified single-case union name resolves to its case, which has no `tryCreate`. The qualified form
  was not compile-tested here.
- **Pinning needs no code.** `Setup.pinFor` reads `experiment.setup`, so once E1 is over `Bench B` revision 1 that
  revision is pinned. The model fact asserts `VersionUnpinned` before the correction and `VersionPinned` after it.

## Testing state

Development runs of each gate's own command, over the final tree (after the comment-only rewrap):

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors | 47 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; suppressed 1434 (ELEV004, excluded) | 211 s | 600 s |
| constructor-unit-tests | Passed!, 1535 of 1535 (1532 + 3) | 73 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 222 s | 600 s |
| ui-tests (`InverseConstructorTests`, `KitWindowTests`) | Passed!, 30 of 30 | 30 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- No run exceeded its deadline.
- Every touched file has 0 CR bytes, and `git diff --numstat` equals the `--ignore-cr-at-eol` count.
- The first full build of the round printed SYSLIB0051 twice from the vendored MathNet C# project. This is
  pre-existing (it is also in the 0312 and 0313 build logs). The final build reports 0 warnings.
- The slice is ready for the supervisor's gate run.

## Artifacts

- `specs/0050/.artifacts/0316-dev-build.log`, `0316-dev-elevation.log`, `0316-dev-constructor-unit-tests.log`,
  `0316-dev-ui-smoke.log`, `0316-dev-ui-tests.log` and `0316-dev-invariants.log`: the development gate runs.
- `specs/0050/.artifacts/0316-dev-mutation.log`: the mutation run (2 expected failures).
- `specs/0050/.artifacts/0316-dev-trx/constructor-unit-tests.trx`: the constructor suite's TRX.
