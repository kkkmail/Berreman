# Step 0316 — state of the world

## Where we are

This step is in spec 0050 section S9.3, which covers one file serving many experiments, exclusions, corrections, the
Data step and the experiment matrix (001 §K).

- Steps 313–315 gave a mapped attachment its point exclusions, made them count in validation and the badge, and added
  the Data step's state with its exclusion messages.
- This step adds setup corrections. An experiment recorded over the wrong setup ("LR was at −45") is re-pointed after
  the fact to a setup revision the kit holds. A mandatory audit note goes into the experiment's file. The lab file and
  the experiment's name are untouched.
- Step 326 adds the view that sends `CorrectSetup`.

## What's working

- Add `Experiment.correctedTo setup note`. It re-points the experiment and appends `{ previous; corrected; note }` to its
  corrections, keeping its name, scan, role, session, notes and data (S9.3).
- Add `SetCorrectionNote`, which sets the Data step's `correctionNote` and writes nothing.
- Add `CorrectSetup`, behind step 185's read-only guard. It refuses with a typed status, and writes nothing, for:
  - a blank note;
  - a setup revision the kit lacks;
  - the setup the experiment already references.
- A successful `CorrectSetup` records the corrected experiment in its own file. It re-badges through the one
  validation, re-reads the review and tells `Corrected <name>: <previous> → <corrected> (<note>).` The corrected
  revision becomes pinned.
- `ExcludePoint` and `CorrectSetup` share one private refusal helper, `refusedWith`. Successful writes share step 315's
  record-and-store core.

## Tests

These were development runs of each gate's own command over the final tree. The arc-runner's gate engine runs the
gates themselves.

| Gate | Result | Time | Deadline |
|---|---|---|---|
| build | exit 0, 0 warnings, 0 errors; no lowercase `error` in stdout | 47 s | 600 s |
| elevation | `ELEV-GATE: OK - 0 new finding(s) in 12 project(s)`; known findings 0; ELEV004 excluded (suppressed 1434) | 211 s | 600 s |
| constructor-unit-tests | Passed!, 1535 of 1535, 0 failed, 0 skipped | 73 s | 180 s |
| ui-smoke | Passed!, 253 of 253 | 222 s | 600 s |
| ui-tests (selected: `InverseConstructorTests`, `KitWindowTests`) | Passed!, 30 of 30 | 30 s | 360 s |
| invariant-s31-\* (16) | each `INVARIANT HOLDS` | under 1 s each | 300 s |

- **constructor-unit-tests: 1535**, which is 1532 + 3, as the slice requires.
  - The TRX (`.artifacts/0316-dev-trx/constructor-unit-tests.trx`) shows the three new facts as `Passed`:
    - `a correction re-points the experiment's setup and appends the audit entry, its name and data unchanged`;
    - `CorrectSetup re-points the experiment to a setup of the kit with its audit note, and the lab file and the
      experiment's name are untouched`;
    - `a correction without a note, to a setup the kit lacks, or to the setup it already references writes nothing`.
  - The changed fact `a kit opened read-only refuses every write message and writes nothing` passes. It also sends
    `CorrectSetup` under the same assertions.
- **ui-tests: 30** over the two selected classes. No Ui test was touched, so their count is unchanged.
- **ui-smoke: 253**, equal to the baseline.
- **Elevation's suppressed count rose from 1432 to 1434.** These are ELEV004 `.value` reads, the rule the gate command
  excludes (spec 0048 `.manual/002` R7). They are measured, not gated. The known-findings list is untouched and empty.
- **Red by construction, then a measured mutation** (`.artifacts/0316-dev-mutation.log`).
  - The new facts name a member and messages that did not exist before this step.
  - Two mutations were applied together: `CorrectSetup`'s read-only guard bypassed, and its "already references"
    refusal dropped. Exactly two facts failed, each with the expected message:
    - the refusal fact got `Corrected E1: 'Setup 1' revision 1 → 'Setup 1' revision 1 (LP1 was at 30).`;
    - the read-only fact got `Not saved to the kit — … read-only: writeKindFile` where the guard's status was
      expected.
  - The view was restored byte-identical (`cmp`).
- After the restore, two doc comments were rewrapped (comment-only), and every gate above was run again over that final
  tree.
- No run exceeded its effective deadline.
- Every touched file is LF only (0 CR bytes), and `git diff --numstat` equals the `--ignore-cr-at-eol` count.

```yaml
gates:
  constructor_unit_tests: 1535
  ui_smoke_tests: 253
  ui_tests: 30
```

## Architecture

- **The correction is a domain member, the write is the view's.** `correctedTo` is pure and lives beside `withRole`. It
  appends, so a second correction follows the first (oldest first, step 313's `corrections`).
- **One lookup decides "the kit holds it".** The arm asks the corrected experiment `slotsIn model.kitSetups`, the same
  name-then-`slotsAt` resolution the badge validation uses. It does not use a second lookup.
- **One record-and-store core.** On success the arm calls `withExperimentDataRecorded` (step 315). That records through
  `KitLibraryContext.recordExperiment`, stores through `withStoredExperiment`, re-badges through `checkExperimentData`
  (the detector may differ in the corrected slots) and re-reads `reviewedDataOpt` when it is this experiment's. A failed
  write keeps the model with `Not saved to the kit — <e.describe>`.
- **The refusal order is the slice's:**
  1. the read-only guard;
  2. an absent experiment, which changes nothing;
  3. a blank note;
  4. a reference the kit lacks;
  5. the experiment's own reference.
- **Pinning follows from the data.** `Setup.pinFor` reads `experiment.setup`, so the corrected revision is pinned with no
  further code. A later change to that setup's table mints its next revision.
- Nothing reads or writes the lab file or the attached kit copy, and the experiment's file keeps its name's stem.

## Deferred

- Nothing. Every requirement of the slice landed in this round. The Data step's views, including the one that sends
  `CorrectSetup` (step 326), are later slices, not deferrals.

## Gotchas

- **The correction fixture is built on step 315's `withDataStepKit`.** It therefore holds E2 as well, also over
  `Setup 1` revision 1 and untouched by every correction. The polarizer is turned and `Bench B` created after E1's data
  is attached. The slice's wording fixes the content, not the order, and the result is the same kit. The fixture
  asserts both experiments over `Setup 1` revision 1, and the setups `Bench B` and `Setup 1` with different slots.
- **Turning the polarizer needs it selected.** No message selects an element by id, and after `withScanKit` the sample
  (added last) is selected. The fixture sets `selection = ElementSelected (SceneElementIndex <polarizer>)` the way
  `PlacedSampleEmissionTests` does, then sends `RotSetAxis (R1, 30°)`.
- **The note's `tryCreate` goes through a private alias, `ExperimentCorrectionNote`.** Step 294's comment on
  `ExperimentSessionName` records that F# reads a qualified `Experiments.SessionName` as the union case and then finds
  no `tryCreate`. `CorrectionNote` has the same shape, so the alias follows that precedent. This round did not
  compile-test the qualified form.
- **A successful `CorrectSetup` keeps `dataDraft.correctionNote` as typed**, as `ExcludePoint` keeps its reason. The
  slice does not say to clear it; step 326's view may decide.
- **The model fact also asserts a refusal the slice does not list.** `Bench B` revision 2, a revision the kit's `Bench B`
  never had, is refused like `Gone`. This exercises `slotsAt`'s revision check, not only the name lookup.
- **The full build prints SYSLIB0051 twice** whenever the vendored `MathNetNumerics/Numerics` C# project recompiles.
  This is pre-existing (it is also in `0312-build.log` and `0313-dev-build-1.log`), not from this step. The final
  incremental build reports 0 warnings.
- `specs/0050/.manifest.state.json` was already dirty from the supervisor when the round began. This round did not
  touch it.

## Changelog

- 2026-11-11 — Step 0316: setup corrections. `CorrectSetup` re-points an experiment to a setup revision of the kit with
  a mandatory audit note in its file. The lab file and the name are untouched (S9.3).
  - Domain: `Experiment.correctedTo`.
  - Ui: `SetCorrectionNote`, `CorrectSetup`, and a shared `refusedWith`.
  - Tests: a `correctedTo` fact, two `ExperimentModelTests` facts over a `Bench B` correction kit, and the read-only
    fact also sends `CorrectSetup`.
