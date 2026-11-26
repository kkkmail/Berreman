# Step 0375 — impl log

## Progress

- Read the task, both system prompts, the project prompt, the slice and its gates snapshot. No operator note or ruling
  is attached.
- Read the seams: step 374's `StudyForward.fs` and its tests, `MuellerInverse.fs` (`MeasurementConfiguration`,
  `Observable`, `MuellerObservation`, `createBerremanForward`), `ItemEvaluation.fs` (`ElementOrientation.sampleAzimuth` /
  `observable`, `CutPlate.ofForward`, `BoundCutPlate`'s `muellerAt`), `Setups.fs` (`SlotRole`, `inLightOrder`),
  `Studies.fs` (`Sample.openedMaterialsIn`), `CrystalPhysics.fs` (`CrystalParameters.opticalProperties`). Confirmed none
  of the new names exists in the tree.
- `StudyForward.fs`: two `StudyForwardError` cases and their renders; `BoundExperiment` gains `observedCrystalOpt` and
  `solverParameters`; `Experiment.boundWith` takes the `Study` second and fills both fields (a local
  `observedCrystalIn` over the drawn slots' object roles); after `readingAt`, `MeasurementConfiguration.elementOrientation`
  and `BoundExperiment.predictedReadingOf`. The Domain built clean on the first attempt.
- `StudyForwardTests.fs`: step 374's eleven `boundWith` calls (in its five facts) pass `layoutStudy`; the error-render
  fact widened by the two cases; `qzDraft` joins `benchDrafts`; the crystal-bench fixtures and the two new facts
  appended. The test project built with `0 Warning(s)`. `StudyForwardTests` ran 15 of 15 Passed (13 before).

## Files modified

- `Berreman/OpticalConstructor/OpticalConstructor.Domain/StudyForward.fs` (+77 / -4).
- `Berreman/OpticalConstructor/OpticalConstructor.Tests/StudyForwardTests.fs` (+171 / -13).

No project file changed. Both files and this round's slice outputs are LF: `git diff --numstat` equals
`--ignore-cr-at-eol`, and a byte count finds no CR. `specs/0050/.manifest.state.json` was already modified when the
round started (supervisor state). This round did not touch it.

## Testing state

All run locally after the final edit (Release x64):

| Gate | Result | Duration | Deadline |
|---|---|---|---|
| build | exit 0, `0 Error(s)`, no lowercase `error` in the log; the 12 warning lines are all NU1902 advisories (exempt); the Tests project built alone with `0 Warning(s)` | 27 s | 600 s |
| constructor-unit-tests (the gate's command, whole suite) | `Test run summary: Passed!`, 1636 succeeded (checkpoint 374: 1634; +2), 0 failed, 0 skipped; both new facts and the widened error-render fact are `Passed` in the TRX under exactly the authorized ids | 72 s | 180 s |
| elevation (arc base `f395597c`, ELEV004 excluded) | `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, exit 0; Domain `findings=0` | 224 s | 600 s |
| twenty s37 invariants | each `section_invariants --find-nothing` exits 0 (`INVARIANT HOLDS`) | a few seconds each | 300 s |

The two new facts reference symbols this step creates (`elementOrientation`, `predictedReadingOf`,
`observedCrystalOpt`, the new error cases). Before the production code they could not compile, which is their red.
Each fact makes discriminating checks:

- the observed crystal is the assignment's, not the working copy's, and the copy's crystal moves the transmitted
  matrix by more than 1e-3;
- the transmitted and reflected predictions differ;
- each prediction equals `createBerremanForward(...).muellerOf` element by element, exactly;
- `ObservationPlateErr` is answered for each of a non-opened object, two objects and an empty object slot;
- `ReadingMismatchErr` is answered for both non-polarimeter readouts, while their crystal is still observed.

Every requirement of the slice landed this round; nothing is deferred.

## Artifacts

- `specs/0050/.artifacts/0375-build.log`
- `specs/0050/.artifacts/0375-constructor-unit-tests.log`
- `specs/0050/.artifacts/0375-elevation.log`
- `specs/0050/.artifacts/0375-invariants.log`
- `specs/0050/.artifacts/0375-invariant-commands.txt` (the twenty commands from the step's gates snapshot)

## Gotchas

- **The stack choice's model binds `_`.** The how-to writes the match as `StackSampleChoice (sampleModel, _, sample)`.
  Nothing reads the model, so the match is `StackSampleChoice (_, _, sample)`, the form `parameterLayoutWith`'s
  `madeOf` already uses. The choice's step 216 payload is still what is matched.
- **"Made of exactly one opened material"** is read as `sample.openedMaterialsIn study` answering exactly one copy.
  That is the copy the how-to names. A layer of a material the study did not open does not refuse the observation.
- **The readout is rendered inline.** `ReadingMismatchErr` names the readout through a match arm in
  `predictedReadingOf`. No `DetectorReadout` render member was added, because the step authorizes no such contract.
- **Refusal order.** A readout other than the polarimeter's answers `ReadingMismatchErr` whatever the crystal.
  `ObservationPlateErr` is a polarimeter's without an observed crystal. Its reason is
  `<setup reference>, which <experiment> references`, the shape `StudyDetectorErr` uses.
- **The fixtures' store holds the probe crystal.** A sample slot is bound when the experiment binds, so the new facts'
  context (`crystalBenchContext`) saves `crystalEntry` into a Built-in store. Without it, `boundWith` answers
  `SampleMaterialErr` before any observation is read.
- **`qzDraft` joined `benchDrafts`.** `placing` and the bench's `choiceOf` now know QZ. No existing fact's values or
  results changed.
- **Matrices are compared by their elements.** `RealMatrix4x4` wraps a MathNet matrix, so Mueller readings are compared
  as their sixteen elements (`elementsOf`), exactly.
- **Shell heredocs.** One `cat >> … <<'EOF'` append failed in the Bash tool on an unmatched quote and wrote nothing.
  The test facts were appended with the Edit tool instead.
