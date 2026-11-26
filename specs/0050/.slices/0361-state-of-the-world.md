# Step 0361 — state of the world

## Where we are

Spec 0050, section S11.1 (studies).

- Steps 0358–0360 gave the study its shape, its file and AddExperimentsToStudy.
- This step lands OpenMaterial's pure half. A study opens a crystal-parameter material version into a study-local
  working copy (001 §I).
  - Every component of the copy is a parameter, generated from the crystal record's own `ParameterAxis` list (001 §N).
  - Every sample made of that version shares the copy (`MaterialScope`).
- A complexity tree publishes no axis and is refused. The forward model that reads the copy is S11.2's.

## What's working

- Added `CrystalParameters.parameterAxes`. It generates a crystal's axes from its record's `axes`, so an opened
  absorbing triclinic material's axes equal `AbsorbingTriclinicParameters.axes` in names, order, field cases and
  read/write.
- Added `OpenedMaterial.openFrom`, `withParameterState` and `currentCrystal`:
  - `openFrom` makes a working copy of known parameters and refuses a complexity tree;
  - `withParameterState` frees or sets one component in place;
  - `currentCrystal` writes each parameter's current value back into the crystal.
- Added `Study.openMaterial`, which refuses a version opened twice, and `Study.withMaterialParameterState`.
- Added `Sample.openedMaterialsIn`, so every sample of an opened version reads the same copy.
- Added `ParameterAxis.parameterValueOf` and `writeValue`, the one conversion each way between an axis value and a
  `ParameterValue`, plus five new `StudyError` cases.
- Added the five `StudyMaterialTests` facts and widened the study-error rendering fact.

## Tests

These are development runs of each gate's own command from `0361.gates`, over the final tree:

- **build:**
  - exit 0, `0 Error(s)`, no lowercase `error` in stdout;
  - the only warnings are NU1902 (exempt);
  - 45 s against a 600 s deadline.
- **elevation:**
  - `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0;
  - suppressed 1518, all ELEV004, which is excluded. That is one more than step 0360's 1517: the one `.value` read in
    `writeValue`, the axis's conversion back.
  - 224 s against a 600 s deadline;
  - with no `ASG_ARC_BASE_COMMIT` in this shell it analysed all 13 projects, wider than the gate's arc-origin scope.
- **constructor-unit-tests:**
  - `Test run summary: Passed!`, 1595 of 1595, 0 skipped. That is 1590 + 5, the rise the acceptance asks for.
  - The TRX shows the five new `StudyMaterialTests` ids and the widened `StudySessionTests` id `Passed`.
  - 69 s against a 180 s deadline.
- **invariant-s36-\* (20):**
  - every snapshot command answers `INVARIANT HOLDS`, each in 0.2 s against 300 s;
  - the new, untracked test file was grepped by hand against every pattern and nothing was found.

No run went past its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1595
```

## Architecture

- **Generated, never hand-written.** `CrystalParameters.parameterAxes` lifts the record's own `axes` onto the crystal.
  - It goes through one private `liftedAxis`, which keeps the name and field case and lifts the access through one
    inner generic function.
  - Three per-case helpers supply that case's record finder and constructor.
  - A crystal of another case reads as the own record and is written back unchanged, because a study only applies
    the axes to its own working copy.
- **One conversion each way.** `ParameterAxis.parameterValueOf` / `writeValue` are the only seams between an axis read
  or write (plain `double`) and a parameter's `ParameterValue`.
- **Declaration order.**
  - The axis and crystal extensions precede `OpenedMaterial`, whose intrinsic members use them.
  - The `Study` members follow step 360's `withExperiments`.
  - `Sample.openedMaterialsIn` is an extension after `Study`.
- **Errors reuse existing renderers.** `UnknownStudyParameterErr` names the component through
  `MaterialParameterRef.describe`. `InvalidStudyParameterErr` carries `Parameter.validate`'s `ItemError.describe`, the
  one parameter-state rule.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **The test probes reuse `CrystalMaterialFixtures`** (`MaterialProxyTests.fs`). Its records already hold fifteen,
  nine and four distinct literal values. Only the `MaterialEntry` values and the `MaterialVersionId` are test-local, so
  no constant is re-transcribed.
- **`ParameterField` holds functions and has no equality.** The tests compare field cases through a test-local name
  of the case.
- **`InvalidStudyParameterErr` repeats a prefix.** Its rendering is "the parameter state is not valid: the parameter
  state is not valid: …", because the slice has it carry `ItemError.describe`.
- **The s36 invariants use `git grep`**, which sees no untracked file. A new file must be checked by hand until it is
  committed.

## Changelog

- 2026-11-13: step 0361 added OpenMaterial's pure half:
  - the generated crystal axes and their `ParameterValue` conversions;
  - `OpenedMaterial.openFrom` / `withParameterState` / `currentCrystal`;
  - `Study.openMaterial` / `withMaterialParameterState`;
  - `Sample.openedMaterialsIn`;
  - five `StudyError` cases.

  `StudyMaterialTests` (+5) was added, and constructor-unit-tests went from 1590 to 1595.
