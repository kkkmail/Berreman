# Step 0360 — state of the world

## Where we are

Spec 0050, section S11.1 (studies). Step 0358 gave the study its Domain shape and CreateStudy's pure half, and step 0359
stored it as its own `<name>.study-json` kit file. This step lands AddExperimentsToStudy's pure half:
`Study.withExperiments` grows a study by experiments of its kit. The study stays a named set of experiments inside ONE
kit (001 §L). There is still no removal member: disabling an experiment is step 365's.

## What's working

- Added `Study.withExperiments`. It keeps the study's experiments in place and appends each given name the study lacks,
  in the given order and each once, leaving every other field unchanged.
- `Study.withExperiments` refuses the first name the kit lacks as `UnknownStudyExperimentErr`, naming it, with nothing
  changed.
- `Study.create` and `Study.withExperiments` now share one private kit-membership check, `Study.unknownExperimentAmong`,
  instead of each holding a copy.
- Added two `StudyTests` facts for the append order and for the refusal.

## Tests

These are development runs of each gate's own command from `0360.gates`, over the final tree:

- **build:**
  - exit 0, `0 Error(s)`, no lowercase `error` in stdout;
  - 76 s against a 600 s deadline;
  - the warnings are NU1902 (exempt) and the pre-existing SYSLIB0051 from the vendored MathNet C# code (see Gotchas).
- **elevation:**
  - `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0, suppressed 1517 (ELEV004 only, which is
    excluded; unchanged from step 0359);
  - 208 s against a 600 s deadline;
  - with no `ASG_ARC_BASE_COMMIT` in this shell it analysed all 13 projects, wider than the gate's arc-origin scope.
- **constructor-unit-tests:**
  - `Test run summary: Passed!`, 1590 of 1590, 0 skipped. That is 1588 + 2, the count rising by 2 as the acceptance
    requires.
  - The TRX shows both authorized ids `Passed`:
    - `adding experiments appends the new ones in order and keeps those the study holds`
    - `adding an experiment the kit lacks changes nothing`
  - 70 s against a 180 s deadline.
- **invariant-s36-\* (20):** every snapshot command answers `INVARIANT HOLDS`, each in 0.2 s against 300 s.

No run went past its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1590
```

## Architecture

- **`withExperiments` is an intrinsic member of `Study`.** It sits beside `create`, and like `create` it takes the kit's
  experiments as its membership oracle, so the Domain stays pure. The kit read and the study write that complete
  AddExperimentsToStudy belong to its IO half.
- **One kit-membership check.** `Study.unknownExperimentAmong` (`static member private`) answers the first name a kit
  lacks as the `UnknownStudyExperimentErr` both members return. `create`'s local `inKit` and its copy of the message
  are deleted, and `create`'s behaviour and refusal order are unchanged.
- **"In place" is literal.** The study's own list is kept exactly as it is. Only the given names it lacks are
  de-duplicated and appended, so nothing already held is reordered.

## Deferred

Nothing. Every requirement of the slice landed this round.

## Gotchas

- **SYSLIB0051.** The vendored C# `MathNetNumerics/Numerics/Exceptions.cs` raises SYSLIB0051 whenever that project
  fully recompiles. It appears in this arc's build logs from step 0001 on and does not trip the build gate's `error`
  match. It is outside this step's `touches` and was left alone.
- **Shared study fixture.** `StudyFileFixtures` (QZ-LR) lives in `KitLibraryTests.fs`, which compiles before
  `StudyTests.fs`, so `StudyTests` can reuse it. QZ-LR sets every non-experiment field to a non-initial value, which is
  what makes "its other fields equal" a real check.

## Changelog

- 2026-11-13: step 0360 added `Study.withExperiments` (AddExperimentsToStudy's pure half). It shares one kit-membership
  check with `Study.create`. Two `StudyTests` facts were added, and constructor-unit-tests went from 1588 to 1590.
