# Step 0358 — state of the world

## Where we are

Spec 0050, section S11.1 (studies: unknowns, scopes, links, sessions and opened materials). Step 0356 gave the
session gain models and step 0357 split a study's experiments into sessions. This step adds the study itself as
Domain data, plus CreateStudy's pure half: `Study.create` over a kit's experiments, refusing a held name or an
unknown experiment. The study file and its wire converters are step 359. Filling an opened material's working copy
is step 361. Fit runs are S11.3.

## What's working

- Added the study's Domain shape to `Studies.fs`: `StudyName`, `StudyNotes`, `ParameterScope` (with
  `SessionBlockScope`), `StudyParameterRef`, `ScopedParameter`, `ParameterLink`, `SessionGainSetting`, `FitMode`,
  `ResidualWeight`, the experiment and detector weight settings, `StudyFitSettings`, `OpenedMaterial` and `Study`.
- Added `Study.create` (CreateStudy's pure half): the named experiments in order, each once, with the initial
  settings. It refuses a name whose `.study-json` another study of the kit holds (ignoring case) and an experiment
  the kit lacks.
- Added `Study.sessionsIn` (sessions split in study order) and `Study.gainModelOf` (the set model, else
  `FreeSessionGain`).
- `StudyError` gains `InvalidStudyNameErr`, `DuplicateStudyNameErr`, `UnknownStudyExperimentErr` and
  `InvalidResidualWeightErr`.
- Added `StudyTests` (five facts) and widened `StudySessionTests`' error-render fact to the four new cases.

## Tests

These are development runs of each gate's own command from `0358.gates`, over the final tree:

- **build:** exit 0, 0 errors, no `error` text in stdout. Warnings are only NU1902 (exempt) and the vendored
  MathNet C# `SYSLIB0051`, as in step 0357. 76 s (23 s incremental after the last test edit) against a 600 s
  deadline.
- **constructor-unit-tests:** `Test run summary: Passed!`, 1579 of 1579, 0 skipped. That is 1574 + 5, the count
  rising by 5 as the acceptance requires. The five new `StudyTests` facts and the widened `every study error renders
  its condition and its payload` passed in the TRX under exactly their authorized ids. 70 s against a 180 s
  deadline.
- **elevation:** `ELEV-GATE: OK - 0 new finding(s) in 13 project(s)`, known findings 0, suppressed 1513 (ELEV004 only,
  excluded). 228 s against a 600 s deadline. With no `ASG_ARC_BASE_COMMIT` in this shell it analysed all 13 projects,
  wider than the gate's arc-origin scope.
- **invariant-s36-\* (20):** every snapshot command reports `INVARIANT HOLDS`. The new untracked `StudyTests.fs` was
  grepped for every pattern as well: nothing found. Under 1 s each against 300 s.

No run went past its effective deadline.

```yaml
gates:
  constructor_unit_tests: 1579
```

## Architecture

- A study names its experiments by `ExperimentName`, because a kit stores each experiment under its name
  (step 290). Its sessions are derived on demand (`sessionsIn`) and never stored. Only the per-session gain choice
  is stored (`sessionGains`), and a session without a setting is fitted freely.
- `ParameterScope` and `FitMode` follow the closed-set pattern (`ExperimentRole`): an int tag 1..n, `tryOfWire`
  returning an option, `text` and `all`. Their wire converters come with the study file (step 359).
- The duplicate-name rule matches the item relabel rule (step 216): every `<name>.study-json` shares the kit root, so
  stems are compared ignoring case. The message names both studies and the shared file name
  (`StudyKitFile.fileNameOf`).
- `OpenedMaterial` is the study-local working copy of a material version (001 §I). It is declared here and filled by
  step 361.

## Deferred

Nothing in this step's scope. As the brief says, the study file and wire converters (step 359), filling opened
materials (step 361) and fit runs (S11.3) belong to their own steps.

## Gotchas

- `Study` reuses the labels `name` / `experiments` that `StudySession` has, and `OpenedMaterial` reuses `version`
  from `MaterialVersionId`. Code compiled after `Studies.fs` that builds a `StudySession` or `MaterialVersionId`
  record should annotate the literal. The new sessions fact writes `({ name = …; experiments = … } : StudySession)`.
  `StagedMuellerPath.fs`, which opens `Studies`, compiles unchanged.
- The session scope case is `SessionBlockScope`, not `SessionScope`, because STL's `TrustFactors.fs` holds
  `SessionScope` (ELEV016).
- `ResidualWeight.tryCreate` binds the `"R"`-rendered value before the interpolated string, because a quoted literal
  cannot sit inside a `$"…"` hole.

## Changelog

- 2026-11-13 — step 0358: added the study's Domain shape (`StudyName` … `Study`), `Study.create`, `sessionsIn` and
  `gainModelOf`, and four `StudyError` cases. Added `StudyTests` (5 facts) and widened the study-error render fact;
  constructor-unit-tests 1574 → 1579.
