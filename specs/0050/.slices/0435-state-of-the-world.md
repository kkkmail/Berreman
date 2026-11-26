# Where we are

Step 0435 lands the fourth part of AT-2, the absorbing-biaxial-noisy acceptance kit (spec 0050 S12.2; 001 §Q AT-2). Steps
0432–0434 built the library drop, the items, the table, the five setups, one experiment per plate, and seed 1's attached
observations. Now the build creates the study `AbsorbingTriclinic` over the five experiments through the window's
`StudyCommand` messages. It opens the material at version 1 and frees its fifteen components from `perturbedStart`
within the harness's box. The new fact proves the study's data and its start residual identical to the harness's.
Steps 0436–0439 add the fit, the ensemble and the committed-kit equality.

# What's working

- AT-2's build creates `AbsorbingTriclinic` over the five plates' experiments and opens the material at version 1.
- It frees the fifteen components from `perturbedStart` within ± `searchBox.halfWidth` × the harness's scale.
- AT-1's Send fact and AT-2 share one start-point residual composition (`studyDataOf`, `studyResidualAtStart`).
- New fact: the stored study, the generated axes against `AbsorbingTriclinicParameters.axes`, and per-material scopes.
- The same fact checks exact data identity and the 704-entry start residual against `throughputResidualFor`.

# Tests

These are development runs, evidence only and not gate verdicts; the supervisor's gates are authoritative.
- The UI test project builds with 0 errors. The only warnings are the exempt NU1902 advisory.
- kit-acceptance over `OpticalConstructor.Ui.Tests.KitAcceptanceTests`: `Passed!`, 10 of 10 succeeded, up from 9, in
  88 s against 3600 s.
  - The TRX names the new fact by its census id, `OpticalConstructor.Ui.Tests.KitAcceptanceTests.AT-2's study opens
    the material across the five plates, loads the harness's observations exactly, and its residual at perturbedStart
    is the harness's`.
  - A temporary probe, since removed, measured all 704 residual entries bit-identical to the harness's. The smallest
    |h| is 1.03e-5, so no entry is trivially zero.
- ui-smoke over every discovered smoke test: `Passed!`, 264 of 264, in 237 s against 600 s.
- ui-tests selecting `StudyViewTests` and `DataStepTests` (both untouched): `Passed!`, 18 of 18, in 29 s against 360 s.
- elevation was not run: the only edited file is in a test project, outside the ELEV scope.
- The twenty-one s42 invariants: each decoded `git grep` finds nothing.

No development run exceeded its effective deadline. Following Invariant 6, the final JSON reports no gate result, so
`commit_ready` is false only because the worker runs no gate. No requirement or authority is blocked.

```yaml
gates:
  kit_acceptance_tests: 10
```

# Architecture

Everything is in `KitAcceptanceTests.fs`.

- **The shared composition**, placed before the first fact:
  - `studyIn` finds the window's study;
  - `studyDataOf` is `prefitWith []` then `dataWith`;
  - `studyResidualAtStart` is `residualWith` at `parameterLayoutWith`'s `startVector`.

  AT-1's Send fact and AT-2's fact both call them.
- **The build.** `buildAbsorbingStudy` sends `CreateStudy`, `OpenMaterial` and fifteen `SetOpenedMaterialParameter`
  messages, checking the status after each. `perturbedStartUnknownsOf` is the one body that computes the freed
  components, from the opened copy's generated axes zipped with the record axes. The build sends it and the fact
  compares against it.
- **Harness pieces.**
  - `absorbingForward`, now named once, serves both the noisy draw and `throughputResidualFor`.
  - `byPlate` orders the harness observations as the study reads them.
  - `assertSameObservation` is the per-observation comparison step 434's fact and this one share.
- **The fact.** Four checks:
  - `assertAbsorbingStudy`: the stored study, scopes and the shared layout;
  - `assertAbsorbingAxes`: names, field cases and reads;
  - `assertAbsorbingStudyData`: exact observation rows;
  - `assertAbsorbingStartResidual`: 704 entries within 1e-12 · max(1, |h_k|), each mismatch named.

# Deferred

Nothing is deferred. Steps 0436–0439 add AT-2's later parts, as the spec plans: the fit, the ensemble and the
committed-kit equality.

# Gotchas

- The study's start residual equals the harness's bit for bit. Writing `s` back through a generated axis is exact, and
  so is `ofScaled` at zero. Any future drift towards the 1e-12 tolerance points at one of those writes.
- `StudyForwardError` has no "study missing" case, so `studyIn` fails the fact, with step 430's message.
- Recording the study re-snapshots the material it opens. That is already the plate's snapshot, so step 432's exact
  library assertion still passes over the shared Lazy build.
- `CrystalPhysics` stays unopened in this file; its types are written qualified.

# Changelog

- 2026-11-15: Step 0435, AT-2's fourth part. The build creates the `AbsorbingTriclinic` study, opens the material and
  frees its fifteen components from `perturbedStart` within the harness's box. AT-1 and AT-2 share
  `studyDataOf` / `studyResidualAtStart`. Added the fact for the stored study, the axes, exact data identity and the
  start residual.
